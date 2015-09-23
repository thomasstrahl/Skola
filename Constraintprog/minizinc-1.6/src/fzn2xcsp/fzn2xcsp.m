%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2010-2012 The University of Melbourne and NICTA.
% See the file COPYING for license information.
%-----------------------------------------------------------------------------%
%
% Authors: Yue Li <liy5@ugrad.unimelb.edu.au>
%          Julien Fischer <juliensf@csse.unimelb.edu.au>
%          Sebastian Brand <sbrand@csse.unimelb.edu.au>
%
% A converter from FlatZinc to XCSP 2.1.
%
%-----------------------------------------------------------------------------%
% Issues:
%
%  - Boolean variables: XCSP does not seem to support non-integer variable
%    domains.  So the translation turns Boolean variables into 0/1 variables
%    and maps the Boolean operators to integer operators where needed.
%
%  - Degenerate models: XCSP seems to require at least one constraint and at
%    at least one variable.  Perhaps a dummy variable and constraint should be
%    emitted in case the Fzn model does not have them?  Similarly, a constraint
%    seems to need at least one variable in its scope.
%
%  - Inlining of parameters or defined variables: it is not currently checked
%    that the assigned value is within the declared domain.
%
%
%  - To do: 'cumulative' and other global constraints
%
%-----------------------------------------------------------------------------%

:- module fzn2xcsp.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module counter.
:- import_module compiler_common.
:- import_module fzn2xcsp_config.
:- import_module parse_stage.
:- import_module symbol_table.
:- import_module types_and_insts.
:- import_module zinc_ast.
:- import_module zinc_common.
:- import_module zinc_error.
:- import_module zinc_frontend.
:- import_module zinc_frontend2.
:- import_module zinc_pprint.
:- import_module map.
:- import_module set.

:- import_module bool.
:- import_module char.
:- import_module exception.
:- import_module getopt_io.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- use_module pprint.
:- import_module require.
:- import_module string.
:- import_module unit.
:- import_module univ.

%-----------------------------------------------------------------------------%

main(!IO) :-
    try_io(main_2, MainResult, !IO),
    (
        MainResult = succeeded(_)
    ;
        MainResult = exception(Excp),
        ( if univ_to_type(Excp, IO_Error)
        then handle_io_error(fzn2xcsp_program, IO_Error, !IO)
        else rethrow(MainResult)
        )
    ).

:- pred main_2(unit::out, io::di, io::uo) is det.

main_2(unit, !IO) :-
    io.command_line_arguments(Args, !IO),
    OptionOps = option_ops_multi(
        fzn2xcsp_short_option,
        fzn2xcsp_long_option,
        (pred(O::out, D::out) is multi :- fzn2xcsp_option_defaults(O, D))
    ),
    process_options(OptionOps, Args, NonOptionArgs, Result, !IO),
    (
        Result = error(Msg),
        bad_cmdline(Msg, !IO)
    ;
        Result = ok(OptionTable),

        % Set up the global state for the frontend.
        %
        lookup_bool_option(OptionTable, verbose, Verbose),
        lookup_bool_option(OptionTable, statistics, Statistics),
        VeryVerbose = no,   % Not currently used by fzn2xcsp.
        init_frontend_globals(Verbose, VeryVerbose, Statistics, !IO),

        % 1. Look for --version/--help, print message and stop if present.
        % 2. Look for exactly one model file.  Complain and stop if not
        %    present or more than one present.
        % 3. Check remaining options.  Complain and stop if something is
        %    wrong.  (Nb: 'process_options' above has already done a lot of
        %    checking, eg. for unknown option names.  This checking is
        %    checking for invalid combinations, option arguments, etc.)
        % 4. Compile the model.
        %
        % Nb: --version and --help go to stdout, errors go to stderr.
        %
        ( if lookup_bool_option(OptionTable, version, yes) then
            io.write_string(fzn2xcsp_version, !IO)

        else if lookup_bool_option(OptionTable, help, yes) then
            io.write_string(fzn2xcsp_usage, !IO)

        else
            (
                NonOptionArgs = [],
                bad_cmdline("no model file specified", !IO)
            ;
                NonOptionArgs = [ModelFileName],
                check_fzn2xcsp_options(OptionTable, ModelFileName,
                    ModelFileNameBase, OutputDestination, [], OptionErrors),
                (
                    % Errors in options found by us.
                    OptionErrors = [FirstOptionErrorMsg | _],
                    bad_cmdline(FirstOptionErrorMsg, !IO)
                ;
                    % We have a model file, we have acceptable options.
                    % Now we can actually convert the model!
                    OptionErrors = [],

                    % Process environment variables.
                    % The FlatZinc to XCSP converter does not currently
                    % use any environment variables.
                    process_fzn2xcsp_environment_vars(_, !IO),

                    AllSearchDirs = ["."],
                    do_fzn2xcsp_stages(AllSearchDirs, ModelFileName,
                        ModelFileNameBase, OutputDestination, OptionTable, !IO)
                )
            ;
                NonOptionArgs = [_, _ | _],
                bad_cmdline("more than one model file specified", !IO)
            )
        )
    ).

%-----------------------------------------------------------------------------%

:- pred check_fzn2xcsp_options(option_table(fzn2xcsp_option)::in, string::in,
    string::out, output_destination::out,
    list(string)::in, list(string)::out) is det.

check_fzn2xcsp_options(OptionTable, ModelFileName,
        ModelFileNameBase, OutputDest, !Errors) :-
    % Look at the model's file extension.
    % If it is not `.fzn' then emit an error.
    ( if string.remove_suffix(ModelFileName, ".fzn", ModelFileNameBase0),
         ModelFileNameBase0 \= ""     % Don't allow just ".fzn" as a name.
    then
        ModelFileNameBase = ModelFileNameBase0
    else
        ModelExtError = "file name must have the form `<file>.fzn'.",
        ModelFileNameBase = "", % Dummy value.
        !:Errors = !.Errors ++ [ModelExtError]
    ),

    lookup_bool_option(OptionTable, output_to_stdout, OutputToStdout),
    (
        OutputToStdout = yes,
        OutputDest = output_to_stdout
    ;
        OutputToStdout = no,
        OutputDest = output_to_file(ModelFileNameBase ++ ".xcsp")
    ),

    % Check stage options.
    %
    StageOptions = [
        dump_before,
        dump_after,
        pprint_before,
        pprint_after,
        stop_before,
        stop_after
    ],
    foldl(check_stage_option(OptionTable, fzn2xcsp_stage_names), StageOptions,
        !Errors).

%-----------------------------------------------------------------------------%

    % NOTE: if you add or remove stages please update the fzn2xcsp man
    % page in g12/zinc/man/fzn2xcsp.1.in.
    %
:- func fzn2xcsp_stage_names = list(string).

fzn2xcsp_stage_names = [
    % Nb: No "top-sorting" in FlatZinc.
    "parsing",
    "structure-checking",
    "type-inst-checking",
    "xcsp-conversion"
].

%-----------------------------------------------------------------------------%

:- pred do_fzn2xcsp_stages(list(string)::in, string::in,
    string::in, output_destination::in, option_table(fzn2xcsp_option)::in,
    io::di, io::uo) is det.

do_fzn2xcsp_stages(SearchDirs, ModelFileName, ModelFileBase,
        OutputDestination, OptionTable, !IO) :-
    Files = [zi_model_file(ModelFileName)],

    trace [io(!TIO)] (
        verbose("Processing file: " ++ ModelFileName, !TIO)
    ),

    DebugSpec = make_fzn2xcsp_debug_spec(OptionTable),
    Control = fzn2xcsp_control(OptionTable),

    % Parsing.  Nb: the pprint-before function will print nothing, because the
    % AST is empty at that point.
    EmptyAST = [],
    AllowIncs = no_includes,  % FlatZinc does not support include items.
    do_io_stage(fzn2xcsp_program, fzn2xcsp_stage_names,
        "parsing", lang_flatzinc, DebugSpec,
        parse_zinc_model(AllowIncs, Files, SearchDirs),
        pprint_ast(pp_lang_flatzinc(print_coercions)), io.write,
        pprint_ast(pp_lang_flatzinc(print_coercions)), dump_ast,
        yes(EmptyAST), MaybeItems1, !IO),

    PreEvaluationStageNameSuffix = "",
    do_analysis_stages(Control, fzn2xcsp_program,
        fzn2xcsp_stage_names, lang_flatzinc, instance_checking,
        PreEvaluationStageNameSuffix,
        DebugSpec, MaybeItems1, MaybeItemsAndSymTbl2, !IO),

    lookup_bool_option(OptionTable, output_comments,
        OutputCommentsOptVal),
    (
        OutputCommentsOptVal = yes,
        OutputComments = output_comments
    ;
        OutputCommentsOptVal = no,
        OutputComments = no_comments
    ),

    lookup_int_option(OptionTable, default_int_lb, DefaultIntLB),
    lookup_int_option(OptionTable, default_int_ub, DefaultIntUB),

    ConversionSettings = conversion_settings(
        DefaultIntLB,
        DefaultIntUB,
        OutputComments
    ),

    flatzinc_to_xcsp(ConversionSettings, MaybeItemsAndSymTbl2, ModelFileBase,
        OutputDestination, !IO).

%-----------------------------------------------------------------------------%

    % A variable of this type specifies whether or not FlatZinc items are
    % written as comments in the XCSP document.
    %
:- type output_comments
    --->    output_comments
    ;       no_comments.

    % A variable of this type specifies the number of columns by which a
    % given element tag must be indented.
    %
:- type col == int.

    % A variable of this type represents an XCSP attribute, specifying its
    % name and value.
    %
:- type attr
    --->    attr(
                name :: string,
                value :: string
            ).

    % A variable of this type represents a list of XCSP attributes.
    %
:- type attrs == list(attr).

    % A variable of this type specifies in which context types (of
    % variables) are being marked up, either in a predicate declaration
    % or in a variable declaration.
    %
:- type decl_context
    --->    pred_decl
    ;       var_decl.

    % A variable of this type is an argument of the predicate expr_to_xcsp
    % and specifies whether or not the start/end tags <expr>/</expr> are
    % written in the markup of the given expr. This allows expr_to_xcsp to
    % mark up the nested exprs in some kinds of exprs, e.g. the elements of
    % lit_set, which should not be marked up as expr elements.
    %
:- type write_expr_tags
    --->    write_expr_tags
    ;       no_expr_tags.

    % A variable of this type is an argument of the predicate
    % write_start_tag and specifies whether or not a newline character
    % is written after the output element start tag.
    %
:- type write_nl
    --->    write_nl
    ;       no_nl.


    % A variable of this type represents a XCSP domain, specifying its
    % name, number of values and values.
    %
:- type domain
    --->    domain(
                dname  :: dom_ref,
                nbVal  :: int,
                dvalue :: string
            ).

:- type dom_ref == int.

    % A variable of this type represents a list of XCSP domains.
    %
:- type domains == list(domain).

    % A variable of this type represents a XCSP variable, specifying its
    % name and related domain.
    %
:- type variable
    --->    variable(
                vname :: string,
                vdom  :: dom_ref
            ).

    % A variable of this type represents a list of XCSP variables.
    %
:- type variables == list(variable).

    % A value of this type is used in type predicate, classifies
    % which expression type is used in predicate representation.
    %
:- type exprT
    --->    functional
    ;       mathml
    ;       postfix
    ;       infix.

    % A value of this type represents an XCSP relation.
    %
:- type relation
    --->    relation(
                rel_ref :: int,
                arity   :: int,
                values  :: list(string)
            ).

:- type relations == list(relation).


    % A value this type represents an XCSP predicate.
    %
:- type xcsp_predicate
    --->    xcsp_predicate(
                pname  :: string,
                param1 :: string,
                expr   :: exprPair
            ).

:- type exprPair
    --->    exprPair(
                exprt   :: exprT,
                content :: string
            ).

:- type xcsp_predicates == list(xcsp_predicate).

    % A value of this type represents an XCSP constraint.
    %
:- type xcsp_constraint
    --->    xcsp_constraint(
                reference :: string,
                scope     :: scope,
                params    :: params
            ).

:- type scope == set(string).
:- type params == list(string).


    % A value of this type represents a list of XCSP constraints.
    %
:- type constraints == list(xcsp_constraint).

    % A map which has a variable name as key and domain as value.
    %
:- type dom_map == map(zinc_name, domain).

:- type arr_size_map == map(zinc_name, int).

:- type par_map == map(zinc_name, string).

:- type predicates == set(pair(fzn_predicate, int)).

    % This structure contains global settings that are used during the
    % conversion to XCSP.
    %
:- type conversion_settings
    --->    conversion_settings(
                    cs_default_int_lb  :: int,
                    cs_default_int_ub  :: int,
                    cs_output_comments :: output_comments
            ).

    % All supported FlatZinc built-in predicates and global
    % constraints.
    %
:- type fzn_predicate
    --->    bool_eq
    ;       bool_ne
    ;       bool_ge
    ;       bool_gt
    ;       bool_le
    ;       bool_lt
    ;       bool_eq_reif
    ;       bool_ne_reif
    ;       bool_ge_reif
    ;       bool_gt_reif
    ;       bool_le_reif
    ;       bool_lt_reif
    ;       int_eq
    ;       int_ne
    ;       int_ge
    ;       int_gt
    ;       int_le
    ;       int_lt
    ;       int_eq_reif
    ;       int_ne_reif
    ;       int_ge_reif
    ;       int_gt_reif
    ;       int_le_reif
    ;       int_lt_reif
    ;       int_lin_eq(int)
    ;       int_lin_eq_reif(int)
    ;       int_lin_ne(int)
    ;       int_lin_ne_reif(int)
    ;       int_lin_ge(int)
    ;       int_lin_ge_reif(int)
    ;       int_lin_gt(int)
    ;       int_lin_gt_reif(int)
    ;       int_lin_le(int)
    ;       int_lin_le_reif(int)
    ;       int_lin_lt(int)
    ;       int_lin_lt_reif(int)
    ;       int_negate
    ;       int_abs
    ;       int_plus
    ;       int_minus
    ;       int_times
    ;       int_div
    ;       int_mod
    ;       int_min
    ;       int_max
    ;       bool_not
    ;       bool_and
    ;       bool_or
    ;       bool_xor
    ;       bool_left_imp
    ;       bool_right_imp
    ;       array_bool_and(int)
    ;       array_bool_or(int)
    ;       bool_clause(int, int)
    ;       bool_clause_reif(int, int)
    ;       array_bool_element(int)
    ;       array_var_bool_element(int)
    ;       array_int_element(int)
    ;       array_var_int_element(int)
    ;       bool2int
    ;       all_different_int(int)
    ;       table_int.

%-----------------------------------------------------------------------------%

:- pred flatzinc_to_xcsp(conversion_settings::in,
    maybe({sast, symbol_table})::in, string::in, output_destination::in,
    io::di, io::uo) is det.

flatzinc_to_xcsp(_, no, _, _, !IO).
flatzinc_to_xcsp(ConvSet, yes({AST, _}), ModelFileBase,
        OutputDestination, !IO) :-
    (
        OutputDestination = output_to_file(OutputFileName),
        io.open_output(OutputFileName, OpenResult, !IO),
        (
            OpenResult = ok(OutputStream)
        ;
            OpenResult = error(IO_Error),
            throw(IO_Error)
        )
    ;
        OutputDestination = output_to_stdout,
        io.stdout_stream(OutputStream, !IO)
    ),
    flatzinc_ast_to_xcsp(OutputStream, ConvSet, AST, ModelFileBase, !IO),
    (
        OutputDestination = output_to_file(_),
        io.close_output(OutputStream, !IO)
    ;
        OutputDestination = output_to_stdout
    ).

    % Converts the given AST (the AST of a FlatZinc model) into XCSP,
    % writing the output to the specified file. The input string is
    % the name of the model, which is required in setting the value of
    % the "model" attribute in the root-element tag.
    %
    % NOTE: We assume that the original FlatZinc model is valid.
    %
:- pred flatzinc_ast_to_xcsp(io.text_output_stream::in,
    conversion_settings::in, sast::in, string::in, io::di, io::uo) is det.

flatzinc_ast_to_xcsp(File, ConvSet, AST, ModelFileBase, !IO) :-
    Indent = 0,
    some [!MI] (
        !:MI = model_info_init,
        % For XCSP we can ignore the solve item.
        AST = sast(Items, _SolveItem, _MaybeOutputItem),
        list.foldl(gather_xcsp_model(ConvSet), Items, !MI),
        post_process_model_info(!MI),

        write_start_tag(File, Indent, "instance", no, write_nl, !IO),
        write_presentation(File, Indent, ModelFileBase, !IO),
        write_domains(File, Indent, !.MI ^ mi_domain_map, !IO),
        write_variables(File, Indent, !.MI ^ mi_variables, !IO),
        write_relations(File, Indent, !.MI ^ mi_relations, !IO),
        write_predicates(File, Indent, !.MI ^ mi_predicates, !IO),
        write_constraints(File, Indent, !.MI ^ mi_assignment_constraints,
            !.MI ^ mi_regular_constraints, !IO),
        write_end_tag(File, yes(Indent), "instance", !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred post_process_model_info(model_info::in, model_info::out) is det.

post_process_model_info(!MI) :-
    % XXX Why do we reverse these?  Does the order matter?
    % XXX Why do we *not* reverse the assignment constraints?
    %
    !MI ^ mi_regular_constraints :=
        list.reverse(!.MI ^ mi_regular_constraints),
    !MI ^ mi_relations := list.reverse(!.MI ^ mi_relations),

    % If there are assignment constraints the we require equality
    % as a predicate.
    %
    AssignmentConstraints = !.MI ^ mi_assignment_constraints,
    (
        AssignmentConstraints = []
    ;
        AssignmentConstraints = [_ | _],
        IntEq = pair(int_eq, 2),
        model_info_update_predicates(IntEq, !MI)
    ).

%-----------------------------------------------------------------------------%
%
% XCSP output procedures
%

:- pred write_presentation(io.text_output_stream::in, int::in, string::in,
    io::di, io::uo) is det.

write_presentation(File, Indent, ModelFileBase, !IO) :-
    write_start_tag(File, indent(Indent), "presentation",
        yes([attr("name", ModelFileBase), attr("format", "XCSP 2.1")]),
        write_nl, !IO),
    insert_space(File, indent(indent(Indent)), !IO),
    io.format(File,
        "This is an XCSP 2.1 file generated from %s.fzn using fzn2xcsp\n",
        [s(ModelFileBase)], !IO),
    write_end_tag(File, yes(indent(Indent)), "presentation", !IO).

:- pred write_domains(io.text_output_stream::in,
    int::in, dom_map::in, io::di, io::uo) is det.

write_domains(File, Indent, DomMap, !IO) :-
    ( if map.is_empty(DomMap) then
        true
      else
        write_start_tag(File, indent(Indent), "domains",
            yes([attr("nbDomains", int_to_string(count(DomMap)))]),
            write_nl, !IO),
        list.foldl(write_domain(File, indent(Indent)), values(DomMap), !IO),
        write_end_tag(File,yes(indent(Indent)), "domains", !IO)
    ).

:- pred write_variables(io.text_output_stream::in,
    int::in, variables::in, io::di, io::uo) is det.

write_variables(File, Indent, Variables, !IO) :-
    (
        Variables = []
    ;
        Variables = [_ | _],
        write_start_tag(File, indent(Indent), "variables",
            yes([attr("nbVariables", int_to_string(length(Variables)))]),
            write_nl, !IO),
        list.foldr(write_variable(File, indent(Indent)), Variables, !IO),
        write_end_tag(File, yes(indent(Indent)), "variables", !IO)
    ).

:- pred write_relations(io.text_output_stream::in,
    int::in, relations::in, io::di, io::uo) is det.

write_relations(File, Indent, Relations, !IO) :-
    (
        Relations = [_ | _],
        list.length(Relations, NRelations),
        write_start_tag(File, indent(Indent), "relations",
            yes([attr("nbRelations", int_to_string(NRelations))]),
            write_nl, !IO),
        list.foldl(write_relation(File, indent(Indent)), Relations, !IO),
        write_end_tag(File, yes(indent(Indent)), "relations", !IO)
    ;
        Relations = []
    ).

:- pred write_predicates(io.text_output_stream::in,
    int::in, predicates::in, io::di, io::uo) is det.

write_predicates(File, Indent, Predicates, !IO) :-
    NPredicates = set.count(Predicates),
    ( if NPredicates > 0 then
        write_start_tag(File, indent(Indent), "predicates",
            yes([attr("nbPredicates", int_to_string(NPredicates))]),
            write_nl, !IO),
        set.fold(write_predicate(File, indent(Indent)), Predicates, !IO),
        write_end_tag(File, yes(indent(Indent)), "predicates", !IO)
      else
        true
    ).

:- pred write_constraints(io.text_output_stream::in, int::in,
    constraints::in, constraints::in, io::di, io::uo) is det.

write_constraints(File, Indent, AssignmentConstraints,
        RegularConstraints, !IO) :-
    NConstraints = length(AssignmentConstraints) +
        length(RegularConstraints),
    ( if NConstraints > 0 then
        write_start_tag(File, indent(Indent), "constraints",
            yes([attr("nbConstraints", int_to_string(NConstraints))]),
            write_nl, !IO),
        list.foldl2(write_constraint(File, indent(Indent), "A"),
            AssignmentConstraints, 1, _NAC, !IO),
        list.foldl2(write_constraint(File, indent(Indent), "C"),
            RegularConstraints, 1, _NRC, !IO),
        write_end_tag(File, yes(indent(Indent)), "constraints", !IO)
      else
        true
    ).

%-----------------------------------------------------------------------------%
%
% The model_info/0 structure
%

    % This structure is used to gather information about the FlatZinc model
    % instance as we traverse the FlatZinc AST.
    %
    % NOTE: the convention used in this program is that the variable name
    %       MI is used to refer to values of this type.
    %
:- type model_info
    --->    model_info(
                mi_variables               :: variables,
                mi_domain_map              :: dom_map,
                mi_parameter_map           :: par_map,
                mi_array_size_map          :: arr_size_map,
                mi_assignment_constraints  :: constraints,
                mi_regular_constraints     :: constraints,
                mi_relations               :: relations,
                mi_predicates              :: predicates
            ).

    % This function returns an empty model_info/0 structure.
    %
:- func model_info_init = model_info.

model_info_init = ModelInfo :-
    Variables = [],
    map.init(DomainMap),
    map.init(ParameterMap),
    map.init(ArraySizeMap),
    AssignConstraints = [],
    RegularConstraints = [],
    Relations = [],
    set.init(Predicates),
    ModelInfo = model_info(
        Variables,
        DomainMap,
        ParameterMap,
        ArraySizeMap,
        AssignConstraints,
        RegularConstraints,
        Relations,
        Predicates
    ).

%-----------------------------------------------------------------------------%
%
% Field update predicates for the model_info/0 structure
%

:- pred model_info_update_variables(variables::in,
    model_info::in, model_info::out) is det.

model_info_update_variables(Vars, !MI) :-
    !MI ^ mi_variables := Vars ++ !.MI ^ mi_variables.

:- pred model_info_update_regular_constraints(constraints::in,
    model_info::in, model_info::out) is det.

model_info_update_regular_constraints(Cs, !MI) :-
    !MI ^ mi_regular_constraints := Cs ++ !.MI ^ mi_regular_constraints.

:- pred model_info_update_parameter_map(string::in, string::in,
    model_info::in, model_info::out) is det.

:- pred model_info_update_assignment_constraints(constraints::in,
    model_info::in, model_info::out) is det.

model_info_update_assignment_constraints(Cs, !MI) :-
    !MI ^ mi_assignment_constraints := Cs ++ !.MI ^ mi_assignment_constraints.

model_info_update_parameter_map(Name, Value, !MI) :-
    !MI ^ mi_parameter_map :=
        map.det_insert(!.MI ^ mi_parameter_map, Name, Value).

:- pred model_info_update_domain_map(string::in, domain::in,
    model_info::in, model_info::out) is det.

model_info_update_domain_map(Values, Domain, !MI) :-
    !MI ^ mi_domain_map :=
        map.det_insert(!.MI ^ mi_domain_map, Values, Domain).

:- pred model_info_update_predicates(pair(fzn_predicate, int)::in,
    model_info::in, model_info::out) is det.

model_info_update_predicates(P, !MI) :-
    !MI ^ mi_predicates := set.insert(!.MI ^ mi_predicates, P).

:- pred model_info_update_relations(relation::in,
    model_info::in, model_info::out) is det.

model_info_update_relations(R, !MI) :-
    !MI ^ mi_relations := [R | !.MI ^ mi_relations].

:- pred model_info_update_array_size_map(zinc_name::in, int::in,
    model_info::in, model_info::out) is det.

model_info_update_array_size_map(Name, UB, !MI) :-
    !MI ^ mi_array_size_map :=
        map.det_insert(!.MI ^ mi_array_size_map, Name, UB).

%-----------------------------------------------------------------------------%
%
% AST traversal
%

:- pred gather_xcsp_model(conversion_settings::in, item::in,
    model_info::in, model_info::out) is det.

gather_xcsp_model(ConvSet, Item, !MI) :-
    Item = item(RawItem, _),
    (
        RawItem = var_decl_item(VarTIExpr, VarName, _Ann, MaybeValue),
        maybe_update_array_size_map(VarName, VarTIExpr, !MI),
        create_variable(ConvSet, VarTIExpr, VarName, MaybeValue, !MI)
    ;
        RawItem = constraint_item(Expr),
        create_constraint(Expr, !MI)
    ;
        RawItem = predfunc_item(_, _, _, _, _, _)
    ;
        RawItem = solve_item(_, _)
    ;
        ( RawItem = assign_item(_, _)
        ; RawItem = enum_item(_, _, _)
        ; RawItem = type_inst_syn_item(_, _, _)
        ; RawItem = annotation_item(_, _)
        ; RawItem = output_item(_)
        ),
        unexpected($pred ++ ": non-FlatZinc item: " ++ string(RawItem))
    ).

%-----------------------------------------------------------------------------%

    % If a variable is an array, record its size in the model_info structure.
    %
:- pred maybe_update_array_size_map(zinc_name::in, ti_expr::in,
    model_info::in, model_info::out) is det.

maybe_update_array_size_map(Name, TIExpr, !MI) :-
    TIExpr = ti_expr(RawTIExpr, _),
    (
        RawTIExpr = raw_ti_expr(_, BaseTIExpr),
        (
            BaseTIExpr =  bte_array_of(IndexExprs, _, _),
            ( if   ti_exprs_are_fzn_index_set(IndexExprs, UB)
              then model_info_update_array_size_map(Name, UB, !MI)
              else unexpected($pred ++ ": bad FlatZinc array index type-inst")
            )
        ;
            ( BaseTIExpr = bte_int
            ; BaseTIExpr = bte_range_expr_as_type_expr(_, _)
            ; BaseTIExpr = bte_ident(_)
            ; BaseTIExpr = bte_bool
            ; BaseTIExpr = bte_float
            ; BaseTIExpr = bte_set_of(_)
            ; BaseTIExpr = bte_typeinst_var(_)
            ; BaseTIExpr = bte_any_typeinst_var(_)
            ; BaseTIExpr = bte_tuple_of(_)
            ; BaseTIExpr = bte_record_of(_)
            ; BaseTIExpr = bte_set_expr_as_type_expr(_)
            ; BaseTIExpr = bte_string
            ; BaseTIExpr = bte_op(_)
            ; BaseTIExpr = bte_ann
            ; BaseTIExpr = bte_bottom
            ; BaseTIExpr = bte_error
            )
        )
    ;
        RawTIExpr = constrained_raw_ti_expr(_, _, _),
        unexpected($pred ++ ": constrained type-inst in FlatZinc: "
            ++ string(RawTIExpr))
    ).

:- pred ti_exprs_are_fzn_index_set(ti_exprs::in, int::out) is semidet.

ti_exprs_are_fzn_index_set(Exprs, UB) :-
    Exprs = [Expr],
    Expr = ti_expr(RawExpr, _),
    RawExpr = raw_ti_expr(_, RangeBaseTIExpr),
    RangeBaseTIExpr = bte_range_expr_as_type_expr(LBExpr, UBExpr),
    LBExpr = expr(lit(int(1)), _, _),
    UBExpr = expr(lit(int(UB)), _, _).

%-----------------------------------------------------------------------------%
%
% Variables and domains
%

:- pred create_variable(conversion_settings::in, ti_expr::in, zinc_name::in,
    var_decl_assignment::in, model_info::in, model_info::out) is det.

create_variable(ConvSet, VarTI, VarName, MaybeVal, !MI) :-
    VarTI = ti_expr(RawTI, _),
    (
        RawTI = raw_ti_expr(Inst, TItail),
        (
            % Declaration of a single variable or parameter.
            %
            ( TItail = bte_bool
            ; TItail = bte_int
            ; TItail = bte_range_expr_as_type_expr(_,_)
            ; TItail = bte_set_expr_as_type_expr(_)
            ),
            build_single_variable(ConvSet, TItail, VarName, MaybeVal, !MI)
        ;
            % Declaration of an array.
            %
            TItail = bte_array_of(IndexTIs, ti_expr(RawElemTI, _), _),
            build_array(ConvSet, IndexTIs, RawElemTI, VarName, MaybeVal, !MI)
        ;
            % Ignore float parameters -- they may legitimately (although
            % redundantly) be present.  If they are used then we should already
            % have got an error message from the frontend concerning whatever
            % uses them.
            TItail = bte_float,
            (
                Inst = par
            ;
                Inst = var,
                % This should have been caught by the frontend.
                unexpected($pred, "declaration of type var float: " ++ VarName)
            )
        ;
            ( TItail = bte_ident(_)
            ; TItail = bte_set_of(_)
            ; TItail = bte_typeinst_var(_)
            ; TItail = bte_any_typeinst_var(_)
            ; TItail = bte_tuple_of(_)
            ; TItail = bte_record_of(_)
            ; TItail = bte_string
            ; TItail = bte_op(_)
            ; TItail = bte_ann
            ; TItail = bte_bottom
            ; TItail = bte_error
            ),
            unexpected($pred, "unexpected variable type: " ++ string(TItail))
        )
    ;
        RawTI = constrained_raw_ti_expr(_,_,_),
        unexpected($pred, "constrained type: " ++ string(RawTI))
    ).

:- pred type_to_domref(conversion_settings::in, base_ti_expr_tail::in,
    dom_ref::out, model_info::in, model_info::out) is det.

type_to_domref(ConversionSettings, TItail, DomRef, !MI) :-
    (
        TItail = bte_bool,
        DomSize = 2,
        DomValues = False ++ " " ++ True,
        fzn_literal_to_xcsp(bool(no)) = False,
        fzn_literal_to_xcsp(bool(yes)) = True
    ;
        (
            TItail = bte_int,
            ConversionSettings^cs_default_int_lb = LB,
            ConversionSettings^cs_default_int_ub = UB
        ;
            TItail = bte_range_expr_as_type_expr(ExprLB, ExprUB),
            expr_to_int(ExprLB) = LB,
            expr_to_int(ExprUB) = UB
        ),
        DomSize = UB - LB + 1,
        DomValues = string.format("%d..%d", [i(LB), i(UB)])
    ;
        TItail = bte_set_expr_as_type_expr(Exprs),
        DomSize = length(Exprs),
        DomValues = fzn_set_to_xcsp(Exprs)
    ;
        ( TItail = bte_float
        ; TItail = bte_array_of(_,_,_)
        ; TItail = bte_record_of(_)
        ; TItail = bte_set_of(_)
        ; TItail = bte_tuple_of(_)
        ; TItail = bte_string
        ; TItail = bte_ident(_)
        ; TItail = bte_typeinst_var(_)
        ; TItail = bte_any_typeinst_var(_)
        ; TItail = bte_op(_)
        ; TItail = bte_ann
        ; TItail = bte_bottom
        ; TItail = bte_error
        ),
        unexpected($pred ++ ": unsupported type: " ++ string(TItail))
    ),
    ( if map.search(!.MI ^ mi_domain_map, DomValues, DomainFound) then
        DomRef = DomainFound^dname
    else
        DomRef = map.count(!.MI ^ mi_domain_map),
        Domain = domain(DomRef, DomSize, DomValues),
        model_info_update_domain_map(DomValues, Domain, !MI)
    ).

%-----------------------------------------------------------------------------%

:- pred build_single_variable(conversion_settings::in, base_ti_expr_tail::in,
    zinc_name::in, var_decl_assignment::in, model_info::in, model_info::out) is det.

build_single_variable(ConvSet, TItail, VarName, MaybeVal, !MI) :-
    (
        % No assignment.
        MaybeVal = no_assignment,
        type_to_domref(ConvSet, TItail, DomRef, !MI),
        Variables = [variable(VarName, DomRef)],
        model_info_update_variables(Variables, !MI)
    ;
        % Assignment.
        MaybeVal = rhs_assignment(expr(RawExpr0, _, _)),
        RawExpr = strip_coercion(RawExpr0),
        (
            % Assigned value is a constant -- add the identifier to the
            % parameter map.
            %
            RawExpr = lit(Lit),
            Value = fzn_literal_to_xcsp(Lit),
            model_info_update_parameter_map(VarName, Value, !MI)
        ;
            % Variable/parameter assignment.
            %
            (
                RawExpr = ident(Id),
                ValName = Id ^ id_name
            ;
                RawExpr = array_access(Arr, Idx),
                ValName = array_access_to_xcsp(Arr, Idx)
            ),
            ( if map.search(!.MI ^ mi_parameter_map, ValName, Value) then
                % Parameter
                % Add the identifier to the parameter map.
                %
                model_info_update_parameter_map(VarName, Value, !MI)
            else
                % Variable
                % Declare the identifier (which must be a variable
                % itself) and create an equality constraint.
                %
                type_to_domref(ConvSet, TItail, DomRef, !MI),
                Variables = [variable(VarName, DomRef)],
                model_info_update_variables(Variables, !MI),

                Scope = ConsParams,
                ConsParams = [VarName, ValName],
                Ref = "int_eq",
                Constraints = [xcsp_constraint(Ref, set(Scope), ConsParams)],
                model_info_update_assignment_constraints(Constraints, !MI)
            )
        ;
            ( RawExpr = app(_,_,_,_)
            ; RawExpr = coerce(_,_,_)
            ; RawExpr = lit_set(_)
            ; RawExpr = lit_simple_array(_)
            ; RawExpr = lit_indexed_array(_)
            ; RawExpr = lit_tuple(_)
            ; RawExpr = lit_record(_)
            ; RawExpr = lit_nonflat_enum(_,_)
            ; RawExpr = lit_ann(_,_)
            ; RawExpr = comprehension(_,_,_)
            ; RawExpr = tuple_access(_,_)
            ; RawExpr = record_access(_,_)
            ; RawExpr = anon_var
            ; RawExpr = if_then_else(_,_,_)
            ; RawExpr = case(_,_)
            ; RawExpr = let(_,_)
            ; RawExpr = lit_nonflat_enum_simple(_,_)
            ),
            unexpected($pred, "unexpected assigned value: " ++ string(RawExpr))
        )
    ;
        MaybeVal = separate_assignment(_, _),
        unexpected($pred, "assign item in FlatZinc?") 
    ).

%-----------------------------------------------------------------------------%

:- pred build_array(conversion_settings::in, ti_exprs::in, raw_ti_expr::in,
    zinc_name::in, var_decl_assignment::in, model_info::in, model_info::out)
    is det.

build_array(ConvSet, IndexTIEs, RawElemTIE, VarName, MaybeVal, !MI) :-
    % Declared array element type and instantiation
    (
        RawElemTIE = raw_ti_expr(ElemInst, ElemBaseTIE)
    ;
        RawElemTIE = constrained_raw_ti_expr(_, _, _),
        unexpected($pred, "constrained type: " ++ string(RawElemTIE))
    ),

    % Array length
    ( if ti_exprs_are_fzn_index_set(IndexTIEs, ArrLen0)
    then ArrLen = ArrLen0
    else unexpected($pred, "non-FlatZinc array index: " ++ string(IndexTIEs))
    ),
    (
        % No assignment -- just declare each array element as a variable.
        %
        MaybeVal = no_assignment,
        type_to_domref(ConvSet, ElemBaseTIE, DomRef, !MI),
        DeclElem = (pred(I::in, !.Vs::in, !:Vs::out) is det :-
            VarName_I = array_elem_to_xcsp_var(VarName, I),
            Var_I = variable(VarName_I, DomRef),
            !:Vs = [Var_I | !.Vs]
        ),
        int.fold_up(DeclElem, 1, ArrLen, [], Variables),
        model_info_update_variables(Variables, !MI)
    ;
        % Assignment
        MaybeVal = rhs_assignment(expr(RawAssignExpr, _, _)),

        % Value must be an array literal of scalar constants.
        ( if RawAssignExpr = lit_simple_array(AssignElemExprs0)
        then AssignElemExprs = AssignElemExprs0
        else unexpected($pred, "non-literal array value")
        ),
        (
            % Parameter array.
            ElemInst = par,
            
            % Add it to the parameter map.
            AddParameter = (pred(Expr_I::in, I::in, (I+1)::out,
                    !.MI::in, !:MI::out) is det :-
                VarName_I = array_elem_to_xcsp_var(VarName, I),
                Value = fzn_lit_expr_to_xcsp(Expr_I),
                model_info_update_parameter_map(VarName_I, Value, !MI)
            ),
            list.foldl2(AddParameter, AssignElemExprs, 1, _, !MI)
        ;
            % Array of variables -- declare each array element as a variable
            % and create an equality constraint between each newly created
            % variable and its assignment.
            ElemInst = var,
            type_to_domref(ConvSet, ElemBaseTIE, DomRef, !MI),
            list.foldl2(add_var_and_equality_constraint(VarName, DomRef),
                AssignElemExprs, 1, _, !MI)
        )
    ;
        MaybeVal = separate_assignment(_, _),
        unexpected($pred, "assign item in FlatZinc?")
    ).

:- pred add_var_and_equality_constraint(zinc_name::in,
    dom_ref::in, expr::in, int::in, int::out,
    model_info::in, model_info::out) is det.

add_var_and_equality_constraint(VarName, DomRef, AssignExpr, I, I + 1,
        !MI) :-
    VarName_I = array_elem_to_xcsp_var(VarName, I),
    Var_I = variable(VarName_I, DomRef),
    model_info_update_variables([Var_I], !MI),

    AssignExpr = expr(RawAssignExpr0, _,_),
    RawAssignExpr = strip_coercion(RawAssignExpr0),
    (
        % Scalar constants
        RawAssignExpr = lit(Lit),
        Value = fzn_literal_to_xcsp(Lit),
        Scope = [VarName_I],
        ConsParams = [VarName_I, Value]
    ;
        % Variable/parameter
        (
            RawAssignExpr = ident(Id),
            ValName = Id ^ id_name
        ;
            RawAssignExpr = array_access(Arr, Idx),
            ValName = array_access_to_xcsp(Arr, Idx)
        ),
        ( if map.search(!.MI ^ mi_parameter_map, ValName, Value) then
            Scope = [VarName_I],
            ConsParams = [VarName_I, Value]
        else
            Scope = ConsParams,
            ConsParams = [VarName_I, ValName]
        )
    ;
        ( RawAssignExpr = app(_,_,_,_)
        ; RawAssignExpr = coerce(_,_,_)
        ; RawAssignExpr = lit_simple_array(_)
        ; RawAssignExpr = lit_indexed_array(_)
        ; RawAssignExpr = lit_set(_)
        ; RawAssignExpr = lit_tuple(_)
        ; RawAssignExpr = lit_record(_)
        ; RawAssignExpr = lit_nonflat_enum(_,_)
        ; RawAssignExpr = lit_ann(_,_)
        ; RawAssignExpr = comprehension(_,_,_)
        ; RawAssignExpr = tuple_access(_,_)
        ; RawAssignExpr = record_access(_,_)
        ; RawAssignExpr = anon_var
        ; RawAssignExpr = if_then_else(_,_,_)
        ; RawAssignExpr = case(_,_)
        ; RawAssignExpr = let(_,_)
        ; RawAssignExpr = lit_nonflat_enum_simple(_,_)
        ),
        unexpected($pred, "unexpected assigned value: "
            ++ string(RawAssignExpr))
    ),
    Ref = "int_eq",
    Constr_I = xcsp_constraint(Ref, set(Scope), ConsParams),
    model_info_update_assignment_constraints([Constr_I], !MI).

%-----------------------------------------------------------------------------%
%
% Constraints and predicates
%

:- pred create_constraint(expr::in, model_info::in, model_info::out) is det.

create_constraint(Expr, !MI) :-
    Expr = expr(RawExpr, _, _),
    ArraySizeMap = !.MI ^ mi_array_size_map,
    ( if RawExpr = app(AppId, _, _, Args0) then
        PredName = AppId ^ id_name,
        Args = Args0,
        ( if is_fzn_pred(PredName, Args, ArraySizeMap, Pred0, Arity0)
        then
            Pred = Pred0,
            Arity = Arity0
        else
            % Unsupported constraints should have been reported
            % by the frontend, so we shouldn't encounter them here.
            %
            unexpected($pred, "unsupported constraint: " ++ PredName)
        )
    else
        unexpected($pred, "constraint is not a predicate app: " ++
            string(RawExpr))
    ),
    ( if is_global_constr(Pred, Args, !MI) then
        true
    else
        list.foldl2(build_scope_and_params(!.MI),
            Args, set.init, Scope, [], ConsParams0),
        model_info_update_predicates(pair(Pred, Arity), !MI),
        Ref = string(Pred),
        ConsParams = reverse(ConsParams0),
        Constraint = xcsp_constraint(Ref, Scope, ConsParams),
        model_info_update_regular_constraints([Constraint], !MI)
    ).

:- pred build_scope_and_params(model_info::in, expr::in,
    scope::in, scope::out, params::in, params::out) is det.

build_scope_and_params(MI, Expr, !Scope, !ConsParams) :-
    ( if    expr_to_xcsp(MI, Expr, _IsScalar, Vals, !Scope)
      then  list.append(reverse(Vals), !ConsParams)
      else  unexpected($pred ++ ": unexpected expression: " ++ string(Expr))
    ).

:- pred is_global_constr(fzn_predicate::in, exprs::in,
    model_info::in, model_info::out) is semidet.

is_global_constr(Pred, ArgEs, !MI) :-
    (
        ( Pred = array_bool_element(_)
        ; Pred = array_int_element(_)
        ; Pred = array_var_bool_element(_)
        ; Pred = array_var_int_element(_)
        ),
        build_element_constraint(ArgEs, !MI)
    ;
        Pred = all_different_int(_),
        build_all_different_constraint(ArgEs, !MI)
    ;
        Pred = table_int,
        build_table_constraint(ArgEs, !MI)
    ;
        ( Pred = int_lin_eq(_), Rel = "<eq/>"
        ; Pred = int_lin_ge(_), Rel = "<ge/>"
        ; Pred = int_lin_gt(_), Rel = "<gt/>"
        ; Pred = int_lin_le(_), Rel = "<le/>"
        ; Pred = int_lin_lt(_), Rel = "<lt/>"
        ; Pred = int_lin_ne(_), Rel = "<ne/>"
        ),
        build_linear_constraint(Rel, ArgEs, !MI)
    ).

%----------------------------------------------------------------------------%
%
% Build an XCSP element constraint
%

:- pred build_element_constraint(exprs::in,
    model_info::in, model_info::out) is det.

build_element_constraint(ArgEs, !MI) :-
    ( if
        ArgEs = [Expr_Idx, Expr_Arr, Expr_Res],
        scalar_expr_to_xcsp(!.MI, Expr_Idx, Idx, set.init, Scope0),
        array_expr_to_xcsp(!.MI, Expr_Arr, Arr, Scope0, Scope1),
        scalar_expr_to_xcsp(!.MI, Expr_Res, Res, Scope1, Scope2)
    then
        ConsParams = [Idx, xcsp_list(Arr), Res],
        Scope = Scope2,
        Ref = "global:element"
    else
        unexpected($pred ++ ": unexpected argument list")
    ),
    Constraint = xcsp_constraint(Ref, Scope, ConsParams),
    model_info_update_regular_constraints([Constraint], !MI).

%----------------------------------------------------------------------------%
%
% Build an XCSP all_different constraint
%

:- pred build_all_different_constraint(exprs::in,
    model_info::in, model_info::out) is det.

build_all_different_constraint(ArgEs, !MI) :-
    ( if ArgEs = [Expr_Arr],
        array_expr_to_xcsp(!.MI, Expr_Arr, Arr, set.init, Scope1)
    then
        ConsParams = [xcsp_list(Arr)],
        Scope = Scope1,
        Ref = "global:allDifferent"
    else
        unexpected($pred ++ ": unexpected argument list")
    ),
    Constraint = xcsp_constraint(Ref, Scope, ConsParams),
    model_info_update_regular_constraints([Constraint], !MI).

%----------------------------------------------------------------------------%
%
% Build an XCSP table constraint
%

:- pred build_table_constraint(exprs::in,
    model_info::in, model_info::out) is det.

build_table_constraint(ArgEs, !MI) :-
    ( if ArgEs = [Expr_Vars, Expr_Values],
        array_expr_to_xcsp(!.MI, Expr_Vars, Vars, set.init, _Scope1),
        array_expr_to_xcsp(!.MI, Expr_Values, Values, set.init, _Scope2)
    then
        Arity = length(Vars),
        ( if list.find_first_match(
                (pred(relation(_, Arity2, Values2)::in) is semidet :-
                    Arity2 = Arity, Values2 = Values),
                !.MI ^ mi_relations, relation(RelRef2, _,_))
        then
            RelRef = RelRef2
        else
            RelRef = length(!.MI ^ mi_relations) + 1,
            Relation = relation(RelRef, Arity, Values),
            model_info_update_relations(Relation, !MI)
        ),

        ConsParams = Vars,

        % The following two assignments specify that
        % it's a constraint in extension!
        Scope = set.init,
        Ref = rel_ref_to_xcsp(RelRef)
    else
        unexpected($pred ++ ": unexpected argument list")
    ),
    Constraint = xcsp_constraint(Ref, Scope, ConsParams),
    model_info_update_regular_constraints([Constraint], !MI).

%----------------------------------------------------------------------------%
%
% Build an XCSP linear constraint (using the weightedSum global)
%

:- pred build_linear_constraint(string::in, exprs::in,
    model_info::in, model_info::out) is det.

build_linear_constraint(RelOp, ArgEs, !MI) :-
    ( if ArgEs = [Expr_Coeffs, Expr_Vars, Expr_Const],
        array_expr_to_xcsp(!.MI, Expr_Coeffs, Coeffs, set.init, Scope0),
        array_expr_to_xcsp(!.MI, Expr_Vars, Vars, Scope0, Scope1),
        scalar_expr_to_xcsp(!.MI, Expr_Const, Const, Scope1, Scope2)
    then
        Arr = list.map_corresponding(xcsp_pair, Coeffs, Vars),
        ConsParams = [xcsp_list(Arr), RelOp, Const],
        Scope = Scope2,
        Ref = "global:weightedSum"
    else
        unexpected($pred ++ ": unexpected argument list")
    ),
    Constraint = xcsp_constraint(Ref, Scope, ConsParams),
    model_info_update_regular_constraints([Constraint], !MI).

%----------------------------------------------------------------------------%
%
% Conversions to XCSP strings
%

:- pred scalar_expr_to_xcsp(model_info::in, expr::in,
    string::out, scope::in, scope::out) is semidet.

scalar_expr_to_xcsp(MI, Expr, Val, !Scope) :-
    expr_to_xcsp(MI, Expr, yes, [Val], !Scope).

:- pred array_expr_to_xcsp(model_info::in, expr::in,
    list(string)::out, scope::in, scope::out) is semidet.

array_expr_to_xcsp(MI, Expr, Vals, !Scope) :-
    expr_to_xcsp(MI, Expr, no, Vals, !Scope).

:- pred expr_to_xcsp(model_info::in, expr::in, bool::out,
    list(string)::out, scope::in, scope::out) is semidet.

expr_to_xcsp(MI, Expr, IsScalar, Vals, !Scope) :-
    Expr = expr(RawExpr, _, _),
    ParameterMap = MI ^ mi_parameter_map,
    (
        RawExpr = lit(Lit),
        IsScalar = yes,
        fzn_literal_to_xcsp(Lit) = Val,
        Vals = [Val]
    ;
        RawExpr = lit_simple_array(Exprs),
        IsScalar = no,
        list.map_foldl(scalar_expr_to_xcsp(MI), Exprs, Vals, !Scope)
    ;
        RawExpr = array_access(expr(ident(id_global(Name)),_,_),
                    [expr(lit(int(Index)),_,_)|_]),
        IsScalar = yes,
        VarName = array_elem_to_xcsp_var(Name, Index),
        value_if_param(ParameterMap, VarName, Val, !Scope),
        Vals = [Val]
    ;
        RawExpr = ident(id_global(VarName)),
        ( if map.search(MI ^ mi_array_size_map, VarName, N) then
            IsScalar = no,
            int.fold_down2((pred(I::in, Vs::in, [Val | Vs]::out,
                !.Scope::in, !:Scope::out ) is det :-
                    array_elem_to_xcsp_var(VarName, I) = VarName_I,
                    value_if_param(ParameterMap, VarName_I, Val, !Scope)
                ), 1, N, [], Vals, !Scope)
        else
            IsScalar = yes,
            value_if_param(ParameterMap, VarName, Val, !Scope),
            Vals = [Val]
        )
    ;
        RawExpr = coerce(_,_,Expr1),
        expr_to_xcsp(MI, Expr1, IsScalar, Vals, !Scope)
    ).


:- pred value_if_param(par_map::in, string::in, string::out,
    scope::in, scope::out) is det.

value_if_param(ParMap, VarName, Res, !Scope) :-
    ( if map.search(ParMap, VarName, Found) then
        Res = Found
    else
        Res = VarName,
        set.insert(VarName, !Scope)
    ).

:- func expr_to_int(expr) = int.

expr_to_int(Expr) = I :-
    ( if    Expr = expr(lit(int(I0)), _, _)
      then  I = I0
      else  unexpected($pred ++ ": not an integer: " ++ string(Expr))
    ).

:- func strip_coercion(raw_expr) = raw_expr.

strip_coercion(Expr) =
    ( if    Expr = coerce(_,_, expr(CoercedExpr,_,_))
      then  CoercedExpr
      else  Expr
    ).

:- func array_access_to_xcsp(expr, exprs) = string.

array_access_to_xcsp(Arr, Idx) = Name :-
    ( if Arr = expr(ident(id_global(VarName)),_,_),
         Idx = [expr(lit(int(Index)),_,_) | _]
    then
        Name = array_elem_to_xcsp_var(VarName, Index)
    else
        unexpected($pred ++ ": unexpected value: " ++ string(Arr))
    ).

:- func array_elem_to_xcsp_var(string, int) = string.

array_elem_to_xcsp_var(BaseName, ElemNum) =
    string.format("%s____%d", [s(BaseName), i(ElemNum)]).

:- func xcsp_pair(string, string) = string.

xcsp_pair(Str1, Str2) = string.format("{ %s %s }", [s(Str1), s(Str2)]).

:- func xcsp_list(list(string)) = string.

xcsp_list(L) = "[ " ++ string.join_list(" ", L) ++ " ]".

    % Output a literal as a string.
    %
:- func fzn_lit_expr_to_xcsp(expr) = string.

fzn_lit_expr_to_xcsp(Expr) = Str :-
    ( if Expr = expr(lit(Scalar),_,_) then
        fzn_literal_to_xcsp(Scalar) = Str
    else if Expr = expr(lit_set(SetElems),_,_) then
        fzn_set_to_xcsp(SetElems) = Str
    else if Expr = expr(coerce(_,_, Expr2),_,_) then
        fzn_lit_expr_to_xcsp(Expr2) = Str
    else unexpected($pred ++ ": unexpected non-literal: " ++ string(Expr))
    ).

:- func fzn_literal_to_xcsp(literal) = string.

fzn_literal_to_xcsp(Literal) = Str :-
    (
        Literal = bool(yes),
        Str = "1"
    ;
        Literal = bool(no),
        Str = "0"
    ;
        Literal = string(Str)
    ;
        Literal = int(C),
        Str = int_to_string(C)
    ;
        Literal = floatstr(Str)
    ).

:- func fzn_expr_to_lit(expr) = string.

fzn_expr_to_lit(Expr) = Str :-
    ( if    Expr = expr(lit(Lit), _, _)
      then  Str = fzn_literal_to_xcsp(Lit)
      else  unexpected($pred ++ ": unexpected non-literal: " ++ string(Expr))
    ).

:- func fzn_set_to_xcsp(exprs) = string is det.

fzn_set_to_xcsp(Exprs) =
    string.join_list(" ", list.map(fzn_expr_to_lit, Exprs)).

:- func xcsp_pred_params(string, int) = string.

xcsp_pred_params(Type, N) = Params :-
    int.fold_down((pred(I::in, L::in, [S | L]::out) is det :-
            S = string.format("%s X%d", [s(Type), i(I)])
        ), 0, N-1, [], Params1),
    string.join_list(" ", Params1) = Params.

:- func fzn_pred_to_xcsp(fzn_predicate, int) = xcsp_predicate.

fzn_pred_to_xcsp(FznPredicate, Arity) = XCSP_Predicate :-
    ( if is_array_pred(FznPredicate, Def1, Params1) then
        Params = Params1,
        Def = Def1
    else
        % if is_int_reif_pred(FznPredicate) then ...
        % if is_bool_pred(FznPredicate) then ...
        Params = xcsp_pred_params("int", Arity),
        Def = fzn_pred_definition(FznPredicate)
    ),
    XCSP_Predicate = xcsp_predicate(string(FznPredicate),
                      Params, exprPair(functional, Def)).


    % Check if the input fzn predicate is an array related constraint.
    %
:- pred is_array_pred(fzn_predicate::in, string::out, string::out) is semidet.

is_array_pred(Pred, Def, Params) :-
    (   Pred = array_bool_and(N),
        build_sum(N, 0) = Def1,
        Def = string.format("iff(eq(%s,%d),eq(X%d,1))",
            [s(Def1), i(N), i(N)]),
        Params = xcsp_pred_params("int", N+1)
    ;
        Pred = array_bool_or(N),
        build_sum(N, 0) = Def1,
        Def = string.format("iff(ge(%s,1),eq(X%d,1))",
            [s(Def1), i(N)]),
        Params = xcsp_pred_params("int", N+1)
    ;
        Pred = bool_clause(N1,N2),
        build_sum(N1, 0) = Pos,
        build_sum(N2, N1) = Neg,
        Def = string.format("ge(sub(%s,%s),%d)",
            [s(Pos), s(Neg), i(1 - N2)]),
        Params = xcsp_pred_params("int", N1+N2)
    ;
        Pred = bool_clause_reif(N1,N2),
        build_sum(N1, 0) = Pos,
        build_sum(N2, N1) = Neg,
        Def = string.format("iff(ge(sub(%s,%s),%d),eq(X%d,1))",
            [s(Pos), s(Neg), i(1-N2), i(N1+N2)]),
        Params = xcsp_pred_params("int", N1+N2)
    ;
        (   Pred = int_lin_eq_reif(N), Rel = "eq"
        ;   Pred = int_lin_ge_reif(N), Rel = "ge"
        ;   Pred = int_lin_gt_reif(N), Rel = "gt"
        ;   Pred = int_lin_le_reif(N), Rel = "le"
        ;   Pred = int_lin_lt_reif(N), Rel = "lt"
        ;   Pred = int_lin_ne_reif(N), Rel = "ne"
        ),
        build_lin_reif_func_rep(N, N-1) = RawPred,
        Def = string.format("iff(%s(%s,X%d),eq(X%d,1))",
            [s(Rel), s(RawPred), i(2*N), i(2*N+1)]),
        Params = xcsp_pred_params("int", 2*N+2)
    ).


:- func build_sum(int, int) = string.

build_sum(N, Start) = Def :-
    ( if N < 1 then
          Def = "0"
      else if N = 1 then
          Def = string.format("X%d", [i(Start)])
      else
          build_sum(N-1, Start) = Def1,
          Def = string.format("add(%s,X%d)", [s(Def1), i(Start + N-1)])
    ).

:- func build_lin_reif_func_rep(int, int) = string.

build_lin_reif_func_rep(N, End) = Def :-
    (   if      End < 1
        then    Def = string.format("mul(X0,X%d)", [i(N)])
        else    build_lin_reif_func_rep(N, End-1) = Def1,
                Def = string.format("add(%s,mul(X%d,X%d))",
                    [s(Def1), i(End), i(End + N)])
    ).

:- func dom_ref_to_xcsp(dom_ref) = string.

dom_ref_to_xcsp(DomRef) = string.format("D%d", [i(DomRef)]).

:- func rel_ref_to_xcsp(int) = string.

rel_ref_to_xcsp(RelRef) = string.format("R%d", [i(RelRef)]).

:- pred is_rel_ref(string::in) is semidet.

is_rel_ref(Ref) :- string.prefix(Ref, "R").

%----------------------------------------------------------------------------%

    % Output a domain to a file
    %
:- pred write_domain(io.text_output_stream::in, int::in, domain::in,
    io::di, io::uo) is det.

write_domain(File, Indent, Domain, !IO) :-
    Domain = domain(DomRef, NbDomValues, Value),
    Indent1 = indent(Indent),
    DomName = dom_ref_to_xcsp(DomRef),
    write_start_tag(File, Indent1, "domain", yes([attr("name", DomName),
        attr("nbValues", int_to_string(NbDomValues))]), write_nl, !IO),
    insert_space(File, indent(Indent1), !IO),
    io.write_string(File, Value, !IO),
    io.nl(File, !IO),
    write_end_tag(File, yes(Indent1), "domain", !IO).

    % Output a variable to a file
    %
:- pred write_variable(io.text_output_stream::in, int::in, variable::in,
    io::di, io::uo) is det.

write_variable(File, Indent, Variable, !IO) :-
    Variable = variable(Name, DomRef),
    DomName = dom_ref_to_xcsp(DomRef),
    write_slash_tag(File, indent(Indent), "variable",
        yes([attr("name", Name), attr("domain", DomName)]), write_nl, !IO).

:- pred write_relation(io.text_output_stream::in, int::in, relation::in,
    io::di, io::uo) is det.

write_relation(File, Indent, Relation, !IO) :-
    Indent1 = indent(Indent),
    Name = rel_ref_to_xcsp(Relation^rel_ref),
    Arity = Relation^arity,
    List = Relation^values,
    write_start_tag(File, Indent1, "relation",
        yes([attr("name", Name), attr("arity", int_to_string(Arity)),
             attr("nbTuples", int_to_string(length(List)/Arity)),
             attr("semantics", "supports") ]),
        write_nl, !IO),

    insert_space(File, indent(Indent1), !IO),

    list.det_split_list(Arity, List, First, Rest),
    string.join_list(" ", First) = FirstStr,
    io.write_string(File, FirstStr, !IO),

    list.foldl2((pred(Value::in, I::in, I1::out, !.IO::di, !:IO::uo) is det :-
            ( if I = 0 then
                I2 = Arity,
                io.write_string(File, "|", !IO),
                io.write_string(File, Value, !IO)
            else
                I2 = I,
                io.write_string(File, " ", !IO),
                io.write_string(File, Value, !IO)
            ),
            I1 = I2 - 1
        ), Rest, 0, _, !IO),
    io.nl(File, !IO),

    write_end_tag(File, yes(Indent1), "relation", !IO).

:- pred write_predicate(io.text_output_stream::in, int::in,
    pair(fzn_predicate, int)::in, io::di, io::uo) is det.

write_predicate(File, Indent, FznPredicate - Arity, !IO) :-
    fzn_pred_to_xcsp(FznPredicate, Arity) =
        xcsp_predicate(Name, Params, exprPair(Type, Exp)),

    Indent1 = indent(Indent),
    Indent2 = indent(Indent1),
    Indent3 = indent(Indent2),

    write_start_tag(File, Indent1, "predicate", yes([attr("name", Name)]),
        write_nl, !IO),

    write_start_tag(File, Indent2, "parameters", no, no_nl, !IO),
    io.write_string(File, Params, !IO),
    write_end_tag(File, no, "parameters", !IO),

    write_start_tag(File, Indent2, "expression", no, write_nl, !IO),
    (
        Type = functional,
        write_start_tag(File, Indent3, "functional", no, no_nl, !IO),
        io.write_string(File, Exp, !IO),
        write_end_tag(File, no, "functional", !IO)
    ;
        ( Type = mathml
        ; Type = postfix
        ; Type = infix
        )
    ),
    write_end_tag(File, yes(Indent2), "expression", !IO),
    write_end_tag(File, yes(Indent1), "predicate", !IO).

:- pred write_constraint(io.text_output_stream::in, int::in,
    string::in, xcsp_constraint::in, int::in, int::out, io::di, io::uo) is det.

write_constraint(File, Indent, Prefix, Constraint, I, I+1, !IO) :-
    Constraint = xcsp_constraint(Ref, Scope, Params),
    Indent1 = indent(Indent),
    Arity = set.count(Scope),
    string.join_list(" ", set.to_sorted_list(Scope)) = ScopeStr,
    string.join_list(" ", Params) = ParamsStr,
    Name = string.format("%s%d", [s(Prefix), i(I)]),

    ( if is_rel_ref(Ref), Arity = 0 then
        % Constraint in extension
        Arity2 = length(Params),
        write_slash_tag(File, Indent1, "constraint",
            yes([attr("name", Name), attr("arity", int_to_string(Arity2)),
                 attr("scope", ParamsStr), attr("reference", Ref)]),
            write_nl, !IO)
    else
        % Constraint in intension
        write_start_tag(File, Indent1, "constraint",
            yes([attr("name", Name), attr("arity", int_to_string(Arity)),
                 attr("scope", ScopeStr), attr("reference", Ref)]),
            write_nl, !IO),
        write_start_tag(File, indent(Indent1), "parameters", no, no_nl, !IO),
        io.write_string(File, ParamsStr, !IO),
        write_end_tag(File, no, "parameters", !IO),
        write_end_tag(File, yes(Indent1), "constraint", !IO)
    ).

%----------------------------------------------------------------------------%
%
% Generic XML procedures
%

    % Writes the start tag of an XML element with the same name as the
    % input string to the specified file. Attributes are included in the
    % tag if given. The input integer represents the number of spaces or
    % columns by which the tag must be indented from the left.
    %
:- pred write_start_tag(io.text_output_stream::in, col::in, string::in,
    maybe(attrs)::in, write_nl::in, io::di, io::uo) is det.

write_start_tag(File, Col, TagName, MaybeAttrs, WriteNewline, !IO) :-
    insert_space(File, Col, !IO),
    io.format(File, "<%s", [s(TagName)], !IO),
    (
        MaybeAttrs = yes(Attrs),
        foldl(write_attr(File), Attrs, !IO)
    ;
        MaybeAttrs = no
    ),
    io.write_string(File, ">", !IO),
    (
        WriteNewline = write_nl,
        io.nl(File, !IO)
    ;
        WriteNewline = no_nl
    ).

    % Writes tag ended with /> of an XML element with the same name as the
    % input string to the specified file. Attributes are included in the
    % tag if given. The input integer represents the number of spaces or
    % columns by which the tag must be indented from the left.
    %
:- pred write_slash_tag(io.text_output_stream::in, col::in, string::in,
    maybe(attrs)::in, write_nl::in, io::di, io::uo) is det.

write_slash_tag(File, Col, TagName, MaybeAttrs, WriteNewline, !IO) :-
    insert_space(File, Col, !IO),
    io.format(File, "<%s", [s(TagName)], !IO),
    (
        MaybeAttrs = yes(Attrs),
        foldl(write_attr(File), Attrs, !IO)
    ;
        MaybeAttrs = no
    ),
    io.write_string(File, "/>", !IO),
    (
        WriteNewline = write_nl,
        io.nl(File, !IO)
    ;
        WriteNewline = no_nl
    ).

    % Writes the end tag of an XML element with the same name as the input
    % string to the specified file. The newline character is always written
    % after the tag. The maybe(int) argument (possibly) represents the
    % number of spaces or columns by which the tag must be indented from
    % the left. If the argument equals yes(Col), then the tag must be
    % indented by Col columns; if it equals no, then the tag is not printed
    % without further indentation, as the corresponding start tag occurs on
    % the same line.
    %
:- pred write_end_tag(io.text_output_stream::in, maybe(col)::in,
    string::in, io::di, io::uo) is det.

write_end_tag(File, MaybeCol, String, !IO) :-
    (
        MaybeCol = no
    ;
        MaybeCol = yes(Col),
        insert_space(File, Col, !IO)
    ),
    io.format(File, "</%s>\n", [s(String)], !IO).

    % Writes an empty XML element of the same name as the input string to
    % the specified file. Attributes are included in the tag if given. The
    % newline character is always written after the empty element. The
    % input integer represents the number of spaces or columns by which
    % the tag must be indented from the left.
    %
:- pred write_empty_element(io.text_output_stream::in, col::in, string::in,
    maybe(attrs)::in, io::di, io::uo) is det.

write_empty_element(File, Col, TagName, MaybeAttrs, !IO) :-
    insert_space(File, Col, !IO),
    io.format(File, "<%s", [s(TagName)], !IO),
    (
        MaybeAttrs = yes(Attrs),
        list.foldl(write_attr(File), Attrs, !IO)
    ;
        MaybeAttrs = no
    ),
    io.write_string(File, "/>\n", !IO).

    % Writes the given attribute name and value to the specified file.
    %
:- pred write_attr(io.text_output_stream::in, attr::in, io::di, io::uo)
    is det.

write_attr(File, Attr, !IO) :-
    Attr = attr(Name, Value),
    io.format(File, " %s=\"%s\"", [s(Name), s(Value)], !IO).

    % Inserts white space in the first Col columns.
    %
:- pred insert_space(io.text_output_stream::in, col::in, io::di, io::uo)
    is det.

insert_space(File, Col, !IO) :-
    io.write_string(File, string.duplicate_char(' ', Col), !IO).

:- func indent(int) = int.

indent(Col) = Col + 2.

%-----------------------------------------------------------------------------%
%
% Option processing
%

:- type fzn2xcsp_option
    --->    help
    ;       version
    ;       verbose
    ;       output_to_stdout
    ;       output_comments
    ;       default_int_lb
    ;       default_int_ub
    ;       ignore_objective
    % Debugging options
    ;       statistics
    ;       pprint_before
    ;       pprint_after
    ;       pprint_ignore_file
    ;       dump_before
    ;       dump_after
    ;       stop_before
    ;       stop_after
    .

    % The valid fzn2xcsp short options.
    %
:- pred fzn2xcsp_short_option(char::in, fzn2xcsp_option::out) is semidet.

fzn2xcsp_short_option('h', help).
fzn2xcsp_short_option('v', verbose).
fzn2xcsp_short_option('S', statistics).
fzn2xcsp_short_option('i', ignore_objective).

    % The valid fzn2xcsp long options.
    %
:- pred fzn2xcsp_long_option(string::in, fzn2xcsp_option::out) is semidet.

fzn2xcsp_long_option("help",                 help).
fzn2xcsp_long_option("version",              version).
fzn2xcsp_long_option("verbose",              verbose).
fzn2xcsp_long_option("output-to-stdout",     output_to_stdout).
fzn2xcsp_long_option("output-comments",      output_comments).
fzn2xcsp_long_option("default-int-lb",       default_int_lb).
fzn2xcsp_long_option("default-int-ub",       default_int_ub).
fzn2xcsp_long_option("ignore-objective",     ignore_objective).

fzn2xcsp_long_option("statistics",           statistics).
fzn2xcsp_long_option("pprint-before",        pprint_before).
fzn2xcsp_long_option("pprint-after",         pprint_after).
fzn2xcsp_long_option("pprint-ignore-file",   pprint_ignore_file).
fzn2xcsp_long_option("dump-before",          dump_before).
fzn2xcsp_long_option("dump-after",           dump_after).
fzn2xcsp_long_option("stop-before",          stop_before).
fzn2xcsp_long_option("stop-after",           stop_after).

    % Nondeterministically returns all the options with their corresponding
    % types and default values.
    % (The second mode is used to check that we cover all options.)
    %
:- pred fzn2xcsp_option_defaults(fzn2xcsp_option, option_data).
:- mode fzn2xcsp_option_defaults(out, out) is multi.
:- mode fzn2xcsp_option_defaults(in, out) is det.

fzn2xcsp_option_defaults(help,               bool(no)).
fzn2xcsp_option_defaults(version,            bool(no)).
fzn2xcsp_option_defaults(verbose,            bool(no)).
fzn2xcsp_option_defaults(output_to_stdout,   bool(no)).
fzn2xcsp_option_defaults(output_comments,    bool(no)).
fzn2xcsp_option_defaults(ignore_objective,   bool(no)).

fzn2xcsp_option_defaults(statistics,         bool(no)).
fzn2xcsp_option_defaults(pprint_before,      accumulating([])).
fzn2xcsp_option_defaults(pprint_after,       accumulating([])).
fzn2xcsp_option_defaults(pprint_ignore_file, accumulating([])).
fzn2xcsp_option_defaults(dump_before,        accumulating([])).
fzn2xcsp_option_defaults(dump_after,         accumulating([])).
fzn2xcsp_option_defaults(stop_before,        accumulating([])).
fzn2xcsp_option_defaults(stop_after,         accumulating([])).
fzn2xcsp_option_defaults(default_int_lb,     int(default_int_lb)).
fzn2xcsp_option_defaults(default_int_ub,     int(default_int_ub)).

%-----------------------------------------------------------------------------%
%
% Default bounds for unbounded integer variables
%

:- func default_int_lb = int.

default_int_lb = -1000000.

:- func default_int_ub = int.

default_int_ub = 1000000.

%-----------------------------------------------------------------------------%

    % NOTE: changes here may need to be reflected in:
    %
    %   g12/zinc/man/fzn2xcsp.1.in
    %
:- func fzn2xcsp_usage = string.

fzn2xcsp_usage = UsageMsg :-
    UsageLines = [
    "Usage: fzn2xcsp [<options>] <file>.fzn"
,   "Options:"
,   "    -h, --help"
,   "        Print this message."
,   "    --version"
,   "        Print version information."
,   "    -v, --verbose"
,   "        Output progress information as compilation proceeds."
,   "   --output-to-stdout"
,   "        Output the XCSP to the standard output rather than to"
,   "        the file <model>.xcsp."
% --output-comments is still NYI.
%,   "   --output-comments"
%,   "       Output the FlatZinc items in comments in the XCSP."
,   "   --default-int-lb <n>"
,   "       Set the lower bound for unbounded integers."
,   "       The default is: " ++ int_to_string(default_int_lb) ++ "."
,   "   --default-int-ub <n>"
,   "       Set the upper bound for unbounded integers."
,   "       The default is: " ++ int_to_string(default_int_ub) ++ "."
,   "   -i, --ignore-objective"
,   "       Ignore the objective, if present, and treat the problem as"
,   "       a satisfiability problem."
,   ""
,   "Debugging options:"
,   "    -S, --statistics"
,   "        Output messages about time/space usage to the stderr."
,   "    --pprint-before <name>"
,   "        Pretty-print the IR before stage <name>."
,   "    --pprint-after <name>"
,   "        Pretty-print the IR after stage <name>."
,   "    --pprint-ignore-file <file>"
,   "        Ignore items from <file> when pretty-printing."
,   "    --dump-before <name>"
,   "        Dump the full IR before stage <name>."
,   "    --dump-after <name>"
,   "        Dump IR after stage <name>."
,   "    --stop-before <name>"
,   "        Stop compilation before stage <name>."
,   "    --stop-after <name>"
,   "        Stop compilation after stage <name>."
,   ""
,   "    Valid stage names:"
    ] ++
    list.map(func(X) = "        " ++ X, fzn2xcsp_stage_names),
    UsageMsg = foldr(func(X, Xs) = X ++ "\n" ++ Xs, UsageLines, "").

:- func fzn2xcsp_version = string.

fzn2xcsp_version = VersionMsg :-
    Version = get_fzn2xcsp_version,
    VersionMsg =
        "G12 FlatZinc to XCSP converter, version " ++ Version ++ "\n"
++      "Copyright (C) 2010-2012 The University of Melbourne and NICTA\n".

:- pred bad_cmdline(string::in, io::di, io::uo) is det.

bad_cmdline(Msg, !IO) :-
    io.write_string(io.stderr_stream,
        "fzn2xcsp: " ++ Msg ++ "\n"
     ++ "fzn2xcsp: use --help for more information.\n",
        !IO),
    io.set_exit_status(1, !IO).

%-----------------------------------------------------------------------------%

    % Return the debug spec that is implicit in the option table.
    % This assumes that we have already checked that the stage names are
    % valid.
    %
:- func make_fzn2xcsp_debug_spec(option_table(fzn2xcsp_option)) = debug_spec.

make_fzn2xcsp_debug_spec(OptionTable) = DebugSpec :-
    lookup_accumulating_option(OptionTable, pprint_before, PPrintBefore),
    lookup_accumulating_option(OptionTable, pprint_after,  PPrintAfter),
    lookup_accumulating_option(OptionTable, pprint_ignore_file,
        PPrintIgnoredFiles),
    lookup_accumulating_option(OptionTable, dump_before,   DumpBefore),
    lookup_accumulating_option(OptionTable, dump_after,    DumpAfter),
    lookup_accumulating_option(OptionTable, stop_before,   StopBefore),
    lookup_accumulating_option(OptionTable, stop_after,    StopAfter),
    DebugSpec = new_debug_spec(PPrintBefore, PPrintAfter,
        PPrintIgnoredFiles, DumpBefore, DumpAfter, StopBefore, StopAfter).

%-----------------------------------------------------------------------------%
%
% Frontend control
%

:- type fzn2xcsp_control
    --->    fzn2xcsp_control(option_table(fzn2xcsp_option)).

:- instance frontend_control(fzn2xcsp_control) where [
    ( warn_unknown_fzn_annotations(_) = no ),
    ( extra_builtin_annotation(_, _, _, _) :- false ),
    ( post_typecheck_var_decl_action(_) = yes(check_var_decl) ),
    ( post_typecheck_constraint_action(_) = yes(check_constraint_decl)),
    ( post_typecheck_solve_action(Ctrl) = Action :-
        Ctrl = fzn2xcsp_control(OptionTable),
        lookup_bool_option(OptionTable, ignore_objective, IgnoreObjective),
        (
            IgnoreObjective = yes,
            Action = no
        ;
            IgnoreObjective = no,
            Action = yes(check_solve_decl)
        )
     )
].

%-----------------------------------------------------------------------------%

:- pred check_var_decl : var_decl_action `with_inst` var_decl_action.

check_var_decl(SrcLocn, TIExpr, Name, _Anns, _MaybeVal, !Errors) :-
    TIExpr = ti_expr(_, TIExprInfo),
    TI = TIExprInfo ^ expr_type_inst,
    (
        TI = ti_var_float,
        add_var_type_not_supported_error(SrcLocn, TI, Name, !Errors)
    ;
        TI = ti_var_set(ElemTI),
        ( if ElemTI = ti_par_int then
            add_var_type_not_supported_error(SrcLocn, TI, Name, !Errors)
          else
            % Non-FlatZinc set var types will have already been detected.
            true
        )
    ;
        TI = ti_array(_IndexTI, ElemTI),
        ( if (ElemTI = ti_var_float ; ElemTI = ti_var_set(ti_par_int)) then
            add_var_type_not_supported_error(SrcLocn, TI, Name, !Errors)
          else
            true
        )
    ;
        % Ignore everything else: other FlatZinc type are supported,
        % error messages for non-FlatZinc types will have already been
        % generated, but not yet emitted.
        %
        ( TI = ti_par_bottom
        ; TI = ti_var_bottom
        ; TI = ti_par_bool
        ; TI = ti_var_bool
        ; TI = ti_par_int
        ; TI = ti_var_int
        ; TI = ti_par_float
        ; TI = ti_par_string
        ; TI = ti_ann
        ; TI = ti_par_set(_)
        ; TI = ti_tuple(_)
        ; TI = ti_record(_)
        ; TI = ti_par_enum(_)
        ; TI = ti_var_enum(_)
        ; TI = ti_par_variable(_, _)
        ; TI = ti_any_variable(_, _)
        ; TI = ti_top
        ; TI = ti_overloaded_op(_)
        ; TI = ti_op(_, _)
        ; TI = ti_error
        ; TI = ti_unknown
        )
    ).

:- pred add_var_type_not_supported_error(src_locn::in, type_inst::in,
    zinc_name::in, zinc_errors::in, zinc_errors::out) is det.

add_var_type_not_supported_error(SrcLocn, Type, Name, !Errors) :-
    ErrMsg = [
        words("error: variable"), quote(Name), words("has type"),
        suffix(type_inst(Type), ":"),
        words("variables of this type are not supported in XCSP.")
    ],
    error_at_locn(ErrMsg, SrcLocn, !Errors).

%-----------------------------------------------------------------------------%

:- pred check_constraint_decl : constraint_action
     `with_inst` constraint_action.

check_constraint_decl(SrcLocn, Expr, !Errors) :-
    Expr = expr(RawExpr, _, _),
    (
        % All FlatZinc constraints are predicate applications.
        RawExpr = app(Id, _ProcNum, _Kind, Args),
        Name = Id ^ id_name,
        ( if is_supported_by_xcsp(Name, Args) then
            true
          else
            list.length(Args, Arity),
            ErrMsg = [
                words("error: the constraint"),
                quote(Name ++ "/" ++ int_to_string(Arity)),
                words("cannot be converted into XCSP.")
            ],
            error_at_locn(ErrMsg, SrcLocn, !Errors)
        )
    ;
        ( RawExpr = ident(_)
        ; RawExpr = lit(_)
        ; RawExpr = array_access(_, _)
        ; RawExpr = lit_set(_)
        ; RawExpr = lit_simple_array(_)
        ; RawExpr = lit_indexed_array(_)
        ; RawExpr = lit_tuple(_)
        ; RawExpr = lit_record(_)
        ; RawExpr = lit_nonflat_enum(_, _)
        ; RawExpr = lit_nonflat_enum_simple(_, _)
        ; RawExpr = lit_ann(_, _)
        ; RawExpr = comprehension(_, _, _)
        ; RawExpr = tuple_access(_, _)
        ; RawExpr = record_access(_, _)
        ; RawExpr = anon_var
        ; RawExpr = if_then_else(_, _, _)
        ; RawExpr = case(_, _)
        ; RawExpr = let(_, _)
        ; RawExpr = coerce(_, _, _)
        )
    ).
    
%-----------------------------------------------------------------------------%
            
:- pred check_solve_decl : solve_action `with_inst` solve_action.

check_solve_decl(SrcLocn, SolveKind, _Anns, !Errors) :-
    (
        SolveKind = satisfy
    ;
        ( SolveKind = minimize(_)
        ; SolveKind = maximize(_)
        ),
            ErrMsg = [
                words("error: XCSP does not support optimization problems."),
                nl,
                words("(Use --ignore-objective to convert this instance into"),
                words("a satisfiability problem.)")
            ],
            error_at_locn(ErrMsg, SrcLocn, !Errors)
    ).

%-----------------------------------------------------------------------------%
%
% Fzn predicate database
%

:- pred is_fzn_pred(string::in, exprs::in, arr_size_map::in,
    fzn_predicate::out, int::out) is semidet.

is_fzn_pred(Str, Args, ArrSizeMap, Pred, Arity) :-
    length(Args, Arity),
    fzn_array_length(Args, 1, ArrSizeMap) = N1,
    fzn_array_length(Args, 2, ArrSizeMap) = N2,
    ( Str = "bool_eq",                Arity=2, Pred = bool_eq
    ; Str = "bool_ne",                Arity=2, Pred = bool_ne
    ; Str = "bool_ge",                Arity=2, Pred = bool_ge
    ; Str = "bool_gt",                Arity=2, Pred = bool_gt
    ; Str = "bool_le",                Arity=2, Pred = bool_le
    ; Str = "bool_lt",                Arity=2, Pred = bool_lt
    ; Str = "bool_eq_reif",           Arity=3, Pred = bool_eq_reif
    ; Str = "bool_ne_reif",           Arity=3, Pred = bool_ne_reif
    ; Str = "bool_ge_reif",           Arity=3, Pred = bool_ge_reif
    ; Str = "bool_gt_reif",           Arity=3, Pred = bool_gt_reif
    ; Str = "bool_le_reif",           Arity=3, Pred = bool_le_reif
    ; Str = "bool_lt_reif",           Arity=3, Pred = bool_lt_reif
    ; Str = "bool_not",               Arity=2, Pred = bool_not
    ; Str = "bool_and",               Arity=3, Pred = bool_and
    ; Str = "bool_or",                Arity=3, Pred = bool_or
    ; Str = "bool_xor",               Arity=3, Pred = bool_xor
    ; Str = "bool_left_imp",          Arity=3, Pred = bool_left_imp
    ; Str = "bool_right_imp",         Arity=3, Pred = bool_right_imp
    ; Str = "array_bool_and",         Arity=2, Pred = array_bool_and(N1)
    ; Str = "array_bool_or",          Arity=2, Pred = array_bool_or(N1)
    ; Str = "bool_clause",            Arity=2, Pred = bool_clause(N1, N2)
    ; Str = "bool_clause_reif",       Arity=3, Pred = bool_clause_reif(N1, N2)
    ; Str = "bool2int",               Arity=2, Pred = bool2int
    ; Str = "int_eq",                 Arity=2, Pred = int_eq
    ; Str = "int_ne",                 Arity=2, Pred = int_ne
    ; Str = "int_ge",                 Arity=2, Pred = int_ge
    ; Str = "int_gt",                 Arity=2, Pred = int_gt
    ; Str = "int_le",                 Arity=2, Pred = int_le
    ; Str = "int_lt",                 Arity=2, Pred = int_lt
    ; Str = "int_eq_reif" ,           Arity=3, Pred = int_eq_reif
    ; Str = "int_ne_reif" ,           Arity=3, Pred = int_ne_reif
    ; Str = "int_ge_reif" ,           Arity=3, Pred = int_ge_reif
    ; Str = "int_gt_reif" ,           Arity=3, Pred = int_gt_reif
    ; Str = "int_le_reif" ,           Arity=3, Pred = int_le_reif
    ; Str = "int_lt_reif" ,           Arity=3, Pred = int_lt_reif
    ; Str = "int_lin_eq",             Arity=3, Pred = int_lin_eq(N1)
    ; Str = "int_lin_ne",             Arity=3, Pred = int_lin_ne(N1)
    ; Str = "int_lin_ge",             Arity=3, Pred = int_lin_ge(N1)
    ; Str = "int_lin_gt",             Arity=3, Pred = int_lin_gt(N1)
    ; Str = "int_lin_le",             Arity=3, Pred = int_lin_le(N1)
    ; Str = "int_lin_lt",             Arity=3, Pred = int_lin_lt(N1)
    ; Str = "int_lin_eq_reif",        Arity=4, Pred = int_lin_eq_reif(N1)
    ; Str = "int_lin_ne_reif",        Arity=4, Pred = int_lin_ne_reif(N1)
    ; Str = "int_lin_ge_reif",        Arity=4, Pred = int_lin_ge_reif(N1)
    ; Str = "int_lin_gt_reif",        Arity=4, Pred = int_lin_gt_reif(N1)
    ; Str = "int_lin_le_reif",        Arity=4, Pred = int_lin_le_reif(N1)
    ; Str = "int_lin_lt_reif",        Arity=4, Pred = int_lin_lt_reif(N1)
    ; Str = "int_negate",             Arity=2, Pred = int_negate
    ; Str = "int_plus",               Arity=3, Pred = int_plus
    ; Str = "int_minus",              Arity=3, Pred = int_minus
    ; Str = "int_times",              Arity=3, Pred = int_times
    ; Str = "int_div",                Arity=3, Pred = int_div
    ; Str = "int_mod",                Arity=3, Pred = int_mod
    ; Str = "int_abs",                Arity=2, Pred = int_abs
    ; Str = "int_max",                Arity=3, Pred = int_max
    ; Str = "int_min",                Arity=3, Pred = int_min
    ; Str = "array_bool_element",     Arity=3, Pred = array_bool_element(N2)
    ; Str = "array_int_element",      Arity=3, Pred = array_int_element(N2)
    ; Str = "array_var_bool_element", Arity=3, Pred = array_var_bool_element(N2)
    ; Str = "array_var_int_element",  Arity=3, Pred = array_var_int_element(N2)
    ; Str = "all_different_int",      Arity=1, Pred = all_different_int(N1)
    ; Str = "xcsp_table_int",         Arity=2, Pred = table_int
    ).


:- func fzn_array_length(exprs, int, arr_size_map) = int.

fzn_array_length(Exprs, I, ArrSizeMap) = N :-
    ( if list.index1(Exprs, I, Expr) then
        ( if
            ( Expr = expr(coerce(_,_,expr(ident(ID),_,_)),_,_)
            ; Expr = expr(ident(ID),_,_)
            ),
            map.search(ArrSizeMap, ID^id_name, N1)
        then
            N = N1
        else if Expr = expr(lit_simple_array(L),_,_) then
            N = length(L)
        else N = 0  % if it's not an array
        )
    else N = 0  % if it's not an array
    ).

:- pred is_supported_by_xcsp(string::in, exprs::in) is semidet.

is_supported_by_xcsp(FznStr, Args) :-
    map.init(ArrSizeMap),   % Dummy value.
    is_fzn_pred(FznStr, Args, ArrSizeMap, _Pred, _Arity).

%-----------------------------------------------------------------------------%

:- pred is_int_reif_pred(fzn_predicate::in) is semidet.

is_int_reif_pred(Pred) :-
    ( Pred = int_eq_reif
    ; Pred = int_ne_reif
    ; Pred = int_ge_reif
    ; Pred = int_gt_reif
    ; Pred = int_le_reif
    ; Pred = int_lt_reif
    ).

:- pred is_bool_pred(fzn_predicate::in) is semidet.

is_bool_pred(Pred) :-
    ( Pred = bool_eq
    ; Pred = bool_ne
    ; Pred = bool_ge
    ; Pred = bool_gt
    ; Pred = bool_le
    ; Pred = bool_lt
    ; Pred = bool_eq_reif
    ; Pred = bool_ne_reif
    ; Pred = bool_ge_reif
    ; Pred = bool_gt_reif
    ; Pred = bool_le_reif
    ; Pred = bool_lt_reif
    ; Pred = bool_not
    ; Pred = bool_and
    ; Pred = bool_or
    ; Pred = bool_xor
    ; Pred = bool_right_imp
    ; Pred = bool_left_imp
    ).

%-----------------------------------------------------------------------------%

:- func fzn_pred_definition(fzn_predicate) = string is det.

fzn_pred_definition(Pred) = Def :-
    ( Pred = bool_not,       Def = "eq(sub(1,X0),X1)"
    ; Pred = bool_and,       Def = "eq(mul(X0,X1),X2)"
    ; Pred = bool_or,        Def = "iff(ge(add(X0,X1),1),eq(X2,1))"
    ; Pred = bool_xor,       Def = "eq(abs(sub(X0,X1)),X2)"
    ; Pred = bool_right_imp, Def = "ne(mul(X0,sub(1,X1)),X2)"
    ; Pred = bool_left_imp,  Def = "ne(mul(sub(1,X0),X1),X2)"

    ; Pred = bool_eq,        Def = "eq(X0,X1)"
    ; Pred = bool_ne,        Def = "ne(X0,X1)"
    ; Pred = bool_ge,        Def = "ne(mul(sub(1,X0),X1),1)"
    ; Pred = bool_lt,        Def = "eq(mul(sub(1,X0),X1),1)"
    ; Pred = bool_gt,        Def = "eq(mul(X0,sub(1,X1),1))"
    ; Pred = bool_le,        Def = "ne(mul(X0,sub(1,X1)),1)"
    ; Pred = bool_eq_reif,   Def = "iff(eq(X0,X1),eq(X2,1))"
    ; Pred = bool_ge_reif,   Def = "ne(mul(sub(1,X0),X1),X2)"
    ; Pred = bool_gt_reif,   Def = "eq(mul(X0,sub(1,X1),X2)"
    ; Pred = bool_le_reif,   Def = "ne(mul(X0,sub(1,X1)),X2)"
    ; Pred = bool_lt_reif,   Def = "eq(mul(sub(1,X0),X1),X2)"
    ; Pred = bool_ne_reif,   Def = "iff(ne(X0,X1),eq(X2,1))"

    ; Pred = bool2int,       Def = "eq(X0,X1)"

    ; Pred = int_eq,         Def = "eq(X0,X1)"
    ; Pred = int_ne,         Def = "ne(X0,X1)"
    ; Pred = int_ge,         Def = "ge(X0,X1)"
    ; Pred = int_gt,         Def = "gt(X0,X1)"
    ; Pred = int_le,         Def = "le(X0,X1)"
    ; Pred = int_lt,         Def = "lt(X0,X1)"
    ; Pred = int_eq_reif,    Def = "iff(eq(X0,X1),eq(X2,1))"
    ; Pred = int_ne_reif,    Def = "iff(ne(X0,X1),eq(X2,1))"
    ; Pred = int_ge_reif,    Def = "iff(ge(X0,X1),eq(X2,1))"
    ; Pred = int_gt_reif,    Def = "iff(gt(X0,X1),eq(X2,1))"
    ; Pred = int_le_reif,    Def = "iff(le(X0,X1),eq(X2,1))"
    ; Pred = int_lt_reif,    Def = "iff(lt(X0,X1),eq(X2,1))"

    ; Pred = int_abs,        Def = "eq(abs(X0),X1)"
    ; Pred = int_negate,     Def = "eq(neg(X0),X1)"
    ; Pred = int_plus,       Def = "eq(add(X0,X1),X2)"
    ; Pred = int_minus,      Def = "eq(sub(X0,X1),X2)"
    ; Pred = int_times,      Def = "eq(mul(X0,X1),X2)"
    ; Pred = int_div,        Def = "eq(div(X0,X1),X2)"
    ; Pred = int_mod,        Def = "eq(mod(X0,X1),X2)"
    ; Pred = int_min,        Def = "eq(min(X0,X1),X2)"
    ; Pred = int_max,        Def = "eq(max(X0,X1),X2)"

    ;
        ( Pred = int_lin_eq(_) ; Pred = int_lin_eq_reif(_)
        ; Pred = int_lin_ne(_) ; Pred = int_lin_ne_reif(_)
        ; Pred = int_lin_le(_) ; Pred = int_lin_le_reif(_)
        ; Pred = int_lin_lt(_) ; Pred = int_lin_lt_reif(_)
        ; Pred = int_lin_ge(_) ; Pred = int_lin_ge_reif(_)
        ; Pred = int_lin_gt(_) ; Pred = int_lin_gt_reif(_)
        ; Pred = array_bool_and(_) ; Pred = array_bool_or(_)
        ; Pred = array_bool_element(_) ; Pred = array_var_bool_element(_)
        ; Pred = array_int_element(_)  ; Pred = array_var_int_element(_)
        ; Pred = bool_clause(_,_) ; Pred = bool_clause_reif(_,_)
        ; Pred = all_different_int(_) ; Pred = table_int
        ),
        unexpected($pred ++ ": unexpected predicate: " ++ string(Pred))
    ).

%-----------------------------------------------------------------------------%
%
% Environment variables
%

:- pred process_fzn2xcsp_environment_vars(list(string)::out,
    io::di, io::uo) is det.

process_fzn2xcsp_environment_vars([], !IO).

%-----------------------------------------------------------------------------%

    % Returns a string that gives the name of this program as it will
    % appear in error messages.
    %
:- func fzn2xcsp_program = string.

fzn2xcsp_program = "fzn2xcsp".

%-----------------------------------------------------------------------------%
:- end_module fzn2xcsp.
%-----------------------------------------------------------------------------%
