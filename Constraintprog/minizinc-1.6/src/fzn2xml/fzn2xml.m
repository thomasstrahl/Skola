%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne and NICTA.
% See the file COPYING for license information.
%-----------------------------------------------------------------------------%
%
% Authors: Sam Abousaid <s.abousaid@ugrad.unimelb.edu.au> 
%          Julien Fischer <juliensf@csse.unimelb.edu.au>
%
% A converter from FlatZinc to XML-FlatZinc.
% 
%-----------------------------------------------------------------------------%

:- module fzn2xml.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module compiler_common.
:- import_module fzn2xml_config.
:- import_module parse_stage.
:- import_module symbol_table.
:- import_module types_and_insts.
:- import_module zinc_ast.
:- import_module zinc_common.
:- import_module zinc_frontend.
:- import_module zinc_frontend2.
:- import_module zinc_pprint.

:- import_module bool.
:- import_module char.
:- import_module dir.
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
        then handle_io_error(fzn2xml_program, IO_Error, !IO)
        else rethrow(MainResult)
        )
    ).

:- pred main_2(unit::out, io::di, io::uo) is det.

main_2(unit, !IO) :-
    io.command_line_arguments(Args, !IO),
    OptionOps = option_ops_multi(
        fzn2xml_short_option,
        fzn2xml_long_option,
        (pred(O::out, D::out) is multi :- fzn2xml_option_defaults(O, D))
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
        VeryVerbose = no,   % Not currently used by fzn2xml.
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
            io.write_string(fzn2xml_version, !IO)
        
        else if lookup_bool_option(OptionTable, help, yes) then
            io.write_string(fzn2xml_usage, !IO)

        else
            (
                NonOptionArgs = [],
                bad_cmdline("no model file specified", !IO)
            ;
                NonOptionArgs = [ModelFileName],
                check_fzn2xml_options(OptionTable, ModelFileName,
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
                    % The FlatZinc to XML converter does not currently
                    % use any environment variables.
                    process_fzn2xml_environment_vars(_, !IO),

                    AllSearchDirs = ["."],
                    do_fzn2xml_stages(AllSearchDirs, ModelFileName,
                        ModelFileNameBase, OutputDestination, OptionTable, !IO)
                )
            ;
                NonOptionArgs = [_, _ | _],
                bad_cmdline("more than one model file specified", !IO)
            )
        )
    ).

%-----------------------------------------------------------------------------%

:- pred check_fzn2xml_options(option_table(fzn2xml_option)::in, string::in,
    string::out, output_destination::out,
    list(string)::in, list(string)::out) is det.

check_fzn2xml_options(OptionTable, ModelFileName,
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
        OutputDest = output_to_file(ModelFileNameBase ++ ".xml")
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
    foldl(check_stage_option(OptionTable, fzn2xml_stage_names), StageOptions,
        !Errors).

%-----------------------------------------------------------------------------%

    % NOTE: if you add or remove stages please update the fzn2xml man
    % page in g12/zinc/man/fzn2xml.1.in.
    %
:- func fzn2xml_stage_names = list(string).

fzn2xml_stage_names = [
    % Nb: No "top-sorting" in FlatZinc.
    "parsing",
    "structure-checking",
    "type-inst-checking",
    "xml-conversion" 
].

%-----------------------------------------------------------------------------%

:- pred do_fzn2xml_stages(list(string)::in, string::in,
    string::in, output_destination::in, option_table(fzn2xml_option)::in,
    io::di, io::uo) is det.

do_fzn2xml_stages(SearchDirs, ModelFileName, ModelFileBase, 
        OutputDestination, OptionTable, !IO) :-
    Files = [zi_model_file(ModelFileName)],

    trace [io(!TIO)] (
        verbose("Processing file: " ++ ModelFileName, !TIO)
    ),

    DebugSpec = make_fzn2xml_debug_spec(OptionTable),
    Control = fzn2xml_control,

    % Parsing.  Nb: the pprint-before function will print nothing, because the
    % AST is empty at that point.
    EmptyAST = [],
    AllowIncs = no_includes,  % FlatZinc does not support include items.
    do_io_stage(fzn2xml_program, fzn2xml_stage_names,
        "parsing", lang_flatzinc, DebugSpec,
        parse_zinc_model(AllowIncs, Files, SearchDirs),
        pprint_ast(pp_lang_flatzinc(print_coercions)), io.write,
        pprint_ast(pp_lang_flatzinc(print_coercions)), dump_ast,
        yes(EmptyAST), MaybeItems1, !IO),

    PreEvaluationStageNameSuffix = "",
    do_analysis_stages(Control, fzn2xml_program,
        fzn2xml_stage_names, lang_flatzinc, instance_checking,
        PreEvaluationStageNameSuffix,
        DebugSpec, MaybeItems1, MaybeItemsAndSymTbl2, !IO),

    lookup_bool_option(OptionTable, output_comments, OutputComments),
    
    % Set the value of the variable OutputComments2, which is of type
    % output_comments, according to the value of the variable
    % OutputComments, which is of type bool.
    (
        OutputComments = yes,
        OutputComments2 = output_comments
    ;
        OutputComments = no,
        OutputComments2 = no_comments
    ),
    flatzinc_to_xml(OptionTable, OutputComments2, MaybeItemsAndSymTbl2,
        ModelFileBase, OutputDestination, !IO).

%-----------------------------------------------------------------------------%

    % A variable of this type specifies whether or not FlatZinc items are
    % written as comments in the XML document. 
    %
:- type output_comments
    --->    output_comments
    ;       no_comments.

    % A variable of this type specifies the number of columns by which a
    % given element tag must be indented.
    %
:- type col == int.

    % Values of this type represent an XML attribute.
    %
:- type attr
    --->    attr(
                name  :: string,
                value :: string
            ).

:- type attrs == list(attr).

%-----------------------------------------------------------------------------%
%
% FlatZinc instance
%

:- pred flatzinc_to_xml(option_table(fzn2xml_option)::in, output_comments::in,
    maybe({sast, symbol_table})::in, string::in, output_destination::in,
    io::di, io::uo) is det.

flatzinc_to_xml(_, _,  no, _, _, !IO).
flatzinc_to_xml(OptionTable, OutputComments, yes({AST, _}), ModelFileBase,
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
    flatzinc_to_xml_2(OptionTable, OutputStream, OutputComments, AST,
        ModelFileBase, !IO),
    (
        OutputDestination = output_to_file(_),
        io.close_output(OutputStream, !IO)
    ;
        OutputDestination = output_to_stdout
    ).

    % Converts the given AST (the AST of a FlatZinc model) into XML,
    % writing the output to the specified file. The input string is
    % the name of the model, which is required in setting the value of
    % the "model" attribute in the root-element tag.
    %
:- pred flatzinc_to_xml_2(option_table(fzn2xml_option)::in,
    io.text_output_stream::in, output_comments::in,
    sast::in, string::in, io::di, io::uo) is det.

flatzinc_to_xml_2(OptionTable, File, Comments, AST, ModelFileBase, !IO) :-
    Col = 0,
    lookup_string_option(OptionTable, system_dtd_location, DTDLocn),
    ( if DTDLocn = "" then
        DTD = "flatzinc.dtd"
      else
        DTD = DTDLocn / "flatzinc.dtd"
    ),
   	io.write_string(File, "<?xml version=\"1.0\"?>\n", !IO),
    io.format(File, "<!DOCTYPE flatzinc SYSTEM \"%s\">\n", [s(DTD)], !IO),
    Attrs = [attr("model", ModelFileBase)],
    write_start_tag(File, Col, "flatzinc", Attrs, !IO),
    AST = sast(Items, SolveItem, _),
    list.foldl(predicate_to_xml(File, Comments, indent(Col)), Items, !IO),
    list.foldl(parameter_to_xml(File, Comments, indent(Col)), Items, !IO),
    list.foldl(variable_to_xml(File, Comments, indent(Col)), Items, !IO),
    list.foldl(constraint_to_xml(File, Comments, indent(Col)), Items, !IO),
    SolveItem = sast_solve_item(SolveKind, SolveAnns, _SolveSrcLocn),
    (
        Comments = output_comments,
        RawSolveItem = solve_item(SolveKind, SolveAnns),
        write_item_as_comment(File, RawSolveItem, !IO)
    ;
        Comments = no_comments
    ),
    solve_item_to_xml(File, indent(Col), SolveKind, SolveAnns, !IO),
    write_end_tag(File, Col, "flatzinc", !IO).

%-----------------------------------------------------------------------------%
%
% Predicate declarations
%

:- pred predicate_to_xml(io.text_output_stream::in, output_comments::in,
    col::in, item::in, io::di, io::uo) is det.

predicate_to_xml(File, Comments, Col, Item, !IO) :-
    Item = item(RawItem, _),
    (
        RawItem = predfunc_item(_, Name, _, Params, _, _),
        (
            Comments = output_comments,
            write_item_as_comment(File, RawItem, !IO)
        ;
            Comments = no_comments
        ),
        list.length(Params, Arity),
        Attrs = [
            attr("name", Name),
            attr("arity", int_to_string(Arity))
        ],
        write_start_tag(File, Col, "predicate", Attrs, !IO),
        list.foldl(pred_arg_decl_to_xml(File, indent(Col)), Params, !IO),
        write_end_tag(File, Col, "predicate", !IO)
    ;
        ( RawItem = var_decl_item(_, _, _, _)
        ; RawItem = constraint_item(_)
        )
    ;
        RawItem = solve_item(_, _),
        unexpected($pred ++ ": solve item encountered")
    ;
        ( RawItem = assign_item(_, _)
        ; RawItem = enum_item(_, _, _)
        ; RawItem = type_inst_syn_item(_, _, _)
        ; RawItem = annotation_item(_, _)
        ; RawItem = output_item(_)
        ),
        unexpected($pred ++ ": Non-FlatZinc item encountered")
    ).

%-----------------------------------------------------------------------------%
%
% Parameters
%

:- pred parameter_to_xml(io.text_output_stream::in, output_comments::in,
    col::in, item::in, io::di, io::uo) is det.

parameter_to_xml(File, Comments, Col, Item, !IO) :-
    Item = item(RawItem, _),
    (
        RawItem = var_decl_item(TIExpr, Name, _, MaybeAssign),
        TI = TIExpr ^ ti_expr_info ^ expr_type_inst,
        IsPar = ti_is_par(TI),
        (
            IsPar = no
        ;
            IsPar = yes,
            (
                Comments = output_comments,
                write_item_as_comment(File, RawItem, !IO)
            ;
                Comments = no_comments
            ),
            Attrs = [attr("name", Name)],
            write_start_tag(File, Col, "parameter", Attrs, !IO),
            (
                MaybeAssign = rhs_assignment(AssignExpr),
                expr_to_xml(File, indent(Col), AssignExpr, !IO)
            ;
                MaybeAssign = no_assignment,
                unexpected($pred, "unassigned parameter.")
            ;
                MaybeAssign = separate_assignment(_, _),
                unexpected($pred, "assign item in FlatZinc?")
            ),
            write_end_tag(File, Col, "parameter", !IO)

        ) 
    ;
        ( RawItem = constraint_item(_)
        ; RawItem = predfunc_item(_, _, _, _, _, _)
        )
    ;
        RawItem = solve_item(_, _),
        unexpected($pred, "solve item encountered")
    ;
        ( RawItem = assign_item(_, _)
        ; RawItem = enum_item(_, _, _)
        ; RawItem = type_inst_syn_item(_, _, _)
        ; RawItem = annotation_item(_, _)
        ; RawItem = output_item(_)
        ),
        unexpected($pred, "Non-FlatZinc item encountered")
    ).

%-----------------------------------------------------------------------------%
%
% Variables
%

:- pred variable_to_xml(io.text_output_stream::in, output_comments::in,
    col::in, item::in, io::di, io::uo) is det.

variable_to_xml(File, Comments, Col, Item, !IO) :-
    Item = item(RawItem, _),
    (
        RawItem = var_decl_item(TIExpr, Name, Anns0, MaybeAssign),
        TI = TIExpr ^ ti_expr_info ^ expr_type_inst,
        IsPar = ti_is_par(TI),
        (
            IsPar = yes
        ;
            IsPar = no, 
            (
                Comments = output_comments,
                write_item_as_comment(File, RawItem, !IO)
            ;
                Comments = no_comments
            ),
            filter_variable_anns(Anns0, Anns, IsIntroducedVar, IsDefinedVar),
            some [!Attrs] (
                (
                    IsIntroducedVar = yes,
                    !:Attrs = [attr("var_is_introduced", "yes")]
                ;
                    IsIntroducedVar = no,
                    !:Attrs = []
                ),
                (
                    IsDefinedVar = yes,
                    !:Attrs = [attr("is_defined_var", "yes") | !.Attrs]
                ;
                    IsDefinedVar = no
                ),
                !:Attrs = [attr("name", Name) | !.Attrs],
                write_start_tag(File, Col, "variable", !.Attrs, !IO),
                ti_expr_to_xml(File, indent(Col), TIExpr, !IO),
                (
                    MaybeAssign = no_assignment
                ;
                    MaybeAssign = rhs_assignment(AssignExpr),
                    expr_to_xml(File, indent(Col), AssignExpr, !IO) 
                ;
                    MaybeAssign = separate_assignment(_, _),
                    unexpected($pred, "assign item in FlatZinc?")
                ),
                list.foldl(output_annotation(File, indent(Col)), Anns, !IO),
                write_end_tag(File, Col, "variable", !IO)
            )
        )
    ;
        ( RawItem = constraint_item(_)
        ; RawItem = predfunc_item(_, _, _, _, _, _)
        )
    ;
        RawItem = solve_item(_, _),
        unexpected($pred, "solve item encountered")
    ;
        ( RawItem = assign_item(_, _)
        ; RawItem = enum_item(_, _, _)
        ; RawItem = type_inst_syn_item(_, _, _)
        ; RawItem = annotation_item(_, _)
        ; RawItem = output_item(_)
        ),
        unexpected($pred, "Non-FlatZinc item encountered")
    ).

%-----------------------------------------------------------------------------%
%
% Constraints
%

:- pred constraint_to_xml(io.text_output_stream::in, output_comments::in,
    col::in, item::in, io::di, io::uo) is det.

constraint_to_xml(File, Comments, Col, Item, !IO) :-
    Item = item(RawItem, _),
    (
        RawItem = constraint_item(Expr),
        (
            Comments = output_comments,
            write_item_as_comment(File, RawItem, !IO)
        ;
            Comments = no_comments
        ),
        Expr = expr(RawExpr, Anns0, _),
        (
            RawExpr = app(AppId, _, _, AppArgs),
            Name = id_name(AppId),
            list.length(AppArgs, Arity),
            ArityStr = int_to_string(Arity),
            filter_constraint_anns(Anns0, _Anns, MaybeDefinesVar),
            (
                MaybeDefinesVar = yes(DefinedVarName),
                Attrs0 = [attr("defines_var", DefinedVarName)]
            ;
                MaybeDefinesVar = no,
                Attrs0 = []
            ),
            Attrs = [attr("name", Name), attr("arity", ArityStr) | Attrs0],
            write_start_tag(File, Col, "constraint", Attrs, !IO),
            list.foldl(expr_to_xml(File, indent(Col)), AppArgs, !IO),
            write_end_tag(File, Col, "constraint", !IO)
        ;
            ( RawExpr = ident(_)
            ; RawExpr = lit(_)
            ; RawExpr = lit_set(_)
            ; RawExpr = lit_simple_array(_)
            ; RawExpr = lit_indexed_array(_)
            ; RawExpr = lit_tuple(_)
            ; RawExpr = lit_record(_)
            ; RawExpr = lit_nonflat_enum(_, _)
            ; RawExpr = lit_nonflat_enum_simple(_, _)
            ; RawExpr = lit_ann(_, _)
            ; RawExpr = comprehension(_, _, _)
            ; RawExpr = array_access(_, _)
            ; RawExpr = tuple_access(_, _)
            ; RawExpr = record_access(_, _)
            ; RawExpr = anon_var
            ; RawExpr = if_then_else(_, _, _)
            ; RawExpr = case(_, _)
            ; RawExpr = let(_, _)
            ; RawExpr = coerce(_, _, _)
            ),
            unexpected($pred, "Invalid constraint-item expression.")
        )
    ;
        ( RawItem = var_decl_item(_, _, _, _)
        ; RawItem = predfunc_item(_, _, _, _, _, _)
        )
    ;
        RawItem = solve_item(_, _),
        unexpected($pred, "solve item encountered")
    ;
        ( RawItem = assign_item(_, _)
        ; RawItem = enum_item(_, _, _)
        ; RawItem = type_inst_syn_item(_, _, _)
        ; RawItem = annotation_item(_, _)
        ; RawItem = output_item(_)
        ),
        unexpected($pred, "Non-FlatZinc item encountered")
    ).

%-----------------------------------------------------------------------------%
%
% Solve item
%
    
:- pred solve_item_to_xml(io.text_output_stream::in, col::in,
    solve_kind::in, exprs::in, io::di, io::uo) is det.

solve_item_to_xml(File, Col, SolveKind, SolveAnns, !IO) :-
    write_start_tag(File, Col, "solve", [], !IO),
    (
        SolveKind = satisfy,
        write_empty_tag(File, indent(Col), "satisfy", [], !IO)
    ;
        ( SolveKind = minimize(ObjExpr), TagName = "minimize"
        ; SolveKind = maximize(ObjExpr), TagName = "maximize"
        ),
        write_start_tag(File, indent(Col), TagName, [], !IO),
        expr_to_xml(File, indent(indent(Col)), ObjExpr, !IO),
        write_end_tag(File, indent(Col), TagName, !IO)
    ),
    list.foldl(output_annotation(File, indent(Col)), SolveAnns, !IO),
    write_end_tag(File, Col, "solve", !IO).

%-----------------------------------------------------------------------------%
%
% Predicate arguments
%
        
:- pred pred_arg_decl_to_xml(io.text_output_stream::in, col::in,
    ti_expr_and_id::in, io::di, io::uo) is det.

pred_arg_decl_to_xml(File, Col, TypeAndId, !IO) :-
    TypeAndId = TIExpr - _,
    ti_expr_to_xml(File, Col, TIExpr, !IO).

%-----------------------------------------------------------------------------%
%
% Type-inst expressions
%

:- pred ti_expr_to_xml(io.text_output_stream::in, col::in,
    ti_expr::in, io::di, io::uo) is det.

ti_expr_to_xml(File, Col, TIExpr, !IO) :-
    TIExpr = ti_expr(RawTIExpr, _),
    (
        RawTIExpr = raw_ti_expr(VarPar, BaseTIExpr),
        (
            VarPar = var,
            var_base_ti_expr_to_xml(File, Col, BaseTIExpr, !IO)
        ;
            VarPar = par,
            par_base_ti_expr_to_xml(File, Col, BaseTIExpr, !IO)
        )
    ;
        RawTIExpr = constrained_raw_ti_expr(CTIExpr, _, _),
        ti_expr_to_xml(File, Col, CTIExpr, !IO)
    ).

:- pred var_base_ti_expr_to_xml(io.text_output_stream::in, col::in,
    base_ti_expr_tail::in, io::di, io::uo) is det.

var_base_ti_expr_to_xml(File, Col, BTE, !IO) :-
    (
        BTE = bte_int,
        output_type_var_int(File, Col, !IO)
    ;
        BTE = bte_range_expr_as_type_expr(LB, UB),
        output_type_var_range(File, Col, LB, UB, !IO)
    ;
        % e.g. var {1, 4, 5}: x;
        BTE = bte_set_expr_as_type_expr(Elems),
        output_type_var_int_set(File, Col, Elems, !IO)
    ;
        BTE = bte_bool,
        output_type_var_bool(File, Col, !IO)
    ;
        BTE = bte_float,
        output_type_var_float(File, Col, !IO)
    ;
        BTE = bte_set_of(ElemTIExpr),
        output_type_var_set_of_int(File, Col, ElemTIExpr, !IO)
    ;
        ( BTE = bte_ident(_)
        ; BTE = bte_array_of(_, _, _)
        ; BTE = bte_typeinst_var(_)
        ; BTE = bte_any_typeinst_var(_)
        ; BTE = bte_tuple_of(_)
        ; BTE = bte_record_of(_)
        ; BTE = bte_string
        ; BTE = bte_op(_)
        ; BTE = bte_ann
        ; BTE = bte_bottom
        ; BTE = bte_error
        ),
        unexpected($pred, "Non-FlatZinc type encountered")
    ).

:- pred par_base_ti_expr_to_xml(io.text_output_stream::in, col::in,
    base_ti_expr_tail::in, io::di, io::uo) is det.

par_base_ti_expr_to_xml(File, Col, BTE, !IO) :-
    (
        BTE = bte_int,
        output_type_int(File, Col, !IO)
    ;   
        BTE = bte_range_expr_as_type_expr(LB, UB),
        output_type_range(File, Col, LB, UB, !IO)
    ;
        % e.g. {2, 4, 6}: p; 
        BTE = bte_set_expr_as_type_expr(Elems),
        output_type_int_set(File, Col, Elems, !IO)
    ;
        BTE = bte_array_of(IndexTIExprs, ElemTIExpr, _),
        IndexSetDesc = ti_exprs_to_index_set_desc(IndexTIExprs),
        (
            IndexSetDesc = implicit_index,
            output_type_implicit_array(File, Col, ElemTIExpr, !IO)
        ;
            IndexSetDesc = explicit_index(Size),
            output_type_array(File, Col, Size, ElemTIExpr, !IO)
        )
    ;
        BTE = bte_bool,
        output_type_bool(File, Col, !IO)
    ;
        BTE = bte_float,
        output_type_float(File, Col, !IO)
    ;
        BTE = bte_set_of(ElemTIExpr),
        ElemDesc = ti_expr_to_elem_desc(ElemTIExpr),
        (
            ElemDesc = ed_set_of_int,
            output_type_set_of_int(File, Col, !IO)
        ;   
            ElemDesc = ed_set_of_range(LB, UB),
            output_type_set_of_int_range(File, Col, LB, UB, !IO)
        ;
            ElemDesc = ed_set_of_int_set(Elems),
            output_type_set_of_int_set(File, Col, Elems, !IO)
        )
    ;
        ( BTE = bte_ident(_)
        ; BTE = bte_typeinst_var(_)
        ; BTE = bte_any_typeinst_var(_)
        ; BTE = bte_tuple_of(_)
        ; BTE = bte_record_of(_)
        ; BTE = bte_string
        ; BTE = bte_op(_)
        ; BTE = bte_ann
        ; BTE = bte_bottom
        ; BTE = bte_error
        ),
        unexpected($pred, "Non-FlatZinc type encountered")
    ).

%-----------------------------------------------------------------------------%
%
% Types
%

:- pred output_type_bool(io.text_output_stream::in, col::in,
    io::di, io::uo) is det.

output_type_bool(File, Col, !IO) :-
    write_empty_tag(File, Col, "bool", [], !IO).

:- pred output_type_int(io.text_output_stream::in, col::in,
    io::di, io::uo) is det.

output_type_int(File, Col, !IO) :-
    write_empty_tag(File, Col, "int", [], !IO).

:- pred output_type_float(io.text_output_stream::in, col::in,
    io::di, io::uo) is det.

output_type_float(File, Col, !IO) :-
    write_empty_tag(File, Col, "float", [], !IO).

:- pred output_type_range(io.text_output_stream::in, col::in,
    expr::in, expr::in, io::di, io::uo) is det.

output_type_range(File, Col, LBExpr, UBExpr, !IO) :-    
    Range = exprs_to_range_desc(LBExpr, UBExpr),
    (
        Range = int_range(LB, UB),
        output_type_int_range(File, Col, LB, UB, !IO)
    ;
        Range = float_range(LB, UB),
        output_type_float_range(File, Col, LB, UB, !IO)
    ).

:- pred output_type_int_range(io.text_output_stream::in, col::in,
    int::in, int::in, io::di, io::uo) is det.

output_type_int_range(File, Col, LB, UB, !IO) :-
    Attrs = int_range_to_attrs(LB, UB),
    write_empty_tag(File, Col, "int-range", Attrs, !IO).

:- pred output_type_float_range(io.text_output_stream::in, col::in,
    string::in, string::in, io::di, io::uo) is det.

output_type_float_range(File, Col, LB, UB, !IO) :-
    Attrs = float_range_to_attrs(LB, UB),
    write_empty_tag(File, Col, "float-range", Attrs, !IO).
            
:- pred output_type_int_set(io.text_output_stream::in,
    col::in, exprs::in, io::di, io::uo) is det.

output_type_int_set(File, Col, Elems, !IO) :-
    list.length(Elems, Card),
    Attrs = [card_to_attr(Card)],
    write_start_tag(File, Col, "int-set", Attrs, !IO),
    insert_space(File, indent(Col), !IO),
    io.write_list(File, Elems, " ", output_raw_int, !IO),
    io.nl(File, !IO),
    write_end_tag(File, Col, "int-set", !IO). 

:- pred output_type_set_of_int(io.text_output_stream::in, col::in,
    io::di, io::uo) is det.

output_type_set_of_int(File, Col, !IO) :-
    write_empty_tag(File, Col, "set-of-int", [], !IO).

:- pred output_type_set_of_int_range(io.text_output_stream::in, col::in,
    int::in, int::in, io::di, io::uo) is det.

output_type_set_of_int_range(File, Col, LB, UB, !IO) :-
    Attrs = int_range_to_attrs(LB, UB),
    write_empty_tag(File, Col, "set-of-int-range", Attrs, !IO).
            
:- pred output_type_set_of_int_set(io.text_output_stream::in, col::in,
    list(int)::in, io::di, io::uo) is det.

output_type_set_of_int_set(File, Col, Elems, !IO) :-
    list.length(Elems, Card),
    Attrs = [card_to_attr(Card)],
    write_start_tag(File, Col, "set-of-int-set", Attrs, !IO),
    insert_space(File, indent(Col), !IO),
    io.write_list(File, Elems, " ", io.write_int, !IO),
    io.nl(File, !IO),
    write_end_tag(File, Col, "set-of-int-set", !IO). 

:- pred output_type_var_bool(io.text_output_stream::in, col::in,
    io::di, io::uo) is det.

output_type_var_bool(File, Col, !IO) :-
    write_empty_tag(File, Col, "var-bool", [], !IO).

:- pred output_type_var_float(io.text_output_stream::in, col::in,
    io::di, io::uo) is det.

output_type_var_float(File, Col, !IO) :-
    write_empty_tag(File, Col, "var-float", [], !IO).

:- pred output_type_var_set_of_int(io.text_output_stream::in, col::in,
    ti_expr::in, io::di, io::uo) is det.

output_type_var_set_of_int(File, Col, TIExpr, !IO) :-
    ElemDesc = ti_expr_to_elem_desc(TIExpr),
    (
        ElemDesc = ed_set_of_int,
        write_empty_tag(File, Col, "var-set-of-int", [], !IO)
    ;
        ElemDesc = ed_set_of_range(Lo, Hi),
        Attrs = int_range_to_attrs(Lo, Hi),
        write_empty_tag(File, Col, "var-set-of-int-range", Attrs, !IO)
    ;
        ElemDesc = ed_set_of_int_set(Elems),
        list.length(Elems, Card),
        Attrs = [card_to_attr(Card)],
        write_start_tag(File, Col, "var-set-of-int-set", Attrs, !IO),
        insert_space(File, indent(Col), !IO),
        io.write_list(File, Elems, " ", io.write_int, !IO),
        io.nl(File, !IO),
        write_end_tag(File, Col, "var-set-of-int-set", !IO)
    ).

:- pred output_type_var_int(io.text_output_stream::in, col::in,
    io::di, io::uo) is det.

output_type_var_int(File, Col, !IO) :-
    write_empty_tag(File, Col, "var-int", [], !IO).
        
:- pred output_type_var_int_set(io.text_output_stream::in,
    col::in, exprs::in, io::di, io::uo) is det.

output_type_var_int_set(File, Col, Elems, !IO) :-
    list.length(Elems, Card),
    Attrs = [card_to_attr(Card)],
    write_start_tag(File, Col, "var-int-set", Attrs, !IO),
    insert_space(File, indent(Col), !IO),
    io.write_list(File, Elems, " ", output_raw_int, !IO),
    io.nl(File, !IO),
    write_end_tag(File, Col, "var-int-set", !IO). 
        
:- pred output_type_var_range(io.text_output_stream::in, col::in,
    expr::in, expr::in, io::di, io::uo) is det.

output_type_var_range(File, Col, LBExpr, UBExpr, !IO) :-
    Range = exprs_to_range_desc(LBExpr, UBExpr),
    (
        Range = int_range(LB, UB),
        output_type_var_int_range(File, Col, LB, UB, !IO)
    ;
        Range = float_range(LB, UB),
        output_type_var_float_range(File, Col, LB, UB, !IO)
    ).

:- pred output_type_var_int_range(io.text_output_stream::in, col::in,
    int::in, int::in, io::di, io::uo) is det.

output_type_var_int_range(File, Col, LB, UB, !IO) :-
    Attrs = int_range_to_attrs(LB, UB),
    write_empty_tag(File, Col, "var-int-range", Attrs, !IO).

:- pred output_type_var_float_range(io.text_output_stream::in, col::in,
    string::in, string::in, io::di, io::uo) is det.

output_type_var_float_range(File, Col, LB, UB, !IO) :-
    Attrs = float_range_to_attrs(LB, UB),
    write_empty_tag(File, Col, "var-float-range", Attrs, !IO).
            
:- pred output_type_implicit_array(io.text_output_stream::in, col::in,
    ti_expr::in, io::di, io::uo) is det.

output_type_implicit_array(File, Col, TIExpr, !IO) :-   
    TIExpr = ti_expr(RawTIExpr, _),
    (
        RawTIExpr = raw_ti_expr(VarPar, BTE),
        (
            VarPar = var,
            output_type_implicit_array_var(File, Col, BTE, !IO)
        ;
            VarPar = par,
            output_type_implicit_array_par(File, Col, BTE, !IO)
        )
    ;
        RawTIExpr = constrained_raw_ti_expr(CTIExpr, _, _),
        output_type_implicit_array(File, Col, CTIExpr, !IO)
    ).

    % array[int] of var ...
    %
:- pred output_type_implicit_array_var(io.text_output_stream::in,
    col::in, base_ti_expr_tail::in, io::di, io::uo) is det.

output_type_implicit_array_var(File, Col, BTE, !IO) :-
    (
        BTE = bte_bool,
        write_empty_tag(File, Col, "implicit-array-var-bool", [], !IO)
    ;
        BTE = bte_int,
        write_empty_tag(File, Col, "implicit-array-var-int", [], !IO)
    ;
        BTE = bte_float,
        write_empty_tag(File, Col, "implicit-array-var-float", [], !IO)
    ;
        BTE = bte_range_expr_as_type_expr(LBExpr, UBExpr),
        Range = exprs_to_range_desc(LBExpr, UBExpr),
        (
            Range = int_range(LB, UB),
            Attrs = int_range_to_attrs(LB, UB),
            write_empty_tag(File, Col, "implicit-array-var-int-range",
                Attrs, !IO)
        ;
            Range = float_range(LB, UB),
            Attrs = float_range_to_attrs(LB, UB),
            write_empty_tag(File, Col, "implicit-array-var-float-range",
                Attrs, !IO)
        )
    ;
        BTE = bte_set_expr_as_type_expr(ElemExprs),
        list.length(ElemExprs, Card),
        Elems = exprs_to_ints(ElemExprs),
        Attrs = [card_to_attr(Card)],
        write_start_tag(File, Col, "implicit-array-var-int-set", Attrs, !IO),
        insert_space(File, indent(Col), !IO),
        io.write_list(File, Elems, " ", io.write_int, !IO),
        io.nl(File, !IO),
        write_end_tag(File, Col, "implicit-array-var-int-set", !IO)
    ;
        BTE = bte_set_of(ElemTIExpr),
        ElemDesc = ti_expr_to_elem_desc(ElemTIExpr),
        (
            ElemDesc = ed_set_of_int,
            write_empty_tag(File, Col, "implicit-array-var-set-of-int",
                [], !IO)
        ;
            ElemDesc = ed_set_of_range(LB, UB),
            Attrs = int_range_to_attrs(LB, UB),
            write_empty_tag(File, Col, "implicit-array-var-set-of-int-range",
                Attrs, !IO)
        ;
            ElemDesc = ed_set_of_int_set(Elems),
            list.length(Elems, Card),
            Attrs = [card_to_attr(Card)],
            write_start_tag(File, Col, "implicit-array-var-set-of-int-set",
                Attrs, !IO),
            insert_space(File, indent(Col), !IO),
            io.write_list(File, Elems, " ", io.write_int, !IO),
            io.nl(File, !IO),
            write_end_tag(File, Col, "implicit-array-var-set-of-int-set", !IO)
        )
    ;
        ( BTE = bte_array_of(_, _, _)
        ; BTE = bte_ident(_)
        ; BTE = bte_typeinst_var(_)
        ; BTE = bte_any_typeinst_var(_)
        ; BTE = bte_tuple_of(_)
        ; BTE = bte_record_of(_)
        ; BTE = bte_string
        ; BTE = bte_op(_)
        ; BTE = bte_ann
        ; BTE = bte_bottom
        ; BTE = bte_error
        ),
        unexpected($pred, "non-FlatZinc variable type.")
    ).

:- pred output_type_implicit_array_par(io.text_output_stream::in,
    col::in, base_ti_expr_tail::in, io::di, io::uo) is det.

output_type_implicit_array_par(File, Col, BTE, !IO) :-
    (
        BTE = bte_bool,
        write_empty_tag(File, Col, "implicit-array-bool", [], !IO)
    ;
        BTE = bte_int,
        write_empty_tag(File, Col, "implicit-array-int", [], !IO)
    ;
        BTE = bte_float,
        write_empty_tag(File, Col, "implicit-array-float", [], !IO)
    ;
        BTE = bte_range_expr_as_type_expr(LBExpr, UBExpr),
        Range = exprs_to_range_desc(LBExpr, UBExpr),
        (
            Range = int_range(LB, UB),
            Attrs = int_range_to_attrs(LB, UB),
            write_empty_tag(File, Col, "implicit-array-int-range",
                Attrs, !IO)
        ;
            Range = float_range(LB, UB),
            Attrs = float_range_to_attrs(LB, UB),
            write_empty_tag(File, Col, "implicit-array-float-range",
                Attrs, !IO)
        )
    ;
        BTE = bte_set_expr_as_type_expr(ElemExprs),
        list.length(ElemExprs, Card),
        Elems = exprs_to_ints(ElemExprs),
        Attrs = [card_to_attr(Card)],
        write_start_tag(File, Col, "implicit-array-int-set", Attrs, !IO),
        insert_space(File, indent(Col), !IO),
        io.write_list(File, Elems, " ", io.write_int, !IO),
        io.nl(File, !IO),
        write_end_tag(File, Col, "implicit-array-int-set", !IO)
    ;
        BTE = bte_set_of(ElemTIExpr),
        ElemDesc = ti_expr_to_elem_desc(ElemTIExpr),
        (
            ElemDesc = ed_set_of_int,
            write_empty_tag(File, Col, "implicit-array-set-of-int", [], !IO)
        ;
            ElemDesc = ed_set_of_range(LB, UB),
            Attrs = int_range_to_attrs(LB, UB),
            write_empty_tag(File, Col, "implicit-array-set-of-int-range",
                Attrs, !IO)
        ;
            ElemDesc = ed_set_of_int_set(Elems),
            list.length(Elems, Card),
            Attrs = [card_to_attr(Card)],
            write_start_tag(File, Col, "implicit-array-set-of-int-set",
                Attrs, !IO),
            insert_space(File, indent(Col), !IO),
            io.write_list(File, Elems, " ", io.write_int, !IO),
            io.nl(File, !IO),
            write_end_tag(File, Col, "implicit-array-set-of-int-set", !IO)
        )
    ;
        ( BTE = bte_array_of(_, _, _)
        ; BTE = bte_ident(_)
        ; BTE = bte_typeinst_var(_)
        ; BTE = bte_any_typeinst_var(_)
        ; BTE = bte_tuple_of(_)
        ; BTE = bte_record_of(_)
        ; BTE = bte_string
        ; BTE = bte_op(_)
        ; BTE = bte_ann
        ; BTE = bte_bottom
        ; BTE = bte_error
        ),
        unexpected($pred, "non-FlatZinc parameter type.")
    ).
            
:- pred output_type_array(io.text_output_stream::in, col::in,
    int::in, ti_expr::in, io::di, io::uo) is det.

output_type_array(File, Col, Size, TIExpr, !IO) :-
    TIExpr = ti_expr(RawTIExpr, _),
    (
        RawTIExpr = raw_ti_expr(VarPar, BTE),
        (
            VarPar = var,
            output_type_array_var(File, Col, Size, BTE, !IO)
        ;
            VarPar = par,
            output_type_array_par(File, Col, Size, BTE, !IO)
        )
    ;
        RawTIExpr = constrained_raw_ti_expr(CTIExpr, _, _),
        output_type_array(File, Col, Size, CTIExpr, !IO)
    ).

:- pred output_type_array_var(io.text_output_stream::in,
    col::in, int::in, base_ti_expr_tail::in, io::di, io::uo) is det.

output_type_array_var(File, Col, Size, BTE, !IO) :-
    SizeAttr = size_to_attr(Size),
    (
        BTE = bte_bool,
        write_empty_tag(File, Col, "array-var-bool", [SizeAttr], !IO)
    ;
        BTE = bte_int,
        write_empty_tag(File, Col, "array-var-int", [SizeAttr], !IO)
    ;
        BTE = bte_float,
        write_empty_tag(File, Col, "array-var-float", [SizeAttr], !IO)
    ;
        BTE = bte_range_expr_as_type_expr(LBExpr, UBExpr),
        Range = exprs_to_range_desc(LBExpr, UBExpr),
        (
            Range = int_range(LB, UB),
            RangeAttrs = int_range_to_attrs(LB, UB),
            Attrs = [SizeAttr | RangeAttrs],
            write_empty_tag(File, Col, "array-var-int-range", Attrs, !IO)
        ;
            Range = float_range(LB, UB),
            RangeAttrs = float_range_to_attrs(LB, UB),
            Attrs = [SizeAttr | RangeAttrs], 
            write_empty_tag(File, Col, "array-var-float-range", Attrs, !IO)
        )
    ;
        BTE = bte_set_expr_as_type_expr(ElemExprs),
        list.length(ElemExprs, Card),
        Elems = exprs_to_ints(ElemExprs),
        CardAttr = card_to_attr(Card),
        Attrs = [SizeAttr, CardAttr],
        write_start_tag(File, Col, "array-var-int-set", Attrs, !IO),
        insert_space(File, indent(Col), !IO),
        io.write_list(File, Elems, " ", io.write_int, !IO),
        io.nl(File, !IO),
        write_end_tag(File, Col, "array-var-int-set", !IO)
    ;
        BTE = bte_set_of(ElemTIExpr),
        ElemDesc = ti_expr_to_elem_desc(ElemTIExpr),
        (
            ElemDesc = ed_set_of_int,
            write_empty_tag(File, Col, "array-var-set-of-int", [SizeAttr], !IO)
        ;
            ElemDesc = ed_set_of_range(LB, UB),
            RangeAttrs = int_range_to_attrs(LB, UB),
            Attrs = [SizeAttr | RangeAttrs],
            write_empty_tag(File, Col, "array-var-set-of-int-range", Attrs, !IO)
        ;
            ElemDesc = ed_set_of_int_set(Elems),
            list.length(Elems, Card),
            CardAttr = card_to_attr(Card),
            Attrs = [SizeAttr, CardAttr],
            write_start_tag(File, Col, "array-var-set-of-int-set", Attrs, !IO),
            insert_space(File, indent(Col), !IO),
            io.write_list(File, Elems, " ", io.write_int, !IO),
            io.nl(File, !IO),
            write_end_tag(File, Col, "array-var-set-of-int-set", !IO) 
        )
    ;
        ( BTE = bte_array_of(_, _, _)
        ; BTE = bte_ident(_)
        ; BTE = bte_typeinst_var(_)
        ; BTE = bte_any_typeinst_var(_)
        ; BTE = bte_tuple_of(_)
        ; BTE = bte_record_of(_)
        ; BTE = bte_string
        ; BTE = bte_op(_)
        ; BTE = bte_ann
        ; BTE = bte_bottom
        ; BTE = bte_error
        ),
        unexpected($pred, "non-FlatZinc variable type.")
    ).

:- pred output_type_array_par(io.text_output_stream::in,
    col::in, int::in, base_ti_expr_tail::in, io::di, io::uo) is det.

output_type_array_par(File, Col, Size, BTE, !IO) :-
    SizeAttr = size_to_attr(Size),
    (
        BTE = bte_bool,
        write_empty_tag(File, Col, "array-bool", [SizeAttr], !IO)
    ;
        BTE = bte_int,
        write_empty_tag(File, Col, "array-int", [SizeAttr], !IO)
    ;
        BTE = bte_float,
        write_empty_tag(File, Col, "array-float", [SizeAttr], !IO)
    ;
        BTE = bte_range_expr_as_type_expr(LBExpr, UBExpr),
        Range = exprs_to_range_desc(LBExpr, UBExpr),
        (
            Range = int_range(LB, UB),
            RangeAttrs = int_range_to_attrs(LB, UB),
            Attrs = [SizeAttr | RangeAttrs],
            write_empty_tag(File, Col, "array-int-range", Attrs, !IO)
        ;
            Range = float_range(LB, UB),
            RangeAttrs = float_range_to_attrs(LB, UB),
            Attrs = [SizeAttr | RangeAttrs],
            write_empty_tag(File, Col, "array-float-range", Attrs, !IO)
        )
    ;
        BTE = bte_set_expr_as_type_expr(ElemExprs),
        list.length(ElemExprs, Card),
        Elems = exprs_to_ints(ElemExprs),
        CardAttr = card_to_attr(Card),
        Attrs = [SizeAttr, CardAttr],
        write_start_tag(File, Col, "array-int-set", Attrs, !IO),
        insert_space(File, indent(Col), !IO),
        io.write_list(File, Elems, " ", io.write_int, !IO),
        io.nl(File, !IO),
        write_end_tag(File, Col, "array-int-set", !IO)
    ;
        BTE = bte_set_of(ElemTIExpr),
        ElemDesc = ti_expr_to_elem_desc(ElemTIExpr),
        (
            ElemDesc = ed_set_of_int,
            write_empty_tag(File, Col, "array-set-of-int", [SizeAttr], !IO)
        ;
            ElemDesc = ed_set_of_range(LB, UB),
            RangeAttrs = int_range_to_attrs(LB, UB),
            Attrs = [SizeAttr | RangeAttrs],
            write_empty_tag(File, Col, "array-set-of-int-range", Attrs, !IO)
        ;
            ElemDesc = ed_set_of_int_set(Elems),
            list.length(Elems, Card), 
            CardAttr = card_to_attr(Card),
            Attrs = [SizeAttr, CardAttr],
            write_start_tag(File, Col, "array-set-of-int-set", Attrs, !IO),
            insert_space(File, indent(Col), !IO),
            io.write_list(File, Elems, " ", io.write_int, !IO),
            io.nl(File, !IO),
            write_end_tag(File, Col, "array-set-of-int-set", !IO)
        )
    ;
        ( BTE = bte_array_of(_, _, _)
        ; BTE = bte_ident(_)
        ; BTE = bte_typeinst_var(_)
        ; BTE = bte_any_typeinst_var(_)
        ; BTE = bte_tuple_of(_)
        ; BTE = bte_record_of(_)
        ; BTE = bte_string
        ; BTE = bte_op(_)
        ; BTE = bte_ann
        ; BTE = bte_bottom
        ; BTE = bte_error
        ),
        unexpected($pred, "non-FlatZinc parameter type.")
    ).

%-----------------------------------------------------------------------------%
%
% Further code for handling range types
%

:- type range_desc
    --->    int_range(int, int)
    ;       float_range(string, string).

:- func exprs_to_range_desc(expr, expr) = range_desc.

exprs_to_range_desc(LBExpr, UBExpr) = RangeDesc :-
    LBExpr = expr(LBRawExpr, _, _),
    UBExpr = expr(UBRawExpr, _, _),
    ( if LBRawExpr = lit(LBLit), UBRawExpr = lit(UBLit) then
        (
            LBLit = int(LBInt),
            ( if UBLit = int(UBInt)
            then RangeDesc = int_range(LBInt, UBInt)
            else unexpected($pred, "ill-formed range expr.")
            )
        ;
            LBLit = floatstr(LBFloatStr),
            ( if UBLit = floatstr(UBFloatStr)
            then RangeDesc = float_range(LBFloatStr, UBFloatStr)
            else unexpected($pred, "ill-formed range expr.")
            ) 
        ;
            ( LBLit = bool(_)
            ; LBLit = string(_)
            ),
            unexpected($pred, "not an integer or float range.")
        )
    else
        unexpected($pred, "Invalid range type.")
    ).      
        
%-----------------------------------------------------------------------------%

:- type elem_desc
    --->    ed_set_of_int                   % set of int
    ;       ed_set_of_range(int, int)       % set of x..y
    ;       ed_set_of_int_set(list(int)).   % set of {x, y, z}

:- func ti_expr_to_elem_desc(ti_expr) = elem_desc.

ti_expr_to_elem_desc(TIExpr) = ElemDesc :-
    TIExpr = ti_expr(RawTIExpr, _),
    (
        RawTIExpr = raw_ti_expr(_, BTE),
        (
            BTE = bte_int,
            ElemDesc = ed_set_of_int
        ;
            BTE = bte_range_expr_as_type_expr(LBExpr, UBExpr),
            RangeDesc = exprs_to_range_desc(LBExpr, UBExpr),
            (
                RangeDesc = int_range(LB, UB),
                ElemDesc = ed_set_of_range(LB, UB)
            ;
                RangeDesc = float_range(_, _),
                unexpected($pred, "not an integer ragne")
            )
        ;
            BTE = bte_set_expr_as_type_expr(ElemExprs),
            Elems = exprs_to_ints(ElemExprs),
            ElemDesc = ed_set_of_int_set(Elems)
        ;
            ( BTE = bte_array_of(_, _, _)
            ; BTE = bte_bool
            ; BTE = bte_float
            ; BTE = bte_set_of(_)
            ; BTE = bte_ident(_)
            ; BTE = bte_typeinst_var(_)
            ; BTE = bte_any_typeinst_var(_)
            ; BTE = bte_tuple_of(_)
            ; BTE = bte_record_of(_)
            ; BTE = bte_string
            ; BTE = bte_op(_)
            ; BTE = bte_ann
            ; BTE = bte_bottom
            ; BTE = bte_error
            ),
            unexpected($pred, "Non-FlatZinc set element type encountered")
        )
    ;
        RawTIExpr = constrained_raw_ti_expr(CTIExpr, _, _),
        ElemDesc = ti_expr_to_elem_desc(CTIExpr)
    ).

%-----------------------------------------------------------------------------%

:- type index_set_desc
    --->    explicit_index(int)     % array[1..n] of ...
    ;       implicit_index.         % array[int] of ...

:- func ti_exprs_to_index_set_desc(ti_exprs) = index_set_desc.

ti_exprs_to_index_set_desc(IndexTIExprs) = IndexSetDesc :-
    (
        IndexTIExprs = [],
        unexpected($pred, "zero-dimensional index set(?)")
    ;
        IndexTIExprs = [IndexTIExpr],
        IndexTIExpr = ti_expr(IndexRawTIExpr, _),
        (
            IndexRawTIExpr = raw_ti_expr(_, IndexBTE),
            (
                IndexBTE = bte_int,
                IndexSetDesc = implicit_index
            ;
                IndexBTE = bte_range_expr_as_type_expr(LBExpr, UBExpr),
                RangeDesc = exprs_to_range_desc(LBExpr, UBExpr),
                (
                    RangeDesc = int_range(LB, UB),
                    else_unexpected(unify(LB, 1),
                        $pred ++ ": non 1-based array"),
                    IndexSetDesc = explicit_index(UB)
                ;
                    RangeDesc = float_range(_, _),
                    unexpected($pred, "float array index")
                )
            ;
                ( IndexBTE = bte_bool
                ; IndexBTE = bte_float
                ; IndexBTE = bte_array_of(_, _, _)
                ; IndexBTE = bte_set_expr_as_type_expr(_)
                ; IndexBTE = bte_set_of(_)
                ; IndexBTE = bte_ident(_)
                ; IndexBTE = bte_typeinst_var(_)
                ; IndexBTE = bte_any_typeinst_var(_)
                ; IndexBTE = bte_tuple_of(_)
                ; IndexBTE = bte_record_of(_)
                ; IndexBTE = bte_string
                ; IndexBTE = bte_op(_)
                ; IndexBTE = bte_ann
                ; IndexBTE = bte_bottom
                ; IndexBTE = bte_error
                ),
                unexpected($pred, "Non-FlatZinc index set type encountered")
            )
        ;
            IndexRawTIExpr = constrained_raw_ti_expr(_, _, _),
            unexpected($pred, "constrained type-inst as index set type")
        )
    ;
        IndexTIExprs = [_, _ | _],
        unexpected($pred, "index set with > 1 dimensions.")
    ).

%-----------------------------------------------------------------------------%
%
% Expressions
%

:- pred expr_to_xml(io.text_output_stream::in, col::in, expr::in,
    io::di, io::uo) is det.

expr_to_xml(File, Col, Expr, !IO) :-
    Expr = expr(RawExpr, _, _),
    (
        RawExpr = ident(Id),
        output_identifier(File, Col, Id, !IO)
    ;
        RawExpr = lit(Literal),
        output_literal(File, Col, Literal, !IO)
    ;
        % The only applications within expressions that will occur in FlatZinc
        % are for range expressions.
        RawExpr = app(AppId, _, _, AppArgs),
        ( if AppId ^ id_name = "..", AppArgs = [LoExpr, HiExpr] then
            Lo = expr_to_integer(LoExpr),
            Hi = expr_to_integer(HiExpr),
            output_int_range_expr(File, Col, Lo, Hi, !IO)
        else
            unexpected($pred, "invalid application in expression")
        )
    ;
        RawExpr = array_access(Array, Index),
        output_array_access(File, Col, Array, Index, !IO)
    ;
        RawExpr = lit_set(SetElems),
        output_set_int_literal(File, Col, SetElems, !IO)
    ;
        RawExpr = lit_simple_array(ArrayElems),
        output_simple_array(File, Col, ArrayElems, !IO)
    ;
        RawExpr = coerce(_, _, CoercedExpr),
        expr_to_xml(File, Col, CoercedExpr, !IO) 
    ;
        ( RawExpr = lit_indexed_array(_)
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
        ),
        unexpected($pred, "not a FlatZinc expression")
    ).

:- pred output_identifier(io.text_output_stream::in, col::in,
    id::in, io::di, io::uo) is det.

output_identifier(File, Col, Id, !IO) :-
    Name = Id ^ id_name,
    Attrs = [attr("name", Name)],
    write_empty_tag(File, Col, "id", Attrs, !IO).
        
:- pred output_literal(io.text_output_stream::in, col::in,
    literal::in, io::di, io::uo) is det.

output_literal(File, Col, Literal, !IO) :-
    (
        Literal = bool(Bool),
        (
            Bool = yes,
            Value = "true"
        ;
            Bool = no,
            Value = "false"
        ),
        Attrs = [attr("value", Value)],
        write_empty_tag(File, Col, "bool-literal", Attrs, !IO)
    ;
        Literal = string(String0),
        % Preserve FlatZinc (Mercury) charcter escapes.
        % Introduce character escapes required by XML.
        some [!Str] (
            !:Str = escape_chars(String0),
            % NOTE: we must escape ampersands before the others.
            % 
            !:Str = string.replace_all(!.Str, "&", "&amp;"),
            !:Str = string.replace_all(!.Str, "\"", "&quot;"),
            !:Str = string.replace_all(!.Str, "'", "&apos;"),
            !:Str = string.replace_all(!.Str, "<", "&lt;"),
            !:Str = string.replace_all(!.Str, ">", "&gt;"),
            String = !.Str
        ),
        Attrs = [attr("value", String)],
        write_empty_tag(File, Col, "string-literal", Attrs, !IO)
    ;
        Literal = int(Integer),
        Value = int_to_string(Integer),
        Attrs = [attr("value", Value)],
        write_empty_tag(File, Col, "int-literal", Attrs, !IO)
    ;
        Literal = floatstr(FloatStr),
        Attrs = [attr("value", FloatStr)],
        write_empty_tag(File, Col, "float-literal", Attrs, !IO)
    ).

:- pred output_int_range_expr(io.text_output_stream::in, col::in,
    int::in, int::in, io::di, io::uo) is det.

output_int_range_expr(File, Col, Lo, Hi, !IO) :-
    Attrs = int_range_to_attrs(Lo, Hi),
    write_empty_tag(File, Col, "int-range-expr", Attrs, !IO).

:- pred output_array_access(io.text_output_stream::in, col::in, expr::in,
    exprs::in, io::di, io::uo) is det.

output_array_access(File, Col, ArrayExpr, IndexExprs, !IO) :-
    (
        IndexExprs = [],
        unexpected($pred, "array access with zero dimensions(?)")
    ;
        % Array accesses in FlatZinc are restricted to having the form:
        % id[<int-literal>].
        %
        IndexExprs = [IndexExpr],
        Name = expr_to_array_name(ArrayExpr),
        Index = expr_to_integer(IndexExpr),
        Attrs = [attr("name", Name), attr("index", int_to_string(Index))],
        write_empty_tag(File, Col, "array-access-expr", Attrs, !IO)
    ;
        IndexExprs = [_, _ | _],
        unexpected($pred, "array access with > 1 dimensions.")
    ).

:- pred output_set_int_literal(io.text_output_stream::in, col::in,
    exprs::in, io::di, io::uo) is det.

output_set_int_literal(File, Col, Elems, !IO) :-
    list.length(Elems, Card),
    Attrs = [card_to_attr(Card)],
    ( if Card = 0 then
        write_empty_tag(File, Col, "set-of-int-literal", Attrs, !IO)
    else
        write_start_tag(File, Col, "set-of-int-literal", Attrs, !IO),
        insert_space(File, indent(Col), !IO),
        io.write_list(File, Elems, " ", output_raw_int, !IO),
        io.nl(File, !IO),
        write_end_tag(File, Col, "set-of-int-literal", !IO) 
    ).

:- pred output_raw_int(expr::in, io::di, io::uo) is det.

output_raw_int(Expr, !IO) :-
    Expr = expr(RawExpr, _, _),
    ( if RawExpr = lit(int(I))
    then io.write_int(I, !IO)
    else unexpected($pred, "not an integer constant")
    ).

:- pred output_simple_array(io.text_output_stream::in, col::in, exprs::in,
    io::di, io::uo) is det.

output_simple_array(File, Col, Elems, !IO) :-
    list.length(Elems, Size),
    ArrayDesc = array_to_desc(Elems),
    (
        ArrayDesc = array_boolean_literals,
        output_array_boolean_literals(File, Col, Size, Elems, !IO)
    ;
        ArrayDesc = array_integer_literals,
        output_array_integer_literals(File, Col, Size, Elems, !IO)
    ;       
        ArrayDesc = array_float_literals,
        output_array_float_literals(File, Col, Size, Elems, !IO)
    ;
        ArrayDesc = array_set_of_int_literals,
        output_array_set_of_int_literals(File, Col, Size, Elems, !IO)
    ;
        ArrayDesc = array_identifiers,
        output_array_identifiers(File, Col, Size, Elems, !IO)
    ;
        ArrayDesc = array_array_accesses(ArrayName),
        output_array_accesses(File, Col, Size, ArrayName, Elems, !IO)
    ;
        ArrayDesc = array_mixed,
        output_array_expr(File, Col, Size, Elems, !IO)
    ).

:- pred output_array_expr(io.text_output_stream::in, col::in, int::in,
    exprs::in, io::di, io::uo) is det.

output_array_expr(File, Col, Size, Elems, !IO) :-
    Attrs = [attr("size", int_to_string(Size))],
    write_start_tag(File, Col, "array-expr", Attrs, !IO),
    list.foldl(expr_to_xml(File, indent(Col)), Elems, !IO),
    write_end_tag(File, Col, "array-expr", !IO).

:- pred output_array_boolean_literals(io.text_output_stream::in, col::in,
    int::in, exprs::in, io::di, io::uo) is det.

output_array_boolean_literals(File, Col, Size, Elems, !IO) :-
    Attrs = [attr("size", int_to_string(Size))],
    write_start_tag(File, Col, "array-bool-literal", Attrs, !IO),
    insert_space(File, indent(Col), !IO),
    io.write_list(File, Elems, "", output_compact_boolean, !IO),
    io.nl(File, !IO), 
    write_end_tag(File, Col, "array-bool-literal", !IO).

:- pred output_compact_boolean(expr::in, io::di, io::uo) is det.

output_compact_boolean(Expr, !IO) :-
    Expr = expr(RawExpr, _, _),
    ( if RawExpr = lit(bool(Value)) then
        Rep = ( if Value = yes then 't' else 'f' ),
        io.write_char(Rep, !IO)
    else if RawExpr = coerce(_, _, CoercedExpr) then
        output_compact_boolean(CoercedExpr, !IO)
    else
        unexpected($pred, "non-Boolean value encountered.")
    ).

:- pred output_array_integer_literals(io.text_output_stream::in, col::in,
    int::in, exprs::in, io::di, io::uo) is det.

output_array_integer_literals(File, Col, Size, Elems, !IO) :-
    Attrs = [attr("size", int_to_string(Size))],
    write_start_tag(File, Col, "array-int-literal", Attrs, !IO),
    insert_space(File, indent(Col), !IO),
    io.write_list(File, Elems, " ", output_compact_integer, !IO),
    io.nl(File, !IO), 
    write_end_tag(File, Col, "array-int-literal", !IO).

:- pred output_compact_integer(expr::in, io::di, io::uo) is det.

output_compact_integer(Expr, !IO) :-
    Expr = expr(RawExpr, _, _),
    ( if RawExpr = lit(int(Value)) then
        io.write_int(Value, !IO)
    else if RawExpr = coerce(_, _, CoercedExpr) then
        output_compact_integer(CoercedExpr, !IO)
    else
        unexpected($pred, "non-integer value encountered.")
    ).

:- pred output_array_float_literals(io.text_output_stream::in, col::in,
    int::in, exprs::in, io::di, io::uo) is det.

output_array_float_literals(File, Col, Size, Elems, !IO) :-
    Attrs = [attr("size", int_to_string(Size))],
    write_start_tag(File, Col, "array-float-literal", Attrs, !IO),
    insert_space(File, indent(Col), !IO),
    io.write_list(File, Elems, " ", output_compact_float, !IO),
    io.nl(File, !IO), 
    write_end_tag(File, Col, "array-float-literal", !IO).

:- pred output_compact_float(expr::in, io::di, io::uo) is det.

output_compact_float(Expr, !IO) :-
    Expr = expr(RawExpr, _, _),
    ( if RawExpr = lit(floatstr(Value)) then
        io.write_string(Value, !IO)
    else if RawExpr = coerce(_, _, CoercedExpr) then
        output_compact_float(CoercedExpr, !IO)
    else
        unexpected($pred, "non-float value encountered.")
    ).

:- pred output_array_set_of_int_literals(io.text_output_stream::in,
    col::in, int::in, exprs::in, io::di, io::uo) is det.

output_array_set_of_int_literals(File, Col, Size, Elems, !IO) :-
    Attrs = [attr("size", int_to_string(Size))],
    write_start_tag(File, Col, "array-set-of-int-literal", Attrs, !IO),
    list.foldl(expr_to_xml(File, indent(Col)), Elems, !IO),
    write_end_tag(File, Col, "array-set-of-int-literal", !IO).

:- pred output_array_identifiers(io.text_output_stream::in, col::in,
    int::in, exprs::in, io::di, io::uo) is det.

output_array_identifiers(File, Col, Size, Elems, !IO) :-
    Attrs = [attr("size", int_to_string(Size))],
    write_start_tag(File, Col, "array-id-expr", Attrs, !IO),
    insert_space(File, indent(Col), !IO),
    io.write_list(File, Elems, " ", output_compact_identifier, !IO),
    io.nl(File, !IO), 
    write_end_tag(File, Col, "array-id-expr", !IO).

:- pred output_compact_identifier(expr::in, io::di, io::uo) is det.

output_compact_identifier(Expr, !IO) :-
    Expr = expr(RawExpr, _, _),
    ( if RawExpr = ident(Id) then
        io.write_string(Id ^ id_name, !IO)
    else if RawExpr = coerce(_, _, CoercedExpr) then
        output_compact_identifier(CoercedExpr, !IO)
    else
        unexpected($pred, "non-identifier encountered.")
    ).
        
:- pred output_array_accesses(io.text_output_stream::in, col::in, int::in,
    zinc_name::in, exprs::in, io::di, io::uo) is det.

output_array_accesses(File, Col, Size, ArrayName, Elems, !IO) :-
    Attrs = [attr("name", ArrayName), attr("size", int_to_string(Size))],
    write_start_tag(File, Col, "array-accesses-expr", Attrs, !IO),
    insert_space(File, indent(Col), !IO),
    io.write_list(File, Elems, " ", output_compact_access, !IO),
    io.nl(File, !IO),
    write_end_tag(File, Col, "array-accesses-expr", !IO).

:- pred output_compact_access(expr::in, io::di, io::uo) is det.

output_compact_access(Expr, !IO) :-
    Expr = expr(RawExpr, _, _),
    ( if RawExpr = array_access(_, [expr(lit(int(Index)), _, _)]) then
        io.write_int(Index, !IO)
    else if RawExpr = coerce(_, _, CoercedExpr) then
        output_compact_access(CoercedExpr, !IO)
    else
        unexpected($pred, "not a FlatZinc array access")
    ).

%-----------------------------------------------------------------------------%

:- type array_desc
    --->    array_boolean_literals   
    ;       array_integer_literals
    ;       array_float_literals
    ;       array_set_of_int_literals
    ;       array_identifiers
    ;       array_array_accesses(zinc_name)
    ;       array_mixed.      

:- func array_to_desc(exprs) = array_desc.

array_to_desc(Elems) = Desc :-
    (
        Elems = [],
        Desc = array_mixed
    ;
        Elems = [FirstElem | RestElems],
        ( if expr_is_bool_lit(FirstElem) then
            Desc = ( if   list.all_true(expr_is_bool_lit, RestElems)
                     then array_boolean_literals
                     else array_mixed
                   )
        else if expr_is_int_lit(FirstElem) then
            Desc = ( if   list.all_true(expr_is_int_lit, RestElems)
                     then array_integer_literals
                     else array_mixed
                   )
        else if expr_is_float_lit(FirstElem) then
            Desc = ( if   list.all_true(expr_is_float_lit, RestElems)
                     then array_float_literals
                     else array_mixed
                   )
        else if expr_is_identifier(FirstElem) then
            Desc = ( if   list.all_true(expr_is_identifier, RestElems)
                     then array_identifiers
                     else array_mixed
                   )
        else if expr_is_array_access(FirstElem, ArrayName) then
            Desc = ( if   is_array_accesses(ArrayName, RestElems)
                     then array_array_accesses(ArrayName)
                     else array_mixed
                   )
        else if expr_is_set_lit(FirstElem) then
            Desc = ( if   list.all_true(expr_is_set_lit, RestElems)
                     then array_set_of_int_literals
                     else array_mixed
                   )    
        else
            Desc = array_mixed
        )
    ).

:- pred is_array_accesses(zinc_name::in, exprs::in) is semidet.

is_array_accesses(_, []).
is_array_accesses(Name, [E | Es]) :-
    expr_is_array_access(E, Name),
    is_array_accesses(Name, Es).

:- pred expr_is_bool_lit(expr::in) is semidet.

expr_is_bool_lit(Expr) :-
    (
        Expr = expr(lit(bool(_)), _, _)
    ;
        Expr = expr(coerce(_, _, CoercedExpr), _, _),
        expr_is_bool_lit(CoercedExpr)
    ).

:- pred expr_is_int_lit(expr::in) is semidet.

expr_is_int_lit(Expr) :-
    (
        Expr = expr(lit(int(_)), _, _)
    ; 
        Expr = expr(coerce(_, _, CoercedExpr), _, _),
        expr_is_int_lit(CoercedExpr)
    ).

:- pred expr_is_float_lit(expr::in) is semidet.

expr_is_float_lit(Expr) :-
    (
        Expr = expr(lit(floatstr(_)), _, _)
    ;
        Expr = expr(coerce(_, _, CoercedExpr), _, _),
        expr_is_float_lit(CoercedExpr)
    ).

:- pred expr_is_identifier(expr::in) is semidet.

expr_is_identifier(Expr) :-
    (
        Expr = expr(ident(_), _, _)
    ;
        Expr = expr(coerce(_, _, CoercedExpr), _, _),
        expr_is_identifier(CoercedExpr)
    ).

:- pred expr_is_array_access(expr::in, zinc_name::out) is semidet.

expr_is_array_access(Expr, ArrayName) :-
    (
        Expr = expr(array_access(IdExpr, _), _, _),
        IdExpr = expr(ident(Id), _, _),
        ArrayName = Id ^ id_name
    ;
        Expr = expr(coerce(_, _, CoercedExpr), _, _),
        expr_is_array_access(CoercedExpr, ArrayName)
    ).
            
:- pred expr_is_set_lit(expr::in) is semidet.

expr_is_set_lit(Expr) :-
    Expr = expr(RawExpr, _, _),
    (
        RawExpr = lit_set(_)
    ;
        RawExpr = app(Id, _, _, Args),
        Id ^ id_name = "..",
        Args = [_, _]
    ;
        RawExpr = coerce(_, _, CoercedExpr),
        expr_is_set_lit(CoercedExpr)
    ).

%-----------------------------------------------------------------------------%
%
% Annotations
%

:- pred output_annotation(io.text_output_stream::in, col::in, expr::in,
    io::di, io::uo) is det.

output_annotation(File, Col, Ann, !IO) :-
    Ann = expr(RawExpr, _, _),
    ( if RawExpr = lit_ann(Id, Args) then
        Name = id_name(Id),
        list.length(Args, Arity),
        Attrs = [attr("name", Name), attr("arity", int_to_string(Arity))],
        ( if Arity = 0 then
            write_empty_tag(File, Col, "annotation", Attrs, !IO)    
        else
            write_start_tag(File, Col, "annotation", Attrs, !IO),
            list.foldl(output_annotation_arg(File, indent(Col)), Args, !IO),
            write_end_tag(File, Col, "annotation", !IO)
        )
    else
        unexpected($pred, "expr is not an annotation.")
    ).

:- pred output_annotation_arg(io.text_output_stream::in, col::in,
    expr::in, io::di, io::uo) is det.

output_annotation_arg(File, Col, Expr, !IO) :-
    Expr = expr(_, _, ExprInfo),
    TI = ExprInfo ^ expr_type_inst,
    %
    % There are two special cases that need to be handled here:
    % The first is if the argument is an annotation itself,
    % the second is fi the argument is an array of annotations.
    % Otherwise, we just emit the XML correpsonding to a FlatZinc
    % expressions as normal.
    %
    ( if TI = ti_ann then
        output_annotation(File, Col, Expr, !IO)
    else if TI = ti_array(_, ti_ann) then
        output_array_annotation(File, Col, Expr, !IO)
    else
        expr_to_xml(File, Col, Expr, !IO)
    ).

:- pred output_array_annotation(io.text_output_stream::in, col::in, expr::in,
    io::di, io::uo) is det.

output_array_annotation(File, Col, Expr, !IO) :-
    Expr = expr(RawExpr, _, _),
    ( if RawExpr = lit_simple_array(ElemExprs) then
        list.length(ElemExprs, Size),
        Attrs = [attr("size", int_to_string(Size))],
        write_start_tag(File, Col, "array-annotation", Attrs, !IO),
        list.foldl(output_annotation(File, indent(Col)), ElemExprs, !IO),
        write_end_tag(File, Col, "array-annotation", !IO)
    else
        unexpected($pred, "not an array of annotations.")
    ).        
    
    % filter_variable_anns(!Anns, IsIntroducedVar, IsDefinedVar):
    % In XML-FlatZinc certain standard FlatZinc annotations are handled using
    % attributes rather than the general markup for annotations.
    % This predicate filters out such annotations.
    %
:- pred filter_variable_anns(exprs::in, exprs::out, bool::out, bool::out)
    is det.

filter_variable_anns(!Anns, IsIntroducedVar, IsDefinedVar) :-
    list.foldl3(filter_variable_anns_2, !.Anns, [], !:Anns, no, IsIntroducedVar,
        no, IsDefinedVar),
    % Reversing the list is not strictly necessary here, but it improves the
    % way the XML aligns with the FlatZinc from which it was generated.
    list.reverse(!Anns).

:- pred filter_variable_anns_2(expr::in, exprs::in, exprs::out,
    bool::in, bool::out, bool::in, bool::out) is det.

filter_variable_anns_2(Ann, !Anns, !IsIntroducedVar, !IsDefinedVar) :-
    RawExpr = Ann ^ raw_expr,
    ( if RawExpr = lit_ann(Id, []), id_name(Id) = "var_is_introduced" then
        !:IsIntroducedVar = yes
    else if RawExpr = lit_ann(Id, []), id_name(Id) = "is_defined_var" then
        !:IsDefinedVar = yes
    else
        !:Anns = [Ann | !.Anns]
    ).
    
    % filter_constraint_anns(!Anns, MaybeDefinesVar):
    %
:- pred filter_constraint_anns(exprs::in, exprs::out, maybe(string)::out)
    is det.

filter_constraint_anns(!Anns, MaybeDefinesVar) :-
    list.foldl2(filter_constraint_anns_2, !.Anns, [], !:Anns,
        no, MaybeDefinesVar),
    list.reverse(!Anns).

:- pred filter_constraint_anns_2(expr::in, exprs::in, exprs::out,
    maybe(string)::in, maybe(string)::out) is det.

filter_constraint_anns_2(Ann, !Anns, !MaybeDefinesVar) :-
    RawExpr = Ann ^ raw_expr,
    ( if    
        RawExpr = lit_ann(Id, Arg),
        id_name(Id) = "defines_var",
        Arg = [expr(ident(DefinedVarId), _, _)],
        DefinedVarName = id_name(DefinedVarId) 
    then
        !:MaybeDefinesVar = yes(DefinedVarName)
    else
        !:Anns = [Ann | !.Anns]
    ).

%-----------------------------------------------------------------------------%
%
% Utility procedures for XML-FlatZinc attributes
%

:- func size_to_attr(int) = attr.

size_to_attr(Size) = attr("size", int_to_string(Size)).

:- func card_to_attr(int) = attr.

card_to_attr(Card) = attr("card", int_to_string(Card)).

:- func float_range_to_attrs(string, string) = attrs.

float_range_to_attrs(Lo, Hi) = [
    attr("lo", Lo),
    attr("hi", Hi)
].

:- func int_range_to_attrs(int, int) = attrs.

int_range_to_attrs(Lo, Hi) = [
    attr("lo", int_to_string(Lo)),
    attr("hi", int_to_string(Hi))
].

%-----------------------------------------------------------------------------%
%
% Utility procedures for use on the AST
%
   
:- func expr_to_array_name(expr) = string.

expr_to_array_name(Expr) = Name :-
    Expr = expr(RawExpr, _, _),
    ( if RawExpr = ident(Id)
    then Name = Id ^ id_name
    else unexpected($pred, "expected an identifier.")
    ).
    
:- func expr_to_integer(expr) = int.

expr_to_integer(Expr) = Int :-
    Expr = expr(RawExpr, _, _),
    ( if RawExpr = lit(int(Int0))
    then Int = Int0
    else unexpected($pred, "not an integer.")
    ).

:- func exprs_to_ints(exprs) = list(int).

exprs_to_ints([]) = [].
exprs_to_ints([E | Es]) = [I | Is] :-
    ( if E = expr(lit(int(I0)), _, _)
    then I = I0
    else unexpected($pred, "expr is not an integer")
    ),
    Is = exprs_to_ints(Es).

%-----------------------------------------------------------------------------%
%
% XML output
%
    
    % write_start_tag(File, Col, Name, Attributes, !IO):
    % Output the start-tag of the XML element Name to File.
    % Col is the number of spaces to emit before the tag is printed.
    %
:- pred write_start_tag(io.text_output_stream::in, col::in, string::in,
    attrs::in, io::di, io::uo) is det.

write_start_tag(File, Col, TagName, Attributes, !IO) :-
    insert_space(File, Col, !IO),
	io.format(File, "<%s", [s(TagName)], !IO),
    list.foldl(write_attr(File), Attributes, !IO),
    io.write_string(File, ">", !IO),
    io.nl(File, !IO).

    % write_end_tag(File, Col, Name, !IO):
    % Output the end-tag of the XML element Name to File.
    % Col is the number of spaces to indent 
:- pred write_end_tag(io.text_output_stream::in, col::in, string::in,
    io::di, io::uo) is det.

write_end_tag(File, Col, String, !IO) :-
    insert_space(File, Col, !IO),
	io.format(File, "</%s>\n", [s(String)], !IO).

    % write_empty_tag(File, Col, Name, Attributes, !IO):
    %
:- pred write_empty_tag(io.text_output_stream::in, col::in, string::in,
	attrs::in, io::di, io::uo) is det.

write_empty_tag(File, Col, TagName, Attributes, !IO) :-
    insert_space(File, Col, !IO),
    io.format(File, "<%s", [s(TagName)], !IO),
    list.foldl(write_attr(File), Attributes, !IO),
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
    insert_space_rec(File, Col, "", !IO).

:- pred insert_space_rec(io.text_output_stream::in, col::in, string::in,
    io::di, io::uo) is det.

insert_space_rec(File, Col, Space, !IO) :-
    Result = ordering(Col, 0),
    (
        Result = (<),
        unexpected($pred, "Invalid number of columns to indent by.")
    ;
        Result = (=),
        io.write_string(File, Space, !IO)
    ;
        Result = (>),
        insert_space_rec(File, Col-1, Space ++ " ", !IO)
    ).

:- func indent(int) = int.

indent(Col) = Col + 2.

    % escape_chars(A) = B:
    % B is the string A with any FlatZinc (Mercury) special characters
    % escaped.
    %
:- func escape_chars(string::in) = (string::uo) is det.

escape_chars(Str) = EscapedStr :-
    string.foldl(escape_char, Str, [], RevEscapedCharList),
    EscapedStr = string.from_rev_char_list(RevEscapedCharList).

:- pred escape_char(char::in, list(char)::in, list(char)::out) is det.

escape_char(Char, !Chars) :-
    ( if Char = ('\a') then
        !:Chars = ['a', '\\' | !.Chars]
    else if Char = ('\b') then
        !:Chars = ['b', '\\' | !.Chars]
    else if Char = ('\f') then
        !:Chars = ['f', '\\' | !.Chars]
    else if Char = ('\n') then
        !:Chars = ['n', '\\' | !.Chars]
    else if Char = ('\r') then
        !:Chars = ['r', '\\' | !.Chars]
    else if Char = ('\t') then
        !:Chars = ['t', '\\' | !.Chars]
    else if Char = ('\v') then
        !:Chars = ['v', '\\' | !.Chars]
    else
        !:Chars = [Char | !.Chars]
    ).
    
    % write_item_as_comment(File, Item, !IO):
    % Emit the FlatZinc item as an XML comment.
    %
:- pred write_item_as_comment(io.text_output_stream::in, raw_item::in,
    io::di, io::uo) is det.

write_item_as_comment(File, RawItem, !IO) :-
    Doc = doc_raw_item(pp_lang_flatzinc(dont_print_coercions), RawItem),
    String = pprint.to_string(80, Doc),
    io.format(File, "<!-- %s -->\n", [s(String)], !IO).

%-----------------------------------------------------------------------------%
%
% Option processing
%

:- type fzn2xml_option
    --->    help
    ;       version
    ;       verbose
    ;       output_to_stdout
    ;       output_comments
    ;       system_dtd_location
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

    % The valid fzn2xml short options.
    %
:- pred fzn2xml_short_option(char::in, fzn2xml_option::out) is semidet.

fzn2xml_short_option('h', help).
fzn2xml_short_option('v', verbose).
fzn2xml_short_option('S', statistics).

    % The valid fzn2xml long options.
    %
:- pred fzn2xml_long_option(string::in, fzn2xml_option::out) is semidet.

fzn2xml_long_option("help",                 help).
fzn2xml_long_option("version",              version).
fzn2xml_long_option("verbose",              verbose).
fzn2xml_long_option("output-to-stdout",     output_to_stdout).
fzn2xml_long_option("output-comments",      output_comments).
fzn2xml_long_option("system-dtd-location",  system_dtd_location).


fzn2xml_long_option("statistics",           statistics).
fzn2xml_long_option("pprint-before",        pprint_before).
fzn2xml_long_option("pprint-after",         pprint_after).
fzn2xml_long_option("pprint-ignore-file",   pprint_ignore_file).
fzn2xml_long_option("dump-before",          dump_before).
fzn2xml_long_option("dump-after",           dump_after).
fzn2xml_long_option("stop-before",          stop_before).
fzn2xml_long_option("stop-after",           stop_after).
    
    % Nondeterministically returns all the options with their corresponding
    % types and default values.
    % (The second mode is used to check that we cover all options.)
    %
:- pred fzn2xml_option_defaults(fzn2xml_option, option_data).
:- mode fzn2xml_option_defaults(out, out) is multi.
:- mode fzn2xml_option_defaults(in, out) is det.

fzn2xml_option_defaults(help,                bool(no)).
fzn2xml_option_defaults(version,             bool(no)).
fzn2xml_option_defaults(verbose,             bool(no)).
fzn2xml_option_defaults(output_to_stdout,    bool(no)).
fzn2xml_option_defaults(output_comments,     bool(no)).
fzn2xml_option_defaults(system_dtd_location, string("")).

fzn2xml_option_defaults(statistics,         bool(no)).
fzn2xml_option_defaults(pprint_before,      accumulating([])).
fzn2xml_option_defaults(pprint_after,       accumulating([])).
fzn2xml_option_defaults(pprint_ignore_file, accumulating([])).
fzn2xml_option_defaults(dump_before,        accumulating([])).
fzn2xml_option_defaults(dump_after,         accumulating([])).
fzn2xml_option_defaults(stop_before,        accumulating([])).
fzn2xml_option_defaults(stop_after,         accumulating([])).

%-----------------------------------------------------------------------------%

    % NOTE: changes here may need to be reflected in:
    %
    %   g12/zinc/man/fzn2xml.1.in
    %   
:- func fzn2xml_usage = string.

fzn2xml_usage = UsageMsg :-
    UsageLines = [
    "Usage: fzn2xml [<options>] <file>.fzn"
,   "Options:"
,   "    -h, --help"
,   "       Print this message."
,   "    --version"
,   "       Print version information."
,   "    -v, --verbose"
,   "       Output progress information as conversion proceeds."
,   "   --output-to-stdout"
,   "       Write the XML document to the standard output instead of to"
,   "       the default output file, <file>.xml."
,   "   --output-comments"
,   "       Include the items in the FlatZinc instance as XML comments in the"
,   "       generated XML document."
,   "   --system-dtd-location <dir>"
,   "       Specify the directory containing the XML-FlatZinc DTD."
,   "       This directory will be used in the DOCTYPE declaration of the"
,   "       generated XML document." 
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
    list.map(func(X) = "        " ++ X, fzn2xml_stage_names),
    UsageMsg = foldr(func(X, Xs) = X ++ "\n" ++ Xs, UsageLines, "").

:- func fzn2xml_version = string.

fzn2xml_version = VersionMsg :-
    Version = get_fzn2xml_version,
    VersionMsg = 
        "G12 FlatZinc to XML converter, version " ++ Version ++ "\n"
++      "Copyright (C) 2009-2012 The University of Melbourne and NICTA\n".

:- pred bad_cmdline(string::in, io::di, io::uo) is det.

bad_cmdline(Msg, !IO) :-
    io.write_string(io.stderr_stream,
        "fzn2xml: " ++ Msg ++ "\n"
     ++ "fzn2xml: use --help for more information.\n",
        !IO),
    io.set_exit_status(1, !IO).

%-----------------------------------------------------------------------------%
    
    % Return the debug spec that is implicit in the option table.
    % This assumes that we have already checked that the stage names are
    % valid.
    %  
:- func make_fzn2xml_debug_spec(option_table(fzn2xml_option)) = debug_spec.

make_fzn2xml_debug_spec(OptionTable) = DebugSpec :-
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

:- type fzn2xml_control
    --->    fzn2xml_control.

:- instance frontend_control(fzn2xml_control) where [
    ( warn_unknown_fzn_annotations(_) = no ),
    ( extra_builtin_annotation(_, _, _, _) :- false ),
    ( post_typecheck_var_decl_action(_) = no ),
    ( post_typecheck_constraint_action(_) = no ),
    ( post_typecheck_solve_action(_) = no )
].

%-----------------------------------------------------------------------------%
%
% Environment variables
%

:- pred process_fzn2xml_environment_vars(list(string)::out,
    io::di, io::uo) is det.

process_fzn2xml_environment_vars([], !IO).

%-----------------------------------------------------------------------------%

    % Returns a string that gives the name of this program as it will
    % appear in error messages.
    %
:- func fzn2xml_program = string.

fzn2xml_program = "fzn2xml".

%-----------------------------------------------------------------------------%
:- end_module fzn2xml.
%-----------------------------------------------------------------------------%
