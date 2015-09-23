%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2012 The University of Melbourne and NICTA.
% See the file COPYING for license information.
%-----------------------------------------------------------------------------%
%
% Author: Nicholas Nethercote <njn@csse.unimelb.edu.au>
%
% A type- and inst-checker for Zinc, MiniZinc and FlatZinc.  It:
%
% - Detects uses of undeclared names, undefined names (where appropriate),
%   doubly-defined names, and mismatches between declarations and definitions
%   (eg. if "n" is declared as a type and defined as a function).
%
% - Adds a scope-number to each id in the AST.
%
% - Builds the symbol table.
%
% - Type-inst checks the code.
%
% - (Zinc and MiniZinc only) Adds coercions where necessary (either with a
%   'coerce' expression, or for some literals by changing it, eg. literal ints
%   become literal floats).  Some places where coercions aren't added:
%   - Anonymous variables aren't coerced.
%   - Empty set and array literals aren't coerced.
%   In both cases, the type-inst can be found in the expr_info.  The reason
%   these coercions aren't added is that they are ignored by several subsequent
%   stages.  AST-to-term conversion has to add them in manually, but that's
%   easier than ignoring them in the code-generator, minizinc-to-flatzinc, and
%   the run-time data file interpreter.
%
% - Annotates expressions with their type-inst.
%
% - Annotates call sites to indicate which procedure of the pred/func/op is
%   being called.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Basic approach:
%
% - Preliminaries: builtins are added to the symbol table.
% - Pass 1: declarations are processed, scopes are computed, every non-global
%   id is labelled with its scope number, and type/inst-checking of
%   expressions is done.  These can be all be done in one pass because the
%   code is topologically sorted.
% - Definedness checking: we check all the symbols and complain if any
%   undefined symbols should have been defined.
%
% About the scope-numbers:  Every global identifier is part of the global
% scope, which is labelled with 'global_scope_number', and so we don't bother
% marking them with their scope.  Each local scope gets allocated a
% scope-number higher than 'global_scope_number', and each non-local id gets
% marked with this scope-number so its symbol can be found in the symbol
% table.  At each point, we maintain a stack of current scopes (ScopeNs), and
% when looking at ids to decide which scope they belong to, we look first in
% the innermost scope, then the next innermost, etc.
%
% About the type-inst-checking:  It is mostly straightforward because Zinc is
% mostly first-order.  The type-inst of any expression can be determined just
% by looking at it, independent of context (bottom-up information flow).  Then
% we check that the expression has an appropriate type-inst for its context,
% possibly coercing it (top-down information flow).
%
% For example, consider this item:
%
%   array[int] of var float: x = [1,2,_];
%
% '1' and '2' are par_int, '_' is var_bottom, thus the overall context for the
% array elements is ti_var_int.  So the '1' and '2' get a coercion added,
% giving us this array expression:
%
%       [ coerce(ti_par_int, ti_var_int, 1),
%         coerce(ti_par_int, ti_var_int, 2), _ ];
%
% (The unfixed value '_' never has an explicit coercion added to it -- its
% type can be found from its annotation.)
%
% We then consider the assignment context, which is ti_array(ti_par_int,
% ti_var_float).  'x' is ti_array(ti_par_int, ti_var_int) which matches, but
% not exactly, so 'x' gets coerced.  You might expect the result to be this:
%
%   array[int] of var float: x =
%     coerce(ti_array(ti_par_int, ti_var_int),
%            ti_array(ti_par_int, ti_var_float),
%       [ coerce(ti_par_int, ti_var_int, 1),
%         coerce(ti_par_int, ti_var_int, 2), _ ];
%     )
%
% but when adding coercions we avoid explicit coercions where possible by
% transforming literals instead.  The final result is thus this:
%
%   array[int] of var float: x =
%     [ coerce(ti_par_int, ti_var_float, 1),
%       coerce(ti_par_int, ti_var_float, 2), _ ];
%
%
% Unimplemented features
% ----------------------
%
% Ones that cause aborts:
% - Variant records
%
% Other:
% - Should probably give warnings when a declared variable overshadows another.
% - User-defined overloading:  it's supported, but several checks that need to
%   be done on overloaded funcs/preds aren't implemented.  This means
%   bad overloading is currently possible, it's up to the modeller to be
%   reasonable.  Look for "[overloading]" for what to add.
%
%-----------------------------------------------------------------------------%

:- module type_inst_check.
:- interface.

%-----------------------------------------------------------------------------%

:- import_module compiler_common.
:- import_module symbol_table.
:- import_module zinc_ast.
:- import_module zinc_error.
:- import_module zinc_common.

    % Type-inst check a list of items.
    % (Used for type-checking the contents of .dzn files in the Zinc
    % runtime.)
    %
:- pred type_inst_check_ast(Ctrl::in)
    : stage({checking, ast, symbol_table}, {ast, symbol_table})
    `with_inst` stage <= frontend_control(Ctrl).

    % Type-inst check a whole model.
    %
:- pred type_inst_check_sast(Ctrl::in)
    : stage({checking, sast, symbol_table}, {sast, symbol_table})
    `with_inst` stage <= frontend_control(Ctrl).

    % Type-inst check an ozn file.
    %
:- pred type_inst_check_oast(Ctrl::in)
    : stage(oast, {oast, symbol_table})
    `with_inst` stage <= frontend_control(Ctrl).

    % Type-inst check a MiniZinc solution w.r.t the symbol table of an
    % already type-inst checked .ozn file.
    %
:- pred type_inst_check_mzn_soln(Ctrl::in, symbol_table::in, symbol_table::out,
    items::in, items::out, zinc_errors::out, zinc_warnings::out)
    is det <= frontend_control(Ctrl).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module builtins.
:- import_module types_and_insts.
:- import_module zinc_error.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

    % This type is used only to reduce the number of args passed around.
:- type tic_state
    --->    tic_state(
                lang        :: lang,            % Language being checked
                checking    :: checking,        % Model or instance checking?
                sym_tbl     :: symbol_table,    % Symbol table
                errors      :: zinc_errors,     % Accumulated errors
                warnings    :: zinc_errors      % Accumulated warnings
            ).

:- type scope_nums == list(int).

:- type tic_X(X)           == ( pred(X, X,       tic_state, tic_state) ).
:- type tic_X_Y(X, Y)      == ( pred(X, X, Y,    tic_state, tic_state) ).
:- inst tic_X              == ( pred(in, out,           in, out) is det  ).
:- inst tic_X_Y            == ( pred(in, out, out,      in, out) is det  ).

    % Sometimes, the way we type-inst check things depends on their context.
    % For example, type-inst variables are only allowed within preds/funcs,
    % and enum accesses are only allowed within case expressions.  This type
    % is used to record these contexts.
    %
:- type context == list(context_elem).
:- type context_elem
    --->    in_predfunc_decl
            % Inside a pred/func arg or return type-inst.

    ;       in_ann_decl
            % Inside an annotation arg.

    ;       in_predfunc_body
             % Inside a pred/func body.

    ;       in_case_label_expr
            % Inside a case label expression.

    ;       in_assigned_let_var
            % Inside an assigned let-variable.

    ;       in_assigned_var_decl
            % Inside an assigned global variable declaration.

    ;       in_other.
            % Inside something else.

%-----------------------------------------------------------------------------%
%
% Top-level of type-checking
%

type_inst_check_ast(Ctrl, Lang, {Checking, Items, SymTbl}, {NItems, NSymTbl},
        Errs, Warns) :-
    some [!S] (
        !:S = tic_state(Lang, Checking, SymTbl, [], []),
        add_builtin_operation_symbols(Ctrl, !S),
        InitScopeNs = [],
        % NOTE: we do foldl + reverse here instead of using map_foldl to avoid
        % stack exhaustion with large models.
        list.foldl2(tic_item_acc(Ctrl, InitScopeNs), Items, [], RevNItems, !S),
        list.reverse(RevNItems, NItems),
        symbol_table.foldl(check_definitions, !.S ^ sym_tbl, !S),
        !.S = tic_state(_NLang, _NChecking, NSymTbl, Errs, Warns)
    ).

    % Assumption:  type-checking is done for a whole model, ie. this predicate
    % is only called once.  In which case it's reasonable to add the builtins
    % here.
type_inst_check_sast(Ctrl, Lang, {Checking, AST, SymTbl}, {NAST, NSymTbl},
        Errs, Warns) :-
    some [!S] (
        !:S = tic_state(Lang, Checking, SymTbl, [], []),
        AST = sast(Items, SolveItem, MaybeOutputItem),
        add_builtin_operation_symbols(Ctrl, !S),
        InitScopeNs = [],

        % NOTE: we do foldl + reverse here instead of using map_foldl to avoid
        % stack exhaustion with large models.
        list.foldl2(tic_item_acc(Ctrl, InitScopeNs), Items, [], RevNItems, !S),
        list.reverse(RevNItems, NItems),

        SolveItem = sast_solve_item(SolveKind, SolveAnns, SolveSrcLocn),
        tic_solve_item(Ctrl, InitScopeNs, SolveSrcLocn, SolveKind, NSolveKind,
            SolveAnns, NSolveAnns, !S),
        NSolveItem = sast_solve_item(NSolveKind, NSolveAnns, SolveSrcLocn),

        map_fold_maybe(tic_item(Ctrl, InitScopeNs), MaybeOutputItem,
            NMaybeOutputItem, !S),
        symbol_table.foldl(check_definitions, !.S ^ sym_tbl, !S),
        NAST = sast(NItems, NSolveItem, NMaybeOutputItem),
        !.S = tic_state(_NLang, _NChecking, NSymTbl, Errs, Warns)
    ).

type_inst_check_oast(Ctrl, _Lang, OAST, {NOAST, NSymTbl},
        Errs, Warns) :-
    some [!S] (
        SymTbl = symbol_table.init,
        !:S = tic_state(lang_minizinc, instance_checking, SymTbl, [], []),
        OAST = oast(Items, OutputItem),
        add_builtin_operation_symbols(Ctrl, !S),
        InitScopeNs = [],

        % NOTE: we do foldl + reverse here instead of using map_foldl to avoid
        % stack exhaustion with large models.
        list.foldl2(tic_item_acc(Ctrl, InitScopeNs), Items, [], RevNItems, !S),
        list.reverse(RevNItems, NItems),

        OutputExpr = OutputItem ^ oast_out_expr,
        expr_has_type_inst(Ctrl, "output expression", [], InitScopeNs,
            ti_array(ti_par_int, ti_par_string), OutputExpr, NOutputExpr, !S),
        NOutputItem = OutputItem ^ oast_out_expr := NOutputExpr,

        symbol_table.foldl(check_definitions, !.S ^ sym_tbl, !S),
        NOAST = oast(NItems, NOutputItem),
        !.S = tic_state(_, _, NSymTbl, Errs, Warns)
    ).

%-----------------------------------------------------------------------------%

type_inst_check_mzn_soln(Ctrl, !SymTbl, !Items, Errors, Warnings) :-
    some [!S] (
        !:S = tic_state(lang_minizinc, instance_checking, !.SymTbl, [], []),
        InitScopeNs = [],

        % NOTE: we do foldl + reverse here instead of using map_foldl to avoid
        % stack exhaustion with large models.
        list.foldl2(tic_item_acc(Ctrl, InitScopeNs), !.Items, [], RevItems,
            !S),
        list.reverse(RevItems, !:Items),

        % XXX we shouldn't check all of the definitions from the .ozn
        % file again!
        symbol_table.foldl(check_definitions, !.S ^ sym_tbl, !S),
        !.S = tic_state(_, _, !:SymTbl, Errors, Warnings)
    ).

%-----------------------------------------------------------------------------%

:- pred add_builtin_operation_symbols(Ctrl::in, tic_state::in, tic_state::out)
    is det <= frontend_control(Ctrl).

add_builtin_operation_symbols(Ctrl, !S) :-
    Lang = !.S ^ lang,
    OpSigs = all_operator_ti_sigs(Lang),
    PrSigs = all_builtin_pred_ti_sigs(Lang),
    FnSigs = all_builtin_func_ti_sigs(Lang),
    AnSigs = all_builtin_ann_ti_sigs(Ctrl, Lang),
    list.foldl(add_builtin_operation_sym(operator_operation),  OpSigs, !S),
    list.foldl(add_builtin_operation_sym(predicate_operation), PrSigs, !S),
    list.foldl(add_builtin_operation_sym(function_operation),  FnSigs, !S),
    list.foldl(add_builtin_annotation_sym, AnSigs, !S).

:- pred add_builtin_operation_sym(operation_kind::in,
    pair(zinc_name, type_inst_sigs)::in, tic_state::in, tic_state::out) is det.

add_builtin_operation_sym(OpKind, Name - TISigs, !S) :-
    % Add a ProcN to each one first.
    P = (pred(TISig::in, ProcInfo::out, !.N::in, !:N::out) is det :-
        ProcInfo = proc_info(!.N, defined, proc_is_not_annotated, TISig),
        !:N = !.N + 1
    ),
    list.map_foldl(P, TISigs, ProcInfos, 1, _),
    ( if !.S ^ lang = lang_flatzinc, ProcInfos \= [_] then
        unexpected($pred, "overloaded FlatZinc built-in: " ++ Name)
    else
        true
    ),
    OpSym = sym_operation(OpKind, ProcInfos),
    Id = id_global(Name),
    add_unseen_symbol(Id, OpSym, builtin, !.S ^ sym_tbl, Temp),
    !S ^ sym_tbl := Temp.

:- pred add_builtin_annotation_sym(pair(zinc_name, type_insts)::in,
    tic_state::in, tic_state::out) is det.

add_builtin_annotation_sym(Name - ArgTIs, !S) :-
    AnnSym = sym_annotation(ArgTIs),
    Id = id_global(Name),
    add_unseen_symbol(Id, AnnSym, builtin, !.S ^ sym_tbl, Temp),
    !S ^ sym_tbl := Temp.

%-----------------------------------------------------------------------------%

:- pred must_be_lang(string::in, list(lang)::in, tic_state::in) is det.

must_be_lang(Str, Langs, S) :-
    ( if list.member(S ^ lang, Langs)
    then true
    else unexpected($pred, "must_be_lang: failed: " ++ Str)
    ).

%-----------------------------------------------------------------------------%
%
% Items.
%

:- pred tic_item_acc(Ctrl::in, scope_nums::in, item::in,
    list(item)::in, list(item)::out, tic_state::in, tic_state::out)
    is det <= frontend_control(Ctrl).

tic_item_acc(Ctrl, ScopeNs, Item0, !Items, !S) :-
    tic_item(Ctrl, ScopeNs, Item0, Item, !S),
    !:Items = [Item | !.Items].

:- pred tic_item(Ctrl::in, scope_nums::in) : tic_X(item)
    `with_inst` tic_X <= frontend_control(Ctrl).

tic_item(Ctrl, ScopeNs, item(RawItem, Locn), item(NRawItem, Locn), !S) :-
    tic_raw_item(Ctrl, Locn, ScopeNs, RawItem, NRawItem, !S).

%-----------------------------------------------------------------------------%

:- pred tic_raw_item(Ctrl::in, src_locn::in, scope_nums::in)
    : tic_X(raw_item) `with_inst` tic_X <= frontend_control(Ctrl).

tic_raw_item(Ctrl, Locn, ScopeNs,
        type_inst_syn_item(Name, AnnEs,  MaybeTIE),
        type_inst_syn_item(Name, NAnnEs, NMaybeTIE), !S) :-
    (
        MaybeTIE  = yes(TIE),
        ti_expr_to_type_inst(Ctrl, [], ScopeNs, TIE, NTIE, TI, !S),
        IsUsableAsValue = ti_expr_is_usable_as_value(NTIE, !.S ^ sym_tbl),
        IsDefnReqd = is_definition_required(!.S, NTIE),
        IsFinite = ti_expr_is_finite(!.S ^ sym_tbl, NTIE),
        TypeSynInfo = type_syn_info_defined(TI, IsUsableAsValue, IsDefnReqd,
            IsFinite),
        NMaybeTIE = yes(NTIE)
    ;
        MaybeTIE  = no,
        TypeSynInfo = type_syn_info_undefined,
        NMaybeTIE = no
    ),
    tic_annotations(Ctrl, [], ScopeNs, AnnEs, NAnnEs, !S),
    add_global_symbol(Locn, sym_type_inst_synonym(TypeSynInfo), Name, !S).

tic_raw_item(Ctrl, Locn, ScopeNs,
        enum_item(EnumName, AnnEs,  MaybeEnumDefn),
        enum_item(EnumName, NAnnEs, NMaybeEnumDefn), !S) :-
    (
        MaybeEnumDefn = enum_defn_no,
        NMaybeEnumDefn = enum_defn_no,
        EnumInfo = enum_info_undefined
    ;
        MaybeEnumDefn = enum_defn_flat(FlatCases),
        % Add the case names to the symbol table.
        DoFlatEnumCase = (pred(CaseName::in, !.S::in , !:S::out) is det :-
            CaseSym = sym_enum_case_name(EnumName),
            add_global_symbol(Locn, CaseSym, CaseName, !S)
        ),
        list.foldl(DoFlatEnumCase, FlatCases, !S),
        EnumInfo = enum_info_flat(FlatCases),
        NMaybeEnumDefn = enum_defn_flat(FlatCases)
    ;
        MaybeEnumDefn = enum_defn_nonflat(Cases),

        % Process the cases.
        DoEnumCase = (pred(Case0::in, Case::out,
                !.CaseInfos::in, !:CaseInfos::out,
                !.AllFieldNames::in, !:AllFieldNames::out, !.S::in, !:S::out)
                is det :-
            Case0 = nonflat_enum_case(CaseName, TIEsNames),

            % Add case name to the symbol table.
            CaseSym = sym_enum_case_name(EnumName),
            add_global_symbol(Locn, CaseSym, CaseName, !S),

            % Record the field names.
            FieldNames = assoc_list.values(TIEsNames),
            !:AllFieldNames = !.AllFieldNames ++ FieldNames,

            % Process the fields and construct the field infos.
            list.map2_foldl(ti_expr_and_name_to_field_info(Ctrl, [], ScopeNs),
                TIEsNames, NTIEsNames, FieldInfos, !S),

            CaseInfo = nfe_case_info(CaseName, FieldInfos),
            !:CaseInfos = cord.snoc(!.CaseInfos, CaseInfo),

            Case = nonflat_enum_case(CaseName, NTIEsNames)
        ),
        list.map_foldl3(DoEnumCase, Cases, NCases, cord.empty, CaseInfos0,
            [], AllFieldNames, !S),
        CaseInfos = cord.list(CaseInfos0),

        % Check for duplicated field names.
        ( if RepFieldName = first_repeated_elem(AllFieldNames) then
            RepFieldNameErr = [
                words("field name"), quote(RepFieldName),
                words("repeated in enum")
            ],
            symbol_error(RepFieldNameErr, Locn, _, !S)
        else
            true
        ),

        % Compute finiteness.
        GetTIEsFromCase = (pred(nonflat_enum_case(_CaseNames, TIEsNames)::in,
                TIEs::in, TIEs0 ++ TIEs::out) is det :-
            TIEs0 = assoc_list.keys(TIEsNames)
        ),
        list.foldl(GetTIEsFromCase, NCases, [], AllTIEs),
        IsFinite = ti_exprs_are_finite(!.S ^ sym_tbl, AllTIEs),
        EnumInfo = enum_info_nonflat(IsFinite, CaseInfos),
        NMaybeEnumDefn = enum_defn_nonflat(NCases)
    ),
    % Add symbol the for enum itself.
    add_global_symbol(Locn, sym_enum(EnumInfo), EnumName, !S),
    tic_annotations(Ctrl, [], ScopeNs, AnnEs, NAnnEs, !S).

tic_raw_item(Ctrl, Locn, ScopeNs, !RawItem, !S) :-
    !.RawItem = var_decl_item(TIE, Name, AnnEs, MaybeE),
    % XXX We should also check if there is a separate assignment to this
    % variable, but we haven't processed the assign items yet.
    (
        ( MaybeE = rhs_assignment(_)
        ; MaybeE = separate_assignment(_, _)
        ),
        Context = [in_assigned_var_decl]
    ;
        MaybeE = no_assignment,
        Context = []
    ),
    % First deal with the type-inst expression.
    ti_expr_to_type_inst(Ctrl, Context, ScopeNs, TIE, NTIE, TI, !S),
    % This must be run after the TIE has been processed, so if it's an
    % identifier TIE, the scope id has been added.
    IsDefnReqd = is_definition_required(!.S, NTIE),

    % Handle the initialisation value, if present.
    (
        MaybeE = rhs_assignment(E),
        IsDefd = defined,
        HasRangeValue = expr_has_range_value(E),
        expr_has_type_inst(Ctrl,
            "initialisation value for `" ++ Name ++ "'",
            /*Context*/[], ScopeNs, TI, E, NE, !S),
        NMaybeE = rhs_assignment(NE)
    ;
        MaybeE = separate_assignment(AssignSrcLocn, E),
        IsDefd = defined,
        HasRangeValue = expr_has_range_value(E),
        expr_has_type_inst(Ctrl,
            "assignment value for `" ++ Name ++ "'",
            /*Context*/[], ScopeNs, TI, E, NE, !S),
        NMaybeE = separate_assignment(AssignSrcLocn, NE)
    ;
        MaybeE  = no_assignment,
        IsDefd  = undefined,
        HasRangeValue = does_not_have_range_value,
        NMaybeE = no_assignment
    ),

    % Check annotations.
    tic_annotations(Ctrl, /*Context*/[], ScopeNs, AnnEs, NAnnEs, !S),

    % Then add the variable to the symbol table.
    Sym = sym_variable(TI, global_var, IsDefd, IsDefnReqd, HasRangeValue),
    add_global_symbol(Locn, Sym, Name, !S),

    % Check that constrained type-insts are not used for FlatZinc parameters.
    % We do this as part of type checking because:
    % (1) it allows us to generate more meaningful error messages than during
    %     parsing
    % (2) for cadmap we want detect this sort of error *after* in the result
    %     of the model transformation.
    %
    Lang = !.S ^ lang,
    (
        ( Lang = lang_minizinc
        ; Lang = lang_zinc
        )
    ;
        Lang = lang_flatzinc,
        tic_fzn_var_decl(Locn, NTIE, Name, !S),
        check_fzn_output_anns(Locn, NTIE, Name, NAnnEs, !S)
    ),

    % Run any client specific type-checking actions for var decl. items.
    %
    MaybeClientAction = post_typecheck_var_decl_action(Ctrl),
    (
        MaybeClientAction = no
    ;
        MaybeClientAction = yes(ClientAction),
        Errors0 = !.S ^ errors,
        ClientAction(Locn, NTIE, Name, NAnnEs, NMaybeE, Errors0, Errors),
        !S ^ errors := Errors
    ),
    !:RawItem = var_decl_item(NTIE, Name, NAnnEs, NMaybeE).

tic_raw_item(Ctrl, Locn, ScopeNs, !RawItem, !S) :-
    !.RawItem = assign_item(Name, E),
    else_unexpected(unify(ScopeNs, []), $pred ++ ": bad ScopeNs"),
    Id = id_global(Name),
    ( if maybe_find_symbol(Id, !.S^sym_tbl, Sym, _) then
        ( if Sym = sym_variable(TI,_,IsDefd,_,_) then
            % Check for multiple definitions.
            (
                IsDefd = defined,
                MultiAssignErr = [
                    words("variable"), quote(Name),
                    words("assigned more than once")
                ],
                symbol_error(MultiAssignErr, Locn, _, !S),
                NE = E
            ;
                IsDefd = undefined,
                Sym2 = Sym ^ has_range_value := expr_has_range_value(E),
                Sym3 = Sym2 ^ is_defined := defined,
                replace_existing_symbol(id_global(Name), Sym3, Locn,
                    !.S ^ sym_tbl, Temp),
                !S ^ sym_tbl := Temp,
                % Check the type-inst.
                expr_has_type_inst(Ctrl,
                    "assignment value for `" ++ Name ++ "'", [],
                    ScopeNs, TI, E, NE, !S)
            )
        else
            NonVarAssignErr = [
                words("assignment to a non-variable"),
                quote(Name)
            ],
            symbol_error(NonVarAssignErr, Locn, _, !S),
            NE = E
        )
    else
        UnDeclErr = [
            quote(Name), words("undeclared")
        ],
        symbol_error(UnDeclErr, Locn, _, !S),
        NE = E
    ),
    !:RawItem = assign_item(Name, NE).

tic_raw_item(Ctrl, _Locn, ScopeNs, output_item(E), output_item(NE), !S) :-
    expr_has_type_inst(Ctrl, "output expression", [], ScopeNs,
        ti_array(ti_par_int, ti_par_string), E, NE, !S).

tic_raw_item(Ctrl, Locn, ScopeNs, constraint_item(E), constraint_item(NE), !S) :-
    type_inst_of_expr(Ctrl, [], ScopeNs, E, NE, TI, !S),
    ( if ti_eq(TI, ti_var_bool) then
        true
    else if ti_eq(TI, ti_par_bool) then
        % Nb: at one point we warned about constraints were par bool, but in
        % practice it tends to happen in reasonable circumstances (not least
        % because of assert() calls) so we now allow it.
        true
    else if ti_eq(TI, ti_par_bottom) then
        % This can happen if the constraint body is a polymorphic expression
        % that aborts, for example:
        %
        %    constraint last([]);
        %    constraint abort("");
        % 
        true
    else
        type_inst_mismatch_error("constraint expression",
            [ti_var_bool], TI, NE^expr_info^expr_src_locn, _, !S)
    ),

    % Run any client specific type-checking actions for constraint items.
    %
    MaybeClientAction = post_typecheck_constraint_action(Ctrl),
    (
        MaybeClientAction = no
    ;
        MaybeClientAction = yes(ClientAction),
        Errors0 = !.S ^ errors,
        ClientAction(Locn, NE, Errors0, Errors),
        !S ^ errors := Errors
    ).

    % Solve items are handled separately below.
    %
tic_raw_item(_, _, _, RawItem, _, _, _) :-
    RawItem = solve_item(_, _),
    unexpected($pred, "solve item encountered.").

    % We type- and inst-check the body (if present) using the type-insts of
    % the arguments.
    %
tic_raw_item(Ctrl, Locn, ScopeNs,
        predfunc_item( PredFuncRet, Name,  ProcN,  ArgTIEsIds,  AnnEs,  MaybeBodyE),
        predfunc_item(NPredFuncRet, Name, NProcN, NArgTIEsIds,  NAnnEs, NMaybeBodyE),
        !S) :-
    % Check the pred/func's ProcN hasn't already been set.
    ( if ProcN \= unset_proc_number
    then unexpected($pred, "(predfunc_item): ProcN already set")
    else true
    ),

    % Process return type-inst, and set up other bits and pieces.
    (
        PredFuncRet = func_ret(RetTIE),
        OperationKind = function_operation,
        ArgKind = func_arg,
        ti_expr_to_type_inst(Ctrl, [in_predfunc_decl], ScopeNs, RetTIE,
            RetTIE2, RetTI, !S),
        % See comment below about the use of unskolemize.
        RetTIE2 = ti_expr(RawTIE2, expr_info(Locn2, TI2)),
        NRetTIE = ti_expr(RawTIE2, expr_info(Locn2, unskolemize(TI2))),
        OpWhat = "function",
        NPredFuncRet = func_ret(NRetTIE)
    ;
        PredFuncRet = test_ret,
        OperationKind = predicate_operation,
        ArgKind = pred_arg,
        RetTI = ti_par_bool,
        NRetTIE = lift_type_inst_to_ti_expr(Locn, RetTI),
        OpWhat = "test",
        NPredFuncRet = test_ret
    ;
        PredFuncRet = pred_ret,
        OperationKind = predicate_operation,
        ArgKind = pred_arg,
        RetTI = ti_var_bool,
        NRetTIE = lift_type_inst_to_ti_expr(Locn, RetTI),
        OpWhat = "predicate",
        NPredFuncRet = pred_ret
    ),

    % Preds/funcs introduce a new scope.
    % For each arg: handle the type-expr, then add it to the symbol table and
    % update its scope-number, and get its type-inst.
    add_new_scope(ScopeNs, NScopeNs, !S),
    DoArg = (
        % Get the type-inst for each arg.
        pred(ArgTIE-ArgId::in, NArgTIE-NArgId::out, !.S::in, !:S::out) is det :-
            % We use the ScopeNs, not NScopeNs, for the ArgTIEs.
            ti_expr_to_type_inst(Ctrl, [in_predfunc_decl], ScopeNs, ArgTIE,
                ArgTIE2, ArgTI, !S),
            % We say func/pred args are 'defined' so that if you reuse the
            % same argument name twice, it is an "argument defined more than
            % once" error.  This is not quite right -- really it is declared
            % more than once, but as long as we allow double declarations in
            % other circumstances then it's easiest to catch this case in this
            % way.
            add_variable_sym_and_update_id(
                ArgTIE ^ ti_expr_info ^ expr_src_locn,
                NScopeNs, ArgTI, ArgKind, defined, defn_not_required,
                ArgId, NArgId, !S),

            % Also, we want any type-inst-var args to be skolemized in the
            % symbol table, and that's what ti_expr_to_type_inst gives us.  But
            % for the pred/func signature that we add to the symbol table, we
            % want any type-inst-vars within that to be unskolemized.  So we
            % change that now.
            ArgTIE2 = ti_expr(RawTIE3, expr_info(Locn3, TI3)),
            NArgTIE = ti_expr(RawTIE3, expr_info(Locn3, unskolemize(TI3)))
    ),
    list.map_foldl(DoArg, ArgTIEsIds, NArgTIEsIds, !S),

    % Check if the body has the right type-inst, if present, using the new
    % scope.
    (
        MaybeBodyE = yes(BodyE),
        What = "body of " ++ OpWhat ++ " `" ++ Name ++ "'",
        expr_has_type_inst(Ctrl, What, [in_predfunc_body], NScopeNs, RetTI,
            BodyE, NBodyE, !S),
        NMaybeBodyE = yes(NBodyE),
        IsDefd = defined
    ;
        MaybeBodyE  = no,
        NMaybeBodyE = no,
        IsDefd = undefined
    ),

    (
        AnnEs = [],
        IsAnnotated = proc_is_not_annotated
    ;
        AnnEs = [_ | _],
        IsAnnotated = proc_is_annotated
    ),

    % Now add the operation to the symbol table.
    % 'add_operation_symbol' so we can use 'unset_proc_number' here.
    NArgTIEs = assoc_list.keys(NArgTIEsIds),
    TIESig = NArgTIEs - NRetTIE,
    add_operation_symbol(Locn, Name, OperationKind, IsDefd, IsAnnotated,
        TIESig, NProcN, !S),
    tic_annotations(Ctrl, [], ScopeNs, AnnEs, NAnnEs, !S).

tic_raw_item(Ctrl, Locn, ScopeNs, annotation_item(Name, ArgTIEsIds),
        annotation_item(Name, NArgTIEsIds), !S) :-
    % Process each arg's type-expr.  But don't add it to the symbol table,
    % because we don't bother to put annotation args in the symbol table.
    % Also, annotation decls introduce a new scope -- this is not important in
    % the Zinc front-end, but when translating to Cadmium term form, we want
    % each argument to have a scope number so that the name mangling works ok
    % (ie.  so it doesn't put 'unset_scope_number', which is negative, in the
    % variable name).
    ArgKind = ann_arg,
    add_new_scope(ScopeNs, NScopeNs, !S),
    DoArg = (
        % This is very similar to the predfunc_item case -- see it for more
        % explanation.
        pred(ArgTIE-ArgId::in, NArgTIE-NArgId::out, !.S::in, !:S::out) is det :-
            % We use the ScopeNs, not NScopeNs, for the ArgTIEs.
            ti_expr_to_type_inst(Ctrl, [in_ann_decl], ScopeNs, ArgTIE,
                ArgTIE2, ArgTI, !S),
            % Nb: once added, these variables are never looked up, because
            % annotations don't have any bodies in which they could occur.
            % However, it's useful to put them in the symbol table because it
            % makes it easy to detect if any two args have the same name.
            add_variable_sym_and_update_id(ArgTIE^ti_expr_info^expr_src_locn,
                NScopeNs, ArgTI, ArgKind, defined, defn_not_required,
                ArgId, NArgId, !S),

            % Unskolemize any variables in the sig -- see the handling of
            % predfunc_items for details.
            ArgTIE2 = ti_expr(RawTIE3, expr_info(Locn3, TI3)),
            NArgTIE = ti_expr(RawTIE3, expr_info(Locn3, unskolemize(TI3)))
    ),
    list.map_foldl(DoArg, ArgTIEsIds, NArgTIEsIds, !S),

    % Create the symbol.
    F = (func(TIE - _Id) = TIE ^ ti_expr_info ^ expr_type_inst),
    TISig = list.map(F, NArgTIEsIds),
    add_global_symbol(Locn, sym_annotation(TISig), Name, !S).

:- pred tic_solve_item(Ctrl::in, scope_nums::in, src_locn::in,
    solve_kind::in, solve_kind::out, exprs::in, exprs::out,
    tic_state::in, tic_state::out) is det <= frontend_control(Ctrl).

tic_solve_item(Ctrl, ScopeNs, Locn, SolveKind, NSolveKind, AnnEs, NAnnEs,
        !S) :-
    (
        SolveKind  = satisfy,
        NSolveKind = satisfy
    ;
        (   SolveKind = minimize(E),
            type_inst_of_expr(Ctrl, [], ScopeNs, E, NE, TI, !S),
            NSolveKind = minimize(NE),
            What = "minimize expression"
        ;
            SolveKind = maximize(E),
            type_inst_of_expr(Ctrl, [], ScopeNs, E, NE, TI, !S),
            NSolveKind = maximize(NE),
            What = "maximize expression"
        ),
        Lang = !.S^lang,
        (
            Lang = lang_zinc
        ;
            % In MiniZinc and FlatZinc, the objective function must be numeric.
            % Try to match as an int first, then as a float.  No need for
            % coercions.  But don't allow var_bottom!  Because then we don't
            % know if it should be an int or a float.
            ( Lang = lang_minizinc
            ; Lang = lang_flatzinc
            ),
            ( if
                ( ti_eq(TI, ti_par_int) ; ti_eq(TI, ti_par_float)
                ; ti_eq(TI, ti_var_int) ; ti_eq(TI, ti_var_float)
                )
            then
                true
            else
                type_inst_mismatch_error(What, [ti_var_int, ti_var_float], TI,
                    E^expr_info^expr_src_locn, _, !S)
            )
        )
    ),
    tic_annotations(Ctrl, [], ScopeNs, AnnEs, NAnnEs, !S),

    % Run any client specific type-checking action for the solve item.
    %
    MaybeClientAction = post_typecheck_solve_action(Ctrl),
    (
        MaybeClientAction = no
    ;
        MaybeClientAction = yes(ClientAction),
        Errors0 = !.S ^ errors,
        ClientAction(Locn, NSolveKind, NAnnEs, Errors0, Errors),
        !S ^ errors := Errors
    ).

%-----------------------------------------------------------------------------%

:- func expr_has_range_value(expr) = has_range_value.

expr_has_range_value(E) = HasRangeValue :-
    ( if  E ^ raw_expr = app(Id, _, _, _), Id ^ id_name = ".."
    then HasRangeValue = has_range_value
    else HasRangeValue = does_not_have_range_value
    ).

%-----------------------------------------------------------------------------%

    % A type-expr is usable as a value if it's a range-expr-as-type,
    % set-expr-as-type, name of a flat enum;  or a synonym of one of these.
    % And it's not prefixed with 'var'.  And it doesn't have an arbitrary type
    % constraint.
    %
:- func ti_expr_is_usable_as_value(ti_expr, symbol_table)
    = is_usable_as_value.

ti_expr_is_usable_as_value(ti_expr(RawTIE, _), SymTbl) =
    ( if RawTIE = raw_ti_expr(VarPar,_),
         VarPar \= var,
         ( RawTIE^base_ti_expr_tail = bte_range_expr_as_type_expr(_,_)
         ; RawTIE^base_ti_expr_tail = bte_set_expr_as_type_expr(_)
         ; RawTIE^base_ti_expr_tail = bte_ident(Id),
           maybe_find_symbol(Id, SymTbl, Sym, _),
           ( Sym = sym_type_inst_synonym(
                type_syn_info_defined(_, usable_as_value, _, _))
           ; Sym = sym_enum(enum_info_flat(_))
           )
         ) then
        usable_as_value
    else
        not_usable_as_value
    ).

%-----------------------------------------------------------------------------%

    % This function determines if an instance-time definition is required for
    % a global/let variable with the given type-inst expr.
    %
:- func is_definition_required(tic_state, ti_expr) = is_defn_required.

is_definition_required(S, TIE) = IsDefnReqd :-
    TIE = ti_expr(RawTIE, _TIEInfo),
    (
        RawTIE = raw_ti_expr(VarPar, BaseTIETail),
        (
            VarPar = par,
            IsDefnReqd = is_definition_required_2(BaseTIETail, S)
        ;
            VarPar = var,
            IsDefnReqd = defn_not_required
        )
    ;
        RawTIE = constrained_raw_ti_expr(ConstrainedTIE, _, _),
        IsDefnReqd = is_definition_required(S, ConstrainedTIE)
    ).

    % Definitions are never needed for type-inst expressions that begin with
    % "var".  This function is just for checking those that aren't.
    %
:- func is_definition_required_2(base_ti_expr_tail, tic_state)
    = is_defn_required.

is_definition_required_2(BaseTIETail, S) = IsDefnReqd :-
    (
        ( BaseTIETail = bte_bottom
        ; BaseTIETail = bte_error
        ),
        unexpected($pred, BaseTIETail^string)
    ;
        ( BaseTIETail = bte_bool
        ; BaseTIETail = bte_int
        ; BaseTIETail = bte_float
        ; BaseTIETail = bte_string
        ; BaseTIETail = bte_ann
        ; BaseTIETail = bte_set_of(_)
        ; BaseTIETail = bte_set_expr_as_type_expr(_)
        ; BaseTIETail = bte_range_expr_as_type_expr(_,_)
        ),
        IsDefnReqd = defn_required
    ;
        BaseTIETail = bte_array_of(IndexTIEs, ElemTIE, _),
        ( if
            ti_exprs_are_finite(S ^ sym_tbl, IndexTIEs) = is_finite
        then
            % Explicitly-indexed -- only needs a definition if the elements
            % require it.
            IsDefnReqd = is_definition_required(S, ElemTIE)
        else
            % Implicitly-indexed -- needs a definition regardless of the
            % element type-inst.
            % XXX since we don't allow vars in data files this should be
            % an error unless the ElemTIE is par.
            IsDefnReqd = defn_required
        )
    ;
        ( BaseTIETail = bte_tuple_of(TIEs)
        ; BaseTIETail = bte_record_of(TIEsNames),
          TIEs = assoc_list.keys(TIEsNames)
        ),
        % If any of the tuple/record elements require a definition, then the
        % whole tuple does.
        RequiresDefn = (pred(TIE::in) is semidet :-
            is_definition_required(S, TIE) = defn_required
        ),
        ( if list.find_first_match(RequiresDefn, TIEs, _)
        then IsDefnReqd = defn_required
        else IsDefnReqd = defn_not_required
        )
    ;
        % This should cause a different kind of error, so let's not worry
        % about providing a definition.
        ( BaseTIETail = bte_typeinst_var(_)
        ; BaseTIETail = bte_any_typeinst_var(_)
        ),
        IsDefnReqd = defn_not_required
    ;
        BaseTIETail = bte_ident(Id),
        ( if maybe_find_symbol(Id, S ^ sym_tbl, Sym, _) then
            (
                ( Sym = sym_variable(_,_,_,_,_)
                ; Sym = sym_enum(_)
                ),
                IsDefnReqd = defn_required
            ;
                Sym = sym_type_inst_synonym(TypeSynInfo),
                (
                    TypeSynInfo = type_syn_info_defined(_, _, IsDefnReqd, _)
                ;
                    % In this case, we'll be complaining about the type-inst
                    % synonym being undefined anyway, so don't worry about
                    % whether a definition is required.
                    TypeSynInfo = type_syn_info_undefined,
                    IsDefnReqd = defn_not_required
                )
            ;
                % This should cause a different kind of error, so let's not
                % worry about providing a definition.
                ( Sym = sym_operation(_,_)
                ; Sym = sym_enum_case_name(_)
                ),
                IsDefnReqd = defn_not_required
            ;
                % Definition not required for annotations.
                Sym = sym_annotation(_),
                IsDefnReqd = defn_not_required
            )
        else
            % This should cause a different kind of error, so let's not
            % worry about providing a definition.
            IsDefnReqd = defn_not_required
        )
    ;
        BaseTIETail = bte_op(_ArgTIEs - _RetTIE),
        IsDefnReqd = defn_required
    ).

%-----------------------------------------------------------------------------%
%
% Type-inst expressions
%

:- pred ti_expr_to_type_inst(Ctrl::in, context::in, scope_nums::in)
    : tic_X_Y(ti_expr, type_inst) `with_inst` tic_X_Y
    <= frontend_control(Ctrl).

ti_expr_to_type_inst(Ctrl, Context, ScopeNs, ti_expr(RawTIE, TIEInfo),
        ti_expr(NRawTIE, NTIEInfo), TI, !S) :-
    raw_ti_expr_to_type_inst(Ctrl, Context, ScopeNs, TIEInfo, RawTIE, NRawTIE,
        TI, !S),
    NTIEInfo = TIEInfo^expr_type_inst := TI.

:- pred raw_ti_expr_to_type_inst(Ctrl::in, context::in, scope_nums::in,
        expr_info::in) : tic_X_Y(raw_ti_expr, type_inst)
        `with_inst` tic_X_Y <= frontend_control(Ctrl).

raw_ti_expr_to_type_inst(Ctrl, Context, ScopeNs, TIEInfo,
        raw_ti_expr(VarPar,  BaseTIETail),
        raw_ti_expr(VarPar, NBaseTIETail), TI, !S) :-
    TIELocn = TIEInfo^expr_src_locn,
    base_ti_expr_tail_to_type_inst(Ctrl, TIELocn, Context, ScopeNs,
        BaseTIETail, NBaseTIETail, TI0, !S),

    % Add any 'par'/'var' to the front.
    (
        VarPar = par,
        TI1 = TI0
    ;
        VarPar = var,
        add_var_to_front_of_type_inst(TIELocn, TI0, TI1, !S)
    ),

    % If it's a set, check the element type-inst.
    ( if TI1 = ti_par_set(ElemTI1) then
        tic_check_par_set_element_type_inst(TIELocn, ElemTI1, !S)
    else if TI1 = ti_var_set(ElemTI1) then
        tic_check_var_set_element_type_inst(TIELocn, ElemTI1, !S)
    else
        true
    ),

    % If it's a var set, and we're not in a pred/func declaration, check the
    % set element type is finite.  We do this by pretending the var set TIE is
    % a par set and passing that into ti_expr_is_finite.  We do this rather
    % than trying to extract the element TIE because the set TIE might be an
    % identifier rather than "var set of X".  It works because the finiteness
    % of "par set of T" is the same as the finiteness of "T".  We set the
    % type_inst annotation to TI1 so it can be looked at if necessary.
    ( if
         TI1 = ti_var_set(_),
         not list.member(in_predfunc_decl, Context),
         not list.member(in_ann_decl, Context),
         not list.member(in_assigned_let_var, Context),
         not list.member(in_assigned_var_decl, Context)
    then
        ParSetTIE = ti_expr(raw_ti_expr(par, NBaseTIETail),
            TIEInfo ^ expr_type_inst := TI1),
        ( if ti_expr_is_finite(!.S^sym_tbl, ParSetTIE) = is_finite then
            TI = TI1
        else
            SetElemErr = [
                words("set element type is not finite")
            ],
            type_inst_error(SetElemErr, TIEInfo ^ expr_src_locn, TI, !S)
        )
    else
        TI = TI1
    ).

raw_ti_expr_to_type_inst(Ctrl, Context, ScopeNs, TIEInfo,
        constrained_raw_ti_expr( TIE,  Id,  TIConE),
        constrained_raw_ti_expr(NTIE, NId, NTIConE), TI, !S) :-
    TIELocn = TIEInfo^expr_src_locn,
    not_in_lang([lang_minizinc, lang_flatzinc],
        "arbitrarily constrained type-insts", TIELocn, !S),

    % Do the inner TIE before adding the new scope for the identifier.
    ti_expr_to_type_inst(Ctrl, Context, ScopeNs, TIE, NTIE, TI0, !S),

    % XXX: this is bogus!  Don't know the inst of this variable yet, it
    % could later be turned from a par into a var.
    % Really the constraint variable isn't a single thing, but can be
    % replicated and have multiple occurrences.
    % [unless we disallow putting 'var' in front of type syns]
    % [less restrictive -- disallow 'var' in front of type syns that involve
    %  arb. constraints.  That makes the syn-inlining that happens later safe,
    %  I think, because we don't have to worry about the variable changing
    %  from par to var.  But it means you have to do this:
    %
    %    type Pos     = (    int: x where x > 0);
    %    type var_Pos = (var int: x where x > 0);
    %
    %  Hmm...]
    %
    % Type constraints introduce a new scope.
    % Add the type constraint variable to the symbol table and update its
    % scope-number, then do the expr using the new scope.
    add_new_scope(ScopeNs, NScopeNs, !S),
    % Nb: when we record the constraint variable, we ignore the constraint on
    % it.  But if its TIE had a constraint, that will be recorded.
    % Eg. if we have ( 1..10: y where y > 5 ), the 1..10 constraint will be
    % recorded for 'y', but the 'y > 5' is ignored.
    add_variable_sym_and_update_id(TIELocn, NScopeNs, TI0,
        type_constraint_var, undefined, defn_not_required, Id, NId, !S),
    type_inst_of_expr(Ctrl, Context, NScopeNs, TIConE, NTIConE, TIConTI, !S),
    ( if ti_leq(!.S^lang, TIConTI, ti_var_bool) then
        TI = TI0
    else
        type_inst_mismatch_error("type constraint", [ti_var_bool], TIConTI,
            TIConE^expr_info^expr_src_locn, TI, !S)
    ).


:- pred base_ti_expr_tail_to_type_inst(Ctrl::in, src_locn::in, context::in,
    scope_nums::in) : tic_X_Y(base_ti_expr_tail, type_inst) `with_inst` tic_X_Y
    <= frontend_control(Ctrl).

base_ti_expr_tail_to_type_inst(Ctrl, Locn, Context, ScopeNs,
        BaseTIETail, NBaseTIETail, TI, !S) :-
    (   ( BaseTIETail = bte_bottom
        ; BaseTIETail = bte_error
        ),
        unexpected($pred, BaseTIETail^string)
    ;
        ( BaseTIETail = bte_bool,   TI = ti_par_bool
        ; BaseTIETail = bte_int,    TI = ti_par_int
        ; BaseTIETail = bte_float,  TI = ti_par_float
        ; BaseTIETail = bte_string, TI = ti_par_string
        ; BaseTIETail = bte_ann,    TI = ti_ann
        ),
        NBaseTIETail = BaseTIETail
    ;
        BaseTIETail = bte_ident(Id),
        ( if add_scopenum_to_id(ScopeNs, !.S^sym_tbl, Id, Id2, Sym) then
            NId = Id2,
            (   % Par sets can be used as types if they're defined, and not in
                % a type-inst constraint and we're only doing model-checking.
                Sym = sym_variable(VarTI, VarKind, IsDefd, IsDefnReqd, _),
                ( if
                    VarKind = type_constraint_var
                then
                    VarAsTypeErr = [
                        words("type-inst constraint variable"),
                        quote(Id ^ id_name),
                        words("used as a type")
                    ],
                    symbol_error(VarAsTypeErr, Locn, TI, !S)
                else if
                    IsDefd = undefined,
                    IsDefnReqd = defn_required,
                    !.S ^ checking = instance_checking
                then
                    UndefTypeErr = [
                        words("undefined"), words(Sym ^ show),
                        quote(Id ^ id_name), words("used as a type")
                    ],
                    symbol_error(UndefTypeErr, Locn, TI, !S)
                else if
                    VarTI = ti_par_set(ElemTI)
                then
                    TI = ElemTI
                else
                    ErrMsg = [
                        words("variable"), quote(Id ^ id_name),
                        words("used as a type-inst is not a fixed set")
                    ],
                    type_inst_error(ErrMsg, Locn, TI, !S)
                )
            ;
                % Type-inst synonyms can be used as types if they are defined.
                Sym = sym_type_inst_synonym(MaybeDefn),
                (
                    MaybeDefn = type_syn_info_defined(TI, _, _, _)
                ;
                    MaybeDefn = type_syn_info_undefined,
                    UndefSynErr = [
                        words("use of undefined"), words(Sym ^ show),
                        quote(Id ^ id_name)
                    ],
                    symbol_error(UndefSynErr, Locn, TI, !S)
                ),
                not_in_lang([lang_minizinc, lang_flatzinc],
                    "type-inst synonyms", Locn, !S)
            ;
                % An enum can be used as a type, unless we're doing
                % instance-checking and it's not defined.
                Sym = sym_enum(EnumInfo),
                IsEnumDefined = enum_is_defined(EnumInfo),
                ( if
                    ( IsEnumDefined = yes
                    ; !.S ^ checking \= instance_checking
                    )
                then
                    TI = ti_par_enum(Id ^ id_name)
                else
                    UndefEnumErr = [
                        words("use of undefined"), words(Sym ^ show),
                        quote(Id ^ id_name)
                    ],
                    symbol_error(UndefEnumErr, Locn, TI, !S)
                ),
                not_in_lang([lang_minizinc, lang_flatzinc], "enums", Locn, !S)
            ;
                % Preds/funcs/ops, enum case names, and annotations cannot be
                % used as types.
                ( Sym = sym_operation(_,_)
                ; Sym = sym_enum_case_name(_)
                ; Sym = sym_annotation(_)
                ),
                NonTypeErr = [
                    words(Sym ^ show), quote(Id ^ id_name),
                    words("used as a type")
                ],
                symbol_error(NonTypeErr, Locn, TI, !S)
            )
        else
            UndeclSymErr = [
                quote(Id ^ id_name), words("undeclared")
            ],
            symbol_error(UndeclSymErr, Locn, TI, !S),
            NId = Id        % leave the scope number unset
        ),
        NBaseTIETail = bte_ident(NId)
    ;
        BaseTIETail = bte_typeinst_var(Tv),
        not_in_lang([lang_minizinc, lang_flatzinc], "type-inst variables",
            Locn, !S),
        ( if ( list.member(in_predfunc_decl, Context)
             ; list.member(in_ann_decl,      Context)
             ; list.member(in_predfunc_body, Context) )
        then
            TI = ti_par_variable(Tv, /*IsSkolemized*/yes)
        else
            BadTIVarErr = [
                words("type-inst variable"),
                quote("$" ++ Tv),
                words("outside a function or predicate")
            ],
            type_inst_error(BadTIVarErr, Locn, TI, !S)
        ),
        NBaseTIETail = bte_typeinst_var(Tv)
    ;
        BaseTIETail = bte_any_typeinst_var(Tv),
        not_in_lang([lang_minizinc, lang_flatzinc], "type-inst variables",
            Locn, !S),
        ( if ( member(in_predfunc_decl, Context)
             ; member(in_ann_decl,      Context)
             ; member(in_predfunc_body, Context) )
        then
            TI = ti_any_variable(Tv, /*IsSkolemized*/yes)
        else
            BadTIVarErr = [
                words("type-inst variable"),
                quote("$" ++ Tv),
                words("outside a function or predicate")
            ],
            type_inst_error(BadTIVarErr, Locn, TI, !S)
        ),
        NBaseTIETail = bte_any_typeinst_var(Tv)
    ;
        BaseTIETail = bte_set_of(ElemTIE),
        ti_expr_to_type_inst(Ctrl, Context, ScopeNs, ElemTIE, NElemTIE,
            ElemTI0, !S),
        type_inst_is_fixed("set type expression has non-fixed element", Locn,
            ElemTI0, ElemTI, !S),
        % Nb: we check the element TI later, after adding any 'var' prefix.
        TI = ti_par_set(ElemTI),
        NBaseTIETail = bte_set_of(NElemTIE)
    ;
        BaseTIETail = bte_array_of(IndexTIEs, ElemTIE, IsListSyntax),
        % Get and check indices.
        list.map2_foldl(ti_expr_to_type_inst(Ctrl, Context, ScopeNs), IndexTIEs,
            NIndexTIEs, IndexTIs, !S),
        (
            IndexTIs = [],
            unexpected($pred, "empty index type list")
        ;
            IndexTIs = [IndexTI1]
        ;
            IndexTIs = [_, _ | _],
            IndexTI1 = ti_tuple(IndexTIs)
        ),
        type_inst_is_fixed("array has non-fixed index element", Locn, IndexTI1,
            IndexTI, !S),

        % Get and check element type-inst.
        ti_expr_to_type_inst(Ctrl, Context, ScopeNs, ElemTIE, NElemTIE,
            ElemTI, !S),
        list.foldl(is_index_ok(Locn, Context), NIndexTIEs, !S),
        tic_check_array_element_type_inst(Locn, ElemTI, !S),

        % Combine index and element type-insts.
        TI = ti_array(IndexTI, ElemTI),
        NBaseTIETail = bte_array_of(NIndexTIEs, NElemTIE, IsListSyntax)
    ;
        BaseTIETail = bte_tuple_of(TIEs),
        not_in_lang([lang_minizinc, lang_flatzinc], "tuples", Locn, !S),
        list.map2_foldl(ti_expr_to_type_inst(Ctrl, Context, ScopeNs), TIEs,
            NTIEs, TIs, !S),
        (
            TIEs = [],
            unexpected($pred, "null tuple")
        ;
            TIEs = [_],
            UnaryTupleErr = [
                words("unary tuples are not allowed")
            ],
            type_inst_error(UnaryTupleErr, Locn, TI, !S)
        ;
            TIEs = [_, _ | _],
            TI = ti_tuple(TIs)
        ),
        NBaseTIETail = bte_tuple_of(NTIEs)
    ;
        BaseTIETail = bte_record_of(TIEsNames),
        not_in_lang([lang_minizinc, lang_flatzinc], "records", Locn, !S),
        FieldNames = assoc_list.values(TIEsNames),
        ( if RepFieldName = first_repeated_elem(FieldNames) then
            RepFieldNameErr = [
                words("field name"), quote(RepFieldName),
                words("repeated in record type-inst expression")
            ],
            symbol_error(RepFieldNameErr, Locn, _, !S)
        else
            true
        ),
        list.map2_foldl(
            ti_expr_and_name_to_type_inst_and_name(Ctrl, Context, ScopeNs),
            TIEsNames, NTIEsNames, TIsNames, !S),
        TI = ti_record(TIsNames),
        NBaseTIETail = bte_record_of(NTIEsNames)
    ;
        BaseTIETail = bte_op(ArgTIEs - RetTIE),
        not_in_lang([lang_minizinc, lang_flatzinc],
            "operation type-inst expressions", Locn, !S),
        ti_expr_to_type_inst(Ctrl, Context, ScopeNs, RetTIE, NRetTIE, RetTI,
            !S),
        list.map2_foldl(ti_expr_to_type_inst(Ctrl, Context, ScopeNs), ArgTIEs,
            NArgTIEs, ArgTIs, !S),
        NBaseTIETail = bte_op(NArgTIEs - NRetTIE),
        ( if list.member(in_ann_decl, Context) then
            TI = ti_op(ArgTIs - RetTI, no)
        else
            Msg = [
                words("operation type-inst expression outside an"),
                words("annotation declaration")
            ],
            type_inst_error(Msg, Locn, TI, !S)
        )
    ;
        BaseTIETail = bte_set_expr_as_type_expr(Es),
        type_inst_of_exprs(Ctrl, "set expression elements", Locn, Context,
            ScopeNs, Es, NEs, ElemTI0, !S),
        What =
            "set expression used as a type expression has non-fixed elements",
        type_inst_is_fixed(What, Locn, ElemTI0, ElemTI, !S),
        TI = ElemTI,
        NBaseTIETail = bte_set_expr_as_type_expr(NEs)
    ;
        % Must be two fixed integer or two fixed floats.
        BaseTIETail = bte_range_expr_as_type_expr(StartE, EndE),
        type_inst_of_expr(Ctrl, Context, ScopeNs, StartE, StartE1, StartTI,
            !S),
        type_inst_of_expr(Ctrl, Context, ScopeNs, EndE,   EndE1,   EndTI,
            !S),
        ExpectedTIs = [ti_par_int, ti_par_float],
        ( if
            ( ti_eq(StartTI, ti_par_int)
            ; ti_eq(StartTI, ti_par_float)
            )
        then
            ErrorOnStart = no
        else
            type_inst_mismatch_error("range start value", ExpectedTIs, StartTI,
                StartE^expr_info^expr_src_locn, _, !S),
            ErrorOnStart = yes
        ),
        ( if
            ( ti_eq(EndTI, ti_par_int)
            ; ti_eq(EndTI, ti_par_float)
            )
        then
            ErrorOnEnd = no
        else
            type_inst_mismatch_error("range end value", ExpectedTIs, EndTI,
                EndE^expr_info^expr_src_locn, _, !S),
            ErrorOnEnd = yes
        ),

        ( if ErrorOnStart = no, ErrorOnEnd = no
        then RangeTI = ti_lub(!.S^lang, StartTI, EndTI)
        else RangeTI = ti_error
        ),

        % If both were ok, we now know if it's an integer range or a float
        % range, so we can insert coercions accordingly.
        ( if 
            ( ti_eq(RangeTI, ti_par_int)
            ; ti_eq(RangeTI, ti_par_float)
            )
        then
            NStartE = make_coerce_expr(StartTI, TI, StartE1),
            NEndE   = make_coerce_expr(EndTI,   TI, EndE1  ),
            TI      = RangeTI
        else
            RangeMismatchErr = [
                words("range start and end type-insts do not match")
            ],
            type_inst_error(RangeMismatchErr, Locn, TI, !S),
            NStartE  = StartE1,
            NEndE    = EndE1
        ),
        NBaseTIETail = bte_range_expr_as_type_expr(NStartE, NEndE)
    ).

:- pred ti_expr_and_name_to_type_inst_and_name(Ctrl::in, context::in,
    scope_nums::in, ti_expr_and_name::in, ti_expr_and_name::out,
    type_inst_and_name::out, tic_state::in, tic_state::out) is det
    <= frontend_control(Ctrl).

ti_expr_and_name_to_type_inst_and_name(Ctrl, Context, ScopeNs, TIE - Name,
        NTIE - Name, TI - Name, !S) :-
    ti_expr_to_type_inst(Ctrl, Context, ScopeNs, TIE, NTIE, TI, !S).

:- pred ti_expr_and_name_to_field_info(Ctrl::in, context::in,
    scope_nums::in, ti_expr_and_name::in, ti_expr_and_name::out,
    nfe_field_info::out, tic_state::in, tic_state::out)
    is det <= frontend_control(Ctrl).

ti_expr_and_name_to_field_info(Ctrl, Context, ScopeNs, TIE - Name,
        NTIE - Name, FieldInfo, !S) :-
    ti_expr_to_type_inst(Ctrl, Context, ScopeNs, TIE, NTIE, TI, !S),
    FieldInfo = nfe_field_info(Name, NTIE, TI).

%-----------------------------------------------------------------------------%

:- pred add_var_to_front_of_type_inst(src_locn::in, type_inst::in,
    type_inst::out, tic_state::in, tic_state::out) is det.

add_var_to_front_of_type_inst(Locn, TI, NTI, !S) :-
    (   TI = ti_par_bool,        NTI = ti_var_bool
    ;   TI = ti_par_int,         NTI = ti_var_int
    ;   TI = ti_par_float,       NTI = ti_var_float
    ;   TI = ti_error,           NTI = ti_error
    ;
        TI = ti_par_enum(Name),
        Sym = find_existing_global_symbol(Name, !.S^sym_tbl),
        % Can put 'var' in front of flat enums, but not non-flat enums.
        ( if Sym = sym_enum(enum_info_flat(_)) then
            NTI = ti_var_enum(Name)
        else if Sym = sym_enum(enum_info_undefined) then
            % If we're using --model-check-only, we might have an undefined
            % enum.  Such an enum must be flat, and thus is varifiable.
            NTI = ti_var_enum(Name)
        else
            ErrMsg = [quote("var enum"),
                words("is not allowed for non-flat enums")
            ],
            type_inst_error(ErrMsg, Locn, NTI, !S)
        )
    ;
        TI  = ti_par_set(ElemTI),
        NTI = ti_var_set(ElemTI)
    ;
        ( TI = ti_var_bool
        ; TI = ti_var_int
        ; TI = ti_var_float
        ; TI = ti_var_enum(_)
        ; TI = ti_var_set(_)
        ),
        ErrMsg = [
            words("prefixing a second"), quote("var"),
            words("to type-inst"), quote(TI ^ string)
        ],
        type_inst_error(ErrMsg, Locn, NTI, !S)
    ;
        ( TI = ti_par_string,         What = "var string"
        ; TI = ti_ann,                What = "var ann"
        ; TI = ti_array(_,_),         What = "var array"
        ; TI = ti_tuple(_),           What = "var tuple"
        ; TI = ti_record(_),          What = "var record"
        ; TI = ti_par_variable(Name,_), What = "var $" ++ Name
        ; TI = ti_any_variable(Name,_), What = "var any $" ++ Name
        ),
        NotAllowedErr = [
            quote(What), words("is not allowed")
        ],
        type_inst_error(NotAllowedErr, Locn, NTI, !S)
    ;
        ( TI = ti_par_bottom
        ; TI = ti_var_bottom
        ; TI = ti_top
        ; TI = ti_overloaded_op(_)
        ; TI = ti_op(_,_)
        ; TI = ti_unknown
        ),
        unexpected($pred, TI^string)
    ).

%-----------------------------------------------------------------------------%

    % Checks that an array index has the right form.
    %
:- pred is_index_ok(src_locn::in, context::in, ti_expr::in,
    tic_state::in, tic_state::out) is det.

is_index_ok(Locn, Context, ti_expr(RawTIE, _TIEInfo), !S) :-
    Lang = !.S ^ lang,
    (
        Lang = lang_zinc
    ;
        ( Lang = lang_minizinc
        ; Lang = lang_flatzinc
        ),
        ( if RawTIE = raw_ti_expr(_VarPar, BaseTIETail0)
        then BaseTIETail = BaseTIETail0
        else unexpected($pred, "arbitrarily constrained type-inst")
        ),
        ( if
            ( list.member(in_predfunc_decl, Context)
            ; list.member(in_ann_decl, Context)
            )
        then
            true
        else if
            (
                BaseTIETail = bte_range_expr_as_type_expr(_,_)
            ;
                Lang = lang_minizinc,
                BaseTIETail = bte_int
            ;
                BaseTIETail = bte_ident(Id),
                ( if maybe_find_symbol(Id, !.S ^ sym_tbl, Sym, _) then
                    Sym ^ has_range_value = has_range_value
                else
                    % If we can't find the symbol, pretend it's an ok
                    % index -- we'll get an error about an undefined
                    % identifier elsewhere.
                    true
                )
            ;
                % Nb: the parser won't create a bte_tuple_of index type.
                % However, it can occur once we convert to Cadmium term form
                % and back.
                BaseTIETail = bte_tuple_of(ElemTIEs),
                list.foldl(is_index_ok(Locn, Context), ElemTIEs, !S)
            )
         then
            true
         else
            not_in_lang([lang_minizinc, lang_flatzinc],
                "array indices without fixed ranges", Locn, !S)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred tic_check_par_set_element_type_inst(src_locn::in, type_inst::in,
    tic_state::in, tic_state::out) is det.

tic_check_par_set_element_type_inst(Locn, ElemTI, !S) :-
    % Zinc par sets can contain any other par type.
    % MiniZinc par sets can only contain bool, int, float.
    % FlatZinc par sets can only contain int.
    Lang = !.S^lang,
    (
        Lang = lang_minizinc,
        ( if
            ( ti_leq(Lang, ElemTI, ti_par_bool)
            ; ti_leq(Lang, ElemTI, ti_par_int)
            ; ti_leq(Lang, ElemTI, ti_par_float)
            )
        then
            true
        else
            not_in_lang([lang_minizinc],
                "set element type-insts other than `int' or `float' or `bool'",
                Locn, !S)
        )
    ;
        Lang = lang_flatzinc,
        ( if ti_leq(Lang, ElemTI, ti_par_int) then
            true
        else
            not_in_lang([lang_flatzinc],
                "set element type-insts other than `int'",
                Locn, !S)
        )
    ;
        Lang = lang_zinc
    ).

:- pred tic_check_var_set_element_type_inst(src_locn::in, type_inst::in,
    tic_state::in, tic_state::out) is det.

tic_check_var_set_element_type_inst(Locn, ElemTI, !S) :-
    % MiniZinc/FlatZinc var sets can only contain int.
    Lang = !.S ^ lang,
    ( if ti_leq(Lang, ElemTI, ti_par_int) then
        true
    else
        not_in_lang([lang_minizinc, lang_flatzinc],
            "var set element type-insts other than `int'", Locn, !S)
    ).

%-----------------------------------------------------------------------------%

:- pred tic_check_array_element_type_inst(src_locn::in, type_inst::in,
    tic_state::in, tic_state::out) is det.

tic_check_array_element_type_inst(Locn, ElemTI, !S) :-
    % MiniZinc/FlatZinc arrays can only contain bool, int, float, string,
    % or sets.
    Lang = !.S ^ lang,
    ( if 
        ( ti_leq(Lang, ElemTI, ti_par_bool)
        ; ti_leq(Lang, ElemTI, ti_var_bool)
        ; ti_leq(Lang, ElemTI, ti_par_int)
        ; ti_leq(Lang, ElemTI, ti_var_int)
        ; ti_leq(Lang, ElemTI, ti_par_float)
        ; ti_leq(Lang, ElemTI, ti_var_float)
        ; ti_leq(Lang, ElemTI, ti_par_string)
        ; ti_leq(Lang, ElemTI, ti_par_set(ti_par_bool))
        ; ti_leq(Lang, ElemTI, ti_par_set(ti_par_int))
        ; ti_leq(Lang, ElemTI, ti_var_set(ti_par_int))
        ; ti_leq(Lang, ElemTI, ti_par_set(ti_par_float))
        ; ti_leq(Lang, ElemTI, ti_ann)
        )
    then
        true
    else
        % Don't report errors due to unresolved overloading in the
        % presence of other errors since those other errors will be
        % the real cause of the problem.
        ( if
            ElemTI = ti_overloaded_op(_),
            !.S ^ errors = [_ | _]
        then
            true
        else
            not_in_lang([lang_minizinc, lang_flatzinc],
                "array element type-insts other than scalars or sets",
                 Locn, !S)
        )
    ).

%-----------------------------------------------------------------------------%

:- func ti_exprs_are_finite(symbol_table, ti_exprs) = is_finite.

ti_exprs_are_finite(SymTbl, TIEs) = IsFinite :-
    ExprIsFinite = (pred(TIE::in) is semidet :-
        ti_expr_is_finite(SymTbl, TIE) = is_finite
    ),
    ( if list.all_true(ExprIsFinite, TIEs)
    then IsFinite = is_finite
    else IsFinite = is_not_finite
    ).

:- func ti_expr_is_finite(symbol_table, ti_expr) = is_finite.

ti_expr_is_finite(SymTbl, ti_expr(RawTIE, TIEInfo)) = IsFinite :-
    ( if ti_has_error(TIEInfo ^ expr_type_inst) then
        % This avoids cascading type-inst errors, because IsFinite=is_finite
        % never triggers errors, whereas IsFinite=is_not_fintie does.
        IsFinite = is_finite
    else
        (
            RawTIE = raw_ti_expr(_VarPar, BaseTIETail),
            IsFinite = bte_is_finite(SymTbl, BaseTIETail)
        ;
            RawTIE = constrained_raw_ti_expr(TIE, _, _),
            IsFinite = ti_expr_is_finite(SymTbl, TIE)
        )
    ).

:- func bte_is_finite(symbol_table, base_ti_expr_tail) = is_finite.

bte_is_finite(_,bte_bottom) = _ :- unexpected($pred, "bte_bottom").
bte_is_finite(_,bte_error)  = _ :- unexpected($pred, "bte_error").
bte_is_finite(_,bte_bool)   = is_finite.
bte_is_finite(_,bte_int)    = is_not_finite.
bte_is_finite(_,bte_float)  = is_not_finite.
bte_is_finite(_,bte_string) = is_not_finite.
bte_is_finite(_,bte_ann)    = is_not_finite.
bte_is_finite(_,bte_set_expr_as_type_expr(_)) = is_finite.
bte_is_finite(_,bte_range_expr_as_type_expr(E1,E2)) =
    % 1 .. 2 is finite, 1.0 .. 2.0 is not.
    ( if 
        E1 ^ expr_info ^ expr_type_inst = ti_par_int,
        E2 ^ expr_info ^ expr_type_inst = ti_par_int
    then
        is_finite
    else
        is_not_finite
    ).

bte_is_finite(SymTbl, bte_set_of(ElemTIE)) =
    ti_expr_is_finite(SymTbl, ElemTIE).

bte_is_finite(SymTbl, TIE) = IsFinite :-
    (
        TIE = bte_array_of(IndexTIEs, ElemTIE, _),
        TIEs = [ElemTIE | IndexTIEs]
    ;
        TIE = bte_tuple_of(TIEs)
    ;
        TIE = bte_record_of(TIEsNames),
        TIEs = assoc_list.keys(TIEsNames)
    ),
    % These types are finite if all their constituent types are finite.
    IsFinite = ti_exprs_are_finite(SymTbl, TIEs).

bte_is_finite(_, bte_typeinst_var(_))     = is_not_finite.
bte_is_finite(_, bte_any_typeinst_var(_)) = is_not_finite.
bte_is_finite(_, bte_op(_))               = is_not_finite.

bte_is_finite(SymTbl, bte_ident(Id)) = IsFinite :-
    % This is safe to call;  if the symbol didn't exist, the TI would have an
    % error and so 'ti_expr_is_finite' would have not called here.
    Sym = find_existing_symbol(Id, SymTbl),
    (
        % If this variable isn't a fixed set, we'll have already complained
        % about it, so we shouldn't complain about it being non-finite.
        % If it is a fixed set, then it is finite.  So in either case, set
        % IsFinite to 'yes'.
        Sym = sym_variable(_, _, _, _, _),
        IsFinite = is_finite
    ;
        Sym = sym_type_inst_synonym(TypeSynInfo),
        (
            TypeSynInfo = type_syn_info_defined(_, _, _, IsFinite)
        ;
            % Symbol checking should have already caught this case.
            TypeSynInfo = type_syn_info_undefined,
            unexpected($pred, "undef'd sym_type_inst_synonym")
        )
    ;
        Sym = sym_enum(EnumInfo),
        (
            EnumInfo = enum_info_nonflat(IsFinite, _)
        ;
            % If it's undefined and been allowed through, it must be a flat
            % enum.
            ( EnumInfo = enum_info_flat(_)
            ; EnumInfo = enum_info_undefined
            ),
            IsFinite = is_finite
        )
    ;
        % Symbol-checking should have caught these cases.
        ( Sym = sym_operation(_,_)
        ; Sym = sym_enum_case_name(_)
        ; Sym = sym_annotation(_)
        ),
        unexpected($pred, "operation, enum const or annotation")
    ).

%-----------------------------------------------------------------------------%

:- pred tic_fzn_var_decl(src_locn::in, ti_expr::in, zinc_name::in,
    tic_state::in, tic_state::out) is det.

tic_fzn_var_decl(Locn, TIExpr, Name, !S) :-
    TIExpr = ti_expr(RawTIExpr, ExprInfo),
    TI = ExprInfo ^ expr_type_inst,
    (
        ( TI = ti_par_bottom
        ; TI = ti_var_bottom
        ; TI = ti_par_bool
        ; TI = ti_var_bool
        ; TI = ti_var_int
        ; TI = ti_var_float
        ; TI = ti_par_string
        ; TI = ti_ann
        ; TI = ti_var_set(_)
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
        % Do nothing: these types are either not FlatZinc types (for which
        % we have already generated error messages) or they are FlatZinc
        % types that cannot from the
    ;
        TI = ti_par_int,
        ( if
            RawTIExpr = raw_ti_expr(_, BaseTIExprTail),
            ( BaseTIExprTail = bte_range_expr_as_type_expr(_, _)
            ; BaseTIExprTail = bte_set_expr_as_type_expr(_)
            )
          then
            fzn_param_with_constr_ti_error(Locn, Name, TI, !S)
          else
            true
        )
    ;
        TI = ti_par_float,
        ( if
            RawTIExpr = raw_ti_expr(_, BaseTIExprTail),
            BaseTIExprTail = bte_range_expr_as_type_expr(_, _)
          then
            fzn_param_with_constr_ti_error(Locn, Name, TI, !S)
          else
            true
        )
    ;
        TI = ti_par_set(ElemTI),
        ( if ElemTI = ti_par_int then
            ( if
                RawTIExpr = raw_ti_expr(_, BaseTIExprTail),
                BaseTIExprTail = bte_set_of(ElemTIExpr),
                ElemTIExpr = ti_expr(ElemRawTIExpr, _),
                ElemRawTIExpr = raw_ti_expr(_, ElemBaseTIExprTail),
                ( ElemBaseTIExprTail = bte_range_expr_as_type_expr(_, _)
                ; ElemBaseTIExprTail = bte_set_expr_as_type_expr(_)
                )
              then
                fzn_param_with_constr_ti_elems_error(Locn, Name, TI, !S)
              else
                true
            )
          else
            true
        )
    ;
        TI = ti_array(_, ArrayElemTI),
        (
            ArrayElemTI = ti_par_int,
            ( if
                RawTIExpr = raw_ti_expr(_, BaseTIExprTail),
                BaseTIExprTail = bte_array_of(_, ElemTIExpr, _),
                ElemTIExpr = ti_expr(ElemRawTIExpr, _),
                ElemRawTIExpr = raw_ti_expr(_, ElemBaseTIExprTail),
                ( ElemBaseTIExprTail = bte_range_expr_as_type_expr(_, _)
                ; ElemBaseTIExprTail = bte_set_expr_as_type_expr(_)
                )
              then
                fzn_param_with_constr_ti_elems_error(Locn, Name, TI, !S)
              else
                true
            )
        ;
            ArrayElemTI = ti_par_float,
            ( if
                RawTIExpr = raw_ti_expr(_, BaseTIExprTail),
                BaseTIExprTail = bte_array_of(_, ElemTIExpr, _),
                ElemTIExpr = ti_expr(ElemRawTIExpr, _),
                ElemRawTIExpr = raw_ti_expr(_, ElemBaseTIExprTail),
                ElemBaseTIExprTail = bte_range_expr_as_type_expr(_, _)
              then
                fzn_param_with_constr_ti_elems_error(Locn, Name, TI, !S)
              else
                true
            )
        ;
            ArrayElemTI = ti_par_set(SetElemTI),
            ( if SetElemTI = ti_par_int then
                ( if
                    RawTIExpr = raw_ti_expr(_, BaseTIExprTail),
                    BaseTIExprTail = bte_array_of(_, ArrayElemTIExpr, _),
                    ArrayElemTIExpr = ti_expr(ArrayElemRawTIExpr, _),
                    ArrayElemRawTIExpr =
                         raw_ti_expr(_, ArrayElemBaseTIExprTail),
                    ArrayElemBaseTIExprTail = bte_set_of(SetElemTIExpr),
                    SetElemTIExpr = ti_expr(SetElemRawTIExpr, _),
                    SetElemRawTIExpr = raw_ti_expr(_, SetElemBaseTIExprTail),
                    ( SetElemBaseTIExprTail = bte_range_expr_as_type_expr(_, _)
                    ; SetElemBaseTIExprTail = bte_set_expr_as_type_expr(_)
                    )
                  then
                    fzn_param_with_constr_ti_elems_error(Locn, Name, TI, !S)
                  else
                    true
                )
              else
                true
            )
        ;
            ( ArrayElemTI = ti_par_bottom
            ; ArrayElemTI = ti_var_bottom
            ; ArrayElemTI = ti_par_bool
            ; ArrayElemTI = ti_var_bool
            ; ArrayElemTI = ti_var_int
            ; ArrayElemTI = ti_var_float
            ; ArrayElemTI = ti_par_string
            ; ArrayElemTI = ti_ann
            ; ArrayElemTI = ti_var_set(_)
            ; ArrayElemTI = ti_array(_, _)
            ; ArrayElemTI = ti_tuple(_)
            ; ArrayElemTI = ti_record(_)
            ; ArrayElemTI = ti_par_enum(_)
            ; ArrayElemTI = ti_var_enum(_)
            ; ArrayElemTI = ti_par_variable(_, _)
            ; ArrayElemTI = ti_any_variable(_, _)
            ; ArrayElemTI = ti_top
            ; ArrayElemTI = ti_overloaded_op(_)
            ; ArrayElemTI = ti_op(_, _)
            ; ArrayElemTI = ti_error
            ; ArrayElemTI = ti_unknown
            )
        )
    ).

:- pred fzn_param_with_constr_ti_error(src_locn::in, zinc_name::in,
    type_inst::in, tic_state::in, tic_state::out) is det.

fzn_param_with_constr_ti_error(Locn, Name, TI, !S) :-
    ErrMsg = [
        words("the"), type_inst(TI), words("parameter"),
        quote(Name),
        words("has a constrained type-inst, but FlatZinc"),
        words("does not allow"), type_inst(TI), words("parameters with"),
        words("constrained type-insts.")
     ],
     type_inst_error(ErrMsg, Locn, _, !S).

:- pred fzn_param_with_constr_ti_elems_error(src_locn::in, zinc_name::in,
    type_inst::in, tic_state::in, tic_state::out) is det.

fzn_param_with_constr_ti_elems_error(Locn, Name, TI, !S) :-
    ErrMsg = [
        words("the"), type_inst(TI), words("parameter"), quote(Name),
        words("has elements that have a constrained type-inst, "),
        words("but FlatZinc does not allow"), type_inst(TI),
        words("parameters whose elements have a constrained type-inst.")
    ],
    type_inst_error(ErrMsg, Locn, _, !S).

:- pred check_fzn_output_anns(src_locn::in, ti_expr::in, zinc_name::in,
    exprs::in, tic_state::in, tic_state::out) is det.

check_fzn_output_anns(SrcLocn, TIExpr, Name, AnnEs, !S) :-
    TIExpr = ti_expr(_r, ExprInfo),
    TI = ExprInfo ^ expr_type_inst,
    (
        ( TI = ti_par_bottom
        ; TI = ti_var_bottom
        ; TI = ti_par_bool
        ; TI = ti_par_int
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
        % Do nothing: these types are either not FlatZinc types (for which
        % we have already generated error messages) or they cannot have an
        % output annotation attached.
    ;
        ( TI = ti_var_bool
        ; TI = ti_var_int
        ; TI = ti_var_float
        ; TI = ti_var_set(_)
        ),
        list.filter(is_output_array_ann, AnnEs, OutputArrayAnnEs),
        (
            OutputArrayAnnEs = []
        ;
            OutputArrayAnnEs = [_ | _],
            ErrMsg = [words("error:"),
                words("the"), type_inst(TI), words("variable"), quote(Name),
                words("is annotated with an output_array/1 annotation, but"),
                words("it is not an array.")
            ],
            % NOTE: we don't report this as a type-inst error even though we
            % check for it during type-inst checking.
            Errors0 = !.S ^ errors,
            error_at_locn(ErrMsg, SrcLocn, Errors0, Errors),
            !S ^ errors := Errors
        )
    ;
        TI = ti_array(_, ArrayElemTI),
        (
            ( ArrayElemTI = ti_par_bottom
            ; ArrayElemTI = ti_var_bottom
            ; ArrayElemTI = ti_par_bool
            ; ArrayElemTI = ti_par_int
            ; ArrayElemTI = ti_par_float
            ; ArrayElemTI = ti_par_string
            ; ArrayElemTI = ti_ann
            ; ArrayElemTI = ti_par_set(_)
            ; ArrayElemTI = ti_array(_, _)
            ; ArrayElemTI = ti_tuple(_)
            ; ArrayElemTI = ti_record(_)
            ; ArrayElemTI = ti_par_enum(_)
            ; ArrayElemTI = ti_var_enum(_)
            ; ArrayElemTI = ti_par_variable(_, _)
            ; ArrayElemTI = ti_any_variable(_, _)
            ; ArrayElemTI = ti_top
            ; ArrayElemTI = ti_overloaded_op(_)
            ; ArrayElemTI = ti_op(_, _)
            ; ArrayElemTI = ti_error
            ; ArrayElemTI = ti_unknown
            )
            % Do nothing: these elements type are either not FlatZinc types
            % (for which we have already generated error messsages) or they
            % cannot have an output_array annotation attached.
        ;
            ( ArrayElemTI = ti_var_bool
            ; ArrayElemTI = ti_var_int
            ; ArrayElemTI = ti_var_float
            ; ArrayElemTI = ti_var_set(_)
            ),
            list.filter(is_output_var_ann, AnnEs, OutputVarAnnEs),
            (
                OutputVarAnnEs = []
            ;
                OutputVarAnnEs = [_ | _],
                ErrMsg = [words("error:"),
                    words("the array"), quote(Name), words("with element type"),
                    type_inst(ArrayElemTI),
                    words("is annotated with an output_var/0 annotation, but"),
                    words("it is not a scalar.")
                ],
                Errors0 = !.S ^ errors,
                error_at_locn(ErrMsg, SrcLocn, Errors0, Errors),
                !S ^ errors := Errors
            )
        )
    ).

:- pred is_output_var_ann(expr::in) is semidet.

is_output_var_ann(Expr) :-
    Expr = expr(RawExpr, _, _),
    RawExpr = lit_ann(Id, []),
    Id = id_global("output_var").

:- pred is_output_array_ann(expr::in) is semidet.

is_output_array_ann(Expr) :-
    Expr = expr(RawExpr, _, _),
    RawExpr = lit_ann(Id, [_]),
    Id = id_global("output_array").

%-----------------------------------------------------------------------------%
%
% Expressions
%

    % Does the expression have the expected type, allowing for parametric
    % polymorphism, overloading and coercions?
    %
:- pred expr_has_type_inst(Ctrl::in, string::in, context::in, scope_nums::in,
    type_inst::in) : tic_X(expr) `with_inst` tic_X
    <= frontend_control(Ctrl).

expr_has_type_inst(Ctrl, What, Context, ScopeNs, ExpTI,
        E @ expr(_RawE, _AnnEs, EInfo), NE, !S) :-
    type_inst_of_expr(Ctrl, Context, ScopeNs, E, E1, TI, !S),
    ( if ti_leq(!.S ^ lang, TI, ExpTI) then
        NE = make_coerce_expr(TI, ExpTI, E1)
      else
        NE = E1,
        type_inst_mismatch_error(What, [ExpTI], TI, EInfo ^ expr_src_locn,
            _, !S)
    ).

:- pred maybe_expr_has_type_inst(Ctrl::in, string::in, context::in,
    scope_nums::in, type_inst::in) : tic_X(maybe(expr)) `with_inst` tic_X
    <= frontend_control(Ctrl).

maybe_expr_has_type_inst(_, _What, _Context, _ScopeNs, _TI, no, no, !S).
maybe_expr_has_type_inst(Ctrl, What, Context, ScopeNs, TI, yes(WhereE),
        yes(NWhereE), !S) :-
    expr_has_type_inst(Ctrl, What, Context, ScopeNs, TI, WhereE, NWhereE, !S).

    % Used for expressions that must be an array (eg. in generators).
:- pred expr_has_array_type_inst(Ctrl::in, string::in, context::in,
    scope_nums::in) : tic_X_Y(expr, type_inst) `with_inst` tic_X_Y
    <= frontend_control(Ctrl).

expr_has_array_type_inst(Ctrl, What, Context, ScopeNs, E @ expr(_RawE, _AnnEs,
        EInfo), NE, ElemTI, !S) :-
    type_inst_of_expr(Ctrl, Context, ScopeNs, E, E1, TI, !S),
    ( if is_array_type_inst(TI, IndexTI, ElemTI0) then
        ElemTI = ElemTI0,
        NE = make_coerce_expr(TI, ti_array(IndexTI, ElemTI), E1)
      else
        ExpectedTIs = [ti_array(ti_par_bottom, ti_par_bottom)],
        type_inst_mismatch_error(What, ExpectedTIs, TI, EInfo^expr_src_locn,
            ElemTI, !S),
        NE = E
    ).

:- pred is_par_set_type_inst(type_inst::in, type_inst::out) is semidet.

is_par_set_type_inst(TI, ElemTI) :-
    (
        TI = ti_par_set(ElemTI)
    ;
        % Nb: we match against TI=ti_error, not ti_has_error(TI).
        TI = ti_error, ElemTI = ti_error
    ).


:- pred is_array_type_inst(type_inst::in, type_inst::out, type_inst::out)
        is semidet.

is_array_type_inst(TI, IndexTI, ElemTI) :-
    (
        TI = ti_array(IndexTI, ElemTI)
    ;
        TI = ti_par_set(ElemTI), IndexTI = ti_par_int
    ;
        % Nb: we match against TI=ti_error, not ti_has_error(TI).
        TI = ti_error, IndexTI = ti_error, ElemTI = ti_error
    ).

%-----------------------------------------------------------------------------%

:- pred type_inst_is_fixed(string::in, src_locn::in,
    type_inst::in, type_inst::out, tic_state::in, tic_state::out) is det.

type_inst_is_fixed(What, Locn, TI, TIOut, !S) :-
    ( if ti_is_fixed(!.S ^ sym_tbl, TI) then
        TIOut = TI
      else
        ErrMsg = [
            words(What),
            words("with type-inst"),
            type_inst(TI)
        ],
        type_inst_error(ErrMsg, Locn, TIOut, !S)
    ).

%-----------------------------------------------------------------------------%

    % This can transform the expression.  For example, if the expression is "3
    % + 4.0", the '3' is coerced to a '3.0' because it's a float addition.
    %
    % Note that an expression may contain type-inst errors, but have an
    % overall type-inst that is not ti_error -- eg. an if-then-else with a
    % type-incorrect condition but type-correct 'then' and 'else' branches.
:- pred type_inst_of_expr(Ctrl::in, context::in, scope_nums::in)
    : tic_X_Y(expr, type_inst) `with_inst` tic_X_Y
    <= frontend_control(Ctrl).

type_inst_of_expr(Ctrl, Context, ScopeNs,
         expr(RawE, AnnEs, EInfo), expr(NRawE, NAnnEs, NEInfo), TI, !S) :-
    Locn = EInfo^expr_src_locn,
    type_inst_of_raw_expr(Ctrl, Locn, Context, ScopeNs, RawE, NRawE, TI, !S),
    NEInfo = EInfo^expr_type_inst := TI,
    tic_annotations(Ctrl, Context, ScopeNs, AnnEs, NAnnEs, !S).

:- pred type_inst_of_raw_expr(Ctrl::in, src_locn::in, context::in,
    scope_nums::in) : tic_X_Y(raw_expr, type_inst) `with_inst` tic_X_Y
    <= frontend_control(Ctrl).

    % An identifier may be a: variable name, quoted operator, flat enum
    % case name, enum/set type name, or a call to a nullary pred/func.
type_inst_of_raw_expr(Ctrl, Locn, Context, ScopeNs, ident(Id), NRawE, TI, !S) :-
    ( if add_scopenum_to_id(ScopeNs, !.S^sym_tbl, Id, NId, Sym) then
        (   Sym = sym_variable(VarTI, VarKind, IsDefd, IsDefnReqd,_),
            % If we're doing model checking or model generation, an undefined
            % global param is ok because it can be specified in a data file.
            % Anything else is an error.
            ( if IsDefd = undefined, IsDefnReqd = defn_required,
                 ( !.S ^ checking = instance_checking ; VarKind \= global_var )
              then
                % In FlatZinc, it shouldn't be possible to have a use of a
                % variable that is not defined but should be, because the
                % syntax prevents it.
                then_unexpected(unify(!.S ^ lang, lang_flatzinc),
                    "use of undefined variable in FlatZinc?"),
                UnDefSymErr = [
                    words("use of undefined"), words(Sym ^ show),
                    quote(Id ^ id_name)
                ],
                symbol_error(UnDefSymErr, Locn, TI, !S)
              else
                TI = VarTI
            ),
            NRawE = ident(NId)
        ;
            Sym = sym_operation(OpKind, ProcInfos),
            ( if any_ti_sig_has_no_args(ProcInfos) then
                % If it's a func/pred that takes no arguments, then convert it
                % into an app.  Otherwise, complain.  We don't check to see if
                % the func/pred is defined here, we let the call to
                % 'type_inst_of_raw_expr' do that.
                RawE2 = app(NId, unset_proc_number, predfunc_app, []),
                type_inst_of_raw_expr(Ctrl, Locn, Context, ScopeNs, RawE2,
                    NRawE, TI, !S)
              else
                % For non-overloaded names, we complain if the procedure isn't
                % defined.  For overloaded ones, we complain if any are
                % undefined;  this is a little imprecise but easier, as we
                % don't know at this point which overloading is actually used.
                % Besides having an undefined *function* (not predicate)
                % doesn't seem much use.
                ( if ProcInfos = [proc_info(ProcN, IsDefd0, IsAnntd0, TISig)] then
                    % In Zinc, some functions maybe undefined since they can be
                    % externally defined.  Only emit an error message if we are
                    % sure if this is not an external function, i.e. there's no
                    % annotation attached to the declaration.  Treat a bodyless
                    % function as being "defined" if there is an annotation or
                    % annotation variable attached to it.
                    ( if
                        IsDefd0 = undefined,
                        !.S ^ lang = lang_zinc,
                        IsAnntd0 = proc_is_annotated,
                        OpKind = function_operation
                       then
                        IsDefd = defined
                       else
                        IsDefd = IsDefd0
                    ),
                    TI = ti_op(TISig, yes(ProcN))
                  else
                    P = (pred(P_Info::in, (ProcN-P_TISig)::out,
                            !.IsDefd::in, !:IsDefd::out) is det :-
                        P_Info = proc_info(ProcN, P_IsDefd, P_IsAnntd, P_TISig),
                        (
                            !.IsDefd = undefined
                        ;
                            !.IsDefd = defined,
                            (
                                P_IsDefd = undefined,
                                % See above for the rationale for this.
                                ( if
                                    !.S ^ lang = lang_zinc,
                                    OpKind = function_operation,
                                    P_IsAnntd = proc_is_annotated
                                  then
                                    true
                                  else
                                    !:IsDefd = undefined
                                )
                            ;
                                P_IsDefd = defined
                            )
                        )
                    ),
                    list.map_foldl(P, ProcInfos, ProcNsTISigs, defined, IsDefd),
                    TI = ti_overloaded_op(ProcNsTISigs)
                ),
                (
                    IsDefd = defined
                ;
                    IsDefd = undefined,
                    UndefProcErr = [
                        words("one or more procedures of"),
                        words(Sym ^ show),
                        quote(Id ^ id_name),
                        words("declared but not defined")
                    ],
                    symbol_error(UndefProcErr, Locn, _, !S)
                ),
                NRawE = ident(NId)
            )
        ;
            Sym = sym_annotation(_),
            % Convert it into a lit_ann, then recheck.
            RawE2 = lit_ann(NId, []),
            type_inst_of_raw_expr(Ctrl, Locn, Context, ScopeNs, RawE2,
                NRawE, TI, !S)
        ;
            % Some types can be used as sets.
            Sym = sym_type_inst_synonym(TypeSynInfo),
            (
                TypeSynInfo = type_syn_info_defined(SynTI, IsUsableAsValue, _,
                    _),
                (
                    IsUsableAsValue = usable_as_value,
                    % It should be defined and fixed.
                    ( if ti_is_fixed(!.S^sym_tbl, SynTI) then
                        TI = ti_par_set(SynTI)
                      else
                        % Symbol checking should have already caught this case.
                        unexpected($pred,
                            ": (ident): bad sym_type_inst_synonym")
                    )
                ;
                    IsUsableAsValue = not_usable_as_value,
                    NotUsableAsValErr = [
                        words(Sym ^ show), quote(Id ^ id_name),
                        words("used as a value, but it is a synonym of a"),
                        words("type that cannot be used as a value")
                    ],
                    symbol_error(NotUsableAsValErr, Locn, TI, !S)
                )
            ;
                TypeSynInfo = type_syn_info_undefined,
                UnDefAsValErr = [
                    words("undefined"), words(Sym ^ show), quote(Id ^ id_name),
                    words("used as a value")
                ],
                symbol_error(UnDefAsValErr, Locn, TI, !S)
            ),
            NRawE = ident(NId)
        ;
            Sym = sym_enum(EnumInfo),
            (
                EnumInfo = enum_info_undefined,
                Checking = !.S ^ checking,
                (
                    Checking = instance_checking,
                    UnDefEnumValErr = [
                        words("undefined"), words(Sym ^ show),
                        quote(Id ^ id_name),
                        words("used as a value")
                    ],
                    symbol_error(UnDefEnumValErr, Locn, TI, !S)
                ;
                    % Enums can be undefined during model checking,
                    % because they can be defined in data files.
                    ( Checking = model_checking
                    ; Checking = model_generation
                    ),
                    TI = ti_par_set(ti_par_enum(Id ^ id_name))
                )
            ;
                % Flat enums can be used as types.
                EnumInfo = enum_info_flat(_),
                TI = ti_par_set(ti_par_enum(Id ^ id_name))
            ;
                % Non-flat enums cannot be used as types.
                EnumInfo = enum_info_nonflat(_, _),
                UnDefNFEValErr = [
                    words(Sym ^ show), quote(Id ^ id_name),
                    words("used as a value")
                ],
                symbol_error(UnDefNFEValErr, Locn, TI, !S)
            ),
            NRawE = ident(NId)
        ;
            Sym = sym_enum_case_name(EnumName),
            Sym2 = find_existing_global_symbol(EnumName, !.S^sym_tbl),
            (
                Sym2 = sym_enum(EnumInfo),
                (
                    (
                        EnumInfo = enum_info_nonflat(_, CaseInfos),
                        ( if    get_field_infos_for_case(CaseInfos,
                                    Id ^ id_name, FieldInfos)
                          then  list.length(FieldInfos, ExpectedLength)
                          else  unexpected($pred, "cannot find field for case")
                        )
                    ;
                        EnumInfo = enum_info_flat(_),
                        ExpectedLength = 0
                    ),
                    ActualLength = 0,
                    ( if ExpectedLength = ActualLength then
                        TI = ti_par_enum(EnumName)
                      else
                        BadArgNumErr = [
                            words("enum literal with case name"),
                            quote(Id ^ id_name),
                            words("has wrong number of arguments:"),
                            words("expected"),
                            suffix(fixed(int_to_string(ExpectedLength)), ","),
                            words("actual"),
                            fixed(int_to_string(ActualLength))
                        ],
                        type_inst_error(BadArgNumErr, Locn, TI, !S)
                    )
                ;
                    EnumInfo = enum_info_undefined,
                    unexpected($pred, "encountered undefined enum")
                )
            ;
                ( Sym2 = sym_variable(_, _, _, _, _)
                ; Sym2 = sym_operation(_, _)
                ; Sym2 = sym_annotation(_)
                ; Sym2 = sym_type_inst_synonym(_)
                ; Sym2 = sym_enum_case_name(_)
                ),
                %
                % We may have ended up here as the result of previous errors,
                % e.g. clashing symbol names, so if errors have been detected
                % earlier then mark the type-inst as ti_error and continue; if
                % no previous errors have been detected we should not get here.
                %
                Errors = !.S ^ errors,
                (
                    Errors = [_ | _],
                    TI = ti_error
                ;
                    Errors = [],
                    unexpected($pred, 
                        "(ident): something wrong with sym_enum_case_name case")
                )
            ),
            NRawE = ident(NId)
        )
      else
        UnDeclIdErr = [
            quote(Id ^ id_name), words("undeclared")
        ],
        symbol_error(UnDeclIdErr, Locn, TI, !S),
        NRawE = ident(Id)   % Leave the scope number unset.
    ).

type_inst_of_raw_expr(_, _Locn, _Context, _ScopeNs, anon_var, anon_var,
        ti_var_bottom, !S).

type_inst_of_raw_expr(_, _Locn, _Context, _ScopeNs, RawE @ lit(Lit), RawE, TI,
        !S) :-
    (   Lit = bool(_),      TI = ti_par_bool
    ;   Lit = int(_),       TI = ti_par_int
    ;   Lit = floatstr(_),  TI = ti_par_float
    ;   Lit = string(_),    TI = ti_par_string
    ).

type_inst_of_raw_expr(Ctrl, Locn, Context, ScopeNs,
        lit_set(ElemEs), lit_set(NElemEs), ti_par_set(ElemTI), !S) :-
    type_inst_of_exprs(Ctrl, "elements of set literal", Locn, Context,
        ScopeNs, ElemEs, NElemEs, ElemTI0, !S),
    tic_check_par_set_element_type_inst(Locn, ElemTI0, !S),
    type_inst_is_fixed("set literal has non-fixed element", Locn,
        ElemTI0, ElemTI, !S).

type_inst_of_raw_expr(Ctrl, Locn, Context, ScopeNs,
        lit_simple_array(ElemEs), lit_simple_array(NElemEs), TI, !S) :-
    type_inst_of_exprs(Ctrl, "elements of simple array literal", Locn,
        Context, ScopeNs, ElemEs, NElemEs, ElemTI, !S),
    tic_check_array_element_type_inst(Locn, ElemTI, !S),
    TI = ti_array(ti_par_int, ElemTI).

type_inst_of_raw_expr(Ctrl, Locn, Context, ScopeNs,
        lit_indexed_array( IndexedEs),
        lit_indexed_array(NIndexedEs), TI, !S) :-
    not_in_lang([lang_minizinc, lang_flatzinc], "indexed array literals",
        Locn, !S),
    % XXX TODO: avoid decomposing the list this way.
    % And do the whole thing with less traversals of the elements.
    decompose_index_exprs(IndexedEs, IndexEs, ElemEs),
    type_inst_of_exprs(Ctrl, "indices of literal array",
        Locn, Context, ScopeNs, IndexEs, NIndexEs, IndexTI, !S),
    type_inst_is_fixed("indices of literal array are non-fixed", Locn, IndexTI,
        IndexT, !S),
    type_inst_of_exprs(Ctrl, "values of literal array",
        Locn, Context, ScopeNs, ElemEs, NElemEs, ElemTI, !S),
    list.foldl_corresponding(make_index_expr_acc, NIndexEs, NElemEs,
        [], RevNIndexedEs),
    list.reverse(RevNIndexedEs, NIndexedEs),
    tic_check_array_element_type_inst(Locn, ElemTI, !S),
    TI = ti_array(IndexT, ElemTI).

type_inst_of_raw_expr(Ctrl, Locn, Context, ScopeNs,
        lit_tuple(TupEs), lit_tuple(NTupEs), TI, !S) :-
    not_in_lang([lang_minizinc, lang_flatzinc], "tuple literals", Locn, !S),
    type_insts_of_exprs(Ctrl, Context, ScopeNs, TupEs, NTupEs, TupTIs, !S),
    TI = ti_tuple(TupTIs).

type_inst_of_raw_expr(Ctrl, Locn, Context, ScopeNs,
        lit_record(FieldsEs), lit_record(NFieldsEs), TI, !S) :-
    not_in_lang([lang_minizinc, lang_flatzinc], "record literals", Locn, !S),
    FieldNames = assoc_list.keys(FieldsEs),
    ( if RepFieldName = first_repeated_elem(FieldNames) then
        RepFieldNameErr = [
            words("field name"), quote(RepFieldName),
            words("repeated in record expression")
        ],
        symbol_error(RepFieldNameErr, Locn, TI, !S),
        NFieldsEs = FieldsEs
      else
        NameAndTypeInstOfField = (
            pred((Name-E)::in, (Name-NE)::out, (P_TI-Name)::out, !.S::in,
                    !:S::out) is det :-
                type_inst_of_expr(Ctrl, Context, ScopeNs, E, NE, P_TI, !S)
        ),
        list.map2_foldl(NameAndTypeInstOfField, FieldsEs, NFieldsEs, TIsNames,
            !S),
        TI = ti_record(TIsNames)
    ).

type_inst_of_raw_expr(Ctrl, Locn, Context, ScopeNs,
        lit_nonflat_enum(CaseId, NamesEs), lit_nonflat_enum(NCaseId, NamesNEs),
        TI, !S) :-
    not_in_lang([lang_minizinc, lang_flatzinc], "enum literals", Locn, !S),
    ( if add_scopenum_to_id(ScopeNs, !.S^sym_tbl, CaseId, CaseId2, Sym) then
        NCaseId = CaseId2,
        ( if
            Sym = sym_enum_case_name(EnumName),
            Sym2 = find_existing_global_symbol(EnumName, !.S ^ sym_tbl),
            Sym2 = sym_enum(EnumInfo),
            (
                EnumInfo = enum_info_nonflat(_, CaseInfos),
                get_field_infos_for_case(CaseInfos, CaseId ^ id_name,
                    FieldInfos)
            ;
                EnumInfo = enum_info_flat(_),
                unexpected($pred, ": case for flat enum")
            )
          then
            P = (pred(FieldInfo::in, (ActualName-E)::in, (ActualName-NE)::out,
                    !.S::in, !:S::out) is det :-
                FieldInfo = nfe_field_info(ExpectedName, _, P_TI),
                ( if    ExpectedName = ActualName
                  then
                        expr_has_type_inst(Ctrl, "value for enum field `" ++
                            ExpectedName ++ "'", Context, ScopeNs, P_TI, E, NE,
                            !S)
                  else
                        BadFieldNameErr = [
                            words("enum literal has wrong field name:"),
                            words("expected"),
                            suffix(quote(ExpectedName), ","),
                            words("actual"), quote(ActualName)
                        ],
                        type_inst_error(BadFieldNameErr, Locn, _, !S),
                        % Might as well check the expression itself for errors.
                        type_inst_of_expr(Ctrl, Context, ScopeNs, E, NE, _, !S)
                )
            ),
            list.length(FieldInfos, ExpectedLength),
            list.length(NamesEs, ActualLength),
            ( if ExpectedLength = ActualLength then
                list.map_corresponding_foldl(P, FieldInfos, NamesEs,
                    NamesNEs, !S),
                TI = ti_par_enum(EnumName)
              else
                BadArgNumErr = [
                    words("enum literal with case name"),
                    quote(CaseId ^ id_name),
                    words("has wrong number of arguments: expected"),
                    words(int_to_string(ExpectedLength) ++ ", actual"),
                    words(int_to_string(ActualLength))
                ],
                type_inst_error(BadArgNumErr, Locn, TI, !S),
                % Type-inst check the expressions anyway, to see if they contain
                % any errors.
                assoc_list.keys_and_values(NamesEs, Names, Es),
                type_insts_of_exprs(Ctrl, Context, ScopeNs, Es, NEs, _, !S),
                NamesNEs = assoc_list.from_corresponding_lists(Names, NEs)
            )
          else
            BadCaseErr = [
                words("using"), words(Sym ^ show), quote(CaseId ^ id_name),
                words("as an enum")
            ],
            symbol_error(BadCaseErr, Locn, TI, !S),
            NamesNEs = NamesEs
        )
      else
        UnDeclCaseErr = [
            quote(CaseId ^ id_name), words("undeclared")
        ],
        symbol_error(UnDeclCaseErr, Locn, TI, !S),
        NCaseId = CaseId,       % leave the scope number unset
        NamesNEs = NamesEs
    ).

type_inst_of_raw_expr(Ctrl, Locn, Context, ScopeNs,
        lit_nonflat_enum_simple(CaseId, Es),
        lit_nonflat_enum_simple(CaseId, NEs), TI, !S) :-
    not_in_lang([lang_minizinc, lang_flatzinc], "enum literals", Locn, !S),
    % Look up CaseId symbol, it should be sym_enum_case_name(EnumName).
    % This must succeed, else the expr would still be an 'app' expression.
    Sym = find_existing_symbol(CaseId, !.S ^ sym_tbl),
    ( if
        Sym = sym_enum_case_name(EnumName),
        Sym2 = find_existing_global_symbol(EnumName, !.S ^ sym_tbl),
        Sym2 = sym_enum(EnumInfo),
        (
            EnumInfo = enum_info_nonflat(_, CaseInfos),
            get_field_infos_for_case(CaseInfos, CaseId ^ id_name, FieldInfos)
        ;
            EnumInfo = enum_info_flat(_),
            FieldInfos = []
        ;
            EnumInfo = enum_info_undefined,
            false
        )
      then
        P = (pred(FieldInfo::in, E::in, NE::out, N::in, N+1::out,
                !.S::in, !:S::out) is det :-
            FieldInfo = nfe_field_info(_, _, P_TI),
            expr_has_type_inst(Ctrl, "value for enum field #" ++ N ^ string,
                Context, ScopeNs, P_TI, E, NE, !S)
        ),
        list.length(FieldInfos, ExpectedLength),
        list.length(Es, ActualLength),
        ( if ExpectedLength = ActualLength then
            list.map_corresponding_foldl2(P, FieldInfos, Es, NEs, 1, _, !S),
            TI = ti_par_enum(EnumName)
          else
            BadArgNumErr = [
                words("simple enum literal with case name"),
                quote(CaseId ^ id_name),
                words("has wrong number of arguments: expected"),
                words(int_to_string(ExpectedLength) ++ ", actual"),
                words(int_to_string(ActualLength))
            ],
            type_inst_error(BadArgNumErr, Locn, TI, !S),
            % Type-inst check the expressions anyway, to see if they contain
            % any errors.
            type_insts_of_exprs(Ctrl, Context, ScopeNs, Es, NEs, _, !S)
        )
      else
        unexpected($pred,
            ": (lit_nonflat_enum_simple): something went wrong")
    ).

type_inst_of_raw_expr(Ctrl, Locn, Context, ScopeNs,
        lit_ann(Id, ArgEs), lit_ann(NId, NArgEs), TI, !S) :-
    % Check the arguments first.
    type_insts_of_exprs(Ctrl, Context, ScopeNs, ArgEs, ArgEs1, ActualTIs, !S),

    % Look up Id symbol, it should be sym_annotation(FormalTIEs).
    %
    ( if add_scopenum_to_id(ScopeNs, !.S^sym_tbl, Id, Id2, Sym) then
        NId = Id2,
        (
            Sym = sym_annotation(FormalTIs),
            % We reuse the code for handling predicate/function calls, which
            % are very similar.  We treat the annotation like a pred/func, by
            % giving it a full type-inst sig.  We use 1 as a dummy proc-number
            % and ti_ann as the return type-inst.  We also fake IsDefd=yes so
            % that 'type_inst_of_app' doesn't complain about calling something
            % that isn't defined.
            ProcInfos = [proc_info(1, defined, proc_is_not_annotated,
                FormalTIs - ti_ann)],
            What = Sym^show,
            % InstFormalTIs are the FormalTIs with the type-vars all
            % instantiated, which is done by looking at the ActualTIs.
            % If there's a problem, InstFormalTIs are unified with
            % ActualTIs so this coercing does nothing.
            type_inst_of_app(What, Locn, Id, ProcInfos, ActualTIs, NActualTIs,
                InstFormalTIs, _NewProcN, TI, _IsDefd, !S),
            NArgEs = list.map_corresponding3(make_coerce_expr, NActualTIs,
                InstFormalTIs, ArgEs1)
        ;
            ( Sym = sym_variable(_, _, _, _, _)
            ; Sym = sym_operation(_, _)
            ; Sym = sym_type_inst_synonym(_)
            ; Sym = sym_enum(_)
            ; Sym = sym_enum_case_name(_)
            ),
            BadAnnSymErr = [
                words(Sym ^ show), quote(Id ^ id_name),
                words("used as an annotation")
            ],
            symbol_error(BadAnnSymErr, Locn, TI, !S),
            NArgEs = ArgEs1
        )
      else
        Lang = !.S^lang,
        (
            ( Lang = lang_zinc
            ; Lang = lang_minizinc
            ),
            UnDeclAnnErr = [
                quote(Id ^ id_name), words("undeclared")
            ],
            symbol_error(UnDeclAnnErr, Locn, TI, !S)
        ;
            % In FlatZinc, unrecognised annotations cause warnings.
            Lang = lang_flatzinc,
            WarnUnknownAnns = warn_unknown_fzn_annotations(Ctrl),
            (
                WarnUnknownAnns = yes,
                UnknownAnnWarn = [
                    words("annotation"), quote(Id ^ id_name),
                    words("not recognised, ignoring")
                ],
                symbol_warning(UnknownAnnWarn, Locn, !S)
            ;
                WarnUnknownAnns = no
            ),
            TI = ti_ann
        ),
        NId = Id,
        NArgEs = ArgEs1
    ).

type_inst_of_raw_expr(Ctrl, Locn, Context, ScopeNs,
        comprehension( CompKind,  Generators,  MaybeWhereE),
        comprehension(NCompKind, NGenerators, NMaybeWhereE), TI, !S) :-
    % Comprehensions introduce a new scope.
    add_new_scope(ScopeNs, NScopeNs, !S),

    % For each generator: handle the expression, then add the generator vars
    % to the symbol table and update their scope-numbers.  Nb: each generator
    % sees the names of the variables in any previous generators.
    DoGenerator = (
        pred(generator(LVars, GenE)::in, generator(NLVars, NGenE)::out,
                !.S::in, !:S::out) is det :-
            % Check the generator expression is an array and get its element
            % type-inst (VarTI).
            expr_has_array_type_inst(Ctrl, "generator expression", Context,
                NScopeNs, GenE, NGenE, VarTI, !S),

            % Add the generator variables to the symbol table.
            list.map_foldl(
                % We say generator vars are 'defined' so that if you reuse the
                % same var name twice, it is an "generator variable defined
                % more than once" error.
                add_variable_sym_and_update_id(Locn, NScopeNs, VarTI,
                    generator_var, defined, defn_not_required), LVars,
                    NLVars, !S)
    ),
    list.map_foldl(DoGenerator, Generators, NGenerators, !S),

    % 'where' expressions must be fixed, at least for now.
    maybe_expr_has_type_inst(Ctrl, "where expression", Context, NScopeNs,
        ti_par_bool, MaybeWhereE, NMaybeWhereE, !S),

    % Handle the head expression.
    (
        CompKind = set_comp(E),
        type_inst_of_expr(Ctrl, Context, NScopeNs, E, NE, ElemTI0, !S),
        NCompKind = set_comp(NE),
        tic_check_par_set_element_type_inst(Locn, ElemTI0, !S),
        type_inst_is_fixed("set comprehension has non-fixed element", Locn,
            ElemTI0, ElemTI, !S),
        TI = ti_par_set(ElemTI)
    ;
        CompKind = simple_array_comp(E),
        type_inst_of_expr(Ctrl, Context, NScopeNs, E, NE, ElemTI, !S),
        NCompKind = simple_array_comp(NE),
        tic_check_array_element_type_inst(Locn, ElemTI, !S),
        TI = ti_array(ti_par_int, ElemTI)
    ;
        CompKind = indexed_array_comp(IndexE - ElemE),
        not_in_lang([lang_minizinc, lang_flatzinc],
            "indexed array comprehensions", Locn, !S),
        type_inst_of_expr(Ctrl, Context, NScopeNs, IndexE, NIndexE, IndexTI0,
            !S),
        type_inst_is_fixed("array comprehension index is non-fixed", Locn,
            IndexTI0, IndexTI, !S),
        type_inst_of_expr(Ctrl, Context, NScopeNs, ElemE, NElemE, ElemTI, !S),
        NCompKind = indexed_array_comp(NIndexE - NElemE),
        tic_check_array_element_type_inst(Locn, ElemTI, !S),
        TI = ti_array(IndexTI, ElemTI)
    ).

    % We unfold the array accesses first (eg. a[1,2,3] --> a[1][2][3]).
    % This means that it we have eg. a set of sets S and we do something
    % like "S[1][2]" that it all works (ie. the coercions are added
    % correctly).
    %
type_inst_of_raw_expr(Ctrl, Locn, Context, ScopeNs,
        array_access( ArrayE,  IndexEs),
        array_access(NArrayE, NIndexEs), TI, !S) :-
    type_inst_of_expr(Ctrl, Context, ScopeNs, ArrayE, ArrayE2, ArrayTI, !S),
    % We check the args here so that any errors in them are found even if the
    % array expression has an error (eg. it's an undeclared identifier).
    list.map2_foldl(type_inst_of_expr(Ctrl, Context, ScopeNs), IndexEs,
        NIndexEs0, ActualIndexTIs, !S),

    % XXX Fix up the type of anonymous indices in array accesses in MiniZinc.
    % The above code will infer their type to be `var bottom', which
    % is what is required for Zinc, but is too general for MiniZinc since
    % it only supports integer array indices.
    %
    Lang = !.S ^ lang,
    (
        Lang = lang_minizinc,
        NIndexEs = list.map(fixup_mzn_anon_array_access_index, NIndexEs0)
    ;
        ( Lang = lang_zinc
        ; Lang = lang_flatzinc
        ),
        NIndexEs = NIndexEs0
    ),

    ( if ( ArrayTI = ti_array(FormalIndexTI, ElemTI)
         ; ArrayTI = ti_par_set(ElemTI), FormalIndexTI = ti_par_int
         )
      then
        ( if ActualIndexTIs = [ActualIndexTI0] then
            ActualIndexTI = ActualIndexTI0
          else
            ActualIndexTI = ti_tuple( ActualIndexTIs )
        ),
        FixedActualIndexTI = parify_type_inst(!.S^sym_tbl, ActualIndexTI),
        ( if not ti_has_top(array_index_ti_lub(!.S^lang,
                            FixedActualIndexTI, FormalIndexTI)) then
            ( if ti_is_fixed(!.S^sym_tbl, ActualIndexTI) then
                TI = ElemTI
              else
                VarifiedElemTI = varify_type_inst(!.S^sym_tbl, ElemTI),
                ( if not ti_has_top(VarifiedElemTI) then
                    TI = VarifiedElemTI
                  else
                    ErrMsg = [
                        words("non-fixed array index cannot be used"),
                        words("to access an array with a non-varifiable"),
                        words("element type-inst"),
                        type_inst(ElemTI)
                    ],
                    type_inst_error(ErrMsg, Locn, TI, !S)
                )
            )
          else
            MaybeArrayName = maybe_id_name(NArrayE),
            (
                MaybeArrayName = yes(ArrayName),
                MsgHead = "index in access to array `" ++ ArrayName ++ "'"
            ;
                MaybeArrayName = no,
                MsgHead = "array index"
            ),
            type_inst_mismatch_error(MsgHead, [FormalIndexTI], ActualIndexTI,
                Locn, TI, !S)
        ),
        NArrayE = make_coerce_expr(ArrayTI, ti_array(FormalIndexTI, ElemTI),
            ArrayE2)
      else
        type_inst_access_error("array", ArrayTI, Locn, TI, !S),
        NArrayE  = ArrayE2
    ).

type_inst_of_raw_expr(Ctrl, Locn, Context, ScopeNs,
        tuple_access( TupE, FieldN),
        tuple_access(NTupE, FieldN), TI, !S) :-
    not_in_lang([lang_minizinc, lang_flatzinc], "tuple accesses", Locn, !S),
    type_inst_of_expr(Ctrl, Context, ScopeNs, TupE, NTupE, TupTI, !S),
    ( if TupTI = ti_tuple(TIs) then
        ( if index1(TIs, FieldN, TI0) then
            TI = TI0
          else
            InvalidFieldNumErr = [
                words("invalid field number"),
                quote(int_to_string(FieldN)),
                words("in tuple access")
            ],
            type_inst_error(InvalidFieldNumErr, Locn, TI, !S)
        )
      else
        type_inst_access_error("tuple", TupTI, Locn, TI, !S)
    ).

type_inst_of_raw_expr(Ctrl, Locn, Context, ScopeNs,
        record_access( RecE, FieldName),
        record_access(NRecE, FieldName), TI, !S) :-
    not_in_lang([lang_minizinc, lang_flatzinc], "record accesses", Locn, !S),
    type_inst_of_expr(Ctrl, Context, ScopeNs, RecE, NRecE, RecTI, !S),
    ( if RecTI = ti_record(TIsNames) then
        ( if TI0 = get_type_inst_of_name(TIsNames, FieldName) then
            TI = TI0
          else
            InvalidFieldErr = [
                words("invalid field name"),
                quote(FieldName),
                words("in record access")
            ],
            type_inst_error(InvalidFieldErr, Locn, TI, !S)
        )
      else if RecTI = ti_par_enum(EnumName) then
        ( if in_case_label_context(Context) then
            Sym = find_existing_global_symbol(EnumName, !.S^sym_tbl),
            ( if Sym = sym_enum(EnumInfo) then
                (
                    EnumInfo = enum_info_nonflat(_, EnumCases),
                    ( if TI0 = get_type_inst_of_enum_field(EnumCases, FieldName)
                      then TI = TI0
                      else
                           InvalidFieldNameErr = [
                               words("invalid field name"), quote(FieldName),
                               words("in enum access")
                           ],
                           type_inst_error(InvalidFieldNameErr, Locn, TI, !S)
                    )
                ;
                    ( EnumInfo = enum_info_flat(_)
                    ; EnumInfo = enum_info_undefined
                    ),
                    unexpected($pred, ": field access for flat enum")
                )
              else
                unexpected($pred, ": enum access problem")
            )
          else
            EnumAccessErr = [
                words("enum access outside case expression")
            ],
            type_inst_error(EnumAccessErr, Locn, TI, !S)
        )
      else
        type_inst_access_error("record or enum", RecTI, Locn, TI, !S)
    ).

type_inst_of_raw_expr(Ctrl, _Locn, Context, ScopeNs,
        if_then_else( IfE,  ThenE @ expr(_,_,ThenEInfo), ElseE),
        if_then_else(NIfE, NThenE, NElseE), TI, !S) :-
    expr_has_type_inst(Ctrl, "condition of if-then-else", Context, ScopeNs,
        ti_par_bool, IfE, NIfE, !S),
    type_inst_of_exprs(Ctrl, "then and else branches of if-then-else",
        ThenEInfo ^ expr_src_locn, Context, ScopeNs, [ThenE, ElseE],
        NThenElseEs, TI, !S),
    NThenE = list.det_index1(NThenElseEs, 1),
    NElseE = list.det_index1(NThenElseEs, 2).

type_inst_of_raw_expr(Ctrl, Locn, Context, ScopeNs,
        case(SelectE, LabelIdsEs), case(NSelectE, NLabelIdsEs), TI, !S) :-
    not_in_lang([lang_minizinc, lang_flatzinc], "case expressions", Locn, !S),
    % Check SelectE is a fixed enum expression.
    type_inst_of_expr(Ctrl, Context, ScopeNs, SelectE, NSelectE, SelectTI,
        !S),
    ( if ( SelectTI = ti_par_enum(_)
         ; % Nb: we match against SelectTI=ti_error, not ti_has_error(SelectTI).
           SelectTI = ti_error
         )
      then
        true
      else
        What = "select expression in case expression",
        type_inst_mismatch_error_2(What, "a fixed enum", SelectTI,
            SelectE^expr_info^expr_src_locn, _, !S)
    ),

    % For each label:
    % - check the label id is that of an enum case
    % - if so, add the parent enum to the set
    ProcessLabel = (
        pred(LabelId-LabelE::in, NLabelId-LabelE::out, P_EnumNameSet::in,
                P_NEnumNameSet::out, !.S::in, !:S::out) is det :-
            ( if add_scopenum_to_id(ScopeNs, !.S^sym_tbl, LabelId,
                    LabelId2, P_Sym) then
                NLabelId = LabelId2,
                ( if P_Sym = sym_enum_case_name(P_EnumName) then
                    P_NEnumNameSet = set.insert(P_EnumNameSet, P_EnumName)
                  else
                    BadLabelErr = [
                        words(P_Sym ^ show), quote(LabelId ^ id_name),
                        words("used as a case label")
                    ],
                    symbol_error(BadLabelErr, Locn, _, !S),
                    P_NEnumNameSet = P_EnumNameSet
                )
              else
                UnDeclLabelErr = [
                    quote(LabelId ^ id_name), words("undeclared")
                ],
                symbol_error(UnDeclLabelErr, Locn, _, !S),
                P_NEnumNameSet = P_EnumNameSet,
                NLabelId = LabelId      % leave the scope number unset
            )
    ),
    set.init(EnumNameSet0),
    list.map_foldl2(ProcessLabel, LabelIdsEs, LabelIdsEs2, EnumNameSet0,
        EnumNameSet, !S),

    % Ensure all the case names belong to the same enum.
    ( if is_singleton(EnumNameSet, EnumName) then
        % Check that all the casenames from that enum are present.
        Sym = find_existing_global_symbol(EnumName, !.S^sym_tbl),
        ( if Sym = sym_enum(EnumInfo) then
            (
                (
                    EnumInfo = enum_info_flat(EnumCaseNames)
                ;
                    EnumInfo = enum_info_nonflat(_, CaseInfos),
                    EnumCaseNames = get_nfe_case_names(CaseInfos)
                ),
                GetLabelName = (func(Id - _) = Id ^ id_name),
                LabelNames = list.map(GetLabelName, LabelIdsEs2),
                SortedLabelNames = sort(LabelNames):list(T),
                SortedEnumCaseNames = sort(EnumCaseNames),
                ( if SortedEnumCaseNames = SortedLabelNames then
                    true
                  else
                    LabelMismatchErr = [
                        words("case labels don't fully match case"),
                        words("names of enum"), quote(EnumName)
                    ],
                    symbol_error(LabelMismatchErr, Locn, _, !S)
                )
            ;
                EnumInfo = enum_info_undefined,
                unexpected($pred, ": case expression with undefined enum")
            )
          else
            unexpected($pred, ": problem with case expression")
        )
      else if set.empty(EnumNameSet) then
        % All the case labels had problems.  Do nothing, as we've already
        % issued errors about them.
        true
      else
        MixedCaseErr = [
            words("case names in case expression from more than one enum")
        ],
        symbol_error(MixedCaseErr, Locn, _, !S)
    ),

    % Check LabelIdsEs2 all have matching types.
    assoc_list.keys_and_values(LabelIdsEs2, LabelIds2, Es2),
    type_inst_of_exprs(Ctrl, "label expressions in case expression",
        Locn, [in_case_label_expr | Context], ScopeNs, Es2, NLabelEs, TI, !S),
    NLabelIdsEs = assoc_list.from_corresponding_lists(LabelIds2, NLabelEs).

    % Nb:  the 'app' may be a call, a simple non-flat enum literal, or an
    % annotation literal.
type_inst_of_raw_expr(Ctrl, Locn, Context, ScopeNs,
         _App @ app(Id, OldProcN, AppKind, ArgEs), NRawE, TI, !S) :-
    else_unexpected(unify(OldProcN, unset_proc_number),
        "type_inst_of_raw_expr(app): proc_number already set"),
    % Check the args first, so that any errors in them are spotted even if
    % the call itself has an error (eg. if the called function doesn't
    % exist).
    type_insts_of_exprs(Ctrl, Context, ScopeNs, ArgEs, ArgEs1,
        ActualTIs, !S),

    ( if add_scopenum_to_id(ScopeNs, !.S^sym_tbl, Id, Id2, Sym) then
        NId = Id2,
        ( if Sym = sym_operation(OpKind, ProcInfos) then
            % If it's a generator call, first check that at least one
            % func/pred/op with this name can take a single array arg.
            % This results in easier-to-understand error messages --
            % otherwise we'd get "function `foo' operand list has
            % invalid type: expected (<whatever `foo' expects>),
            % actual (array of <something>)".
            IsGenCallSuitable = (
                pred((proc_info(_,_,_,[ArgTI] - _RetTI))::in) is semidet :-
                    ArgTI = ti_array(_,_)
            ),
            ( if AppKind = gen_call_app,
                 list.all_false(IsGenCallSuitable, ProcInfos)
              then
                NArgEs = ArgEs1,
                NewProcN = unset_proc_number,
                BadGenCallExprErr = [
                    words(Sym ^ show), quote(Id ^ id_name),
                    words("cannot be used in a generator call expression"),
                    words("because it cannot be passed a single array"),
                    words("argument")
                ],
                type_inst_error(BadGenCallExprErr, Locn, TI, !S)
              else
                % Ok, passes all the tests so far... keep trying.
                What = Sym ^ show,
                % InstFormalTIs are the FormalTIs with the type-inst
                % vars all instantiated, which is done by looking at
                % the ActualTIs.  If there's a problem, InstFormalTIs
                % are unified with ActualTIs so this coercing does
                % nothing.
                type_inst_of_app(What, Locn, Id, ProcInfos, ActualTIs,
                    NActualTIs, InstFormalTIs, NewProcN, TI, IsDefd, !S),
                NArgEs = list.map_corresponding3(make_coerce_expr,
                    NActualTIs, InstFormalTIs, ArgEs1),
                % We check for definedness only after working out
                % which procedure we've called.  We allow bodyless predicates
                % in Zinc and MiniZinc, because that's how global constraints
                % are implemented natively.
                % We allow bodyless functions in Zinc (MiniZinc doesn't support
                % functions anyway) because the mapping from Zinc -> CoreZinc
                % uses them.
                % XXX we should have a post-Cadmium check that all calls
                %     bodyless predicates have been mapped away.  (It's a bit
                %     difficult to do this in a useful fashion as Cadmium
                %     (still) throws the context information away.)
                %
                % XXX: We should arguably only allow body-less predicates
                % in globals.{zinc,mzn} because they pollute the
                % namespace.  Alternatively, a module system would fix the
                % namespace pollution.
                %
                ( if ( IsDefd = defined
                     ; OpKind = predicate_operation
                     ; OpKind = function_operation, !.S ^ lang = lang_zinc
                     )
                  then
                    true
                  else
                    NotDefdErr = [
                        words(Sym ^ show), quote(Id ^ id_name),
                        words("declared but not defined")
                    ],
                    symbol_error(NotDefdErr, Locn, _, !S)
                )
            ),
            NRawE = app(NId, NewProcN, AppKind, NArgEs)

          else if Sym = sym_enum_case_name(_) then
            % Convert it to a lit_nonflat_enum_simple, then reinvoke this
            % function on it.
            RawE1 = lit_nonflat_enum_simple(NId, ArgEs1),
            type_inst_of_raw_expr(Ctrl, Locn, Context, ScopeNs, RawE1,
                NRawE, TI, !S)

          else if Sym = sym_annotation(_) then
            % Convert it to a lit_ann, then reinvoke this function on it.
            % We deliberately use 'Id' and 'NArgEs', as if the expression
            % hasn't been processed at all.  This means we do slightly
            % more work, but avoid problems with things (eg. proc_numbers)
            % being set unexpectedly.
            RawE1 = lit_ann(Id, ArgEs),
            type_inst_of_raw_expr(Ctrl, Locn, Context, ScopeNs, RawE1,
                NRawE, TI, !S)

          else
            BadCallErr = [
                words("tried to call"), words(Sym ^ show),
                quote(Id ^ id_name)
            ],
            symbol_error(BadCallErr, Locn, TI, !S),
            NRawE = app(NId, OldProcN, AppKind, ArgEs1)
        )
      else
        UndeclErr = [
            quote(Id ^ id_name), words("undeclared")
        ],
        symbol_error(UndeclErr, Locn, TI, !S),
        NRawE = app(Id, OldProcN, AppKind, ArgEs)
    ).

type_inst_of_raw_expr(Ctrl, Locn, Context, ScopeNs,
        let(LocalVars, E), let(NLocalVars, NE), TI, !S) :-
    % Let expressions introduce a new scope.
    add_new_scope(ScopeNs, NScopeNs, !S),

    % For each let variable: handle the type-expr and initialisation
    % expression (if present), then add the var to the symbol table
    % and update its scope-number.  Nb:  each variable initialisation
    % expression sees the names of the previous variables.
    DoLetVar = (
        pred(local_var(VarTIE @ ti_expr(_,VarTIEInfo), VarId, AnnEs,
                MaybeInitE)::in,
                local_var(NVarTIE, NId, NAnnEs, NMaybeInitE)::out,
                !.S::in, !:S::out) is det :-
            (
                MaybeInitE = yes(_),
                ContextPrime = [in_assigned_let_var | Context]
            ;
                MaybeInitE = no,
                ContextPrime = Context
            ),
            % Nb: we use the declared type-inst of each variable when finding
            % the type of the let expression, not the type-inst inferred from
            % the variable's expression.
            ti_expr_to_type_inst(Ctrl, ContextPrime, NScopeNs, VarTIE,
                NVarTIE, VarTI, !S),
            (
                MaybeInitE = yes(InitE),
                expr_has_type_inst(Ctrl, "initialisation value for `"
                    ++ VarId ^ id_name ++ "'", Context, NScopeNs, VarTI, InitE,
                    NInitE, !S),
                NMaybeInitE = yes(NInitE),
                IsDefd = defined
            ;
                % Symbol-checking will have complained if this variable lacks
                % an initialiser when it should have one (ie. if it's fixed
                % and used).
                 MaybeInitE = no,
                NMaybeInitE = no,
                IsDefd = undefined
            ),

            % Check annotations.
            tic_annotations(Ctrl, Context, NScopeNs, AnnEs, NAnnEs, !S),

            % Nb: When deciding if a let variable must be defined,
            % --model-check-only is irrelevant.  Thus they are different
            % to global variables.
            IsDefnReqd = is_definition_required(!.S, NVarTIE),
            ( if IsDefd = undefined, IsDefnReqd = defn_required then
                % DummySym just used to generate the error message.
                DummySym = sym_variable(VarTI, let_var, undefined,
                    defn_not_required, does_not_have_range_value),
                UnassignedErr = [
                    words(DummySym ^ show), quote(VarId ^ id_name),
                    words("must be assigned")
                ],
                symbol_error(UnassignedErr, Locn, _, !S)
              else
                true
            ),
            add_variable_sym_and_update_id(VarTIEInfo ^ expr_src_locn,
                NScopeNs, VarTI, let_var, IsDefd, IsDefnReqd, VarId, NId, !S)
    ),
    list.map_foldl(DoLetVar, LocalVars, NLocalVars, !S),

    % Handle the 'in' expression.
    type_inst_of_expr(Ctrl, Context, NScopeNs, E, NE, TI, !S).

type_inst_of_raw_expr(_, _, _, _, RawE, _, _, !S) :-
    RawE = coerce(_, _, _),
    unexpected($pred, ": " ++ RawE ^ string).

:- pred decompose_index_exprs(index_exprs::in, exprs::out, exprs::out) is det.

decompose_index_exprs(IndexExprs, Indexes, Exprs) :-
    list.foldl2(decompose_index_expr, IndexExprs, [], RevIndexes, [], RevExprs),
    list.reverse(RevIndexes, Indexes),
    list.reverse(RevExprs, Exprs).

:- pred decompose_index_expr(index_expr::in,
    exprs::in, exprs::out,
    exprs::in, exprs::out) is det.

decompose_index_expr(IndexExpr, !Indexes, !Exprs) :-
    IndexExpr = Index - Expr,
    !:Indexes = [Index | !.Indexes],
    !:Exprs = [Expr | !.Exprs].

:- pred make_index_expr_acc(expr::in, expr::in,
    index_exprs::in, index_exprs::out) is det.

make_index_expr_acc(Index, Elem, !IndexExprs) :-
    !:IndexExprs = [Index - Elem | !.IndexExprs].

%-----------------------------------------------------------------------------%

:- pred any_ti_sig_has_no_args(proc_infos::in) is semidet.

any_ti_sig_has_no_args([ProcInfo | ProcInfos]) :-
    ProcInfo = proc_info(_, _, _, ArgTIs - _RetTI),
    (
        ArgTIs = []
    ;
        ArgTIs = [_ | _],
        any_ti_sig_has_no_args(ProcInfos)
    ).

%-----------------------------------------------------------------------------%

:- func get_type_inst_of_enum_field(nfe_case_infos, zinc_name) = type_inst
    is semidet.

get_type_inst_of_enum_field([CaseInfo | CaseInfos], FieldName) =
        FieldTI :-
    CaseInfo = nfe_case_info(_CaseName, FieldInfos),
    ( if FieldTI0 = get_type_inst_of_field_2(FieldInfos, FieldName)
    then FieldTI = FieldTI0
    else FieldTI = get_type_inst_of_enum_field(CaseInfos, FieldName)
    ).

:- func get_type_inst_of_field_2(nfe_field_infos, zinc_name) = type_inst
    is semidet.

get_type_inst_of_field_2([FieldInfo | FieldInfos], Name) = TI :-
    FieldInfo = nfe_field_info(FieldName, _, FieldTI),
    ( if Name = FieldName
    then TI = FieldTI
    else TI = get_type_inst_of_field_2(FieldInfos, Name)
    ).

:- func get_type_inst_of_name(type_insts_and_names, zinc_name) = type_inst
    is semidet.

get_type_inst_of_name([TI0 - Name0 | TIsNames], Name) =
    ( if Name = Name0 then TI0 else get_type_inst_of_name(TIsNames, Name) ).

:- pred in_case_label_context(context::in) is semidet.

in_case_label_context([ContextElem | ContextElems]) :-
    ( if ContextElem = in_case_label_expr
    then true
    else in_case_label_context(ContextElems)
    ).

%-----------------------------------------------------------------------------%

    % Used for any group of expressions that should have the same type (eg.
    % sets, lists, arrays).  Checks they're all compatible, adds coercions to
    % the ones that need it and returns the lub type.
    %
:- pred type_inst_of_exprs(Ctrl::in, string::in, src_locn::in, context::in,
    scope_nums::in) : tic_X_Y(exprs, type_inst) `with_inst` tic_X_Y
    <= frontend_control(Ctrl).

type_inst_of_exprs(Ctrl, What, Locn, Context, ScopeNs, Es, NEs, TI, !S) :-
    % A little hack:  if there are zero elements, the TI is ti_par_bottom.
    % However, if there are elements, we use ti_error as the initial LubTI
    % because it's the true identify for ti_lub.  This ensures that if all
    % elements are ti_error, then LubTI=ti_error as it should.
    (
        Es = [],
        LubTI = ti_par_bottom,
        RevNEs = []
    ;
        Es = [_ | _],
        list.foldl3(get_elem_type_inst_and_lub(Ctrl, ScopeNs, Context), Es,
            [], RevNEs, ti_error, LubTI, !S)
    ),
    ( if ti_has_top(LubTI) then
        IncompatibleTIsErr = [
            words(What), words("have incompatible type-insts")
        ],
        type_inst_error(IncompatibleTIsErr, Locn, TI, !S)
    else
        TI = LubTI
    ),
    % Coerce each element to TI.
    list.foldl(add_coercion(TI), RevNEs, [], NEs).

:- pred get_elem_type_inst_and_lub(Ctrl::in, scope_nums::in,
    context::in, expr::in, exprs::in, exprs::out, type_inst::in, type_inst::out,
    tic_state::in, tic_state::out) is det <= frontend_control(Ctrl).

get_elem_type_inst_and_lub(Ctrl, ScopeNs, Context, E, !NEs, !P_LubTI, !S) :-
    % Pair an expression with its type, and also lub its type with LubT.
    type_inst_of_expr(Ctrl, Context, ScopeNs, E, NE, P_TI, !S),
    !:P_LubTI = ti_lub(!.S ^ lang, P_TI, !.P_LubTI),
    !:NEs = [NE | !.NEs].

:- pred add_coercion(type_inst::in, expr::in, exprs::in, exprs::out) is det.

add_coercion(TI, E, !CoercedEs) :-
    E = expr(_, _, EInfo),
    CoercedE = make_coerce_expr(EInfo ^ expr_type_inst, TI, E),
    !:CoercedEs = [CoercedE | !.CoercedEs].

    % Used for any group of expressions that need not have the same type (eg.
    % tuples, function call args).  Returns a list of the type-insts.
    %
:- pred type_insts_of_exprs(Ctrl::in, context::in, scope_nums::in)
    : tic_X_Y(exprs, type_insts) `with_inst` tic_X_Y
    <= frontend_control(Ctrl).

type_insts_of_exprs(Ctrl, Context, ScopeNs, Es, NEs, TIs, !S) :-
    list.map2_foldl(type_inst_of_expr(Ctrl, Context, ScopeNs), Es, NEs, TIs,
        !S).

%-----------------------------------------------------------------------------%

:- pred type_inst_of_app(string::in, src_locn::in, id::in, proc_infos::in,
    type_insts::in, type_insts::out, type_insts::out, int::out,
    type_inst::out, is_defined::out, tic_state::in, tic_state::out) is det.

type_inst_of_app(What, Locn, Id, ProcInfos, ActualTIs, NActualTIs,
        InstFormalTIs, ProcNOut, TIOut, IsDefdOut, !S) :-

    % Overloading makes things difficult -- if we have a pred/func arg, we
    % don't know which procedure it refers to.  So we generate all the
    % possibilities, and check them one by one.  Eg. if the ActualTIs are as
    % follows:
    %
    %  [ ti_overloaded_op([ 1-[i,i]-i, 2-[f,f]-f ], ti_op(1-[b]-b) ]
    %
    % then we generate these possibilities:
    %
    %  [ [ ti_op([i,i]-i, ti_op([b]-b) ]
    %  , [ ti_op([f,f]-f, ti_op([b]-b) ]
    %  ]
    %
    % and check through them until we find one that matches.  In other words,
    % we replace any 'ti_overloaded_op' type-insts with 'ti_op' type-insts.
    %
    ActualTIss = generate_all_actual_tiss(ActualTIs),
    ( if any_sig_matches_any_arglist(!.S^lang, !.S^sym_tbl, ProcInfos,
            ActualTIss, NActualTIs0, InstFormalTIs0, ProcNOut0, RetTI,
            IsDefdOut0) then
        % Matches one of the built-in's op-sigs!  Use it.
        NActualTIs         = NActualTIs0,
        InstFormalTIs      = InstFormalTIs0,
        ProcNOut           = ProcNOut0,
        TIOut              = RetTI,
        IsDefdOut          = IsDefdOut0
      else
        % No matching op-sigs.  Error.  Print the expected type-inst-sigs in
        % the error message.
        NActualTIs = ActualTIs,
        InstFormalTIs = ActualTIs,
        ProcNOut = unset_proc_number,
        IsDefdOut= defined,     % We say 'yes' to avoid cascading errors.
        What2    = What ++ " `" ++ Id ^ id_name ++ "' argument list",
        GetTISig = (func(proc_info(_, _, _, X)) = X),
        TISigs   = list.map(GetTISig, ProcInfos),
        Expected = assoc_list.keys(TISigs),   % Strip off the return type-insts.
        type_inst_mismatch_error_3(What2, Expected, ActualTIs, Locn, TIOut, !S)
    ).

%-----------------------------------------------------------------------------%

% When operation F is called:
% for each possible interpretation of ActualTIs   (any_sig_matches_any_arglist)
%   for each TISig of F                          (any_sig_matches_this_arglist)
%     if TISig matches ActualTIs, succeed       (this_sig_matches_this_arglist)
:- pred any_sig_matches_any_arglist(lang::in, symbol_table::in,
        proc_infos::in, list(type_insts)::in, type_insts::out, type_insts::out,
        int::out, type_inst::out, is_defined::out) is semidet.

any_sig_matches_any_arglist(Lang, SymTbl, ProcInfos, [ActualTIs | ActualTIss],
        NActualTIs, InstFormalTIs, ProcNOut, TIOut, IsDefdOut) :-
    ( if any_sig_matches_this_arglist(Lang, SymTbl, ProcInfos, ActualTIs,
            NActualTIs0, InstFormalTIs0, ProcNOut0, TIOut0,
            IsDefdOut0) then
        NActualTIs    = NActualTIs0,
        InstFormalTIs = InstFormalTIs0,
        ProcNOut      = ProcNOut0,
        TIOut         = TIOut0,
        IsDefdOut     = IsDefdOut0
      else
        any_sig_matches_any_arglist(Lang, SymTbl, ProcInfos, ActualTIss,
            NActualTIs, InstFormalTIs, ProcNOut, TIOut, IsDefdOut)
    ).


    % Try to match any of the TISigs with the ActualTIs.
:- pred any_sig_matches_this_arglist(lang::in, symbol_table::in,
        proc_infos::in, type_insts::in, type_insts::out, type_insts::out,
        int::out, type_inst::out, is_defined::out) is semidet.

any_sig_matches_this_arglist(Lang, SymTbl,
        [proc_info(ProcN, IsDefd, _, TISig) | Rest], ActualTIs, NActualTIs,
        InstFormalTIs, ProcNOut, TIOut, IsDefdOut) :-
    ( if this_sig_matches_this_arglist(Lang, SymTbl, TISig, ActualTIs,
            NActualTIs0, InstFormalTIs0, TIOut0) then
        NActualTIs    = NActualTIs0,
        InstFormalTIs = InstFormalTIs0,
        ProcNOut      = ProcN,
        TIOut         = TIOut0,
        IsDefdOut     = IsDefd
      else
        any_sig_matches_this_arglist(Lang, SymTbl, Rest, ActualTIs, NActualTIs,
            InstFormalTIs, ProcNOut, TIOut, IsDefdOut)
    ).


    % Try to match the ActualTIs with the FormalTIs.  FinalTIs gives
    % the actual TIs after successful matching and addition of coercions.
:- pred this_sig_matches_this_arglist(lang::in, symbol_table::in,
        type_inst_sig::in, type_insts::in, type_insts::out, type_insts::out,
        type_inst::out) is semidet.

this_sig_matches_this_arglist(Lang, SymTbl, FormalTIs - RetTI, ActualTIs,
        NActualTIs, InstFormalTIs, NRetTI) :-
    list.same_length(FormalTIs, ActualTIs),                     % semidet
    % Work out how the type-inst variables should be instantiated, then
    % instantiate them.  Any bogus ones result in an InstFormalTI of ti_top.
    % This matches anything (since we're checking ActualTI <= InstFormalTI)
    % so we check for ti_top in the instantiated args TIs, and fail if
    % they are present.  (We check for ti_top in the instantiated return value
    % higher up.)
    %
    % Nb: we rename the ActualTIs to avoid any clashes with the FormalTIs,
    % because that screws it up.  Then we undo that, so that everything that
    % follows works.
    ActualTIs2 = map(rename_ti_vars(rename), ActualTIs),
    TCons = make_tcons(geq, FormalTIs, ActualTIs2),
    ti_solve(Lang, SymTbl, TCons, [], map.init, TIVMap),
    InstFormalTIs0 = map(instantiate_type_inst_vars(/*Must*/yes, SymTbl,
        TIVMap), FormalTIs),
    InstActualTIs0 = map(instantiate_type_inst_vars(/*Must*/yes, SymTbl,
        TIVMap), ActualTIs2),
    list.all_false(ti_has_top, InstFormalTIs0 ++ InstActualTIs0),   % semidet
    all_true_two_lists(ti_leq(Lang), InstActualTIs0, InstFormalTIs0),
    RetTI2 = instantiate_type_inst_vars(/*Must*/yes, SymTbl, TIVMap, RetTI),
    NActualTIs = ActualTIs,     % We've settled on one of the ActualTIss.
    InstFormalTIs = map(rename_ti_vars(dename), InstFormalTIs0),
    NRetTI = rename_ti_vars(dename, RetTI2).

%-----------------------------------------------------------------------------%

:- func fixup_mzn_anon_array_access_index(expr) = expr.

fixup_mzn_anon_array_access_index(Expr0) = Expr :-
    ( if
            Expr0 = expr(RawExpr, Anns, ExprInfo0),
            RawExpr = anon_var,
            ExprInfo0 = expr_info(SrcLocn, TI),
            TI = ti_var_bottom
      then
            ExprInfo = expr_info(SrcLocn, ti_var_int),
            Expr = expr(RawExpr, Anns, ExprInfo)
      else
            Expr = Expr0
    ).

%-----------------------------------------------------------------------------%

:- type rename_kind
    --->    rename
    ;       dename.

:- func renaming_suffix = string.

renaming_suffix = "__renamed".

:- func rename_ti_vars(rename_kind, type_inst) = type_inst.

rename_ti_vars(K, TI) = NTI :-
    (
        ( TI = ti_par_bottom
        ; TI = ti_var_bottom
        ; TI = ti_par_bool
        ; TI = ti_var_bool
        ; TI = ti_par_int
        ; TI = ti_var_int
        ; TI = ti_par_float
        ; TI = ti_var_float
        ; TI = ti_par_string
        ; TI = ti_ann
        ; TI = ti_par_enum(_)
        ; TI = ti_var_enum(_)
        ; TI = ti_top
        ; TI = ti_error
        ),
        NTI = TI
    ;
        TI = ti_par_set(ElemTI),
        NTI = ti_par_set(rename_ti_vars(K, ElemTI))
    ;
        TI = ti_var_set(ElemTI),
        NTI = ti_var_set(rename_ti_vars(K, ElemTI))
    ;
        TI = ti_array(KTI,VTI),
        NTI = ti_array(rename_ti_vars(K, KTI), rename_ti_vars(K, VTI))
    ;
        TI = ti_tuple(TIs),
        NTI = ti_tuple(map(rename_ti_vars(K), TIs))
    ;
        TI = ti_record(TIsNames),
        assoc_list.keys_and_values(TIsNames, TIs, Names),
        NTIs = map(rename_ti_vars(K), TIs),
        NTIsNames = assoc_list.from_corresponding_lists(NTIs, Names),
        NTI = ti_record(NTIsNames)
    ;
        TI = ti_par_variable(Name, Sk),
        (
            K = rename,
            NName = Name ++ renaming_suffix
        ;
            K = dename,
            NName = string.remove_suffix_if_present(renaming_suffix, Name)
        ),
        NTI = ti_par_variable(NName, Sk)
    ;
        TI = ti_any_variable(Name, Sk),
        (
            K = rename,
            NName = Name ++ renaming_suffix
        ;
            K = dename,
            NName = string.remove_suffix_if_present(renaming_suffix, Name)
        ),
        NTI = ti_any_variable(NName, Sk)
    ;
        TI = ti_op(ArgTIs - RetTI, MaybeProcN),
        NTI = ti_op(map(rename_ti_vars(K), ArgTIs) - rename_ti_vars(K, RetTI),
                    MaybeProcN)
    ;
        ( TI = ti_unknown
        ; TI = ti_overloaded_op(_)
        ),
        NTI = ti_error
    ).

%-----------------------------------------------------------------------------%

:- func unskolemize(type_inst) = type_inst.

unskolemize(TI) = NTI :-
    (
        ( TI = ti_par_bottom
        ; TI = ti_var_bottom
        ; TI = ti_par_bool
        ; TI = ti_var_bool
        ; TI = ti_par_int
        ; TI = ti_var_int
        ; TI = ti_par_float
        ; TI = ti_var_float
        ; TI = ti_par_string
        ; TI = ti_ann
        ; TI = ti_par_enum(_)
        ; TI = ti_var_enum(_)
        ; TI = ti_top
        ; TI = ti_error
        ),
        NTI = TI
    ;
        TI = ti_par_set(ElemTI),
        NTI = ti_par_set(unskolemize(ElemTI))
    ;
        TI = ti_var_set(ElemTI),
        NTI = ti_var_set(unskolemize(ElemTI))
    ;
        TI = ti_array(KTI,VTI),
        NTI = ti_array(unskolemize(KTI), unskolemize(VTI))
    ;
        TI = ti_tuple(TIs),
        NTI = ti_tuple(map(unskolemize, TIs))
    ;
        TI = ti_record(TIsNames),
        assoc_list.keys_and_values(TIsNames, TIs, Names),
        NTIs = map(unskolemize, TIs),
        NTIsNames = assoc_list.from_corresponding_lists(NTIs, Names),
        NTI = ti_record(NTIsNames)
    ;
        TI = ti_par_variable(Name, _),
        NTI = ti_par_variable(Name, no)
    ;
        TI = ti_any_variable(Name, _),
        NTI = ti_any_variable(Name, no)
    ;
        TI = ti_op(ArgTIs - RetTI, MaybeProcN),
        NTI = ti_op(map(unskolemize, ArgTIs) - unskolemize(RetTI),
                    MaybeProcN)
    ;
        ( TI = ti_unknown
        ; TI = ti_overloaded_op(_)
        ),
        NTI = ti_error
    ).

%-----------------------------------------------------------------------------%

:- func generate_all_actual_tiss(type_insts) = list(type_insts).

generate_all_actual_tiss([]) = [[]].
generate_all_actual_tiss([ActualTI | ActualTIs]) = ActualTIss :-
    ActualTIss0 = generate_all_actual_tiss(ActualTIs),
    ( if ActualTI = ti_overloaded_op(TISigs) then
        % Split the ti_overloaded_op, which contains multiple TISigs, into
        % multiple ti_ops, each of which contains a single TISig.
        F = ( func(ProcN - TISig) = ti_op(TISig, yes(ProcN)) ),
        ActualTIs0 = map(F, TISigs),
        ActualTIss = prepend_each_T_to_each_Ts_in_Tss(ActualTIs0, ActualTIss0)
      else
        ActualTIss = prepend_T_to_each_Ts_in_Tss(ActualTI, ActualTIss0)
    ).


    % Eg: p([0,1], [[2], [4,5], [7]]) -->
    %       [[0,2,3], [0,4,5,6], [0,7], [[1,2,3], [1,4,5,6], [1,7]]
:- func prepend_each_T_to_each_Ts_in_Tss(list(T), list(list(T))) =
    list(list(T)).

prepend_each_T_to_each_Ts_in_Tss([],_) = [].
prepend_each_T_to_each_Ts_in_Tss([T0 | Ts0], Tss) =
    prepend_T_to_each_Ts_in_Tss(T0, Tss) ++
    prepend_each_T_to_each_Ts_in_Tss(Ts0, Tss).

    % Eg: p(1, [[2], [4,5], [7]]) --> [[1,2], [1,4,5], [1,7]]
:- func prepend_T_to_each_Ts_in_Tss(T, list(list(T))) = list(list(T)).

prepend_T_to_each_Ts_in_Tss(_, []) = [].
prepend_T_to_each_Ts_in_Tss(T, [Ts | Tss]) =
    [[T | Ts] | prepend_T_to_each_Ts_in_Tss(T, Tss)].

%-----------------------------------------------------------------------------%
%
% Annotations
%

:- pred tic_annotations(Ctrl::in, context::in, scope_nums::in)
    : tic_X(exprs) `with_inst` tic_X
    <= frontend_control(Ctrl).

tic_annotations(Ctrl, Context, ScopeNs, AnnEs, NAnnEs, !S) :-
    list.map_foldl(tic_annotation(Ctrl, Context, ScopeNs), AnnEs, NAnnEs, !S).

:- pred tic_annotation(Ctrl::in, context::in, scope_nums::in)
    : tic_X(expr) `with_inst` tic_X <= frontend_control(Ctrl).

tic_annotation(Ctrl, Context, ScopeNs,
        expr(RawE, AnnEs, EInfo), expr(NRawE, NAnnEs, NEInfo), !S) :-
    AnnE = expr(RawE, [], EInfo),
    expr_has_type_inst(Ctrl, "annotation", Context, ScopeNs, ti_ann, AnnE,
        NAnnE, !S),
    NAnnE = expr(NRawE, _, NEInfo),
    tic_annotations(Ctrl, Context, ScopeNs, AnnEs, NAnnEs, !S).

%-----------------------------------------------------------------------------%
%
% Type-inst-variable resolution
%

    % Type-inst constraints -- either 'TI1 == TI2' or 'TI >= TI2'.
    %
:- type tcons == list(tcon).
:- type tcon
    --->    tcon(tcmp, type_inst, type_inst).

:- type tcmp
    --->    eq
    ;       geq.

    % A map from a type-inst var name (eg. "$T") to a single type-inst.  Note
    % that this ignores the par/any-ness that may precede any type-inst vars
    % -- that is handled by the checking that goes on elsewhere.
    %
:- type ti_var_map == map(zinc_name, type_inst).

:- func make_tcons(tcmp, type_insts, type_insts) = tcons.

make_tcons(Cmp, XTIs, YTIs) = TCons :-
    F = ( func(XTI, YTI) = tcon(Cmp, XTI, YTI) ),
    TCons = list.map_corresponding(F, XTIs, YTIs).

    % This solves a series of type-inst constraints (in 'Cons'), each of which
    % has one of the following two forms:
    %
    %   TI1 >= TI2          % TI1 is a supertype of TI2 -- coercion allowed.
    %   TI1 == TI2          % TI1 must equal TI2 -- no coercion allowed.
    %
    % If the TIs are compound, they get broken down into simpler constraints,
    % eg.:
    %
    %   array[int] of any $T >= array[int] of float
    % -->
    %   int == int              % equality, because indices must be equal.
    %   any $T >= float
    %
    % If we have equalities with a variable on the LHS, eg:
    %
    %   any $T == int
    %
    % we add $T->int to the solution (stored in !TIVMap) and substitute int for
    % $T everywhere.
    %
    % If we have inequalities with a variable on the LHS, eg:
    %
    %   any $T >= float
    %
    % then we save that (in 'Cons2') for processing at the end.  Also, if the
    % LHS is 'par $T' we upcast it to 'any $T'.  This is because by this point
    % we've checked that the RHS was par, was fixed, and it avoids problems
    % later on (because $T could be bound to a non-par type-inst).
    %
    % When we have nothing but these constraints left, we collect all the ones
    % for each variable, eg:
    %
    %   any $U >= var int
    %   any $U >= par float
    %
    % Then we lub all the RHSs to find the assignment for the variable (we get
    % 'any $U == var float' in this case);  that can then be handled normally,
    % and we return temporarily to solving 'Cons' again.  Once 'Cons' and
    % 'Cons2' are both empty, we've finished.
    %
:- pred ti_solve(lang::in, symbol_table::in, tcons::in, tcons::in,
    ti_var_map::in, ti_var_map::out) is semidet.

ti_solve(_Lang, _SymTbl, [], [], !TIVMap).

ti_solve(Lang, SymTbl, [], [Con2 @ tcon(Cmp, XTI, YTI) | Cons2], !TIVMap) :-
    % We only have Cons2 'any $T >= ti' constraints left, process the first
    % one.
    else_unexpected(unify(Cmp, geq), $pred ++ ": non-geq cmp"),
    ( if XTI = ti_par_variable(_, _) then
        % These should be converted to ti_any_variable by now.
        unexpected($pred, ": ti_par_variable")

      else if XTI = ti_any_variable(VarX, IsSkolemized) then
        else_unexpected(unify(IsSkolemized, no), $pred ++ ": unskolemized"),
        % An 'X >= TI' constraint.  Search for 'VarX >= TI2' in the rest of
        % Cons2.  If any such ones are present, remove them and replace them
        % with 'VarX = lub([TI, TI2, ...])'.  Otherwise, assume 'VarX = TI'.
        get_tis_for_ti_var(VarX, Cons2, Cons2b, YTIs),
        (   YTIs = [],
            post_eq(SymTbl, VarX, XTI, YTI, [], NCons, Cons2, NCons2, !TIVMap)
        ;
            YTIs = [_|_],
            YLubTI = foldl(ti_lub(Lang), [YTI | YTIs], ti_par_bottom),
            post_eq(SymTbl, VarX, XTI, YLubTI, [], NCons, Cons2b, NCons2,
                !TIVMap),
            NCons2 = Cons2b
        )

      else
        % Not a 'X >= TI' constraint -- put it back into Cons for normal
        % processing.
        NCons = [Con2],
        NCons2 = Cons2
    ),
    ti_solve(Lang, SymTbl, NCons, NCons2, !TIVMap).

ti_solve(Lang, SymTbl, [tcon(Cmp, XTI, YTI) | Cons], Cons2, !TIVMap) :-
    % We have normal constraints left, process the first one.
    (   (   XTI = ti_par_variable(VarX, IsSkolemized),
            ti_is_fixed(SymTbl, YTI),
            IsPar = yes
        ;
            XTI = ti_any_variable(VarX, IsSkolemized),
            IsPar = no
        ),
        (   % If it's skolemized, we treat it like a constant (same as the
            % par_int/var_int/par_bool/etc cases below).
            IsSkolemized = yes,
            ( Cmp = eq,  ti_eq(       YTI, XTI)
            ; Cmp = geq, ti_leq(Lang, YTI, XTI)
            ),
            NCons = Cons,
            NCons2 = Cons2
        ;
            % If it's not skolemized, we treat it like a variable.
            IsSkolemized = no,
            ( if XTI = YTI then
                % Ignore X==X identities.
                NCons = Cons,
                NCons2 = Cons2
              else
                (   Cmp = eq,     % VarX = YTI
                    ( if IsPar = no then
                        post_eq(SymTbl, VarX, XTI, YTI, Cons, NCons, Cons2,
                            NCons2, !TIVMap)
                      else
                        % Upcast, and treat as a >= constraint.  Eg. if we have
                        % 'par $T == par int', it becomes 'any $T >= par int'.
                        NCons = Cons,
                        NCons2 =
                            [tcon(geq, ti_any_variable(VarX, no), YTI) | Cons2]
                    )
                ;
                    % Add the 'VarX >= TI' constraint to !Cons2 -- we'll deal
                    % with it at the end.  While doing so, we upcast it to
                    % 'any', as mentioned above.
                    Cmp = geq,    % VarX >= YTI
                    NCons = Cons,
                    NCons2 = [tcon(geq, ti_any_variable(VarX, no), YTI) | Cons2]
                )
            )
        )
    ;
        ( XTI = ti_par_bottom
        ; XTI = ti_var_bottom
        ; XTI = ti_par_bool
        ; XTI = ti_var_bool
        ; XTI = ti_par_int
        ; XTI = ti_var_int
        ; XTI = ti_par_float
        ; XTI = ti_var_float
        ; XTI = ti_par_string
        ; XTI = ti_ann
        ; XTI = ti_par_enum(_)
        ; XTI = ti_var_enum(_)
          % This can happen if a func/pred has an argument with a bogus
          % type, eg. a unary tuple.
        ; XTI = ti_error
        ),
        ( Cmp = eq,  ti_eq(       YTI, XTI)
        ; Cmp = geq, ti_leq(Lang, YTI, XTI)
        ),
        NCons = Cons,
        NCons2 = Cons2
    ;
        XTI = ti_par_set(XElemTI),
        ( if YTI = ti_par_set(YElemTI) then
            NCons = [tcon(Cmp, XElemTI, YElemTI) | Cons]
          else if ( YTI = ti_par_bottom ; YTI = ti_error ) then
            NCons = [tcon(Cmp, XElemTI, YTI) | Cons]
          else
            fail
        ),
        NCons2 = Cons2
    ;
        XTI = ti_var_set(XElemTI),
        ( if ( YTI = ti_par_set(YElemTI), Cmp = geq
             ; YTI = ti_var_set(YElemTI)
             ) then
            NCons = [tcon(Cmp, XElemTI, YElemTI) | Cons]
          else if ( YTI = ti_par_bottom ; YTI = ti_var_bottom ;
                    YTI = ti_error ) then
            % Once we move inside the var_set, the element must be a par.  So
            % we parify YTI.  Eg. if we have XTI==var_set_of_$T,
            % YTI==var_bottom, when we unify the element type we want to
            % unify XTI==par_$T, YTI==par_bottom.
            NCons = [tcon(Cmp, XElemTI, parify_type_inst(SymTbl, YTI)) | Cons]
          else
            fail
        ),
        NCons2 = Cons2
    ;
        % Nb: It's possible that the XIndexTI (eg. par float) and
        % YIndexTI (eg. par int) will unify successfully even though an
        % array[int] is not acceptable when expecting an array[float].
        % Fortunately, this will be caught later on.
        XTI = ti_array(XIndexTI, XElemTI),
        ( if ( YTI = ti_par_set(YElemTI), YIndexTI = ti_par_int, Cmp = geq
             ; YTI = ti_array(YIndexTI, YElemTI)
             ) then
            NCons = [tcon(eq, XIndexTI, YIndexTI),
                     tcon(Cmp, XElemTI, YElemTI) | Cons]
          else if ( YTI = ti_par_bottom ; YTI = ti_error ) then
            NCons = [tcon(eq, ti_par_bottom, YTI),
                     tcon(Cmp, XElemTI, YTI) | Cons]
          else
            fail
        ),
        NCons2 = Cons2
    ;
        XTI = ti_tuple(XTupTIs),
        ( if YTI = ti_tuple(YTupTIs) then
            list.same_length(XTupTIs, YTupTIs),     % semidet
            NCons = make_tcons(Cmp, XTupTIs, YTupTIs) ++ Cons
          else if ( YTI = ti_par_bottom ; YTI = ti_error ) then
            NCons = make_tcons(Cmp, XTupTIs,
                               list.duplicate(length(XTupTIs), YTI)) ++ Cons
          else
            fail
        ),
        NCons2 = Cons2
    ;
        XTI = ti_record(XRecTIsNames),
        ( if ( YTI = ti_tuple(YRecTIs), Cmp = geq
             ; YTI = ti_record(YRecTIsNames),
               YRecTIs = assoc_list.keys(YRecTIsNames)
             ),
             list.same_length(XRecTIsNames, YRecTIs)
          then
            XRecTIs = assoc_list.keys(XRecTIsNames),
            NCons = make_tcons(Cmp, XRecTIs, YRecTIs) ++ Cons
          else if ( YTI = ti_par_bottom ; YTI = ti_error ) then
            XRecTIs = assoc_list.keys(XRecTIsNames),
            NCons = make_tcons(Cmp, XRecTIs,
                        list.duplicate(length(XRecTIsNames), YTI)) ++ Cons
          else
            fail
        ),
        NCons2 = Cons2
    ;
        XTI = ti_op(XArgTIs - XRetTI, _),
        ( if YTI = ti_op(YArgTIs - YRetTI, _) then
            list.same_length(XArgTIs, YArgTIs),
            NCons = [tcon(eq, XRetTI, YRetTI) |
                     make_tcons(eq, XArgTIs, YArgTIs)] ++ Cons
          else
            fail
        ),
        NCons2 = Cons2
    ;
        % These ones should never be encountered here.
        ( XTI = ti_top
        ; XTI = ti_overloaded_op(_)
        ; XTI = ti_unknown
        ),
        unexpected($pred, ": " ++ XTI^string)
    ),
    ti_solve(Lang, SymTbl, NCons, NCons2, !TIVMap).


:- pred get_tis_for_ti_var(zinc_name::in, tcons::in, tcons::out,
    type_insts::out) is det.

get_tis_for_ti_var(_VarX, [], [], []).
get_tis_for_ti_var(VarX, [Con2 @ tcon(_Cmp, XTI, YTI) | Cons2], NCons2, YTIs) :-
    get_tis_for_ti_var(VarX, Cons2, Cons2b, YTIs0),
    ( if ( XTI = ti_par_variable(VarX, Sk)
         ; XTI = ti_any_variable(VarX, Sk)
         ) then
        else_unexpected(unify(Sk, no), $pred ++ ": skolemize ti-var"),
        NCons2 = Cons2b,
        YTIs = [YTI | YTIs0]
      else
        NCons2 = [Con2 | Cons2b],
        YTIs = YTIs0
    ).


:- pred post_eq(symbol_table::in, zinc_name::in, type_inst::in, type_inst::in,
    tcons::in, tcons::out, tcons::in, tcons::out,
    ti_var_map::in, ti_var_map::out) is det.

post_eq(SymTbl, VarX, XTI, YTI, Cons, NCons, Cons2, NCons2, !TIVMap) :-
    ( if YTI = XTI then
        NCons = Cons,
        NCons2 = Cons2
      else
        % 0. Create a function that does VarX=YTI substitution.
        map.det_insert(VarX, YTI, map.init, TmpTIVMap),
        F = (
            func(TI) =
                instantiate_type_inst_vars(/*MustBePresent*/no, SymTbl,
                    TmpTIVMap, TI)
        ),

        % 1. Do VarX := YTI in every RHS in the TIVMap.  (Using a temporary
        %    TIVMap containing just VarX=YTI for the job.)
        F2 = (
            func(_,TI) = F(TI)
        ),
        !:TIVMap:ti_var_map = map.map_values(F2, !.TIVMap),

        % 2. Do VarX := YTI in remaining constraints.
        F3 = (
            func(tcon(F_Cmp, F_TI1, F_TI2)) = tcon(F_Cmp, F(F_TI1), F(F_TI2))
        ),
        NCons = map(F3, Cons),
        NCons2 = map(F3, Cons2),

        % XXX: no occurs check!  Ok, since we don't allow recursive
        % types?

        % 3. Bind VarX := YTI in the TIVMap;  shouldn't already be
        % present.
        map.det_insert(VarX, YTI, !TIVMap)
    ).

%-----------------------------------------------------------------------------%

:- func instantiate_type_inst_vars(bool, symbol_table, ti_var_map, type_inst)
    = type_inst.

instantiate_type_inst_vars(Must, SymTbl, TIVMap, TI) = NTI :-
    (
        ( TI = ti_par_bottom
        ; TI = ti_var_bottom
        ; TI = ti_par_bool
        ; TI = ti_var_bool
        ; TI = ti_par_int
        ; TI = ti_var_int
        ; TI = ti_par_float
        ; TI = ti_var_float
        ; TI = ti_par_string
        ; TI = ti_ann
        ; TI = ti_par_enum(_)
        ; TI = ti_var_enum(_)
          % This can happen if a func/pred has an argument with a bogus type,
          % eg. a unary tuple.
        ; TI = ti_error
        ),
        NTI = TI
    ;
        TI = ti_par_set(ElemTI),
        NElemTI = instantiate_type_inst_vars(Must, SymTbl, TIVMap, ElemTI),
        NTI = ti_par_set(NElemTI)
    ;
        TI = ti_var_set(ElemTI),
        NElemTI = instantiate_type_inst_vars(Must, SymTbl, TIVMap, ElemTI),
        NTI = ti_var_set(NElemTI)
    ;
        TI = ti_array(IndexTI, ElemTI),
        NIndexTI = instantiate_type_inst_vars(Must, SymTbl, TIVMap, IndexTI),
        NElemTI  = instantiate_type_inst_vars(Must, SymTbl, TIVMap, ElemTI),
        NTI = ti_array(NIndexTI, NElemTI)
    ;
        TI = ti_tuple(TIs),
        NTIs = map(instantiate_type_inst_vars(Must, SymTbl, TIVMap), TIs),
        NTI = ti_tuple(NTIs)
    ;
        TI = ti_record(TIsNames),
        TIs = assoc_list.keys(TIsNames),
        NTIs = map(instantiate_type_inst_vars(Must, SymTbl, TIVMap), TIs),
        F = (func(F_TI, _-Name) = F_TI-Name),
        NTIsNames = map_corresponding(F, NTIs, TIsNames),
        NTI = ti_record(NTIsNames)
    ;
        TI = ti_par_variable(Tv, IsSkolemized),
        (   IsSkolemized = yes,
            NTI = TI
        ;
            IsSkolemized = no,
            % If the typevar is not in the map, then the unification must have
            % failed.  Eg. because a "set of $T" formal arg was given an
            % integer actual arg.  So we just use ti_top and it propagates
            % upwards until caught at the right place.
            % (Exception:  if Must=no, we just leave the typevar unchanged.)
            ( if   TI0 = map.search(TIVMap, Tv)
              then NTI = parify_type_inst(SymTbl, TI0)
              else
                (   Must = yes, NTI = ti_top
                ;   Must = no,  NTI = TI
                )
            )
        )
    ;
        TI = ti_any_variable(Tv, IsSkolemized),
        (
            IsSkolemized = yes,
            NTI = TI
        ;
            IsSkolemized = no,
            ( if   TI0 = map.search(TIVMap, Tv)
              then NTI = TI0
              else
                ( Must = yes, NTI = ti_top
                ; Must = no,  NTI = TI
                )
            )
        )
    ;
        TI = ti_op(ArgTIs - RetTI, MaybeProcN),
        NArgTIs = map(instantiate_type_inst_vars(Must, SymTbl, TIVMap), ArgTIs),
        NRetTI  = instantiate_type_inst_vars(Must, SymTbl, TIVMap, RetTI),
        NTI = ti_op(NArgTIs - NRetTI, MaybeProcN)
    ;
        % These ones should never be encountered here.
        TI = ti_top,
        NTI = ti_top
    ;
        ( TI = ti_overloaded_op(_)
        ; TI = ti_unknown
        ),
        NTI = ti_error
    ).

%-----------------------------------------------------------------------------%
%
% Adding coercions.
%

    % We could do a lot more compile-time coercions if we made the effort
    % (eg. coercing literal set expressions to literal list expressions).
    %
    % Nb: have to be careful to get the type annotations right.
:- func make_coerce_expr(type_inst, type_inst, expr) = expr.

make_coerce_expr(TI1, TI2, E @ expr(RawE, AnnEs, EInfo)) =
        expr(NRawE, NAnnEs, NEInfo):-
    % E should be annotated with TI1, with one exception (involving operations
    % and overloading).
    ( if TI1 = EInfo ^ expr_type_inst then
        true
    else if TI1 = ti_op(_,_), EInfo^expr_type_inst = ti_overloaded_op(_) then
        true
    else if ti_has_error(EInfo ^ expr_type_inst) then
        true
    else
        unexpected($pred, ": E's annotation doesn't match T1: " ++
            TI1 ^ string ++ " vs. " ++ EInfo^expr_type_inst^string)
    ),

    % Coercion obviously not necessary if TIs are the same, and for simplicity
    % we don't bother if either TI has an error, because we won't be compiling
    % beyond this stage anyway.  Also, we don't need (or want) to coerce
    % operations, even though they may look like the types don't match.
    ( if
        ( TI1 = TI2
        ; ti_has_error(TI1)
        ; ti_has_error(TI2)
        )
    then
        NRawE = RawE,
        NAnnEs = AnnEs,
        NTI2 = TI2
    else if TI1 = ti_op(_, yes(ProcN1)), TI2 = ti_op(TISig2, no) then
        % Weird situation:  we've got a TISig like (int,int)->int in both TI1
        % and TI2, but TI1 has the procedure number also, whereas TI2 doesn't.
        % So we extract ProcN1 and use it with TISig2.
        NRawE = RawE,
        NAnnEs = AnnEs,
        NTI2 = ti_op(TISig2, yes(ProcN1))
    else
        NRawE = make_coerce_raw_expr(TI1, TI2, E),
        % If we wrapped E in a 'coerce' then its annotation are present within
        % the coercion.  If not, we add them to the new (implicitly coerced)
        % expression.
        NAnnEs = ( if NRawE = coerce(_,_,_) then [] else AnnEs ),
        NTI2 = TI2
    ),
    % Set the type-inst annotation in the resulting expression.
    % (If TI1 = TI2 this is a no-op.)
    NEInfo = EInfo ^ expr_type_inst := NTI2.

:- func make_coerce_raw_expr(type_inst, type_inst, expr) = raw_expr.

make_coerce_raw_expr(TI1, TI2, E) = NRawE :-
    RawE = E ^ raw_expr,
    (
        RawE = ident(_Id),
        NRawE = coerce(TI1, TI2, E)
    ;
        % Note that anonymous variables are left uncoerced.
        %
        % Nb: the '_' may have been coerced before, so TI1 isn't
        % necessarily ti_var_bottom at this point.
        RawE  = anon_var,
        NRawE = anon_var
    ;
        RawE = lit(Lit),
        NRawE = make_coerce_lit(TI1, TI2, Lit, E)
    ;
        % Note that empty set literals are left uncoerced -- the coercion is
        % pushed inside and disappears.
        RawE = lit_set(ElemEs),
        NRawE = make_coerce_lit_set(TI1, TI2, E, ElemEs)
    ;
        % Note that empty array literals are left uncoerced -- the coercion is
        % pushed inside and disappears.
        RawE = lit_simple_array(ElemEs),
        NRawE = make_coerce_lit_simple_array(TI1, TI2, ElemEs)
    ;
        % Note that empty array literals are left uncoerced -- the coercion is
        % pushed inside and disappears.
        RawE = lit_indexed_array(IndexedEs),
        NRawE = make_coerce_lit_indexed_array(TI1, TI2, IndexedEs)
    ;
        RawE = lit_tuple(Es),
        NRawE = make_coerce_lit_tuple(TI1, TI2, Es)
    ;
        RawE = lit_record(NamesEs),
        NRawE = make_coerce_lit_record(TI1, TI2, NamesEs)
    ;
        RawE = comprehension(CompKind, Gens, MaybeWhereE),
        NRawE = make_coerce_comprehension(TI1, TI2, E, CompKind, Gens,
            MaybeWhereE)
    ;
        RawE = array_access(ArrayE, IndexEs),
        NRawE = make_coerce_array_access(TI1, TI2, ArrayE, IndexEs, E)
    ;
        ( RawE = tuple_access(_,_)
        ; RawE = record_access(_,_)
        ; RawE = if_then_else(_,_,_)
        ; RawE = let(_,_)
        ; RawE = case(_,_)
        ),
        NRawE = coerce(TI1, TI2, E)
    ;
        RawE = app(AppId, AppProcNum, AppKind, AppArgs),
        NRawE = make_coerce_app(TI1, TI2, AppId, AppProcNum, AppKind, AppArgs,
            E)
    ;
        % The exprs in the lit should have already been coerced.  So no need
        % to do anything here.
        RawE = lit_nonflat_enum(_CaseId, _NamesEs),
        ( if
            TI1 = ti_par_enum(EnumName),
            TI2 = ti_par_enum(EnumName)
        then
            % lit enum -> lit enum
            NRawE = RawE
        else
            unexpected($pred, "problem with lit_nonflat_enum")
        )
    ;
        % coerce(Y, Z, coerce(X, Y, E)) --> coerce(X, Z, E)
        RawE = coerce(C_TI1, _C_TI2, C_E),
        NRawE = coerce(C_TI1, TI2, C_E)
    ;
        ( RawE = lit_nonflat_enum_simple(_,_)
        ; RawE = lit_ann(_,_)
        ),
        unexpected($pred, RawE^string)
    ).

:- func make_coerce_lit(type_inst, type_inst, literal, expr) = raw_expr.

make_coerce_lit(TI1, TI2, Lit, E) = NRawE :-
    ( if
        Lit = int(I),
        TI1 = ti_par_int,
        TI2 = ti_par_float
    then
        % lit int -> lit float
        NRawE = lit(floatstr(I^string ++ ".0"))
    else
        NRawE = coerce(TI1, TI2, E)
    ).

:- func make_coerce_lit_set(type_inst, type_inst, expr, exprs) = raw_expr.

make_coerce_lit_set(TI1, TI2, E, ElemEs) = NRawE :-
    ( if TI1 = ti_par_set(ElemTI1) then
        ( if TI2 = ti_par_set(ElemTI2) then
            % lit par_set -> lit par_set
            NElemEs = map(make_coerce_expr(ElemTI1, ElemTI2), ElemEs),
            NRawE = lit_set(NElemEs)
          else if TI2 = ti_var_set(_) then
            % lit par_set -> lit var_set;  do as normal
            NRawE = coerce(TI1, TI2, E)
          else if TI2 = ti_array(ti_par_int, ElemTI2) then
            % lit set -> lit simple array
            % Nb: we can coerce the elements, but we can't change it to a
            % lit_simple_array, because that could change the ordering.
            % Eg. {3,2,1} would become [3,2,1], but really the coerced
            % result should be [1,2,3].  And we can't determine the correct
            % ordering until run-time because it requires expression
            % evaluation.
            NElemEs = list.map(make_coerce_expr(ElemTI1, ElemTI2), ElemEs),
            NRawE = coerce(ti_par_set(ElemTI2),
                           ti_array(ti_par_int, ElemTI2),
                           E^raw_expr := lit_set(NElemEs))
          else
            unexpected($pred, ": lit_set, TI2 = ", TI2^string)
        )
      else
        unexpected($pred, ": lit_set, TI1")
    ).

:- func make_coerce_lit_simple_array(type_inst, type_inst, exprs) = raw_expr.

make_coerce_lit_simple_array(TI1, TI2, ElemEs) = NRawE :-
    % Nb: The index types will be the same unless one of them is ti_error.
    ( if TI1 = ti_array(_, ElemTI1) then
        ( if TI2 = ti_array(_, ElemTI2) then
            % lit simple array -> lit simple array
            % First coerce the elements, if necessary.
            NElemEs = map(make_coerce_expr(ElemTI1, ElemTI2), ElemEs),
            NRawE = lit_simple_array(NElemEs)
        else
            unexpected($pred, ": lit_simple_array, TI2")
        )
    else
        unexpected($pred, ": lit_simple_array, TI1")
    ).

:- func make_coerce_lit_indexed_array(type_inst, type_inst, index_exprs)
    = raw_expr.

make_coerce_lit_indexed_array(TI1, TI2, IndexedEs) = NRawE :-
    ( if
        TI1 = ti_array(_, ElemTI1),
        TI2 = ti_array(_, ElemTI2)
    then
        % lit array -> lit array
        % First coerce the elements, if necessary.
        F = (func(IndexE - ValueE) =
            IndexE - make_coerce_expr(ElemTI1, ElemTI2, ValueE)
        ),
        NIndexedEs = list.map(F, IndexedEs),
        NRawE = lit_indexed_array(NIndexedEs)
      else
        unexpected($pred, ": lit_indexed_array")
    ).

:- func make_coerce_lit_tuple(type_inst, type_inst, exprs) = raw_expr.

make_coerce_lit_tuple(TI1, TI2, Es) = NRawE :-
    ( if TI1 = ti_tuple(TIs1) then
        ( if TI2 = ti_tuple(TIs2) then
            % lit tuple -> lit tuple
            NEs = map_corresponding3(make_coerce_expr, TIs1, TIs2, Es),
            NRawE = lit_tuple(NEs)
        else if TI2 = ti_record(TIsNames2) then
            % lit tuple -> lit record
            F = (
                func(F_TI1, F_TI2 - F_Name, F_E) =
                    F_Name - make_coerce_expr(F_TI1, F_TI2, F_E)
            ),
            NNamesEs = list.map_corresponding3(F, TIs1, TIsNames2, Es),
            NRawE = lit_record(NNamesEs)
        else
            unexpected($pred, ": lit_tuple, TI2")
        )
    else
        unexpected($pred, ": lit_tuple, TI1")
    ).

:- func make_coerce_lit_record(type_inst, type_inst, named_exprs) = raw_expr.

make_coerce_lit_record(TI1, TI2, NamesEs) = NRawE :-
    ( if TI1 = ti_record(TIsNames1), TI2 = ti_record(TIsNames2) then
        % lit record -> lit record
        F = (
            func(F_TI1 - _, F_TI2 - _, F_Name:zinc_name - F_E) =
                F_Name - make_coerce_expr(F_TI1, F_TI2, F_E)
        ),
        NNamesEs = list.map_corresponding3(F, TIsNames1, TIsNames2, NamesEs),
        NRawE = lit_record(NNamesEs)
    else
        unexpected($pred, ": lit_record")
    ).

:- func make_coerce_comprehension(type_inst, type_inst,
    expr, comprehension_kind, generators, maybe(expr)) = raw_expr.

make_coerce_comprehension(TI1, TI2, E, CompKind, Gens, MaybeWhereE) = NRawE :-
    (
        CompKind = set_comp(HeadE),
        ( if TI1 = ti_par_set(ElemTI1) then
            ( if TI2 = ti_par_set(ElemTI2) then
                % par set comp -> par set comp
                NHeadE = make_coerce_expr(ElemTI1, ElemTI2, HeadE),
                NCompKind = set_comp(NHeadE),
                NRawE = comprehension(NCompKind, Gens, MaybeWhereE)
            else if TI2 = ti_var_set(_) then
                % par set comp -> var set comp;  do as normal
                NRawE = coerce(TI1, TI2, E)
            else if TI2 = ti_array(ti_par_int, ElemTI2) then
                % par set comp -> simple array comp
                % Nb: we can coerce the elements, but we can't change it to
                % a simple_array_comp, because that could change the
                % ordering.  Eg. {i | i in [3,2,1]} would become
                % [i | i in [3,2,1]], which evaluates to [3,2,1], but
                % really the coerced result should be [1,2,3].  And we
                % can't determine the correct ordering until run-time
                % because it requires expression evaluation.
                NHeadE = make_coerce_expr(ElemTI1, ElemTI2, HeadE),
                NCompKind = set_comp(NHeadE),
                NRawE = coerce(ti_par_set(ElemTI2),
                               ti_array(ti_par_int, ElemTI2),
                               E^raw_expr := comprehension(NCompKind, Gens,
                                                           MaybeWhereE))
            else
                unexpected($pred,
                    ": set comprehension, TI2 = " ++ TI2^string)
            )
        else
            unexpected($pred, ": set comprehension, TI1")
        )
    ;
        CompKind = simple_array_comp(HeadE),
        ( if
            TI1 = ti_array(_, ElemTI1),
            TI2 = ti_array(_, ElemTI2)
        then
            % simple array comp -> simple array comp
            NHeadE = make_coerce_expr(ElemTI1, ElemTI2, HeadE),
            NCompKind = simple_array_comp(NHeadE),
            NRawE = comprehension(NCompKind, Gens, MaybeWhereE)
        else
            unexpected($pred, ": simple array comprehension")
        )
    ;
        CompKind = indexed_array_comp(IndexE - ElemE),
        ( if
            TI1 = ti_array(_, ElemTI1),
            TI2 = ti_array(_, ElemTI2)
        then
            % indexed array comp -> indexed array comp
            NElemE = make_coerce_expr(ElemTI1, ElemTI2, ElemE),
            NCompKind = indexed_array_comp(IndexE - NElemE),
            NRawE = comprehension(NCompKind, Gens, MaybeWhereE)
        else
            unexpected($pred, ": array comprehension")
        )
    ).

:- func make_coerce_array_access(type_inst, type_inst, expr, exprs, expr) =
    raw_expr.        

make_coerce_array_access(TI1, TI2, ArrayE, IndexEs, E) = NRawE :-
    % Push the coercion into an array access if the array expression is a
    % simple array literal or an array1d cast around a simple array
    % literal.
    ArrayTI = ArrayE ^ expr_info ^ expr_type_inst,
    ArrayRawE = ArrayE ^ raw_expr,
    ( if
        ArrayTI = ti_array(IndexTI, _),
        % NOTE: we can only do this if we can be sure that the
        % coercion can be pushed into the array expression.
        (
            ArrayRawE = lit_simple_array(_)
        ;
            ArrayRawE = app(id_global("array1d"), 1, _, [_, Array1dArg2E]),
            Array1dArg2E ^ raw_expr = lit_simple_array(_)
        )
    then
        NArrayE = make_coerce_expr(ti_array(IndexTI, TI1),
            ti_array(IndexTI, TI2), ArrayE),
        NRawE = array_access(NArrayE, IndexEs)
    else
        NRawE = coerce(TI1, TI2, E)
    ).

:- func make_coerce_app(type_inst, type_inst, id, int, app_kind, exprs,
    expr) = raw_expr.

make_coerce_app(TI1, TI2, AppId, AppProcNum, AppKind, AppArgs, E) = NRawE :-
    ( if
        AppId ^ id_name = "array1d", AppProcNum = 1,
        AppArgs = [IndexE, ArrayLitE],
        ( IndexE ^ raw_expr = lit_set([])
        ; ArrayLitE ^ raw_expr = lit_simple_array(_)
        )
    then
        % Push coercions into the first argument of an array1d cast
        % if it is the empty set.  We need do do this to ensure that
        % the index set has the correct element type.  (This is because
        % empty set literals are initially assigned the element type-inst
        % par_bottom.)
        ( if 
            TI1 = ti_array(ti_par_bottom, ElemTI1),
            TI2 = ti_array(IndexTI, _),
            IndexE ^ raw_expr = lit_set([])
        then
            NIndexE = make_coerce_expr(ti_par_set(ti_par_bottom),
                ti_par_set(IndexTI), IndexE),
            NTI1 = ti_array(IndexTI, ElemTI1)
        else
            NIndexE = IndexE,
            NTI1 = TI1
        ),

        % Push coercions into the second argument of an array1d cast
        % if it is a simple array literal.
        % NOTE: pushing the coercions into the first argument may
        % have changed its type: we _must_ use the updated type
        % here.
        ( if
            ArrayLitE ^ raw_expr = lit_simple_array(_),
            NTI1 = ti_array(ti_par_int, _),
            TI2 = ti_array(ti_par_int, _)
        then
            NArrayLitE = make_coerce_expr(NTI1, TI2, ArrayLitE)
        else
            NArrayLitE = ArrayLitE
        ),
        NAppArgs = [NIndexE, NArrayLitE],
        NRawE = app(AppId, AppProcNum, AppKind, NAppArgs)
    else if
        is_arrayNd_cast(AppId ^ id_name, CastArity),
        list.length(AppArgs, NumArgs),
        NumArgs = CastArity + 1,
        list.det_split_last(AppArgs, IndexArgs, ArrayLitE)
    then
        % Push coercions into the second argmement of an arrayNd
        % cast if it is a simple array literal.
        ( if
            ArrayLitE ^ raw_expr = lit_simple_array(_),
            ArrayLitTI = ArrayLitE ^ expr_info ^ expr_type_inst,
            ArrayLitTI = ti_array(ArrayLitIndexTI, _),
            TI1 = ti_array(IndexTI, _),
            TI2 = ti_array(IndexTI, ElemTI)
        then
            NArrayLitE = make_coerce_expr(ArrayLitTI,
                ti_array(ArrayLitIndexTI, ElemTI), ArrayLitE)
        else
            NArrayLitE = ArrayLitE
        ),
        NAppArgs = IndexArgs ++ [NArrayLitE],
        NRawE = app(AppId, AppProcNum, AppKind, NAppArgs)
    else 
        NRawE = coerce(TI1, TI2, E)
    ).

:- pred is_arrayNd_cast(string::in, int::out) is semidet.

is_arrayNd_cast("array2d", 2).
is_arrayNd_cast("array3d", 3).
is_arrayNd_cast("array4d", 4).
is_arrayNd_cast("array5d", 5).
is_arrayNd_cast("array6d", 6).

%-----------------------------------------------------------------------------%
%
% Type/type-inst conversions
%

    % Varification.  This is the same as lubbing the TI with ti_var_bottom
    % except for records and tuples.
    %
:- func varify_type_inst(symbol_table, type_inst) = type_inst.

varify_type_inst(_, ti_par_bottom ) = ti_var_bottom.
varify_type_inst(_, ti_var_bottom ) = ti_var_bottom.
varify_type_inst(_, ti_par_bool   ) = ti_var_bool.
varify_type_inst(_, ti_var_bool   ) = ti_var_bool.
varify_type_inst(_, ti_par_int    ) = ti_var_int.
varify_type_inst(_, ti_var_int    ) = ti_var_int.
varify_type_inst(_, ti_par_float  ) = ti_var_float.
varify_type_inst(_, ti_var_float  ) = ti_var_float.
varify_type_inst(_, ti_par_string ) = ti_top.
varify_type_inst(_, ti_ann        ) = ti_top.
varify_type_inst(_, ti_par_set(TI)) = ti_var_set(TI).
varify_type_inst(_, ti_var_set(TI)) = ti_var_set(TI).
varify_type_inst(_, ti_array(_,_) ) = ti_top.
varify_type_inst(SymTbl, ti_tuple(TIs)) =
    ti_tuple(map(varify_type_inst(SymTbl), TIs)).
varify_type_inst(SymTbl, ti_record(TIsNames)) = RecTI :-
    F = ( func(P_TI - P_Name) = varify_type_inst(SymTbl, P_TI) - P_Name ),
    RecTI = ti_record(map(F, TIsNames)).
    % Enums can only be varified if they are flat.
varify_type_inst(SymTbl, ti_par_enum(Name) ) = TI :-
    Sym = find_existing_global_symbol(Name, SymTbl),
    % We can varify flat enums, but not non-flat ones.
    % If the enum has no definition then it is a flat enum, since
    % non-flat enums cannot appear in data files.
    TI = ( if
                Sym = sym_enum(EnumInfo),
                ( EnumInfo = enum_info_flat(_)
                ; EnumInfo = enum_info_undefined
                )
           then
            ti_var_enum(Name)
           else
            ti_top
         ).
    % If it's already 'var', then it must be flat, so varifying is ok.
varify_type_inst(_, ti_var_enum(Name)   ) = ti_var_enum(Name).
varify_type_inst(_, ti_par_variable(_,_)) = ti_top.
varify_type_inst(_, ti_any_variable(_,_)) = ti_top.
varify_type_inst(_, ti_top              ) = ti_top.
varify_type_inst(_, ti_op(_,_)          ) = _ :-
    unexpected($pred, ": ti_op").
varify_type_inst(_, ti_overloaded_op(_) ) = _ :-
    unexpected($pred, ": ti_overloaded_op").
varify_type_inst(_, ti_error            ) = ti_error.
varify_type_inst(_, ti_unknown          ) = ti_unknown.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
%
% Operator/function/predicate type-inst signature checking, esp. overloadings
%

    % This checks the type signatures for a single operation symbol (ie. one
    % possibly-overloaded operator/function/predicate symbol, such as '+').
    %
    % CHECK 0 [not done yet] [overloading]
    % -------
    % User-defined functions must not overload existing built-ins.  Because
    % they're treated differently -- no cloning is done -- so this wouldn't
    % work.
    % [no longer true, but maybe still shouldn't allow it?]
    %
    % CHECK 1 [not done yet] [overloading]
    % -------
    % The rules for checking overloadings basically disallow ambiguity:
    % - if the type-inst vars match exactly in all overloadings (modulo
    %   renaming) that's ok
    % - if in some overloadings we have type-inst vars and in others we
    %   don't, the type-inst vars must not possibly match the non-type-inst
    %   vars.
    %
    % I'm not sure of the exact algorithm, it's some kind of unification,
    % probably a bit like the one in 'ti_solve', but you have to be careful
    % with the type-inst vars.
    % Examples:
    %   [(T, T), (int, string)]    T == [int, string] --> top       ok
    %   [(T, T), (U, U)]           T == [U]                         ok
    %   [(T, T), (U, T)]           T == [U, T]      multiple TIvars! bad
    %   [(T, T), (U, int)]         T == [U, int]    multiple with TIvars! bad
    %   [(int, string), (T, T)]    [int, string] == T               ok
    %   [(T, U), (int, string)]    T == [int], U == [string] --> non-top! bad
    %   [(T, int), (string, U)]    T = [string], U = [int]   --> non-top! bad
    %
    % Nb: any type-inst var in the return type should also be named in the
    % arguments (eg. (T,T)->T is ok, (T,T)->U is not ok).  But we don't have
    % to explicitly check for this because you can't write a function like
    % this.  (Hmm, if we don't check we could just have a declaration like
    % "function $U: f($T: x);"... but that's fairly harmless.)
    %
    % CHECK 2  [not done yet] [overloading]
    % -------
    % Overloadings must be closed under glb (aka type conjunction).  eg.
    %   [(int,float)->float, (float,int)->float]
    % is not acceptable as the glb is (int,int)->float, which is not one of
    % the overloadings.
    %
    % CHECK 3  [not done yet] [overloading]
    % -------
    % They should be semantically equivalent w.r.t. coercions, eg
    %   int_to_float(x + y) = int_to_float(x) + int_to_float(y).
    % If we allow the user to specify the multiple bodies separately, it's
    % up to them to ensure this.
    %
    % If we made all versions of a pred/func have the same body, eg.
    % (imaginary syntax):
    %   function plus(int, int) -> int
    %            plus(float, float) -> float
    %       plus(x, y) = x + y;
    % It would be ok.  That's how modes are done with Mercury.
    %
    % CHECK 4  [not done yet] [overloading]
    % -------
    % They must be monotonic.  ie. you can't have:
    %   [(int, int) -> float, (float, float) -> int]
    %
:- pred check_ti_expr_sig_pair(zinc_name::in, src_locn::in, ti_expr_sig::in,
    ti_expr_sig::in, tic_state::in, tic_state::out) is det.

check_ti_expr_sig_pair(_Name, _Locn, _TIESig1 @ (ArgTIEs1 - _),
        _TIESig2 @ (ArgTIEs2 - _), !S) :-
    ( if list.same_length(ArgTIEs1, ArgTIEs2) then
        % Need to unify ArgTIs1 with ArgTIs2 and check they're not in conflict
        unimplemented("check_ti_expr_sig_pair")
      else
        % Different numbers of arguments -- no possibility of ambiguity.
        true
    ).

%-----------------------------------------------------------------------------%
%
% Global variable assignment post checking
%

:- pred check_definitions(id::in, symbol::in, src_locn::in,
    tic_state::in, tic_state::out) is det.

check_definitions(Id, Sym, Locn, !S) :-
    % For global variables and enums, if they were undefined it might be due to
    % a missing data file.
    PossibleSuffix = " (did you forget to specify a data file?)",
    (
        Sym = sym_variable(TI, VarKind, IsDefd, IsDefnReqd, _),
        ( if
            % This occurs for implicitly indexed arrays with var elements
            % and also for annotation variables.
            % XXX the error message is not very accurate, for the former,
            % since only as much of the defintion that defines the index
            % set is required; for the latter we should mention that
            % it is an annotation variable that is involved.
            IsDefnReqd = defn_required,
            IsDefd = undefined,
            not ti_is_fixed(!.S ^ sym_tbl, TI)
          then
            IsBadSym = yes("")
          else if
             !.S^checking = instance_checking,
             IsDefnReqd = defn_required,
             IsDefd = undefined
          then
            Suffix = ( if VarKind = global_var then PossibleSuffix else "" ),
            IsBadSym = yes(Suffix)
          else
            IsBadSym = no
        )
    ;
        Sym = sym_type_inst_synonym(TypeSynInfo),
        (
            TypeSynInfo = type_syn_info_defined(_, _, _, _),
            IsBadSym = no
        ;
            TypeSynInfo = type_syn_info_undefined,
            IsBadSym = yes("")
        )
    ;
        Sym = sym_enum(EnumInfo),
        ( if   !.S^checking = instance_checking, EnumInfo = enum_info_undefined
          then IsBadSym = yes(PossibleSuffix)
          else IsBadSym = no
        )
    ;
        % For each of these, either it's ok for it to be declared but not
        % defined, or it's always defined if present in the symbol table.
        ( Sym = sym_operation(_,_)
        ; Sym = sym_annotation(_)
        ; Sym = sym_enum_case_name(_)
        ),
        IsBadSym = no
    ),
    (
        IsBadSym = no
    ;
        IsBadSym = yes(Suffix2),
        UndefErr = [
            words(Sym ^ show), quote(Id ^ id_name),
            words("must be defined" ++ Suffix2)
        ],
        symbol_error(UndefErr, Locn, _, !S)
    ).

%-----------------------------------------------------------------------------%
%
% Symbol table operations
%

:- pred add_global_symbol(src_locn::in, symbol::in, zinc_name::in,
    tic_state::in, tic_state::out) is det.

add_global_symbol(Locn, Sym, Name, !S) :-
    add_symbol(Locn, Sym, id_global(Name), !S).

:- pred add_symbol(src_locn::in, symbol::in, id::in,
    tic_state::in, tic_state::out) is det.

add_symbol(NewLocn, NewSym, Id, !S) :-
    ( if NewSym = sym_operation(_, _) then
        unexpected($pred, ": should use 'add_operation_symbol' instead")
      else
        true
    ),
    ( if is_operator(!.S ^ lang, Id ^ id_name) then
        % The grammar should prevent this from happening.
        unexpected($pred, ": non-operation symbol with an operator name")
      else
        true
    ),
    ( if maybe_find_symbol(Id, !.S ^ sym_tbl, OldSym, OldLocn) then
        ( if
            OldSym = sym_enum(OldMaybeT),
            NewSym = sym_enum(NewMaybeT)
          then
            OldIsDefd = enum_is_defined(OldMaybeT),
            NewIsDefd = enum_is_defined(NewMaybeT),
            (
                OldIsDefd = no,  NewIsDefd = no
            ;
                OldIsDefd = no,  NewIsDefd = yes,
                replace_existing_symbol(Id, NewSym, NewLocn, !.S ^ sym_tbl,
                    Temp),
                !S ^ sym_tbl := Temp
            ;
                OldIsDefd = yes, NewIsDefd = no
            ;
                OldIsDefd = yes, NewIsDefd = yes,
                DupEnumErr = [
                    words(NewSym ^ show), quote(Id ^ id_name),
                    words("defined elsewhere, at"), src_locn(OldLocn)
                ],
                symbol_error(DupEnumErr, NewLocn, _, !S)
            )
          else
            (
                % We handle the built-in case separately since OldSym
                % will not have a source location.
                OldLocn = builtin,
                AlreadyDeclErr = [
                    words(NewSym ^ show), quote(Id ^ id_name),
                    words("shadows a built-in"),
                    words(OldSym ^ show)
                ]
            ;
                ( OldLocn = src_locn(_, _)
                ; OldLocn = unknown
                ),
                AlreadyDeclErr = [
                    words(NewSym ^ show), quote(Id ^ id_name),
                    words("declared and/or defined as a"),
                    words(OldSym ^ show),
                    words("elsewhere, at"),
                    src_locn(OldLocn)
                ]
            ),
            symbol_error(AlreadyDeclErr, NewLocn, _, !S)
        )
      else
        add_unseen_symbol(Id, NewSym, NewLocn, !.S ^ sym_tbl, Temp),
        !S ^ sym_tbl := Temp
    ).

:- func enum_is_defined(enum_info) = bool.

enum_is_defined(enum_info_undefined) = no.
enum_is_defined(enum_info_flat(_)) = yes.
enum_is_defined(enum_info_nonflat(_, _)) = yes.

:- pred add_operation_symbol(src_locn::in, zinc_name::in, operation_kind::in,
    is_defined::in, proc_is_annotated::in, ti_expr_sig::in, int::out,
    tic_state::in, tic_state::out) is det.

add_operation_symbol(NewLocn, Name, NewKind, NewIsDefd, NewIsAnnotated,
        NewTIESig, ProcNOut, !S) :-
    Id = id_global(Name),
    NewTIESig = ArgTIEs - RetTIE,
    GetTI = (func(TIE) = TIE ^ ti_expr_info ^ expr_type_inst),
    NewTISig = map(GetTI, ArgTIEs) - GetTI(RetTIE),
    ( if maybe_find_symbol(Id, !.S ^ sym_tbl, OldSym, OldLocn) then
        ( if OldSym = sym_operation(OldKind, OldInfos),
             % Nb: we don't allow overloading of operators, but we do
             % allow a symbol to be both a function and a predicate.
             ( OldKind = NewKind
             ; OldKind = predicate_operation, NewKind = function_operation
             ; NewKind = predicate_operation, OldKind = function_operation
             )
          then
            Lang = !.S ^ lang,
            (
                ( Lang = lang_zinc
                ; Lang = lang_minizinc
                ),
                ( if ti_sig_seen_before(NewIsDefd, NewTISig, OldInfos) then
                    % We mock up a dummy Sym just to pass to '^show' --
                    % only the NewKind is important.
                    NewSym = sym_operation(NewKind, []),
                    DupTISigErr = [
                        words(NewSym ^ show), quote(Id ^ id_name),
                        words("declared and/or defined with the same"),
                        words("type-inst signature elsewhere, at"),
                        src_locn(OldLocn)
                    ],
                    symbol_error(DupTISigErr, NewLocn, _, !S),
                    ProcNOut = unset_proc_number
                  else
                    % XXX [overloading]  Should check the overloading is ok
                    % with this (or something like it):
                    %   foldl(check_type_inst_sig_pair(Name,Locn,TISig),
                    %       OldTISigs,!S),
                    %
                    % Add the NewTISig, re-sort the list.
                    ProcNOut = list.length(OldInfos) + 1,
                    CmpSigs = (
                        pred(proc_info(_,_,_,TISig1)::in,
                             proc_info(_,_,_,TISig2)::in, Cmp::out) is det :-
                            ti_sig_compare(!.S^lang, TISig1, TISig2, Cmp)
                    ),
                    NewProcInfo = proc_info(ProcNOut, NewIsDefd,
                        NewIsAnnotated, NewTISig),
                    NewUnsortedInfos = OldInfos ++ [NewProcInfo],
                    list.sort(CmpSigs, NewUnsortedInfos, NewSortedInfos),
                    NewSym = sym_operation(NewKind, NewSortedInfos),
                    replace_existing_symbol(Id, NewSym, NewLocn, !.S ^ sym_tbl,
                        Temp),
                    !S ^ sym_tbl := Temp
                )
            ;
                % FlatZinc doesn't allow any form of overloading.
                Lang = lang_flatzinc,
                OverloadedPredErr = [
                    words("the predicate"), quote(Id ^ id_name),
                    words("is already declared at"), src_locn(OldLocn)
                ],
                symbol_error(OverloadedPredErr, NewLocn, _, !S),
                ProcNOut = unset_proc_number
            )
          else
            % We mock up a dummy Sym just to pass to '^show' -- only the
            % NewKind is important.
            NewSym = sym_operation(NewKind, []),
            AlreadyDeclMsg = [
                words(NewSym ^ show), quote(Id ^ id_name),
                words("declared as a"), words(OldSym ^ show),
                words("elsewhere, at"), src_locn(OldLocn)
            ],
            symbol_error(AlreadyDeclMsg, NewLocn, _, !S),
            ProcNOut = unset_proc_number
        )
      else
        ProcNOut = 1,  % The first procedure for a symbol has ProcN=1.
        NewProcInfo = proc_info(ProcNOut, NewIsDefd, NewIsAnnotated, NewTISig),
        NewSym = sym_operation(NewKind, [NewProcInfo]),
        add_unseen_symbol(Id, NewSym, NewLocn, !.S^sym_tbl, Temp),
        !S ^ sym_tbl := Temp
    ).


    % We search for NewTISig in the first ProcInfos.
    % If we find it, we succeed.
    %
:- pred ti_sig_seen_before(is_defined::in, type_inst_sig::in, proc_infos::in)
    is semidet.

ti_sig_seen_before(NewIsDefd, NewTISig, [ProcInfo | ProcInfos]) :-
    ProcInfo = proc_info(_, _, _, TISig),
    ( if type_inst_sig_eq(TISig, NewTISig) then
        true
      else
        ti_sig_seen_before(NewIsDefd, NewTISig, ProcInfos)
    ).

%-----------------------------------------------------------------------------%

:- pred add_variable_sym_and_update_id(src_locn::in, list(int)::in,
    type_inst::in, variable_kind::in, is_defined::in, is_defn_required::in,
    id::in, id::out, tic_state::in, tic_state::out) is det.

add_variable_sym_and_update_id(Locn, NScopeNs, VarTI, VarKind, IsDefd,
        IsDefnReqd, Id, NId, !S) :-
    Name = Id ^ id_name,
    (
        NScopeNs = [],
        NId = id_global(Name)
    ;
        NScopeNs = [NewScopeN | _],
        NId = id_scoped(Name, NewScopeN)
    ),
    ( if VarTI = ti_par_set(_) then
        HasRangeValue = has_range_value
      else
        HasRangeValue = does_not_have_range_value
    ),
    add_symbol(Locn, sym_variable(VarTI, VarKind, IsDefd, IsDefnReqd,
            HasRangeValue), NId, !S).

%-----------------------------------------------------------------------------%

    % Given a stack of scope numbers, we look through them to find the
    % innermost scope that declares the given name;  if there is one, we
    % update the scope number in the id.  If the stack of scope numbers
    % is empty then we check if the global scope declares the given name.
    %
    % Fail if the name is not declared in any of these scopes.
    %
:- pred add_scopenum_to_id(list(int)::in, symbol_table::in, id::in,
    id::out, symbol::out) is semidet.

add_scopenum_to_id(ScopeNs0, SymTbl, Id, NId, Sym) :-
    Name = Id ^ id_name,
    (
        ScopeNs0 = [ScopeN | ScopeNs],
        SearchId = id_scoped(Name, ScopeN),
        ( if    maybe_find_symbol(SearchId, SymTbl, Sym0, _)
          then  NId = SearchId, Sym = Sym0
          else  add_scopenum_to_id(ScopeNs, SymTbl, Id, NId, Sym)
        )
    ;
        % The base case is to check whether the name is declared in the
        % global scope.
        ScopeNs0 = [],
        SearchId = id_global(Name),
        ( if    maybe_find_symbol(SearchId, SymTbl, Sym0, _)
          then  NId = SearchId, Sym = Sym0
          else  false
        )
    ).

%-----------------------------------------------------------------------------%

:- pred add_new_scope(list(int)::in, list(int)::out,
    tic_state::in, tic_state::out) is det.

add_new_scope(ScopeNs, [NewScopeN | ScopeNs], !S) :-
    get_new_scope_number(NewScopeN, !.S^sym_tbl, Temp),
    !S ^ sym_tbl := Temp.

%-----------------------------------------------------------------------------%
%
% Errors and warnings
%

:- pred type_inst_error(error_msg::in, src_locn::in, type_inst::out,
    tic_state::in, tic_state::out) is det.

type_inst_error(ErrMsg0, Locn, ti_error, !S) :-
    Errors0 = !.S ^ errors,
    ErrMsg = [words("type-inst error:") | ErrMsg0],
    error_at_locn(ErrMsg, Locn, Errors0, Errors),
    !S ^ errors := Errors.

%-----------------------------------------------------------------------------%

:- pred type_inst_mismatch_error(string::in, type_insts::in, type_inst::in,
    src_locn::in, type_inst::out, tic_state::in, tic_state::out) is det.

type_inst_mismatch_error(WhatStr, ExpectedTIs, ActualTI, Locn, TI, !S) :-
    % We only report a mismatch error in the case where the actual TI is
    % not already erroneous.  Doing otherwise can result in a series
    % of errors concerning the *use* of a value whereas the error that is
    % important is the one concerning the *definition* of a value.
    ( if ti_has_error(ActualTI) then
        TI = ti_error
      else
        type_inst_mismatch_error_wrk(WhatStr,
            type_insts_to_error_msg(ExpectedTIs),
            [type_inst(ActualTI)], Locn, TI, !S)
    ).

    % Like type_inst_mismatch_error, but the "expected" thing is a string, not
    % a list(type_inst).
:- pred type_inst_mismatch_error_2(string::in, string::in, type_inst::in,
    src_locn::in, type_inst::out, tic_state::in, tic_state::out) is det.

type_inst_mismatch_error_2(WhatStr, ExpectedStr, ActualTI, Locn, TI, !S) :-
    type_inst_mismatch_error_wrk(WhatStr,
        [words(ExpectedStr)],
        [type_inst(ActualTI)], Locn, TI, !S).


    % Another variant on type_inst_mismatch_error.
:- pred type_inst_mismatch_error_3(string::in, list(type_insts)::in,
    type_insts::in, src_locn::in, type_inst::out,
    tic_state::in, tic_state::out) is det.

type_inst_mismatch_error_3(WhatStr, ExpectedTISigs, ActualTISig, Locn, TI,
        !S) :-
    type_inst_mismatch_error_wrk(WhatStr,
        type_inst_sigs_to_error_msg(ExpectedTISigs),
        [type_inst_sig_args(ActualTISig)], Locn, TI, !S).


:- pred type_inst_mismatch_error_wrk(string::in, error_msg::in, error_msg::in,
    src_locn::in, type_inst::out, tic_state::in, tic_state::out) is det.

type_inst_mismatch_error_wrk(WhatStr, ExpectedErrMsg0, ActualErrMsg, Locn, TI,
        !S) :-
    list.det_split_last(ExpectedErrMsg0, FirstParts, LastPart),
    ExpectedErrMsg = FirstParts ++ [suffix(LastPart, ",")],
    ErrMsg =
        [ words(WhatStr)
        , words("has invalid type-inst: expected")
        ] ++
        ExpectedErrMsg ++
        [ words("actual")
        ] ++
        ActualErrMsg,
    type_inst_error(ErrMsg, Locn, TI, !S).

:- func type_insts_to_error_msg(type_insts) = error_msg.

type_insts_to_error_msg([]) = [].
type_insts_to_error_msg([TI]) = [type_inst(TI)].
type_insts_to_error_msg([TI1, TI2 | TIs]) =
    [type_inst(TI1), words("or") | type_insts_to_error_msg([TI2 | TIs])].

:- func type_inst_sigs_to_error_msg(list(type_insts)) = error_msg.

type_inst_sigs_to_error_msg([]) = [].
type_inst_sigs_to_error_msg([TISig]) = [type_inst_sig_args(TISig)].
type_inst_sigs_to_error_msg([TISig1, TISig2 | TISigs]) =
    [ type_inst_sig_args(TISig1), words("or")
    | type_inst_sigs_to_error_msg([TISig2 | TISigs])
    ].

%-----------------------------------------------------------------------------%

:- pred type_inst_access_error(string::in, type_inst::in, src_locn::in,
    type_inst::out, tic_state::in, tic_state::out) is det.

type_inst_access_error(What, AccessTI, Locn, TI, !S) :-
    % Avoid error cascades -- don't report an error if the accessed thing
    % contains an error.
    ( if AccessTI = ti_error then
        TI = ti_error
      else
        ErrMsg = [
            words(What),
            words("access attempted on an expression of type-inst"),
            type_inst(AccessTI)
        ],
        type_inst_error(ErrMsg, Locn, TI, !S)
    ).

%-----------------------------------------------------------------------------%

:- pred symbol_error(error_msg::in, src_locn::in, type_inst::out,
    tic_state::in, tic_state::out) is det.

symbol_error(ErrMsg, Locn, ti_error, !S) :-
    Errors0 = !.S ^ errors,
    error_at_locn([words("symbol error:") | ErrMsg], Locn, Errors0, Errors),
    !S ^ errors := Errors.

:- pred symbol_warning(error_msg::in, src_locn::in,
    tic_state::in, tic_state::out) is det.

symbol_warning(ErrMsg, Locn, !S) :-
    Warnings0 = !.S ^ warnings,
    error_at_locn([words("symbol warning:") | ErrMsg], Locn,
        Warnings0, Warnings),
    !S ^ warnings := Warnings.

%-----------------------------------------------------------------------------%

:- pred not_in_lang(list(lang)::in, string::in, src_locn::in,
    tic_state::in, tic_state::out) is det.

not_in_lang([], _, _, !S).
not_in_lang([Lang | Langs], What, Locn, !S) :-
    ( if !.S^lang = Lang then
        Msg = [
            words(Lang ^ show),
            words("does not permit"),
            words(What)
        ],
        type_inst_error(Msg, Locn, _, !S)
      else
        true
    ),
    not_in_lang(Langs, What, Locn, !S).

%-----------------------------------------------------------------------------%

:- func maybe_id_name(expr) = maybe(zinc_name).

maybe_id_name(Expr) = MaybeName :-
    Expr = expr(RawExpr, _, _),
    ( if RawExpr = ident(Id) then
        MaybeName = yes(Id ^ id_name)
      else
        MaybeName = no
    ).

%-----------------------------------------------------------------------------%
:- end_module type_inst_check.
%-----------------------------------------------------------------------------%
