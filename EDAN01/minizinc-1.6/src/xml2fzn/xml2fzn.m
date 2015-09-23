%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2009-2010 The University of Melbourne and NICTA.
% See the file COPYING for license information.
%-----------------------------------------------------------------------------%
%
% Authors: Sam Abousaid <s.abousaid@ugrad.unimelb.edu.au>
%          Julien Fischer <juliensf@csse.unimelb.edu.au>
%
% Convert XML-FlatZinc into regular FlatZinc.
% 
%-----------------------------------------------------------------------------%

:- module xml2fzn.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parsing.
:- import_module xml.
:- import_module xml.cat.
:- import_module xml.doc.
:- import_module xml.encoding.
:- import_module xml.parse.

:- import_module xml2fzn_config.

:- import_module array.
:- import_module bool.
:- import_module char.
:- import_module exception.
:- import_module getopt_io.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
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
        ( if univ_to_type(Excp, IO_Error) then
            handle_io_error(IO_Error, !IO)
          else if univ_to_type(Excp, ConversionError) then
            handle_conversion_error(ConversionError, !IO)
          else if univ_to_type(Excp, Xml2FznError) then
            handle_xml2fzn_error(Xml2FznError, !IO)
          else
            rethrow(MainResult)
        )
    ).

:- pred main_2(unit::out, io::di, io::uo) is det.

main_2(unit, !IO) :-
    io.command_line_arguments(Args, !IO),
    OptionOps = option_ops_multi(
        xml2fzn_short_option,
        xml2fzn_long_option,
        (pred(O::out, D::out) is multi :- xml2fzn_option_defaults(O, D))
    ),
    process_options(OptionOps, Args, NonOptionArgs, Result, !IO),
    (
        Result = error(Msg),
        bad_cmdline(Msg, !IO)
    ;
        Result = ok(OptionTable),
        ( if lookup_bool_option(OptionTable, version, yes) then
            io.write_string(xml2fzn_version, !IO)

          else if lookup_bool_option(OptionTable, help, yes) then
            io.write_string(xml2fzn_usage, !IO)

          else
            (
                NonOptionArgs = [],
                bad_cmdline("no file specified", !IO)
            ;
                NonOptionArgs = [File],
                process_file(OptionTable, File, !IO)
            ;
                NonOptionArgs = [_, _ | _],
                bad_cmdline("more than one file specified", !IO)
            )
        )
    ).

%-----------------------------------------------------------------------------%

:- pred process_file(option_table(xml2fzn_option)::in, string::in,
    io::di, io::uo) is det.

process_file(OptionTable, FileName, !IO) :-
    io.see(FileName, OpenResult, !IO),
    (
        OpenResult = ok,
        io.read_file_as_string(ReadResult, !IO),
        (
            ReadResult = error(_PartialRes, ReadError),
            handle_io_error(ReadError, !IO)
        ;
            ReadResult = ok(Text),
            parsing.pstate(mkEntity(Text), mkEncoding(utf8), init, !IO),
            parsing.io((pred(Dirs0::out, !.IO::di, !:IO::uo) is det :-
                get_environment_var("XML_DIRS", MStr, !IO),
                (
                    MStr = no,
                    Str = "."
                ;
                    MStr = yes(Str)
                ),
                split((':'), Str, Dirs0)
            ), Dirs, !IO),
	        parsing.set(gDirs, dirs(Dirs), !IO),
            Cat = catalog(map.init),
	        parsing.set(gCatalog, Cat, !IO),
	        map.from_assoc_list([
		        pair.("ASCII"		- mkEncoding(ascii7)),
		        pair.("ascii"		- mkEncoding(ascii7)),
                pair.("Latin-1"	- mkEncoding(latin1)),
                pair.("Latin1"	- mkEncoding(latin1)),
                pair.("UTF-8"		- mkEncoding(utf8)),
                pair.("utf-8"		- mkEncoding(utf8))
	        ], Encodings),
	        parsing.set(gEncodings, encodings(Encodings), !IO),
	        xml.parse.document(!IO),
            parsing.get(gContent, ContentStore, !IO),
	        parsing.finish(Res, !IO),
            (
                Res = ok((_DTD, Doc)),
                lookup_string_option(OptionTable, output_to_file, OutputFile),
                ( if    OutputFile = ""
                  then
                        xml_to_flatzinc(ContentStore, Doc, !IO),
                        io.nl(!IO)
                  else  
                        io.tell(OutputFile, TellResult, !IO),
                        (
                            TellResult = ok, 
                            xml_to_flatzinc(ContentStore, Doc, !IO),
                            io.told(!IO)
                        ;
                            TellResult = error(TellError),
                            handle_io_error(TellError, !IO)
                        )
                )
            ;
                Res = error(Err),
                io.stderr_stream(StdErr, !IO),
	    	    io.format(StdErr, "%s: %s\n", [s(FileName), s(Err)], !IO),
                io.set_exit_status(1, !IO)
            )
        )
    ;
        OpenResult = error(OpenError),
        handle_io_error(OpenError, !IO)
    ).

%-----------------------------------------------------------------------------%

% From extras/xml/tryit.m.

:- pred split(char, string, list(string)).
:- mode split(in, in, out) is det.

split(C, Str0, Strs) :-
    string__to_char_list(Str0, Chars),
    split1(C, [], Strs0, Chars, _),
    reverse(Strs0, Strs).

:- pred split1(char, list(string), list(string), list(char), list(char)).
:- mode split1(in, in, out, in, out) is det.

split1(_C, Strs, Strs, [], []).
split1(C, Strs0, Strs) -->
	=([_|_]),
	split2(C, [], Cs0),
	{ reverse(Cs0, Cs) },
	( { Cs \= [] } ->
	    { string__from_char_list(Cs, Str) },
	    { Strs1 = [Str|Strs0] }
	;
	    { Strs1 = Strs0 }
	),
	split1(C, Strs1, Strs).

:- pred split2(char, list(char), list(char), list(char), list(char)).
:- mode split2(in, in, out, in, out) is det.

split2(_C, Cs, Cs, [], []).
split2(C, Cs0, Cs) -->
    [C0],
    ( { C = C0 } ->
    	{ Cs = Cs0 }
    ;
    	split2(C, [C0|Cs0], Cs)
    ).

%-----------------------------------------------------------------------------%
%
% Top-level
%

:- pred xml_to_flatzinc(contentStore::in, document::in,
    io::di, io::uo) is det.

xml_to_flatzinc(CS, Document, !IO) :-
    Children = Document ^ content,
    array.foldl(output_toplevel_content(CS), Children, !IO).

:- pred output_toplevel_content(contentStore::in, content::in,
    io::di, io::uo) is det.

output_toplevel_content(CS, Content, !IO) :-
    (
        Content = element(Element),
        output_toplevel_element(CS, Element, !IO)
    ;
        ( Content = pi(_, _)
        ; Content = comment(_)
        ; Content = data(_)
        )
    ).

:- pred output_toplevel_element(contentStore::in, element::in,
    io::di, io::uo) is det.

output_toplevel_element(CS, Element, !IO) :-
    Element = element(EName, Attrs, ContentRef),
    Children = gather_child_elements(CS, ContentRef),
    ( if EName = "predicate" then
        output_predicate_item(CS, Attrs, Children, !IO)
      else if EName = "parameter" then
        output_parameter_item(CS, Attrs, Children, !IO)
      else if EName = "variable" then
        output_variable_item(CS, Attrs, Children, !IO)
      else if EName = "constraint" then
        output_constraint_item(CS, Attrs, Children, !IO)
      else if EName = "solve" then
        output_solve_item(CS, Attrs, Children, !IO)
      else
        % Skip over anything that is not one of the top-level model
        % elements.
        true
    ).

%----------------------------------------------------------------------------%
%
% Predicate items
%

:- pred output_predicate_item(contentStore::in, list(attribute)::in,
    list(element)::in, io::di, io::uo) is det.

output_predicate_item(CS, Attrs, Elems, !IO) :-
    Name = lookup_reqd_name_attribute("predicate", Attrs, "name"),
    io.format("predicate %s(", [s(Name)], !IO),
    ArgNo = 1,
    output_predicate_args(CS, yes, ArgNo, Elems, !IO),
    io.write_string(");\n", !IO).

:- pred output_predicate_args(contentStore::in, bool::in, int::in,
    list(element)::in, io::di, io::uo) is det.

output_predicate_args(_, _, _, [], !IO).
output_predicate_args(CS, IsFirst, ArgNo, [E | Es], !IO) :-
    E = element(EName, Attrs, Content),
    ( if is_pred_arg_type(EName, Type) then
        FznType = type_to_fzn_type(CS, EName, Type, Attrs, Content),
        (
            IsFirst = yes
        ;
            IsFirst = no,
            io.write_string(", ", !IO)
        ),
        % XML-FlatZinc does not preserve the predicate argument names,
        % so we just call them x1, ... xN.  (It doesn't matter since
        % FlatZinc doesn't ever use the names.)
        io.format("%s: x%d", [s(FznType), i(ArgNo)], !IO),
        output_predicate_args(CS, no, ArgNo + 1, Es, !IO)
      else
        Msg = "element <" ++ EName ++
            "> is not a valid predicate argument type",
        throw_conversion_error(Msg)
    ).

:- pred is_pred_arg_type(string::in, fzn_type::out) is semidet.

is_pred_arg_type(TagName, Type) :-
    ( if is_var_type(TagName, Type0) then
        Type = Type0
      else if is_par_type(TagName, Type0) then
        Type = Type0 
      else if is_pred_arg_only_type(TagName, Type0) then
        Type = Type0
      else
        false
    ).

:- pred is_pred_arg_only_type(string::in, fzn_type::out) is semidet.

is_pred_arg_only_type(TagName, Type) :-
    (
        TagName = "int-range",
        Type = type_scalar(par, stype_int_range)
    ;
        TagName = "int-set",
        Type = type_scalar(par, stype_int_set)
    ; 
        TagName = "float-range",
        Type = type_scalar(par, stype_float_range)
    ;
        TagName = "set-of-int-range",   
        Type = type_scalar(par, stype_set_of_int_range)
    ;
        TagName = "set-of-int-set",   
        Type = type_scalar(par, stype_set_of_int_set)
    ;  
        TagName = "array-int-range",
        Type = type_array(index_explicit, par, stype_int_range)
    ;  
        TagName = "array-int-set",
        Type = type_array(index_explicit, par, stype_int_set)
    ;
        TagName = "array-float-range",
        Type = type_array(index_explicit, par, stype_float_range)
    ;  
        TagName = "array-set-of-int-range",
        Type = type_array(index_explicit, par, stype_set_of_int_range)
    ;  
        TagName = "array-set-of-int-set",
        Type = type_array(index_explicit, par, stype_set_of_int_set)
    ;
        TagName = "implicit-array-bool",
        Type = type_array(index_implicit, par, stype_bool)
    ;
        TagName = "implicit-array-int",
        Type = type_array(index_implicit, par, stype_int)
    ;
        TagName = "implicit-array-int-range",
        Type = type_array(index_implicit, par, stype_int_range)
    ;  
        TagName = "implicit-array-int-set",
        Type = type_array(index_implicit, par, stype_int_set)
    ;
        TagName = "implicit-array-float",
        Type = type_array(index_implicit, par, stype_float)
    ;
        TagName = "implicit-array-float-range",
        Type = type_array(index_implicit, par, stype_float_range)
    ; 
        TagName = "implicit-array-set-of-int",
        Type = type_array(index_implicit, par, stype_set_of_int)
    ; 
        TagName = "implicit-array-set-of-int-range",
        Type = type_array(index_implicit, par, stype_set_of_int_range)
    ;  
        TagName = "implicit-array-set-of-int-set",
        Type = type_array(index_implicit, par, stype_set_of_int_set)
    ;
        TagName = "var-set-of-int",
        Type = type_scalar(var, stype_set_of_int)
    ;
        TagName = "array-var-set-of-int",
        Type = type_array(index_explicit, var, stype_set_of_int)
    ;
        TagName = "implicit-array-var-bool",
        Type = type_array(index_implicit, var, stype_bool)
    ;
        TagName = "implicit-array-var-int",
        Type = type_array(index_implicit, var, stype_int)
    ;
        TagName = "implicit-array-var-int-range",
        Type = type_array(index_implicit, var, stype_int_range)
    ;  
        TagName = "implicit-array-var-int-set",
        Type = type_array(index_implicit, var, stype_int_set)
    ;
        TagName = "implicit-array-var-float",
        Type = type_array(index_implicit, var, stype_float)
    ;
        TagName = "implicit-array-var-float-range",
        Type = type_array(index_implicit, var, stype_float_range)
    ; 
        TagName = "implicit-array-var-set-of-int",
        Type = type_array(index_implicit, var, stype_set_of_int)
    ; 
        TagName = "implicit-array-var-set-of-int-range",
        Type = type_array(index_implicit, var, stype_set_of_int_range)
    ;  
        TagName = "implicit-array-var-set-of-int-set",
        Type = type_array(index_implicit, var, stype_set_of_int_set)
    
    ).

%----------------------------------------------------------------------------%
%
% Parameters
%

:- pred output_parameter_item(contentStore::in, list(attribute)::in,
    list(element)::in, io::di, io::uo) is det.

output_parameter_item(CS, Attributes, Children, !IO) :-
    VarName = lookup_reqd_name_attribute("parameter", Attributes, "name"),
    (
        Children = [],
        throw_conversion_error("missing children for <parameter> element")
    ;
        Children = [Assign],
        Assign = element(AssignName, AssignAttrs, AssignContent),
        ( if is_scalar_parameter_assignment(AssignName, AssignType) then
            TypeStr = type_to_fzn_type(CS, AssignName, AssignType, [], []),
            io.format("%s: %s = ", [s(TypeStr), s(VarName)], !IO),
            output_expression(CS, AssignName, AssignAttrs, AssignContent, !IO),
            io.write_string(";\n", !IO)
          else if is_array_parameter_assignment(AssignName, AssignElemType) then
            Size = lookup_reqd_integer_attribute(AssignName, AssignAttrs,
                "size"),
            ElemTypeStr = type_to_fzn_type(CS, AssignName, AssignElemType,
                [], []),
            io.format("array[1..%s] of %s: %s = ",
                [s(Size), s(ElemTypeStr), s(VarName)], !IO),
            output_expression(CS, AssignName, AssignAttrs, AssignContent, !IO),
            io.write_string(";\n", !IO)
          else
            throw_conversion_error("incorrect child for <parameter> element")
        )
    ;
        Children = [_, _ | _],
        throw_conversion_error("too many children for <parameter> element")
    ).

    % Parameters in XML-FlatZinc do not have there type listed separately.
    % The type must be inferred from the assignment.
    %
:- pred is_scalar_parameter_assignment(string::in, fzn_type::out) is semidet.

is_scalar_parameter_assignment(Name, Type) :-
    (
        Name = "bool-literal",
        Type = type_scalar(par, stype_bool)
    ;
        Name = "int-literal",
        Type = type_scalar(par, stype_int)
    ;
        Name = "float-literal",
        Type = type_scalar(par, stype_float)
    ;
        Name = "int-range-expr",
        Type = type_scalar(par, stype_set_of_int)
    ;
        Name = "set-of-int-literal",
        Type = type_scalar(par, stype_set_of_int)
    ).

    % is_array_parameter_assignment(Name, ElemType):
    % Note that this returns the element type, not the overall array type.
    %
:- pred is_array_parameter_assignment(string::in, fzn_type::out)
    is semidet.

is_array_parameter_assignment(Name, ElemType) :-
    (
        Name = "array-bool-literal",
        ElemType = type_scalar(par, stype_bool)
    ;
        Name = "array-int-literal",
        ElemType = type_scalar(par, stype_int)
    ;
        Name = "array-float-literal",
        ElemType = type_scalar(par, stype_float)
    ;
        Name = "array-set-of-int-literal",
        ElemType = type_scalar(par, stype_set_of_int)
    ).

:- pred is_par_type(string::in, fzn_type::out) is semidet.

is_par_type(Name, Type) :-
    (
        Name = "bool",
        Type =  type_scalar(par, stype_bool)
    ;
        Name = "int",
        Type = type_scalar(par, stype_int)
    ;
        Name = "float",
        Type = type_scalar(par, stype_float)
    ;
        Name = "set-of-int",
        Type = type_scalar(par, stype_set_of_int)
    ; 
        Name = "array-bool",  
        Type = type_array(index_explicit, par, stype_bool)
    ;
        Name = "array-int",
        Type = type_array(index_explicit, par, stype_int)
    ;
        Name = "array-float",
        Type = type_array(index_explicit, par, stype_float)
    ;
        Name = "array-set-of-int",
        Type = type_array(index_explicit, par, stype_set_of_int)
    ).

%----------------------------------------------------------------------------%
%
% Variable items
%

:- pred output_variable_item(contentStore::in, list(attribute)::in,
    list(element)::in, io::di, io::uo) is det.

output_variable_item(CS, Attrs, Children, !IO) :-
    VarName = lookup_reqd_name_attribute("variable", Attrs, "name"),
    (
        Children = [TypeE | MaybeAssignAndAnnsE],
        TypeE = element(TypeName, TypeAttrs, TypeContent),
        ( if is_var_type(TypeName, Type0) then
            Type = Type0
          else
            Msg = "first child of <variable> element is not a" ++ 
                " valid variable type",
            throw_conversion_error(Msg)
        ),
        FznType = type_to_fzn_type(CS, TypeName, Type, TypeAttrs, TypeContent),
        io.format("%s: %s", [s(FznType), s(VarName)], !IO),
        ( 
            MaybeAssignAndAnnsE = [AssignE | AnnEs0],
            AssignE = element(AssignEName, _, _),
            ( if is_variable_assignment(AssignEName) then
                MaybeAssign = yes(AssignE) : maybe(xml.doc.element),
                AnnEs = AnnEs0
              else
                MaybeAssign = no,
                AnnEs = MaybeAssignAndAnnsE
            )
        ;
            MaybeAssignAndAnnsE = [],
            MaybeAssign = no,
            AnnEs = []
        ),

        MaybeIsDefinedVar = lookup_implied_yn_attribute("variable", Attrs,
            "is_defined_var"),
        ( if MaybeIsDefinedVar = yes("yes") then
            io.write_string(" :: is_defined_var", !IO)
          else
            true
        ),

        MaybeIsIntroducedVar = lookup_implied_yn_attribute("variable", Attrs,
            "var_is_introduced"),
        ( if MaybeIsIntroducedVar = yes("yes") then
            io.write_string(" :: var_is_introduced", !IO)
          else
            true
        ),
        
        output_annotations(CS, AnnEs, !IO),

        (
            MaybeAssign = no
        ;
            MaybeAssign = yes(Assign),
            io.write_string(" = ", !IO),
            Assign = element(AssignName, AssignAttrs, AssignContent),
            output_expression(CS, AssignName, AssignAttrs, AssignContent, !IO)
        ),
        io.write_string(";\n", !IO)
    ;
        Children = [],
        throw_conversion_error("missing children for <variable> element")
    ).

:- pred is_var_type(string::in, fzn_type::out) is semidet.

is_var_type("var-bool",             type_scalar(var, stype_bool)).
is_var_type("var-int",              type_scalar(var, stype_int)).
is_var_type("var-int-range",        type_scalar(var, stype_int_range)).
is_var_type("var-int-set",          type_scalar(var, stype_int_set)).
is_var_type("var-float",            type_scalar(var, stype_float)).
is_var_type("var-float-range",      type_scalar(var, stype_float_range)).
is_var_type("var-set-of-int-range", type_scalar(var, stype_set_of_int_range)).
is_var_type("var-set-of-int-set",   type_scalar(var, stype_set_of_int_set)).
is_var_type("array-var-bool",       type_array(index_explicit, var, stype_bool)).
is_var_type("array-var-int",        type_array(index_explicit, var, stype_int)).
is_var_type("array-var-int-range",  type_array(index_explicit, var, stype_int_range)).
is_var_type("array-var-int-set",    type_array(index_explicit, var, stype_int_set)).
is_var_type("array-var-float",      type_array(index_explicit, var, stype_float)).
is_var_type("array-var-float-range",type_array(index_explicit, var, stype_float_range)).
is_var_type("array-var-set-of-int-range",type_array(index_explicit, var, stype_set_of_int_range)).
is_var_type("array-var-set-of-int-set",   type_array(index_explicit, var, stype_set_of_int_set)).

:- pred is_variable_assignment(string::in) is semidet.

is_variable_assignment("id").
is_variable_assignment("bool-literal").
is_variable_assignment("int-literal").
is_variable_assignment("float-literal").
is_variable_assignment("int-range-expr").
is_variable_assignment("set-of-int-literal").
is_variable_assignment("array-bool-literal").
is_variable_assignment("array-int-literal").
is_variable_assignment("array-float-literal").
is_variable_assignment("array-set-of-int-literal").
is_variable_assignment("array-id-expr").
is_variable_assignment("array-access-expr").
is_variable_assignment("set-of-int-expr").
is_variable_assignment("array-accesses-expr").
is_variable_assignment("array-expr").

%----------------------------------------------------------------------------%
%
% Constraint items
%

:- pred output_constraint_item(contentStore::in, list(attribute)::in,
    list(element)::in, io::di, io::uo) is det.

output_constraint_item(CS, ParentAttrs, Elems, !IO) :-
    Name = lookup_reqd_name_attribute("constraint", ParentAttrs, "name"),
    io.format("constraint %s(", [s(Name)], !IO),
    IsFirst = yes,
    write_constraint_arguments(CS, IsFirst, Elems, AnnElems, !IO),
    io.write_string(")", !IO),
    MaybeDefinesVar = lookup_implied_name_attribute("constraint", ParentAttrs,
        "defines_var"),
    (
        MaybeDefinesVar = yes(DefinedVar),
        io.format(" :: defines_var(%s)", [s(DefinedVar)], !IO)
    ;
        MaybeDefinesVar = no
    ),
    output_annotations(CS, AnnElems, !IO),
    io.write_string(";\n", !IO).

:- pred write_constraint_arguments(contentStore::in, bool/*IsFirst*/::in,
    list(element)::in, list(element)::out, io::di, io::uo) is det.

write_constraint_arguments(_, _, [], [], !IO).
write_constraint_arguments(CS, IsFirst, [E | Es], AnnElems, !IO) :-
    E = element(ArgName, ArgAttrs, ArgContent),
    ( if is_constraint_arg_expr(ArgName) then
        (
            IsFirst = yes
        ;
            IsFirst = no,
            io.write_string(", ", !IO)
        ),
        output_expression(CS, ArgName, ArgAttrs, ArgContent, !IO),
        write_constraint_arguments(CS, no, Es, AnnElems, !IO)
      else if ArgName = "annotation" then
        AnnElems = [E | Es]
      else 
        throw_conversion_error("unknown child for constraint element")
    ).

:- pred is_constraint_arg_expr(string::in) is semidet.

is_constraint_arg_expr("id").
is_constraint_arg_expr("bool-literal").
is_constraint_arg_expr("int-literal").
is_constraint_arg_expr("float-literal").
is_constraint_arg_expr("int-range-expr").
is_constraint_arg_expr("set-int-literal").
is_constraint_arg_expr("array-bool-literal").
is_constraint_arg_expr("array-int-literal").
is_constraint_arg_expr("array-float-literal").
is_constraint_arg_expr("array-set-int-literal").
is_constraint_arg_expr("array-id-expr").
is_constraint_arg_expr("array-access-expr").
is_constraint_arg_expr("set-of-int-expr").
is_constraint_arg_expr("array-accesses-expr").
is_constraint_arg_expr("array-expr").

%----------------------------------------------------------------------------%
%
% Solve item
%

:- pred output_solve_item(contentStore::in, list(attribute)::in,
    list(element)::in, io::di, io::uo) is det.

output_solve_item(CS, Attrs, Children, !IO) :-
    io.write_string("solve", !IO),
    (
        % There should be no attributes for a solve element.
        Attrs = []
    ;
        Attrs = [_ | _],
        throw_conversion_error("unexpected attributes for <solve> element.") 
    ),
    (
        Children = [],
        throw_conversion_error("expected at least one child for <solve> element.")
    ;
        Children = [SolveKind | AnnElems],
        output_annotations(CS, AnnElems, !IO),
        SolveKind = element(SolveKindName, _SolveKindAttrs, SolveKindContent),
        ( if SolveKindName = "satisfy" then
            io.write_string(" satisfy;", !IO)
          else if SolveKindName = "minimize" then
            io.write_string(" minimize ", !IO),
            output_objective(CS, SolveKindContent, !IO),
            io.write_string(";", !IO)
          else if SolveKindName = "maximize" then
            io.write_string(" maximize ", !IO),
            output_objective(CS, SolveKindContent, !IO),
            io.write_string(";", !IO)
          else
            throw_conversion_error("unknown child for <solve> element")
        )
    ).

:- pred output_objective(contentStore::in, list(ref(content))::in,
    io::di, io::uo) is det.

output_objective(CS, ContentRefs, !IO) :-
    ObjElems = gather_child_elements(CS, ContentRefs),
    (
        ObjElems = [],
        throw_conversion_error("missing objective")
    ;
        ObjElems = [ObjElem],
        ObjElem = element(ObjName, ObjAttrs, ObjContent),
        ( if is_objective_expr(ObjName) then
            output_expression(CS, ObjName, ObjAttrs, ObjContent, !IO)
          else
            throw_conversion_error("invalid objective expression")
        )
    ;
        ObjElems = [_, _ | _],
        throw_conversion_error("too many elements for objective")
    ).

:- pred is_objective_expr(string::in) is semidet.

is_objective_expr("id").
is_objective_expr("array-access-expr").
            
%----------------------------------------------------------------------------%
%
% Types
%

    % FlatZinc scalar types.
    % Since FlatZinc only supports sets of ints we treat them as scalar too.
    %
:- type fzn_scalar_type
    --->    stype_bool
    ;       stype_int
    ;       stype_int_range
    ;       stype_int_set
    ;       stype_float
    ;       stype_float_range
    ;       stype_set_of_int
    ;       stype_set_of_int_range
    ;       stype_set_of_int_set.

:- type fzn_inst
    --->    par
    ;       var.

:- type fzn_array_index
    --->    index_implicit
    ;       index_explicit.

:- type fzn_type
    --->    type_scalar(fzn_inst, fzn_scalar_type)
    ;       type_array(fzn_array_index, fzn_inst, fzn_scalar_type).

:- func type_to_fzn_type(contentStore, string, fzn_type, list(attribute),
     list(ref(content))) = string.

type_to_fzn_type(CS, TagName, Type, Attrs, Content) = FznType :-
    (
        Type = type_scalar(Inst, ScalarType),
        FznType = scalar_type_inst_to_string(CS, Attrs, Inst, ScalarType,
            Content)
    ;
        Type = type_array(IndexKind, ElemInst, ElemType),
        ElemTIStr = scalar_type_inst_to_string(CS, Attrs, ElemInst, ElemType,
            Content),
        (
            IndexKind = index_implicit,
            IndexStr = "int"
        ;
            IndexKind = index_explicit,
            Size = lookup_reqd_integer_attribute(TagName, Attrs, "size"),
            IndexStr = "1.." ++ Size
        ),
        string.format("array[%s] of %s", [s(IndexStr), s(ElemTIStr)], FznType)
    ).

:- func scalar_type_inst_to_string(contentStore, list(attribute), fzn_inst,
    fzn_scalar_type, list(ref(content))) = string.

scalar_type_inst_to_string(CS, Attrs, Inst, Type, Content) = Str  :-
    TypeStr = scalar_type_to_string(CS, Attrs, Type, Content),
    (
        Inst = var,
        Str = "var " ++ TypeStr
    ;
        Inst = par,
        Str = TypeStr
    ).

:- func scalar_type_to_string(contentStore, list(attribute), fzn_scalar_type,
    list(ref(content))) = string.

scalar_type_to_string(_, _, stype_bool, _) = "bool".
scalar_type_to_string(_, _, stype_int, _) = "int".
scalar_type_to_string(_, Attrs, stype_int_range, _) = Str :-
    Lo = lookup_reqd_integer_attribute("int-range", Attrs, "lo"),
    Hi = lookup_reqd_integer_attribute("int-range", Attrs, "hi"),
    string.format("%s..%s", [s(Lo), s(Hi)], Str).
scalar_type_to_string(CS, _, stype_int_set, Children) = Str :-
    (
        Children = [],
        throw_conversion_error("no children for <int-set>")
    ;
        Children = [ChildRef],
        Child = ref(CS, ChildRef),
        Str = cdata_to_int_set(Child, "int-set")
    ;
        Children = [_, _ | _],
        throw_conversion_error("too many children for <int-set>")
    ).
scalar_type_to_string(_, _, stype_float, _) = "float".
scalar_type_to_string(_, Attrs, stype_float_range, _) = Str :-
    Lo = lookup_reqd_float_attribute("float-range", Attrs, "lo"),
    Hi = lookup_reqd_float_attribute("float-range", Attrs, "hi"),
    string.format("%s..%s", [s(Lo), s(Hi)], Str).
scalar_type_to_string(_, _, stype_set_of_int, _) = "set of int".
scalar_type_to_string(_, Attrs, stype_set_of_int_range, _) = Str :-
    Lo = lookup_reqd_integer_attribute("set-of-int-range", Attrs, "lo"),
    Hi = lookup_reqd_integer_attribute("set-of-int-range", Attrs, "hi"),
    string.format("set of %s..%s", [s(Lo), s(Hi)], Str).
scalar_type_to_string(CS, _, stype_set_of_int_set, Children) = Str :-
    (
        Children = [],
        throw_conversion_error("no children for <set-of-int-set>")
    ;
        Children = [ChildRef],
        Child = ref(CS, ChildRef),
        SetStr = cdata_to_int_set(Child, "set-of-int-set"),
        Str = "set of " ++ SetStr
    ;
        Children = [_, _ | _],
        throw_conversion_error("too many children for <set-of-int-set>")
    ).

:- func cdata_to_int_set(content, string) = string.

cdata_to_int_set(Content, EName) = Str :-
    (
        ( Content = element(_)
        ; Content = pi(_, _)
        ; Content = comment(_)
        ),
        Msg = "expected character data in element <" ++ EName ++ ">",
        throw_conversion_error(Msg)
    ;
        Content = data(CDATA),
        Elems = string.words(CDATA),
        ( if list.all_true(is_integer_literal, Elems) then
            (
                Elems = [],
                Str = "{}"
            ;
                Elems = [_ | _],
                Str = "{" ++ string.join_list(", ", Elems) ++ "}"
            )
          else
            Msg = "CDATA in child of <" ++ EName ++
                ">  does not contain integer literals",
            throw_conversion_error(Msg)
        )
    ).

%----------------------------------------------------------------------------%
%
% Expressions
%

:- pred output_expression(contentStore::in, element::in,
    io::di, io::uo) is det.

output_expression(CS, Element, !IO) :-
    Element = element(Name, Attrs, Content),
    output_expression(CS, Name, Attrs, Content, !IO).

:- pred output_expression(contentStore::in, string::in, list(attribute)::in,
    list(ref(content))::in, io::di, io::uo) is det.

output_expression(CS, Name, Attrs, Content, !IO) :-
    ( if Name = "id" then
        IdName = lookup_reqd_name_attribute(Name, Attrs, "name"),
        io.write_string(IdName, !IO)
      else if Name = "bool-literal" then
        Value = lookup_reqd_bool_attribute("bool-literal", Attrs, "value"),
        io.write_string(Value, !IO)
      else if Name = "int-literal" then
        Value = lookup_reqd_integer_attribute(Name, Attrs, "value"),
        io.write_string(Value, !IO)
      else if Name = "float-literal" then
        Value = lookup_reqd_float_attribute(Name, Attrs, "value"),
        io.write_string(Value, !IO)
      else if Name = "string-literal" then
        Value0 = lookup_reqd_string_attribute(Name, Attrs, "value"),
        Value = undo_xml_escapes(Value0),
        io.format("\"%s\"", [s(Value)], !IO)
      else if Name = "int-range-expr" then
        Lo = lookup_reqd_integer_attribute(Name, Attrs, "lo"),
        Hi = lookup_reqd_integer_attribute(Name, Attrs, "hi"),
        io.format("%s..%s", [s(Lo), s(Hi)], !IO)
      else if Name = "set-of-int-literal" then
        output_set_of_int_literal(CS, Content, !IO)
      else if Name = "array-access-expr" then
        ArrayName = lookup_reqd_name_attribute(Name, Attrs, "name"),
        Index = lookup_reqd_integer_attribute(Name, Attrs, "index"),
        io.format("%s[%s]", [s(ArrayName), s(Index)], !IO) 
      else if Name = "array-bool-literal" then
        output_array_bool_literal(CS, Content, !IO)
      else if Name = "array-float-literal" then
        output_array_float_literal(CS, Content, !IO)
      else if Name = "array-int-literal" then
        output_array_int_literal(CS, Content, !IO)
      else if Name = "array-set-of-int-literal" then
        output_array_set_of_int_literal(CS, Content, !IO)
      else if Name = "array-id-expr" then
        output_array_id_expr(CS, Content, !IO)
      else if Name = "array-accesses-expr" then
        ArrayName = lookup_reqd_name_attribute(Name, Attrs, "name"),
        output_array_accesses_expr(CS, Content, ArrayName, !IO)
      else if Name = "array-expr" then
        output_array_expr(CS, Content, !IO)
      else
        io.write_string("<" ++ string(Name) ++ ">", !IO)
        %throw_conversion_error("unknown expression element")
    ).

%
% Output set of int literals
%

:- pred output_set_of_int_literal(contentStore::in,
    list(ref(content))::in, io::di, io::uo) is det.

output_set_of_int_literal(CS, Content, !IO) :-
    (
        % An empty element will be used if the set is empty.   
        Content = [],
        io.write_string("{}", !IO)
    ;
        Content = [SetLitRef],
        CDATA = ref(CS, SetLitRef),
        SetLitStr = cdata_to_int_set(CDATA, "set-of-int-literal"),
        io.write_string(SetLitStr, !IO)
    ;
        Content = [_, _ | _],
        throw_conversion_error("expected zero or one children for <set-of-int-literal> element")
    ).

% 
% Output compact arrays of Boolean literals
%

:- pred output_array_bool_literal(contentStore::in,
    list(ref(content))::in, io::di, io::uo) is det.

output_array_bool_literal(CS, Content, !IO) :-
    CDATA = extract_cdata("array-bool-literal", CS, Content),
    cdata_to_bools(CDATA, ArrayBools, _Num),
    io.write_string(ArrayBools, !IO).

:- pred cdata_to_bools(string::in, string::out, int::out) is det.

cdata_to_bools(CDATA, Str, Num) :-
    string.foldl(char_to_bool, CDATA, [], RevBools),
    list.reverse(RevBools, Bools),
    list.length(Bools, Num),
    (
        Bools = [],
        Str = "[]"
    ;
        Bools = [_ | _],
        Str = "[" ++ string.join_list(", ", Bools) ++ "]"
    ).

:- pred char_to_bool(char::in, list(string)::in, list(string)::out) is det.

char_to_bool(C, !Bools) :-
    ( if C = 't' then
        !:Bools = ["true" | !.Bools]
      else if C = 'f' then
        !:Bools = ["false" | !.Bools]
      else if char.is_whitespace(C) then
        true
      else
        throw_conversion_error("expected 't' or 'f' in Boolean array literal encoding")
    ).

%
% Output compact arrays of float literals
%

:- pred output_array_float_literal(contentStore::in,
    list(ref(content))::in, io::di, io::uo) is det.

output_array_float_literal(CS, Content, !IO) :-
    EName = "array-float-literal",
    CDATA = extract_cdata(EName, CS, Content),
    ArrayFloats = cdata_to_floats(EName, CDATA),
    io.write_string(ArrayFloats, !IO).

    % cdata_to_floats(ElemName, CDATA) = ArrayFloatLit:
    % 
:- func cdata_to_floats(string, string) = string.

cdata_to_floats(EName, CDATA) = ArrayFloatLit :-
    Elems = string.words(CDATA),
    ( if list.all_true(is_float_literal, Elems) then
        ArrayFloatLit = "[" ++ string.join_list(", ", Elems) ++ "]"
      else
        Msg = "CDATA in child of <" ++ EName ++
            "> does not contain float literals",
        throw_conversion_error(Msg)
    ).

%
% Output compact arrays of integer literals
%

:- pred output_array_int_literal(contentStore::in,
    list(ref(content))::in, io::di, io::uo) is det.

output_array_int_literal(CS, Content, !IO) :-
    EName = "array-int-literal",
    CDATA = extract_cdata(EName, CS, Content),
    ArrayIntLit = cdata_to_ints(EName, CDATA),
    io.write_string(ArrayIntLit, !IO).

:- func cdata_to_ints(string, string) = string.

cdata_to_ints(EName, CDATA) = ArrayIntLit :-
    Elems = string.words(CDATA),
    ( if list.all_true(is_integer_literal, Elems) then
        ArrayIntLit  = "[" ++ string.join_list(", ", Elems) ++ "]"
      else
        Msg = "CDATA in child of <" ++ EName ++
            "> does not contain integer literals",
        throw_conversion_error(Msg)
    ).

%
% Output compact arrays of identifiers
%

:- pred output_array_id_expr(contentStore::in,
    list(ref(content))::in, io::di, io::uo) is det.

output_array_id_expr(CS, Content, !IO) :-
    EName = "array-id-expr",
    CDATA = extract_cdata(EName, CS, Content),
    ArrayIds = cdata_to_ids(EName, CDATA),
    io.write_string(ArrayIds, !IO). 

:- func cdata_to_ids(string, string) = string.

cdata_to_ids(EName, CDATA) = ArrayId :-
    Elems = string.words(CDATA),
    ( if list.all_true(is_fzn_identifier, Elems) then
        ArrayId = "[" ++ string.join_list(", ", Elems) ++ "]"
      else
        Msg = "CDATA in child of <" ++ EName ++
            "> does not contain FlatZinc identifiers",
        throw_conversion_error(Msg)
    ).

%
% Output compact arrays of array accesses
%

:- pred output_array_accesses_expr(contentStore::in,
    list(ref(content))::in, string::in, io::di, io::uo) is det.

output_array_accesses_expr(CS, Content, ArrayName, !IO) :-
    CDATA = extract_cdata("array-accesses-expr", CS, Content),
    Indicies = string.words(CDATA),
    ( if list.all_true(is_integer_literal, Indicies) then
        OutputAccess = (pred(I::in, !.IO::di, !:IO::uo) is det :-
            io.format("%s[%s]", [s(ArrayName), s(I)], !IO)
        ),
        io.write_string("[", !IO),
        io.write_list(Indicies, ", ", OutputAccess, !IO),
        io.write_string("]", !IO)
      else
        Msg = "CDATA in child of <array-accesses-expr>" ++
            "does not contain integer literals",
        throw_conversion_error(Msg)
    ).

%
% Output arrays of sets of ints
%

:- pred output_array_set_of_int_literal(contentStore::in,
    list(ref(content))::in, io::di, io::uo) is det.

output_array_set_of_int_literal(CS, ContentRefs, !IO) :-
    output_array_expr(CS, ContentRefs, !IO).

%
% Output ordinary non-compact arrays
%
        
:- pred output_array_expr(contentStore::in, list(ref(content))::in,
    io::di, io::uo) is det.

output_array_expr(CS, Content, !IO) :-
    io.write_string("[", !IO),
    Children = gather_child_elements(CS, Content),
    io.write_list(Children, ", ", output_expression(CS), !IO),
    io.write_string("]", !IO).

%----------------------------------------------------------------------------%
%
% Annotations
%

:- pred output_annotations(contentStore::in, list(element)::in,
    io::di, io::uo) is det.


output_annotations(CS, AnnElems, !IO) :-
    (
        AnnElems = []
    ;
        AnnElems = [AnnElem | AnnElemsPrime],
        AnnElem = element(AnnName, AnnAttrs, AnnContent),
        ( if AnnName = "annotation" then
            output_annotation(CS, AnnAttrs, AnnContent, !IO)
          else
            throw_conversion_error("Not an annotation")
        ), 
        output_annotations(CS, AnnElemsPrime, !IO)    
    ).

:- pred output_annotation(contentStore::in, list(attribute)::in,
    list(ref(content))::in, io::di, io::uo) is det.

output_annotation(CS, Attrs, ContentRefs, !IO) :-
    Name = lookup_reqd_name_attribute("annotation", Attrs, "name"),
    ArgElems = gather_child_elements(CS, ContentRefs),
    (
        ArgElems = [], 
        io.format(" ::  %s ", [s(Name)], !IO)
    ;
        ArgElems = [_ | _],
        io.format(" :: %s(", [s(Name)], !IO),
        output_annotation_args(CS, /*IsFirst*/yes, ArgElems, !IO),
        io.write_string(")", !IO)
    ).

:- pred output_annotation_args(contentStore::in, bool::in,
    list(element)::in, io::di, io::uo) is det.

output_annotation_args(_, _, [], !IO).
output_annotation_args(CS, IsFirst, [E | Es], !IO) :-
    (
        IsFirst = yes
    ;
        IsFirst = no,
        io.write_string(", ", !IO)
    ),
    E = element(Name, Attrs, Content),
    ( if Name = "annotation" then
        output_nested_annotation(CS, E, !IO)
      else if Name = "array-annotation" then
        output_annotation_array(CS, Attrs, Content, !IO)
      else
        output_expression(CS, Name, Attrs, Content, !IO)
    ),
    output_annotation_args(CS, no, Es, !IO).

:- pred output_nested_annotation(contentStore::in, element::in,
    io::di, io::uo) is det.

output_nested_annotation(CS, Element, !IO) :-
    Element = element(EName, Attrs, ContentRefs),
    Name = lookup_reqd_name_attribute(EName, Attrs, "name"),
    ArgElems = gather_child_elements(CS, ContentRefs),
    (
        ArgElems = [], 
        io.format("%s", [s(Name)], !IO)
    ;
        ArgElems = [_ | _],
        io.format("%s(", [s(Name)], !IO),
        output_annotation_args(CS, /*IsFirst*/yes, ArgElems, !IO),
        io.write_string(") ", !IO)
    ).

:- pred output_annotation_array(contentStore::in, list(attribute)::in,
    list(ref(content))::in, io::di, io::uo) is det.

output_annotation_array(CS, _Attrs, Content, !IO) :-
    Children = gather_child_elements(CS, Content),
    io.write_string("[", !IO),
    io.write_list(Children, ", ", output_nested_annotation(CS), !IO),
    io.write_string("]", !IO).

%----------------------------------------------------------------------------%

    % gather_child_elements(CS, ContentRefs) = Elements:
    % Return the element referred to by ContentRefs in document order.
    %
:- func gather_child_elements(contentStore, list(ref(content)))
    = list(element).

gather_child_elements(CS, ContentRefs) = Elements :-
    Contents = list.map(xml.doc.ref(CS), ContentRefs),
    Elements = gather_child_elements_2(Contents). 

:- func gather_child_elements_2(list(content)) = list(element).

gather_child_elements_2([]) = [].
gather_child_elements_2([C | Cs]) = Elems :-
    Elems0 = gather_child_elements_2(Cs),
    (
        C = element(Elem),
        Elems = [Elem | Elems0]
    ;
        ( C = pi(_, _)
        ; C = comment(_)
        ; C = data(_)
        ),
        Elems = Elems0
    ).

:- func extract_cdata(string, contentStore, list(ref(content))) = string.

extract_cdata(EName, CS, ContentRefs) = CDATA :-
    (
        ContentRefs = [],
        NoChildMsg = "no children for element <" ++ EName ++ ">",
        throw_conversion_error(NoChildMsg)
    ;
        ContentRefs = [ContentRef],
        Content = ref(CS, ContentRef),
        (
            Content = data(CDATA)
        ;
            ( Content = element(_)
            ; Content = pi(_, _)
            ; Content = comment(_)
            ),
            NoCDATAMsg = "element <" ++ EName ++ "> does not contain" ++
                " character data",
            throw_conversion_error(NoCDATAMsg)
        )
    ;
        ContentRefs = [_, _ | _],
        MultipleChildMsg = "expected only single child for element <"
            ++ EName ++ ">",
        throw_conversion_error(MultipleChildMsg)
    ). 

    % Replace any XML character escapes in attributes values.
    %
:- func undo_xml_escapes(string) = string.

undo_xml_escapes(!.Str) = !:Str :-
    string.replace_all(!.Str, "&quot;", "\\\"", !:Str),
    string.replace_all(!.Str, "&#39;", "\\\"", !:Str),

    string.replace_all(!.Str, "&apos;", "'", !:Str),
    string.replace_all(!.Str, "&#34;", "'", !:Str),

    string.replace_all(!.Str, "&lt;", "<", !:Str),
    string.replace_all(!.Str, "&#60;", "<", !:Str),

    string.replace_all(!.Str, "&gt;", ">", !:Str),
    string.replace_all(!.Str, "&#62;", ">", !:Str),

    string.replace_all(!.Str, "&amp;", "&", !:Str),
    string.replace_all(!.Str, "&#38;", "&", !:Str).

%-----------------------------------------------------------------------------%
% 
% Code for handling attributes
%

    % Lookup the value of a required attribute where the value must be a
    % FlatZinc Boolean value.
    %
:- func lookup_reqd_bool_attribute(string, list(attribute), string) = string.

lookup_reqd_bool_attribute(EName, Attrs, AName) = Value :-
    (
        Attrs = [],
        throw(missing_reqd_attribute(EName, AName))
    ;
        Attrs = [Attr | AttrsPrime],
        ( if    Attr = attribute(AName, AValue)
          then  
                % Check that the attribute value is actually a Boolean.
                ( if    is_fzn_bool(AValue)
                  then  Value = AValue
                  else  BadAttr = ill_typed_attribute(EName, AName,
                            "a FlatZinc Boolean literal"),
                        throw(BadAttr)
                )
          else
                Value = lookup_reqd_bool_attribute(EName, AttrsPrime, AName)
        )
    ).

:- pred is_fzn_bool(string::in) is semidet.

is_fzn_bool("false").
is_fzn_bool("true"). 

    % Lookup the value of a required attribute where the value must be an
    % integer value.
    %
:- func lookup_reqd_integer_attribute(string, list(attribute), string) = string.

lookup_reqd_integer_attribute(EName, Attrs, AName) = Value :-
    (
        Attrs = [],
        throw(missing_reqd_attribute(EName, AName))
    ;
        Attrs = [Attr | AttrsPrime],
        ( if    Attr = attribute(AName, AValue)
          then  
                % Check that the attribute value is actually an integer.
                ( if    string.to_int(AValue, _)
                  then  Value = AValue
                  else  BadAttr = ill_typed_attribute(EName, AName,
                            "an integer literal"),
                        throw(BadAttr)
                )
          else
                Value = lookup_reqd_integer_attribute(EName, AttrsPrime, AName)
        )
    ). 
    
    % lookup_reqd_float_attribute(ElementName, Attributes, Name) = Value:
    % Lookup the value of a required attribute where the value must be a
    % floating-point value.
    %
:- func lookup_reqd_float_attribute(string, list(attribute), string) = string.

lookup_reqd_float_attribute(EName, Attrs, AName) = Value :-
    (
        Attrs = [],
        throw(missing_reqd_attribute(EName, AName))
    ;
        Attrs = [Attr | AttrsPrime],
        ( if    Attr = attribute(AName, AValue)
          then  
                % Check that the attribute value is actually a float.
                ( if    string.to_float(AValue, _)
                  then  Value = AValue
                  else  BadAttr = ill_typed_attribute(EName, AName,
                            "a float literal"),
                        throw(BadAttr)
                )
          else
                Value = lookup_reqd_float_attribute(EName, AttrsPrime, AName)
        )
    ). 
        
:- func lookup_reqd_string_attribute(string, list(attribute), string) = string.

lookup_reqd_string_attribute(EName, Attrs, AName) = Value :-
    (
        Attrs = [],
        throw(missing_reqd_attribute(EName, AName))
    ;
        Attrs = [Attr | AttrsPrime],
        ( if    Attr = attribute(AName, AValue)
          then  Value = AValue
          else  Value = lookup_reqd_float_attribute(EName, AttrsPrime, AName)
        )
    ). 

    % Lookup a required attribute where the value of the attribute must
    % be a valid FlatZinc name.
    %
:- func lookup_reqd_name_attribute(string, list(attribute), string) = string.

lookup_reqd_name_attribute(EName, Attrs, AName) = Value :-
    (
        Attrs = [],
        throw(missing_reqd_attribute(EName, AName))
    ;
        Attrs = [Attr | AttrsPrime],
        ( if    Attr = attribute(AName, AValue)
          then  
                % Check that the attribute value is actually an identifier. 
                ( if    is_fzn_identifier(AValue)
                  then  Value = AValue
                  else  BadAttr = ill_typed_attribute(EName, AName,
                            "a FlatZinc identifier"),
                        throw(BadAttr)
                )
          else
                Value = lookup_reqd_float_attribute(EName, AttrsPrime, AName)
        )
    ). 

    % Succeeds iff the given string is a valid FlatZinc identifier.
    %
:- pred is_fzn_identifier(string::in) is semidet.

is_fzn_identifier(Id0) :-
    IsUnderscore = (pred(C::in) is semidet :- C = '_' ),
    Id = string.lstrip_pred(IsUnderscore, Id0), 
    string.first_char(Id, FirstChar, Rest),
    char.is_alpha(FirstChar),
    string.is_all_alnum_or_underscore(Rest).

:- func lookup_implied_yn_attribute(string, list(attribute), string)
    = maybe(string).

lookup_implied_yn_attribute(EName, Attrs, AName) = Value :-
    (
        Attrs = [],
        Value = no
    ;
        Attrs = [Attr | AttrsPrime],
        ( if    Attr = attribute(AName, AValue)
          then
                % Check that the attribute value is "yes" or "no".
                ( if    is_yes_or_no(AValue)
                  then  Value = yes(AValue)
                  else  
                        Msg = "value of attribute `" ++ AName ++ "' must" ++
                            "be \"yes\" or \"no\"",
                        throw_conversion_error(Msg)
                )
          else
                Value = lookup_implied_yn_attribute(EName, AttrsPrime, AName)
        )
    ).

:- pred is_yes_or_no(string::in) is semidet.

is_yes_or_no("no").
is_yes_or_no("yes").

:- func lookup_implied_name_attribute(string, list(attribute), string)
    = maybe(string).

lookup_implied_name_attribute(EName, Attrs, AName) = MaybeValue :-
    (
        Attrs = [],
        MaybeValue = no
    ;
        Attrs = [Attr | AttrsPrime],
        ( if    Attr = attribute(AName, AValue)
          then
                % Check that the attribute value is a FlatZinc name.
                ( if    is_fzn_identifier(AValue)
                  then  MaybeValue = yes(AValue)
                  else  
                        Msg = "value of attribute `" ++ AName ++ 
                            "' must be a valid FlatZinc identifier",
                        throw_conversion_error(Msg)
                )
          else
                MaybeValue = lookup_implied_name_attribute(EName, AttrsPrime,
                    AName)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred is_integer_literal(string::in) is semidet.

is_integer_literal(I) :-
    string.to_int(I, _).

:- pred is_float_literal(string::in) is semidet.

is_float_literal(F) :-
    string.to_float(F, _).

%-----------------------------------------------------------------------------%
%
% Error handling
%

:- type xml2fzn_error
    --->    missing_reqd_attribute(
                mra_tag_name  :: string,
                mra_attr_name :: string
            )
    ;
            ill_typed_attribute(
                ila_tag_name :: string,
                ila_attr_nae :: string,
                ila_expected :: string
            ).

%-----------------------------------------------------------------------------%
%
% Option processing
%

:- type xml2fzn_option
    --->    help
    ;       version
    ;       output_to_file. 

:- pred xml2fzn_short_option(char::in, xml2fzn_option::out) is semidet.

xml2fzn_short_option('h', help).
xml2fzn_short_option('o', output_to_file).

:- pred xml2fzn_long_option(string::in, xml2fzn_option::out) is semidet.

xml2fzn_long_option("help",             help).
xml2fzn_long_option("version",          version).
xml2fzn_long_option("output-to-file",   output_to_file).

:- pred xml2fzn_option_defaults(xml2fzn_option, option_data).
:- mode xml2fzn_option_defaults(out, out) is multi.
:- mode xml2fzn_option_defaults(in, out) is det.

xml2fzn_option_defaults(help,           bool(no)).
xml2fzn_option_defaults(version,        bool(no)).
xml2fzn_option_defaults(output_to_file, string("")).

%-----------------------------------------------------------------------------%

:- func xml2fzn_version = string.

xml2fzn_version = VersionMsg :-
    Version = get_xml2fzn_version,
    VersionMsg =
        "G12 XML to FlatZinc converter, version " ++ Version ++ "\n"
++      "Copyright (C) 2009-2012 The University of Melbourne and NICTA\n".

%-----------------------------------------------------------------------------%

:- func xml2fzn_usage = string.

xml2fzn_usage = UsageMsg :-
    UsageLines = [
    "Usage: xml2fzn [<options>] <file>"
,   "Options:"
,   "   -h, --help"
,   "       Print this message."
,   "   --version"
,   "       Print version information."
,   "   -o <file>, --output-to-file <file>"
,   "        Output the FlatZinc to the specified file."
],
    UsageMsg = list.foldr(func(X, Xs) = X ++ "\n" ++ Xs, UsageLines, "").

%-----------------------------------------------------------------------------%
%
% Error handling procedures
%

:- pred bad_cmdline(string::in, io::di, io::uo) is det.

bad_cmdline(Msg, !IO) :-
    io.write_string(io.stderr_stream,
        "xml2fzn: " ++ Msg ++ "\n"
     ++ "xml2fzn: use --help for more information.\n",
        !IO),
    io.set_exit_status(1, !IO).

:- pred handle_io_error(io.error::in, io::di, io::uo) is det.

handle_io_error(Error, !IO) :-
    io.stderr_stream(Stderr, !IO),
    Msg = io.error_message(Error),
    io.format(Stderr, "xml2fzn: I/O error: %s\n", [s(Msg)], !IO),
    io.set_exit_status(1, !IO).

%-----------------------------------------------------------------------------%

    % Please do not use this type for new errors.  Instead extend the
    % xml2fzn_error/0 type.
    %
:- type conversion_error
    --->    conversion_error(string).

:- pred handle_conversion_error(conversion_error::in, io::di, io::uo) is det.

handle_conversion_error(Error, !IO) :-
    % Flush stdout to make sure that the error message doesn't get
    % intermingled with any other output.
    io.nl(!IO),
    io.flush_output(!IO),
    Error = conversion_error(Msg),
    io.stderr_stream(Stderr, !IO),
    io.write_string(Stderr, "xml2fzn: error: " ++ Msg ++ "\n", !IO), 
    io.set_exit_status(1, !IO).

:- pred throw_conversion_error(string::in) is erroneous.

throw_conversion_error(Msg) :-
    ConvError = conversion_error(Msg),
    throw(ConvError).
            
%-----------------------------------------------------------------------------%

:- pred handle_xml2fzn_error(xml2fzn_error::in, io::di, io::uo) is det.

handle_xml2fzn_error(Error, !IO) :-
    % Flush stdout to make sure that the error message doesn't get
    % intermingled with any other output.
    io.nl(!IO),
    io.flush_output(!IO),
    io.stderr_stream(Stderr, !IO),
    (
        Error = missing_reqd_attribute(EName, AName),
        io.format(Stderr,
            "xml2fzn: error: missing required " ++
            "\"%s\" attribute for element <%s>\n",
            [s(AName), s(EName)], !IO)
    ;
        Error = ill_typed_attribute(EName, AName, ExpectedDesc),
        io.format(Stderr,
            "xml2fzn: error: invalid attribute value for " ++
            "attribute \"%s\" of element <%s>: expected %s\n",
            [s(AName), s(EName), s(ExpectedDesc)], !IO)
    ),
    io.set_exit_status(1, !IO).

%-----------------------------------------------------------------------------%
:- end_module xml2fzn.
%-----------------------------------------------------------------------------%
