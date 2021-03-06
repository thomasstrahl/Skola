%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2009 The University of Melbourne and NICTA.
% See the file COPYING for license information.
%-----------------------------------------------------------------------------%
% 
% This module has been automatically generated by the script
% make_minizinc_conf.
% DO NOT EDIT.
%
%----------------------------------------------------------------------------%

:- module minizinc_conf.
:- interface.
:- import_module char.

%----------------------------------------------------------------------------%
%
% Version information
%

:- func minizinc_major_version = string.
:- func minizinc_minor_version = string.
:- func minizinc_patch_version = string.

%----------------------------------------------------------------------------%
%
% Installation directory
%

:- func install_prefix = string.

%----------------------------------------------------------------------------%
%
% Search directory separator character
%

:- pred is_search_dir_separator(char::in) is semidet.

%----------------------------------------------------------------------------%
%
% Compilation environment
%

:- func minizinc_compilation_grade = string.

:- func minizinc_build_date = string.

:- func minizinc_opt_flags = string.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

minizinc_major_version = "1".

minizinc_minor_version = "6".

minizinc_patch_version = "0".

install_prefix = "/home/mercury/minizinc-1.6/tools-1.6/minizinc-pkg-builddir-29082/install".

is_search_dir_separator(':').

minizinc_compilation_grade = $grade.

minizinc_build_date = "Wed Sep 19 03:42:03 EST 2012".

minizinc_opt_flags = "--optimize-constructor-last-call --no-common-struct".

%----------------------------------------------------------------------------%
:- end_module minizinc_conf.
%----------------------------------------------------------------------------%
