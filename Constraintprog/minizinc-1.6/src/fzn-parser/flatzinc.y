// Parser for FlatZinc 1.1.
// Authors: Nick Nethercote
//          Julien Fischer
//
// NOTE: the parser produced by the following grammar does not ensure
// that expressions are type correct.  Further type-checking after parsing
// is required for this.
//
// This file is in the public domain, and can be used without copyright
// restrictions.

%{
#include <stdio.h>
#include <stdlib.h>
%}

 
// Possible values for attributed tokens.
%union {
    char*   string_val;
    int     int_val;
    double  float_val;
};

// Token kinds
%token <int_val>    INT_LITERAL
       <string_val> STRING_LITERAL IDENT UNDERSCORE_IDENT
       <float_val>  FLOAT_LITERAL 
       ARRAY BOOL CONSTRAINT FALSE FLOAT INT MAXIMIZE MINIMIZE OF
       PREDICATE SATISFY SET SOLVE TRUE VAR DOTDOT COLONCOLON 
       
%%

//---------------------------------------------------------------------------
// Model top-level
//---------------------------------------------------------------------------

// Nb: these rules are left-recursive, which is good for Yacc as they run in
// constant stack space.  Earlier versions were right-recursive, and this
// caused stack overflows on large models.  The error recovery isn't great,
// but it's better than none.

model          : pred_decl_items var_decl_items constraint_items model_end

pred_decl_items : pred_decl_items pred_decl_item ';'
                | pred_decl_items error ';' { yyerrok; }
		| /* empty */

var_decl_items : var_decl_items var_decl_item ';'
               | /* empty */
 
constraint_items: constraint_items constraint_item ';'
               | /* empty */
 
model_end      : solve_item ';'
    
    
//---------------------------------------------------------------------------
// Items
//---------------------------------------------------------------------------

pred_decl_item:
    PREDICATE IDENT '(' pred_decl_args ')'

var_decl_item:
    VAR    non_array_ti_expr_tail ':' ident_anns var_decl_item2
  |        non_array_ti_expr_tail ':' ident_anns '=' expr
  | ARRAY '[' INT_LITERAL DOTDOT INT_LITERAL ']' OF array_decl_tail

var_decl_item2:
    '=' expr
  | /*empty*/

array_decl_tail:
        non_array_ti_expr_tail ':' ident_anns '=' array_literal
  | VAR non_array_ti_expr_tail ':' ident_anns array_decl_tail2
  
array_decl_tail2:
    '=' array_literal
  | /*empty*/

ident_anns:
    IDENT annotations
    | UNDERSCORE_IDENT annotations

constraint_item:
    CONSTRAINT constraint_elem annotations

constraint_elem:
    IDENT '(' exprs ')'

solve_item:
    SOLVE annotations solve_kind

solve_kind:
    SATISFY
  | MINIMIZE expr
  | MAXIMIZE expr

//---------------------------------------------------------------------------
// Predicate parameters
//---------------------------------------------------------------------------

pred_decl_args:
    pred_decl_arg "," pred_decl_args
  | pred_decl_arg

pred_decl_arg:
     non_array_ti_expr_tail ':' IDENT
   | VAR non_array_ti_expr_tail ':' IDENT
   | ARRAY '[' pred_arg_array_index ']' OF  pred_arg_array_tail ':' IDENT

pred_arg_array_index:
    INT
  | INT_LITERAL DOTDOT INT_LITERAL

pred_arg_array_tail:
    non_array_ti_expr_tail
  | VAR non_array_ti_expr_tail

//---------------------------------------------------------------------------
// Type-Inst Expression Tails
//---------------------------------------------------------------------------

non_array_ti_expr_tail:
    scalar_ti_expr_tail
  | set_ti_expr_tail

scalar_ti_expr_tail:
    bool_ti_expr_tail
  | int_ti_expr_tail
  | float_ti_expr_tail

bool_ti_expr_tail:
    BOOL

int_ti_expr_tail:
    INT
  | INT_LITERAL DOTDOT INT_LITERAL
  | '{' int_literals '}'

int_literals:
    INT_LITERAL ',' int_literals
  | INT_LITERAL

float_ti_expr_tail:
    FLOAT
  | FLOAT_LITERAL DOTDOT FLOAT_LITERAL

set_ti_expr_tail:
    SET OF int_ti_expr_tail

//---------------------------------------------------------------------------
// Expressions
//---------------------------------------------------------------------------

exprs:
    expr ',' exprs
  | expr

expr:
    bool_literal
  | INT_LITERAL 
  | FLOAT_LITERAL
  | STRING_LITERAL
  | set_literal
  | array_literal
  | array_access_expr
  | IDENT
  | UNDERSCORE_IDENT
  | IDENT '(' exprs ')'	/* An annotation value with > 0 arguments. */

bool_literal: FALSE | TRUE

set_literal:
    '{' exprs '}'
  | '{' '}'
  | INT_LITERAL DOTDOT INT_LITERAL

array_literal:
    '[' exprs ']'
  | '[' ']'

array_access_expr: IDENT '[' INT_LITERAL ']'
  | UNDERSCORE_IDENT '[' INT_LITERAL ']'

//---------------------------------------------------------------------------
// Annotations
//---------------------------------------------------------------------------

annotations:
    COLONCOLON expr annotations
  | /* empty */

%%

#include "lex.yy.c"

char* filename;

int main(int argc, char *argv[])
{
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <file.fzn>\n", argv[0]);
        exit(1);
    }

    filename = argv[1];
    yyin = fopen(filename, "r");
    if (yyin == NULL) {
        fprintf(stderr, "cannot open file: '%s'\n", filename);
        exit(1);
    }

    yyparse();
    return 0;
}

int yyerror(char *s)
{
    if (0 == strcmp(yytext, "")) {
        fprintf(stderr,
            "%s:%d: %s before end of file\n", filename, yylineno, s);
    } else {
        fprintf(stderr,
            "%s:%d: %s before '%s'\n", filename, yylineno, s, yytext);
    }
    return 0;
}

/*
** This is only defined so the Flex library isn't needed.
*/
int yywrap()
{
    return 1;
}

