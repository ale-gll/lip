%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token AND
%token OR
%token EOF

%left OR
%left AND (* && operator has higher priority over || operator *)

%start <boolExpr> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | e1 = expr; OR; e2 = expr { If (e1, True, e2) }
  | e1 = expr; AND; e2 = expr { If (e1, e2, False) }
  | LPAREN; e = expr; RPAREN { e }
;

