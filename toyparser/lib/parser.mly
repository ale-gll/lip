%{
open Ast
%}

%token <string> CONST
%token <string> HEX
%token PLUS
%token MINUS
%token UMINUS
%token STAR
%token SLASH
%token LPAREN
%token RPAREN
%token EOF

%left PLUS MINUS
%left STAR SLASH
%right UMINUS

%start <ast> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | n = HEX { Const(int_of_string n) }
  | e1 = expr; PLUS; e2 = expr { Add (e1, e2) }
  | e1 = expr; MINUS; e2 = expr { Sub (e1, e2) }
  | MINUS; e = expr %prec UMINUS { Neg (e) }  (* %prec assigns higher precedence to UMINUS to resolve ambiguity *)
  | e1 = expr; STAR; e2 = expr { Mul (e1,e2) }
  | e1 = expr; SLASH; e2 = expr { Div (e1,e2) }
  | LPAREN; e=expr; RPAREN {e}
;
