%{
open Ast
%}

(* type expr tokens *)
%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token PLUS
%token MINUS
%token MUL
%token EQ
%token LEQ
%token <string> ID
%token <string> CONST

(*type decl tokens*)
%token INT 
%token BOOL

(* type cmd tokens *)
%token SKIP
%token TAKES
%token SEQ
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO

%token LPAREN
%token RPAREN
%token LBRACE 
%token RBRACE
%token EOF

(* Precedence rules *)
%left SEQ
%nonassoc ELSE DO
%left OR
%left AND
%nonassoc NOT
%left EQ LEQ
%left PLUS MINUS
%left MUL

%start <cmd> prog

%%

(* Program *)
prog:
  | c = cmd; EOF { c }

(* Expressions *)
expr:
  | x = ID { Var(x) }
  | n = CONST { Const(int_of_string n) }
  | TRUE { True }
  | FALSE { False }
  | NOT; e = expr { Not e }
  | e1 = expr; AND; e2 = expr { And(e1, e2) }
  | e1 = expr; OR; e2 = expr { Or(e1, e2) }
  | e1 = expr; PLUS; e2 = expr { Add(e1, e2) }
  | e1 = expr; MINUS; e2 = expr { Sub(e1, e2) }
  | e1 = expr; MUL; e2 = expr { Mul(e1, e2) }
  | e1 = expr; EQ; e2 = expr { Eq(e1, e2) }
  | e1 = expr; LEQ; e2 = expr { Leq(e1, e2) }
  | LPAREN; e = expr; RPAREN { e }

(* Declarations *)
decl:
    | INT; ide = ID { IntVar ide }
    | BOOL; ide = ID { BoolVar ide }

decl_list:
    | d = decl; SEQ; d1 = decl_list { d :: d1 }  /* $1; SEQ; $3 */
    | { [] }

(* Commands *)
cmd:
  | SKIP { Skip }
  | IF; e = expr; THEN; c1 = cmd; ELSE; c2 = cmd { If(e, c1, c2) }
  | WHILE; e = expr; DO; c = cmd { While(e, c) }
  | x = ID; TAKES; e=expr { Assign(x, e) }
  | c1 = cmd; SEQ; c2 = cmd { Seq(c1, c2) }
  | LBRACE; dl = decl_list; c = cmd; RBRACE { Decl(dl, c) }

