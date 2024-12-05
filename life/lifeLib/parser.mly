%{
    open Rule
%}

%token <string> DIGIT
%token SURVIVE 
%token BIRTH
%token SLASH
%token EOF

(* The %start declaration says that parsing a "rule" will return an OCaml value of type Main.rule *)
%start <Rule.rule> rule

%% 

(* First production rule *)
rule:
    | SURVIVE; slist = digit_list; SLASH; BIRTH; blist = digit_list; EOF { Life (slist, blist) }

(* Second and final production rule *)
digit_list:
    | i = DIGIT { [int_of_string i] }
    | i = DIGIT; dl = digit_list { (int_of_string i) :: dl }
