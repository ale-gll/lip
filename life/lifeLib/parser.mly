%{
    open Rule

    (* Given two integers, returns the range [i,..,j] *)
    let rec expand_range (i:int) (j:int) = 
        if i>j then [] else i::expand_range (i+1) j

%}

%token <string> DIGIT
%token SURVIVE 
%token BIRTH
%token SLASH
%token EXT
%token COMMA
%token RANGE
%token EOF

(* The %start declaration says that parsing a "rule" will return an OCaml value of type Main.rule *)
%start <Rule.rule> rule

%% 

(* First production rule *)
rule:
    | SURVIVE; s = digit_list; SLASH; BIRTH; b = digit_list; EOF
    { Life (s, b) } (* Not using extended S/B rules *)

    | EXT; SURVIVE; s = extended_digit_list; SLASH; BIRTH; b = extended_digit_list; EOF 
    { Life (s, b) } (* S and B not opt *)

    | EXT; s = extended_digit_list; SLASH; BIRTH; b = extended_digit_list; EOF 
    { Life (s, b) } (* S opt *)

    | EXT; SURVIVE; s = extended_digit_list; SLASH; b = extended_digit_list; EOF 
    { Life (s, b) } (* B opt *)

    | EXT; s = extended_digit_list; SLASH; b = extended_digit_list; EOF 
    { Life (s, b) } (* S and B opt *)



(* Second production rule *)
digit_list:
    | i = DIGIT { [int_of_string i] }
    | i = DIGIT; dl = digit_list { [int_of_string i] @ dl }

extended_digit_list:
    | i = DIGIT { [int_of_string i] }
    | i = DIGIT; RANGE; j = DIGIT { expand_range (int_of_string i) (int_of_string j) }
    | l1 = extended_digit_list; COMMA; l2 = extended_digit_list { l1 @ l2 }
