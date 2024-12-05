{
    open Parser
}

(* Only digits *)
let num = ['0'-'9']

rule read_token =
    parse
    | "S" { SURVIVE }
    | "B" { BIRTH }
    | "/" { SLASH }
    | num { DIGIT (Lexing.lexeme lexbuf) }
    | eof { EOF }
