{
    open Parser
}

(* Only digits *)
let white = [' ' '\t']+
let digit = ['0'-'9']

rule read_token =
    parse
    | white { read_token lexbuf }   (* ignores any white space *)
    | "E" { EXT }
    | "S" { SURVIVE }
    | "B" { BIRTH }
    | "/" { SLASH }
    | "," { COMMA }
    | ".." { RANGE }
    | digit { DIGIT (Lexing.lexeme lexbuf) } (* captures the string that the lexer matched in the input *)
    | eof { EOF }
