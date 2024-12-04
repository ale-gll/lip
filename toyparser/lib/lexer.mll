{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let hex_num = '0' ['x' 'X'] ['0'-'9' 'a'-'f' 'A'-'F']+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { SLASH }
  | hex_num { HEX (Lexing.lexeme lexbuf)}
  | num { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }

