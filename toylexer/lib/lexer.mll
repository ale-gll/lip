{
  open Token
}


let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let to_upper = ['A'-'Z']
let lower_vowel = ['a' 'e' 'i' 'o' 'u']
let vowel = ['a' 'e' 'i' 'o' 'u' 'A' 'E' 'I' 'O' 'U']
let consonant = ['b'-'d' 'f'-'h' 'j'-'n' 'p'-'t' 'v'-'z'
                 'B'-'D' 'F'-'H' 'J'-'N' 'P'-'T' 'V'-'Z']
let minus = ['-']
let number = ['0'-'9']
let point = ['.']
let hex_start = "0x" | "0X"
let hex_number = ['0'-'9' 'A'-'F' 'a'-'f']

let regex_atok = to_upper chr*
let regex_btok = lower_vowel+
let regex_ctok = consonant* vowel? consonant*
let regex_dtok = minus? number* (point number*)?
let regex_etok = hex_start hex_number+
let id = letter chr*



rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }  
  | regex_atok { ATOK (Lexing.lexeme lexbuf) }
  | regex_btok { BTOK (Lexing.lexeme lexbuf) }
  | regex_ctok { CTOK (Lexing.lexeme lexbuf) }
  | regex_dtok { DTOK (Lexing.lexeme lexbuf) }
  | regex_etok { ETOK (Lexing.lexeme lexbuf) }
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }    
  | eof { EOF }

