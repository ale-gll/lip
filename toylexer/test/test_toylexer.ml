open Toylexer.Token
open Toylexer.Main


(* let%test "test_frequencies_1" =
  lexer "x=y; x=x+1" |> frequency 3 = [(CTOK "x",3); (ASSIGN,2); (ID "y", 1)] *)

(* YOUR TESTS HERE *)

(* ATOK token testing *)
let%test "test_atok_token" = 
  lexer "String" = [ATOK "String"; EOF]

let%test "test_btok" = 
  lexer "aeiou" = [BTOK "aeiou"; EOF]

let%test "test_ctok" = 
  lexer "bye" = [CTOK "bye"; EOF]

let%test "test_dtok"=
  lexer "3.14; -7.; -.3" = [DTOK "3.14"; SEQ; DTOK "-7."; SEQ; DTOK "-.3"; EOF]

let%test "test_dtok" = 
  lexer "0X9; 0x543" = [ETOK "0X9"; SEQ; ETOK "0x543"; EOF]

