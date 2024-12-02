open Toyparser.Main

let%test "test_eval_1" = parse "1 + 2 + 3 + (1 + 2)" |> eval = Ok 9

(* YOUR TESTS HERE *)
let%test "test_eval_2" = parse "(1+2)+4" |> eval = Ok 7

let%test "test_eval_3_fails" = 
  parse "(1+2)+3+(4+6)" |> eval = Ok 16

let%test "test_parser" = 
  parse "5+4+1" = Add(Add (Const 5, Const 4), Const 1)