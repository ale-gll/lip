open Toyparser.Main

let%test "test_eval_1" = parse "1 + 2 + 3 + (1 + 2)" |> eval = Ok 9

(* YOUR TESTS HERE *)
let%test "test_eval_2" = parse "(1+2)+4" |> eval = Ok 7

let%test "test_eval_3" = parse "(1+2)+3+(4+6)" |> eval = Ok 16

let%test "test_eval_4" = parse "(2 * 7) - 10" |> eval = Ok 4

let%test "test_eval_5" = parse "(3 / 3) + 5 * 3 - (1 + 2)" |> eval = Ok 13

let%test "test_eval_6" = parse "-1 - 2 - -3" |> eval = Ok 0

let%test "test_eval_7" = parse "0X1F + 5" |> eval = Ok 36

(* AST tree testing *)

let%test "test_ast_w_addition" = parse "5+4+1" = Add(Add (Const 5, Const 4), Const 1)

let%test "test_ast_w_subtraction" = parse "1 + 2 - 3" = Sub( Add(Const 1, Const 2), Const 3)

let%test "test_ast_w_multiplication" = parse "1 - (2 * 4)" = Sub (Const 1, Mul (Const 2, Const 4))

let%test "test_ast_w_division" = parse "(20 / 4) + (5 * 2)" = Add ( Div (Const 20, Const 4), Mul (Const 5, Const 2))

let%test "test_division_by_zero" = parse "(4+5) + 7/0" |> eval = Error "Error: tried to divide 7 by zero"

let%test "test_ast_unary_minus" = parse "-5 * (-3)" = Mul (Neg (Const 5), Neg (Const 3))

let%test "test_ast_hex_numbers" = parse "0x01 + 4 / 2" = Add (Const 0x01, Div (Const 4, Const 2))

