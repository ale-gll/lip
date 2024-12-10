open BoolexprLib.Main
open BoolexprLib.Ast

let test_eval expr exp_result =
  (expr |> parse |> eval) = exp_result

(* ### Unit tests for task 4 *)

let%test "test_eval_1" = test_eval "false" false

let%test "test_eval_2" = test_eval "true" true

let%test "test_eval_3" = test_eval "if true then false else true" false

let%test "test_eval_4" = test_eval "if false then false else true" true

let%test "test_eval_5" = test_eval "if true then (if true then false else true) else (if true then true else false)" false

let%test "test_eval_6" = test_eval "if (if false then false else false) then (if false then true else false) else (if true then false else true)" false

let%test "test_eval_7" = test_eval "if (if (if false then false else false) then (if false then true else false) else (if true then false else true)) then (if false then true else false) else (if true then false else true)" false


(* ### Unit tests for task 5 *)

(* 1. trace1 makes progress on any If expression (test at most three); *)

let%test "test_trace1_1" = trace1 (If(True, If(False, True, False), True)) = If(False, True, False)

let%test "test_trace1_2" = trace1 (If(False, True, If(True, True, False))) = If(True, True, False)

let%test "test_trace1_3" = trace1 (If(If(True,False,True),True, False)) = If(False, True, False)

(* If trace1 can't make progress on an expression, then it is a value 
   (Tip: use the is_value predicate from lib/ast.ml); *)

let is_trace1_applicable expr = not(is_value expr)

let%test "test_trace1_cant_progress" = is_trace1_applicable True = false

let%test "trace1_reduces_in_10_steps" =  
  let expr = 
    parse "if (if false then false else false) then (if false then true else false) else (if true then false else true)" 
  in
  
  (* Count the number of iterations *)
  let rec count_trace1_steps e steps =
    match e with
    | If(_,_,_) -> count_trace1_steps (trace1 e) (steps+1)
    | _ -> steps
  in

  (count_trace1_steps expr 0) <= 10