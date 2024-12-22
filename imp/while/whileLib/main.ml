open Ast
open Types
open Prettyprint

let parse (s:string) : cmd = 
  let lexbuf = Lexing.from_string s in 
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;


exception UnboundVar of string
exception NoRuleApplies
exception TypeError of string
exception NotANaturalNumber 

(* Implementation of the big step semantics *)
let rec eval_expr (st:state) (e:expr) : exprval =
  match e with
  | True -> Bool true
  | False -> Bool false
  | Var v -> st v
  | Const n -> Nat n

  | Not (expr) ->
    (match eval_expr st expr with
    | Bool b -> Bool (not b)
    | _ -> raise (TypeError "Expected boolean expression")
  )
  | And(e1, e2) ->
    (match eval_expr st e1, eval_expr st e2 with
    | Bool b1, Bool b2 -> Bool (b1 && b2)
    | _ -> raise (TypeError "Expected boolean expression")
  )
  | Or(e1, e2) -> 
    (match eval_expr st e1, eval_expr st e2 with
    | Bool b1, Bool b2 -> Bool (b1 || b2)
    | _ -> raise (TypeError "Expected boolean expressions")
  )
  | Add(e1, e2) ->
    (match eval_expr st e1, eval_expr st e2 with
    | Nat n1, Nat n2 -> Nat (n1 + n2)
    | _ -> raise (TypeError "Expected natural numbers expressions")
  )
  | Sub(e1, e2) ->
   (match eval_expr st e1, eval_expr st e2 with
    | Nat n1, Nat n2 -> Nat (n1 - n2)
    | _ -> raise (TypeError "Expected natural numbers expressions")
  )
  | Mul (e1, e2) ->
    (match eval_expr st e1, eval_expr st e2 with
     | Nat n1, Nat n2 -> Nat (n1 * n2)
     | _ -> failwith ("TypeError: Expected integer expressions")
  )
  | Eq (e1, e2) -> 
    (match eval_expr st e1, eval_expr st e2 with
    | Nat n1, Nat n2 -> Bool (n1 = n2)
    | Bool b1, Bool b2 -> Bool (b1 = b2)
    | _ -> raise (TypeError "Mismatched types in equality check")
  )
  | Leq (e1, e2) -> 
    (match eval_expr st e1, eval_expr st e2 with
    | Nat n1, Nat n2 -> Bool (n1 <= n2)
    | _ -> raise (TypeError "Expected integer expressions")
    )
  ;;

(* Update the state of the program binding id to a new value 
  Note: state = ide -> exprval is a function that maps a ide to a value, that's why st is applied to y *)
let bind (st:state) (id:ide) (e:exprval) : state = 
  fun y -> if id = y then e else st y

(* trace1 implements the small-step semantics 
  trace1: conf -> conf *)   
let rec trace1 = function
| St _ -> raise NoRuleApplies
| Cmd (cmd, st) ->
  (match cmd with
  | Skip -> St st  (* with the skip command, the system doesn't do anything and the state doesn't change *)
  | Assign(s, e) ->
    let value = eval_expr st e in 
    let new_state = bind (st) s value in 
    St new_state

  | Seq(c1, c2) -> 
    (match trace1 (Cmd(c1,st)) with
    | Cmd(c1', st') -> Cmd(Seq(c1', c2), st') (* update sequence with the reduced c1 *)
    | St st' -> Cmd(c2, st')    (* c1 is done *)
  )
  | If(e0, c1, c2) ->
    (match eval_expr st e0 with
    | Bool true -> Cmd (c1, st) 
    | Bool false -> Cmd (c2, st)
    | _ -> raise (TypeError (string_of_expr e0 ^ "must be a Bool expression"))
    )
  | While(e, c1) ->
    (match eval_expr st e with
    | Bool true -> Cmd(Seq(c1, While(e, c1)), st)
    | Bool false -> St st
    | _ -> raise (TypeError (string_of_expr e ^ "must be a Bool expression"))
  )
)


(* x identifier not defined *)
let empty_state = fun x-> raise (UnboundVar x)

let rec trace_rec n conf =
  (* when n>0 reduce further *)
  if n > 0 then
    try
      let conf' = trace1 conf in
      conf :: (trace_rec (n-1) conf')
    with NoRuleApplies -> [conf]
  else
    [conf]


let trace n cmd = trace_rec n (Cmd(cmd, empty_state))



