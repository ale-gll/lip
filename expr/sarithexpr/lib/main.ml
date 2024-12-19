open Ast

let rec string_of_expr = function
  | True -> "True"
  | False -> "False" 
  | Not e -> "not" ^ (string_of_expr e)
  | And (e0, e1) -> (string_of_expr e0) ^ "and" ^ (string_of_expr e1)
  | Or (e0, e1) -> (string_of_expr e0) ^ "or" ^ (string_of_expr e1)
  | If(e0, e1, e2) -> "If(" ^ (string_of_expr e0) ^ ", " ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
  | Zero -> "0"
  | Succ e -> "succ" ^ (string_of_expr e)
  | Pred e -> "pred" ^ (string_of_expr e)
  | IsZero e -> "iszero" ^ (string_of_expr e)


let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in 
  let ast = Parser.prog Lexer.read lexbuf in 
  ast


exception NoRuleApplies

(* Small step semantics implementation *)
let rec trace1 = function
(* Boolean negation *)
  | Not e ->
    (match e with
    | True -> False
    | False -> True
    | Zero | Succ _ | Pred _ -> raise NoRuleApplies
    | _ -> Not (trace1 e)
    )
  
  (* Conjunction *)
  | And (e1, e2) -> 
    (match e1,e2 with
    | True, _ -> trace1 e2
    | False, _ -> False
    | (Zero | Succ _ | Pred _), _ -> raise NoRuleApplies
    | _, (Zero | Succ _ | Pred _) -> raise NoRuleApplies
    | _ -> And (trace1 e1, e2))

  (* Disjunction *)
  | Or (e1, e2) ->
    (match e1,e2 with
    | True, _ -> True
    | False, _ -> trace1 e2
    | (Zero | Succ _ | Pred _), _ -> raise NoRuleApplies
    | _, (Zero | Succ _ | Pred _) -> raise NoRuleApplies
    | _ -> Or(trace1 e1, e2))
  
  (*Conditionals*)
  | If(e0,e1,e2) -> 
    (match e0 with
    | True -> e1
    | False -> e2
    | Zero | Succ _ | Pred _ -> raise NoRuleApplies
    | _ -> If(trace1 e0, e1, e2))

  (* Successor of a natural number *)
  | Succ e ->
    (match trace1 e with
    | Zero | Succ _ | Pred _ -> Succ (trace1 e)
    | _ -> raise NoRuleApplies)

  (*Predecessor of a natural number *)
  | Pred e ->
    (match e with
    | Zero | True | False -> raise NoRuleApplies
    | Succ e' -> e'
    | _ -> Pred (trace1 e))
  
  (* Check if zero *)
  | IsZero e ->
    (match e with
    | Zero -> True
    | Succ _ -> False
    | True | False -> raise NoRuleApplies
    | _ -> IsZero (trace1 e))

  | _ -> raise NoRuleApplies 


let rec trace expr = 
  try 
    let e = trace1 expr in
    expr::(trace e)
  with NoRuleApplies -> [expr]
;;


(* Eval implements the big step semantics *)
let rec eval = function
  | True -> Bool true
  | False -> Bool false

  (*Boolean negation*)
  | Not (e) -> 
    (match eval e with 
    | Bool b -> Bool (not b)
    | _ -> raise (Invalid_argument "'Not' expects a boolean expression")
    )

  (* Boolean conjunction *)
  | And (e1, e2) -> 
    (match eval e1 with
    | Bool b -> if b then eval e2 else Bool false
    | _ -> raise (Invalid_argument "'And' expects a boolean expression")
    )
  
  (* Boolean disjunction *)
  | Or (e1, e2) -> 
    (match eval e1 with 
    | Bool b -> if b then Bool true else eval e2
    | _ -> raise (Invalid_argument "'Or' expects a boolean expression")
    )
  
  (* If statements *)
  | If(e0,e1,e2) ->
    ( match eval e0 with
    | Bool true -> eval e1
    | Bool false -> eval e2
    | _ -> raise (Invalid_argument "If condition must evaluate to a Bool")
    )

  (* zero and successor of a natural number *)
  | Zero -> Nat 0
  | Succ e ->
    (match eval e with
    | Nat n -> Nat (1 + n)
    | _ -> raise (Invalid_argument "Succ expects a natural number")
    )

  (* Predecessor of a number *)
  | Pred Zero -> raise (Invalid_argument "Pred cannot be applied to zero")
  | Pred (Succ e) -> eval e
  | Pred e -> 
    (match eval e with
    | Nat 0 -> raise (Invalid_argument "Pred cannot be applied to zero")
    | Nat n -> Nat (n-1)
    | _ -> raise (Invalid_argument "Pred expects a natural number")
    )

  (* Check if zero *)
  | IsZero Zero -> Bool true
  | IsZero e -> 
    (match eval e with 
    | Nat n -> Bool (n=0)
    | _ -> raise (Invalid_argument "IsZero expects a natural number")
    )


let string_of_type = function
| BoolT -> "BoolT" 
| NatT -> "NatT"

exception TypeError of string

(* typecheck e : expr -> exprtype *)
let rec typecheck = function
  | True | False -> BoolT
  | Zero -> NatT

  | Not e ->
    (match typecheck e with
    | BoolT -> BoolT
    | _ -> raise (TypeError (string_of_expr e ^ "has type Nat, but type Bool was expected")))

  | And (e1,e2) ->
    (match typecheck e1, typecheck e2 with
    | BoolT, BoolT -> BoolT
    | BoolT, NatT -> raise (TypeError (string_of_expr e2 ^ "has type Nat, but type Bool was expected"))
    | NatT, BoolT -> raise (TypeError (string_of_expr e1 ^ "has type Nat, but type Bool was expected"))
    | _ -> raise (TypeError "And expects a (Bool, Bool) expression"))

  | Or (e1, e2) -> 
    (match typecheck e1, typecheck e2 with
    | BoolT, BoolT -> BoolT
    | BoolT, NatT -> raise (TypeError (string_of_expr e2 ^ "has type Nat, but type Bool was expected"))
    | NatT, BoolT -> raise (TypeError (string_of_expr e1 ^ "has type Nat, but type Bool was expected"))
    | _ -> raise (TypeError "Or expects a (Bool, Bool) expression"))

  | If(e0, e1, e2) ->
    (let t0 = typecheck e0 in
    let t1 = typecheck e1 in
    let t2 = typecheck e2 in

    match t0 with
    | BoolT -> 
      if t1 = t2 
      then t1 
      else raise (TypeError (string_of_expr e1 ^ "and" ^ string_of_expr e2 ^ "do not have the same type"))
    | _ -> raise (TypeError (string_of_expr e0 ^ "has type Nat, but type Bool was expected"))
  )
  | Succ e->
    (match typecheck e with
    | NatT -> NatT
    | _ -> raise (TypeError (string_of_expr e ^ "has type Bool, but type Nat was expected")))

  | Pred e ->
    (match typecheck e with
    | NatT -> NatT
    | _ -> raise (TypeError (string_of_expr e ^ "has type Bool, but type Nat was expected")))

  | IsZero e -> 
    (match typecheck e with
    | NatT -> BoolT
    | _ -> raise (TypeError (string_of_expr e ^ "has type Bool, but type Nat was expected")))
