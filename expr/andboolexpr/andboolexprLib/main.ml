open Ast

let rec string_of_boolexpr = function
  | True -> "True"
  | False -> "False" 
  | Not e -> "not" ^ (string_of_boolexpr e)
  | And (e0, e1) -> (string_of_boolexpr e0) ^ "and" ^ (string_of_boolexpr e1)
  | Or (e0, e1) -> (string_of_boolexpr e0) ^ "or" ^ (string_of_boolexpr e1)
  | If(e0, e1, e2) -> "If(" ^ (string_of_boolexpr e0) ^ ", " ^ (string_of_boolexpr e1) ^ ", " ^ (string_of_boolexpr e2) ^ ")"


let parse (s : string) : boolExpr =
  let lexbuf = Lexing.from_string s in 
  let ast = Parser.prog Lexer.read lexbuf in 
  ast


exception NoRuleApplies

let rec trace1 = function
  | Not e -> 
    (match e with 
    | True -> False
    | False -> True
    | _ -> Not (trace1 e)   
    )
    | And (e1, e2) -> 
    (match e1 with
      | True -> trace1 e2
      | False -> False
      | _ -> And (trace1 e1, e2))
    
    | Or (e1, e2) ->
    (match e1 with
      | True -> True
      | False -> trace1 e2
      | _ -> Or (trace1 e1, e2))
  
    | If (True, e1, _) -> e1
    | If (False, _, e2) -> e2
    | If (e0, e1, e2) -> If (trace1 e0, e1, e2)
  | _ -> raise NoRuleApplies 


let rec trace expr = 
  try 
    let e = trace1 expr in
    expr::(trace e)
  with NoRuleApplies -> [expr]


(* Eval implements the big step semantics *)
let rec eval = function
  True -> true
  | False -> false
  | Not (e) -> not (eval e)
  | And (e1, e2) -> if eval e1 then eval e2 else false
  | Or (e1, e2) -> if eval e1 then true else eval e2
  | If(e0,e1,e2) ->
    ( match eval e0 with
    | true -> eval e1
    | false -> eval e2
    )
