open Ast
open Types

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in 
  let ast = Parser.prog Lexer.read lexbuf in 
  ast
;;

exception NoRuleApplies 

let fetch_value st x = 
  match (topenv st) x with 
  | IVar l
  | BVar l -> getmem st l

(* eval_expr: state -> expr -> memval. Evaluates an expression expr with the current
  state of the program *)
let rec eval_expr (st :state) (e :expr) : memval =   
  match e with
  | True -> Bool true
  | False -> Bool false
  | Var x -> fetch_value st x
  | Const x -> Int x  (* Const is always a Int _ *)
  | Not x -> (
    match eval_expr st x with
    | Bool b -> Bool (not b)
    | _ -> raise (TypeError "Expected boolean in 'Not'")
  )
  | And (e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Bool b1, Bool b2 -> Bool (b1 && b2)
    | _ -> raise (TypeError "Expected booleans in 'And'")
  )
  | Or(e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Bool b1, Bool b2 -> Bool (b1 || b2)
    | _ -> raise (TypeError "Expected booleans in 'Or'")
  )
  | Add (e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Int i1, Int i2 -> Int (i1 + i2)
    | _ -> raise (TypeError "Expected int expressions in 'Add'")
  )
  | Sub (e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Int i1, Int i2 -> Int (i1 - i2)
    | _ -> raise (TypeError "Expected int expressions in 'Sub'")
  )
  | Mul (e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Int n1, Int n2 -> Int (n1 * n2)
    | _ -> raise (TypeError "Expected int expressions in 'Mul'")
  )
  | Eq (e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Int n1, Int n2 -> Bool (n1 = n2)
    | Bool b1, Bool b2 -> Bool (b1 = b2)
    | _ -> raise (TypeError "Mismatched types in 'Eq' expression")
  )
  | Leq (e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Int n1, Int n2 -> Bool (n1 <= n2)
    | _ -> raise (TypeError "Expected integers in 'Leq'")
  )
;;

let bind f x v = fun y -> if x=y then v else f y ;;

(* eval_decl : state -> decl list -> state *)
let eval_decl (st: state) (decls: decl list) : state = 
  
  let rec eval_decl_rec (env, l) decls = 
    match decls with
    | [] -> (env, l)
    | IntVar ide :: tl -> 
      let env' = bind env ide (IVar l) in 
      eval_decl_rec (env', l+1) tl
    | BoolVar ide :: tl -> 
      let env' = bind env ide (BVar l) in 
      eval_decl_rec (env', l+1) tl
  in

  let new_env, new_loc = eval_decl_rec (topenv st, getloc st) decls in 

  make_state (new_env :: getenv st) (getmem st) new_loc
;;

(* trace1 : conf -> conf *)
let rec trace1 = function
| St _ -> raise NoRuleApplies
| Cmd (cmd, state) -> (
  match cmd with
  | Skip -> St state
  | Assign (ide, expr) -> (
    (* Evaluate the expression *)
    let expr_value = eval_expr state expr in 
    let env = topenv state in 
    
    match expr_value, env ide with 
    | Int n, IVar loc -> St (setmem state (bind_mem (getmem state) loc (Int n)))
    | Bool b, BVar loc -> St (setmem state (bind_mem (getmem state) loc (Bool b)))
    | _ -> raise (TypeError "Type mismatch in 'assign' statement")
  )
  | Seq (cmd1, cmd2) -> ( 
    (* Reduce cmd1 *)
    match trace1 (Cmd(cmd1, state)) with
    | Cmd(cmd1', state') -> trace1 (Cmd(Seq(cmd1', cmd2), state'))
    | St state' -> Cmd(cmd2, state')  (* cmd1 done, reduce cmd2 *)
  )
  | If (e0, e1, e2) -> (
    (* e0 must be evaluated to Bool *)
    match eval_expr state e0 with
    | Bool true -> trace1 (Cmd(e1, state))
    | Bool false -> trace1 (Cmd(e2, state))
    | _ -> raise (TypeError "Expected boolean expression in 'if' condition")
  )
  | While(expr, cmd) -> (
    match eval_expr state expr with
    | Bool true -> Cmd(Seq(cmd, While(expr, cmd)), state)
    | Bool false -> St state
    | _ -> raise (TypeError "Expected boolean expression in 'while' condition ")
    )
  | Decl(dl, cmd) -> Cmd(Block(cmd), eval_decl state dl)
  | Block(cmd) -> ( 
    match trace1 (Cmd(cmd, state)) with
    | St st' -> St (setenv st' (popenv st'))
    | Cmd(cmd', st') -> Cmd(Block(cmd'), st')
  )
)
;;

(* trace: int -> cmd -> conf list *)
let trace (n:int) (cmd:cmd) : conf list = 
  
  let rec trace_rec n conf = 
    if n > 0 then
      try
        let new_conf = trace1 conf in 
        conf :: (trace_rec (n-1) new_conf)
      with _ -> [conf]
    else
      [conf]
  in

  trace_rec n (Cmd(cmd, state0))
;;
