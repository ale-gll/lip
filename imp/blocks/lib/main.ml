open Ast
open Types

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in 
  let ast = Parser.prog Lexer.read lexbuf in 
  ast
;;

exception NoRuleApplies 

(* lookup_env checks if a identifier is present inside the stack of envoirments *)
let rec lookup_env (ide:string) (env : env list) =
  match env with
  | [] -> raise (UnboundVar ide)
  | hd::tl -> try hd ide with _ -> lookup_env ide tl
;;

(* eval_expr: state -> expr -> memval. Evaluates an expression expr with the current
  state of the program *)
let rec eval_expr (st :state) (e :expr) : memval = 
  let env_stack = getenv st in
  let mem = getmem st in (* this is a function. mem: loc -> memval*)
  
  match e with
  | True -> Bool true
  | False -> Bool false
  | Var x -> (
    let env_val = lookup_env x env_stack in
    match env_val with
    | IVar loc 
    | BVar loc -> mem loc
    )
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


(* eval_decl : state -> decl list -> state *)
let eval_decl (st:state) (dl: decl list) : state = 

  let rec eval_decl_rec (env, loc) dl = 
    match dl with
    | [] -> (env, loc)
    | (IntVar ide) :: tl -> 
      let env' = bind_env env ide (IVar loc) in 
      eval_decl_rec (env', loc + 1) tl

    | (BoolVar ide) :: tl ->
      let env' = bind_env env ide (BVar loc) in
      eval_decl_rec (env', loc + 1) tl
  in

  (* Update the env stack and first free location in memory*)
  let updated_env, updated_loc = eval_decl_rec (topenv st, getloc st) dl in 
  
  let new_env_stack = updated_env :: popenv st in 

  (* Create a new state *)
  make_state new_env_stack (getmem st) updated_loc 
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
    
    (* Find the location associated to the variable 'ide' *)
    let curr_env = topenv state in
    let loc, type_of_var = 
      match (try Some (curr_env ide) with _ -> None) with
      | Some (IVar l) -> l, "Int"
      | Some (BVar l) -> l, "Bool"
      | None -> raise (UnboundVar ide)
    in

    (* Check if type is correct *)
    let is_type_correct = 
      match type_of_var, expr_value with
    | "Int", Int _ | "Bool", Bool _ ->  true
    | _, _ -> false
    in

    if is_type_correct then 
      (* Update memory with the new_value*)
      let new_mem = bind_mem (getmem state) loc expr_value in 
      (* Create the new state by setting the new memory *)
      St (setmem state new_mem)
    else 
      raise (TypeError "Mismatched types in assignment")
  )
  | Seq (cmd1, cmd2) -> ( 
    (* Reduce cmd1 *)
    match trace1 (Cmd(cmd1, state)) with
    (* cmd1' is the reduced cmd1 and state' is the possible new state *)
    | Cmd(cmd1', state') -> trace1 (Cmd(Seq(cmd1', cmd2), state'))
    (* cmd1 is done, keep reducing cmd2 *)
    | St state' -> Cmd(cmd2, state')
  )
  | If (e0, e1, e2) -> (
    (* e0 must be evaluated to Bool *)
    match eval_expr state e0 with
    | Bool true -> trace1 (Cmd(e1, state))
    | Bool false -> trace1 (Cmd(e2, state))
    | _ -> raise (TypeError "Expected boolean expression in 'if' condition")
  )
  | While(expr, cmd) ->(
    match eval_expr state expr with
    | Bool true -> Cmd(Seq(cmd, While(expr, cmd)), state)
    | Bool false -> St state
    | _ -> raise (TypeError "Expected boolean expression in 'while' condition ")
    )
  | Decl(dl, cmd) -> (
    (* Update the state with the new variables *)
    let new_state = eval_decl state dl in 
    (* Reduce further with the updated state from the declarations execution *)
    Cmd(cmd, new_state)
    )
  | Block(cmd) -> ( 
    (* Before executing the commands inside the block, it's appropriate to save the environment stack
    to the state it was before the block began. This step should protect the outer scopes.
    This also works for intermediate steps: 
    We continue restoring 'saved_env_stack' to ensure the scope visible to the block remains consistent
    and doesn't leak outside. *)
    let saved_env_stack = getenv state in 

    match trace1 (Cmd(cmd, state)) with
    (* Restore the original env stack before the block was executed *)
    | St final_state -> St (setenv final_state saved_env_stack)
    | Cmd(cmd', state') -> Cmd (Block(cmd'), setenv state' saved_env_stack)
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
