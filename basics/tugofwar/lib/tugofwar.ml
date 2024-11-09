(* tokens *)
type token = A | B | X

(** Converts a string into a list of char *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []


(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)             
let toklist_of_string s = 
  let ls = explode s in
  List.filter_map( 
    function
    | 'A' -> Some A
    | 'B' -> Some B
    | '=' -> Some X
    | _ -> None 
  ) ls


(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)

let valid l = 
  let ls_of_a, others = List.partition( fun x -> x = A) l in
  let ls_of_b, ls_of_x = List.partition( fun x -> x = B) others in
  
  l = ls_of_a @ ls_of_x @ ls_of_b
;;
    

(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)

let win l = let countA, countB = 
  List.fold_left( fun a b -> match b with
        A -> (1 + fst a, snd a)
      | B -> (fst a, 1 + snd a)
      | _ -> (fst a, snd a)
    ) (0,0) l in 

if countA > countB then A
else if countB > countA then B else X

(* val string_of_winner : token -> string *)
let string_of_winner w = match w with
| X -> "Draw"
| _ -> "Winner is " ^ (if w = A then "A" else "B")
  
