open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* frequency : int -> 'a list -> ('a * int) list *)
let frequency n ls = 
  (* function that counts occurrences of a list element *)
  let count x = 
    List.fold_left( 
      fun a b -> if b=x then a+1 else a
      ) 0 ls 
  in

  (* compare_by_frequency uses the built-in Ocaml compare function 
  to order elements in descending order. 
  If "compare f1 f2" was used, the list would ordered in ascending order *)
  let compare_by_frequency (_,f1) (_,f2) = 
    compare f2 f1
  in

  (* Delete duplicates from list *)
  let no_dup = 
    List.fold_left(
      fun a b -> if List.mem b a then a else a @ [b]
      ) [] ls 
  in

  (* Calculates the occurrences of an element in the main list ls and returns a list of couples *)
  let mapped_list = 
    List.map(fun x -> (x, count x)) no_dup 
  in

  (* Sorts the list by frequency *)
  let sorted = 
    List.sort compare_by_frequency mapped_list 
  in

  (* Returns the first n elements *)
  List.filteri(fun i _ -> i < n) sorted 
;;
