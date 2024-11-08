(** Language recognizer for regex "[01]+" *)
let rec lang1 l = match l with 
| [x] when x='1' || x='0' -> true
| hd::tl when hd=='0' || hd=='1' -> lang1 tl
| _ -> false;;

(** Language recognizer for regex "0?1*" *)
let lang2 l = 
  fst (
    List.fold_left(fun a b -> 
        if snd a == 0 && b=='0' then (fst a, 1)
        else if snd a == 0 && b=='1' then (fst a, 1)
        else if snd a == 1 && b=='1' then (fst a, snd a)
        else (false, snd a)) (true, 0) l
        )
;; 

(** Language recognizer for regex "0[01]*0". Recursive function called by "lang3" *)
let lang3 l = match l with
  |['0'] -> false   (* Only has one 0 *)
  | '0'::tl -> (match List.rev tl with
                |'0'::_ -> true   (* Ends with 0 *)
                | _ -> false)     
  | _ -> false  (* Doesn't start with 0*)
;;

          
(** Language recognizer for regex "0*10*10*" *)
let rec lang4_rec l i = match l with
  | [x] when i==2 && x=='0' -> true
  | [x] when i==1 && x=='1' -> true
  | '1':: tl -> lang4_rec tl (i+1)
  | '0':: tl -> lang4_rec tl i
  | _ -> false
;;

let lang4 l = lang4_rec l 0;;


(** Language recognizer for regex '(00|11)+' *) 
let rec lang5 l = match l with 
  | hd::[el] when hd==el && (hd=='0' || hd=='1') -> true
  | '0'::'0'::tl -> lang5 tl
  | '1'::'1'::tl -> lang5 tl 
  | _ -> false
;; 
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers;;
  
