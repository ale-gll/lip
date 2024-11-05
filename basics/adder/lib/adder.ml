(* val addlist : int list -> int *)
(* addlist l adds the element of the list of integers l *)

let addlist l = List.fold_left( fun a b -> a + b ) 0 l;; (* replace 0 with actual code *)