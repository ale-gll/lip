module T = ANSITerminal
open LifeLib.Main
open LifeLib.Rule

let rule_s23b3 = Life([2;3], [3])

(* Function to read from stdin and parse the rule *)
let parse_from_stdin () =
  let input = read_line () in (* Read from stdin *)
  let rule = parse input in
  Printf.printf "Life([%s], [%s])\n"
    (String.concat ", " (List.map string_of_int (fst (extract_life rule))))
    (String.concat ", " (List.map string_of_int (snd (extract_life rule))))


let _ = match Array.length(Sys.argv) with
  | _ when Sys.argv.(1) = "parse" ->
    parse_from_stdin () 
  | 2 -> let k = int_of_string (Sys.argv.(1)) in  (* Sys.argv(0): program name *)
    T.erase T.Screen;
    T.save_cursor();
    Random.self_init();
    let w = loop init_w k rule_s23b3 in
    display w;
    ignore(read_line());
    T.restore_cursor();
    print_newline()
     
   | 3 -> let rule_string = Sys.argv.(1) in
    let rule = parse rule_string in 
    let k = int_of_string (Sys.argv.(2)) in
    T.erase T.Screen;
    T.save_cursor();
    Random.self_init();
    let w = loop init_w k rule in
    display w;
    ignore(read_line());
    T.restore_cursor();
    print_newline()
  (* wrong usage *)
  | _ -> failwith "Usage: dune exec life <S/B rule> <n_rounds>"
