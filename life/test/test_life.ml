open LifeLib.Main

let%test "test_life_parser" = parse "S23/B3" = Life([2;3],[3])

let%test "test_extended_SB_rules1" = parse "E2..5,9/B1..4" = Life([2;3;4;5;9], [1;2;3;4]) 

let%test "test_extended_SB_rules2" = parse "ES2..5,7..12/2,3" = Life([2;3;4;5;7;8;9], [2;3])