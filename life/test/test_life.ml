open LifeLib.Main

let%test "test_life_parser" = parse "S23/B3" = Life([2;3],[3])

