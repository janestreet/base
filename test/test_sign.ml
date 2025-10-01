open! Import
open! Sign

let%test _ = of_int 37 = Pos
let%test _ = of_int (-22) = Neg
let%test _ = of_int 0 = Zero
let%test _ = Int.( = ) (to_int Neg) (-1)
let%test _ = Int.( = ) (to_int Zero) 0
let%test _ = Int.( = ) (to_int Pos) 1

let%test_unit "( * )" =
  List.cartesian_product all all
  |> List.iter ~f:(fun (s1, s2) ->
    [%test_result: int] (to_int (s1 * s2)) ~expect:(Int.( * ) (to_int s1) (to_int s2)))
;;

let%expect_test ("hash coherence" [@tags "64-bits-only"]) =
  check_hash_coherence [%here] (module Sign) all;
  [%expect {| |}]
;;
