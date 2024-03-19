open! Import
open! Sign_or_nan

let%test "of_int" = of_int 37 = Pos && of_int (-22) = Neg && of_int 0 = Zero

let%expect_test ("hash coherence" [@tags "64-bits-only"]) =
  check_hash_coherence [%here] (module Sign_or_nan) all;
  [%expect {| |}]
;;

let%expect_test "to_string_hum" =
  List.iter all ~f:(fun t ->
    let string = to_string_hum t in
    print_endline string;
    match to_sign_exn t with
    | exception _ -> ()
    | sign -> require_equal [%here] (module String) string (Sign.to_string_hum sign));
  [%expect {|
    negative
    zero
    positive
    not-a-number
    |}]
;;
