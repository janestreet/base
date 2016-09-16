open! Import
open! Int

let examples =
    [ -1
    ; 0
    ; 1
    ; 2
    ; 3
    ; 4
    ; 5
    ; 7
    ; 8
    ; 9
    ; 63
    ; 64
    ; 65 ]
;;

let examples_64_bit =
  [ Int.min_value
  ; Int.min_value + 1
  ; Int.max_value - 1
  ; Int.max_value ]
;;

let print_for ints f =
  List.iter ints ~f:(fun i ->
    print_s [%message
      ""
        ~_:(i : int)
        ~_:(Or_error.try_with (fun () -> f i) : int Or_error.t)])
;;

let%expect_test "[floor_log2]" =
  print_for examples floor_log2;
  [%expect {|
    (-1 (Error ("[Int.floor_log2] got invalid input" -1)))
    (0 (Error ("[Int.floor_log2] got invalid input" 0)))
    (1 (Ok 0))
    (2 (Ok 1))
    (3 (Ok 1))
    (4 (Ok 2))
    (5 (Ok 2))
    (7 (Ok 2))
    (8 (Ok 3))
    (9 (Ok 3))
    (63 (Ok 5))
    (64 (Ok 6))
    (65 (Ok 6)) |}];
;;

let%expect_test "[floor_log2]" [@tags "64-bits-only"] =
  print_for examples_64_bit floor_log2;
  [%expect {|
    (-4_611_686_018_427_387_904 (
      Error ("[Int.floor_log2] got invalid input" -4611686018427387904)))
    (-4_611_686_018_427_387_903 (
      Error ("[Int.floor_log2] got invalid input" -4611686018427387903)))
    (4_611_686_018_427_387_902 (Ok 61))
    (4_611_686_018_427_387_903 (Ok 61)) |}];
;;

let%expect_test "[ceil_log2]" =
  print_for examples ceil_log2;
  [%expect {|
    (-1 (Error ("[Int.floor_log2] got invalid input" -1)))
    (0 (Error ("[Int.floor_log2] got invalid input" 0)))
    (1 (Ok 0))
    (2 (Ok 1))
    (3 (Ok 2))
    (4 (Ok 2))
    (5 (Ok 3))
    (7 (Ok 3))
    (8 (Ok 3))
    (9 (Ok 4))
    (63 (Ok 6))
    (64 (Ok 6))
    (65 (Ok 7)) |}];
;;

let%expect_test "[ceil_log2]" [@tags "64-bits-only"] =
  print_for examples_64_bit ceil_log2;
  [%expect {|
    (-4_611_686_018_427_387_904 (
      Error ("[Int.floor_log2] got invalid input" -4611686018427387904)))
    (-4_611_686_018_427_387_903 (
      Error ("[Int.floor_log2] got invalid input" -4611686018427387903)))
    (4_611_686_018_427_387_902 (Ok 62))
    (4_611_686_018_427_387_903 (Ok 62)) |}];
;;
