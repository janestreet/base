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

let%test_module "int_math" =
  (module struct

    let test_cases () =
      let cases = [ 0xAA; 0xAA_AA; 0xAA_AA_AA;  0x80; 0x80_08; 0x80_00_08; ]
      in
      match Word_size.word_size with
      | W64 -> (* create some >32 bit values... *)
        (* We can't use literals directly because the compiler complains on 32 bits. *)
        let cases = cases @ [ (0xAA_AA lsl 16) lor 0xAA_AA;
                              (0x80_00 lsl 16) lor 0x00_08; ] in
        let added_cases = List.map cases ~f:(fun x -> x lsl 16) in
        List.concat [ cases; added_cases ]
      | W32 -> cases
    ;;

    let%test_unit "ceil_pow2" =
      List.iter (test_cases ())
        ~f:(fun x -> let p2 = ceil_pow2 x in
             assert( (is_pow2 p2) && (p2 >= x && x >= (p2 / 2)) )
           )
    ;;

    let%test_unit "floor_pow2" =
      List.iter (test_cases ())
        ~f:(fun x -> let p2 = floor_pow2 x in
             assert( (is_pow2 p2) && ((2 * p2) >= x && x >= p2) )
           )
    ;;
  end)
