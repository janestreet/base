open! Import
open! Int63

let%expect_test ("hash coherence" [@tags "64-bits-only"]) =
  check_int_hash_coherence [%here] (module Int63);
  [%expect {| |}]
;;

let%test_unit _ = [%test_result: t] max_value ~expect:(of_int64_exn 4611686018427387903L)

let%test_unit _ =
  [%test_result: t] min_value ~expect:(of_int64_exn (-4611686018427387904L))
;;

let%test_unit _ =
  [%test_result: t] (of_int32_exn Int32.min_value) ~expect:(of_int32 Int32.min_value)
;;

let%test_unit _ =
  [%test_result: t] (of_int32_exn Int32.max_value) ~expect:(of_int32 Int32.max_value)
;;

let%test "typical random 0" = Exn.does_raise (fun () -> random zero)

let%test_module "Overflow_exn" =
  (module struct
    open Overflow_exn

    let%test_module "( + )" =
      (module struct
        let test t = Exn.does_raise (fun () -> t + t)
        let%test "max_value / 2 + 1" = test (succ (max_value / of_int 2))
        let%test "min_value / 2 - 1" = test (pred (min_value / of_int 2))
        let%test "min_value + min_value" = test min_value
        let%test "max_value + max_value" = test max_value
      end)
    ;;

    let%test_module "( - )" =
      (module struct
        let%test "min_value -  1" = Exn.does_raise (fun () -> min_value - one)
        let%test "max_value - -1" = Exn.does_raise (fun () -> max_value - neg one)

        let%test "min_value / 2 - max_value / 2 - 2" =
          Exn.does_raise (fun () ->
            (min_value / of_int 2) - (max_value / of_int 2) - of_int 2)
        ;;

        let%test "min_value - max_value" =
          Exn.does_raise (fun () -> min_value - max_value)
        ;;

        let%test "max_value - min_value" =
          Exn.does_raise (fun () -> max_value - min_value)
        ;;

        let%test "max_value - -max_value" =
          Exn.does_raise (fun () -> max_value - neg max_value)
        ;;
      end)
    ;;

    let is_overflow = Exn.does_raise

    let%test_module "( * )" =
      (module struct
        let%test "1 * 1" = one * one = one
        let%test "1 * 0" = one * zero = zero
        let%test "0 * 1" = zero * one = zero
        let%test "min_value * -1" = is_overflow (fun () -> min_value * neg one)
        let%test "-1 * min_value" = is_overflow (fun () -> neg one * min_value)

        let%test "46116860184273879 * 100" =
          of_int64_exn 46116860184273879L * of_int 100 = of_int64_exn 4611686018427387900L
        ;;

        let%test "46116860184273879 * 101" =
          is_overflow (fun () -> of_int64_exn 46116860184273879L * of_int 101)
        ;;
      end)
    ;;

    let%test_module "( / )" =
      (module struct
        let%test "1 / 1" = one / one = one
        let%test "min_value / -1" = is_overflow (fun () -> min_value / neg one)
        let%test "min_value / 1" = min_value / one = min_value
        let%test "max_value / -1" = max_value / neg one = min_value + one
      end)
    ;;
  end)
;;

let%expect_test "[floor_log2]" =
  let floor_log2 t = print_s [%sexp (floor_log2 t : int)] in
  show_raise (fun () -> floor_log2 zero);
  [%expect {| (raised ("[Int.floor_log2] got invalid input" 0)) |}];
  floor_log2 one;
  [%expect {| 0 |}];
  for i = 1 to 8 do
    floor_log2 (i |> of_int)
  done;
  [%expect {|
    0
    1
    1
    2
    2
    2
    2
    3
    |}];
  floor_log2 ((one lsl 61) - one);
  [%expect {| 60 |}];
  floor_log2 (one lsl 61);
  [%expect {| 61 |}];
  floor_log2 max_value;
  [%expect {| 61 |}]
;;

let%expect_test "binary" =
  quickcheck_m
    [%here]
    (module struct
      type t = int64 [@@deriving quickcheck, sexp_of]
    end)
    ~f:(fun int64 -> ignore (Binary.to_string (of_int64_trunc int64) : string));
  [%expect {| |}]
;;

let test_binary i =
  let i = of_int64_exn i in
  Binary.to_string_hum i |> print_endline;
  Binary.to_string i |> print_endline;
  print_s [%sexp (i : Binary.t)]
;;

let%expect_test ("binary emulation" [@tags "js-only"]) =
  test_binary 0b1L;
  [%expect {|
    0b1
    0b1
    0b1
    |}];
  test_binary 0b0L;
  [%expect {|
    0b0
    0b0
    0b0
    |}]
;;

let%expect_test "binary" =
  test_binary 0b01L;
  [%expect {|
    0b1
    0b1
    0b1
    |}];
  test_binary 0b100L;
  [%expect {|
    0b100
    0b100
    0b100
    |}];
  test_binary 0b101L;
  [%expect {|
    0b101
    0b101
    0b101
    |}];
  test_binary 0b10_1010_1010_1010L;
  [%expect {|
    0b10_1010_1010_1010
    0b10101010101010
    0b10_1010_1010_1010
    |}];
  test_binary 0b11_1111_0000_0000L;
  [%expect {|
    0b11_1111_0000_0000
    0b11111100000000
    0b11_1111_0000_0000
    |}]
;;
