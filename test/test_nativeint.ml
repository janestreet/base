open! Import
open! Nativeint

let%expect_test "hash coherence" =
  check_int_hash_coherence [%here] (module Nativeint);
  [%expect {| |}]
;;

type test_case = nativeint * int32 * int64

let test_cases : test_case list =
  [ 0x0000_0011n, 0x1100_0000l, 0x1100_0000_0000_0000L
  ; 0x0000_1122n, 0x2211_0000l, 0x2211_0000_0000_0000L
  ; 0x0011_2233n, 0x3322_1100l, 0x3322_1100_0000_0000L
  ; 0x1122_3344n, 0x4433_2211l, 0x4433_2211_0000_0000L
  ]
;;

let%expect_test "bswap native" =
  List.iter test_cases ~f:(fun (arg, bswap_int32, bswap_int64) ->
    let result = bswap arg in
    match Sys.word_size_in_bits with
    | 32 -> assert (Int32.equal bswap_int32 (Nativeint.to_int32_trunc result))
    | 64 -> assert (Int64.equal bswap_int64 (Nativeint.to_int64 result))
    | _ -> assert false)
;;

let%expect_test "binary" =
  quickcheck_m
    [%here]
    (module struct
      type t = nativeint [@@deriving quickcheck, sexp_of]
    end)
    ~f:(fun (t : t) -> ignore (Binary.to_string t : string));
  [%expect {| |}]
;;

let test_binary i =
  Binary.to_string_hum i |> print_endline;
  Binary.to_string i |> print_endline;
  print_s [%sexp (i : Binary.t)]
;;

let%expect_test "binary" =
  test_binary 0b01n;
  [%expect {|
    0b1
    0b1
    0b1
    |}];
  test_binary 0b100n;
  [%expect {|
    0b100
    0b100
    0b100
    |}];
  test_binary 0b101n;
  [%expect {|
    0b101
    0b101
    0b101
    |}];
  test_binary 0b101010_10101010n;
  [%expect {|
    0b10_1010_1010_1010
    0b10101010101010
    0b10_1010_1010_1010
    |}];
  test_binary 0b111111_00000000n;
  [%expect {|
    0b11_1111_0000_0000
    0b11111100000000
    0b11_1111_0000_0000
    |}]
;;
