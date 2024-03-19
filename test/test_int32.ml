open! Import
open! Int32

let%expect_test "hash coherence" =
  check_int_hash_coherence [%here] (module Int32);
  [%expect {| |}]
;;

let numbers = [ 0x10_20l; 0x11_22_33l; 0x11_22_33_1Fl; 0x11_22_33_44l ]
let test = test_conversion ~to_string:(fun x -> Int32.Hex.to_string_hum x)

let%expect_test "bswap16" =
  List.iter numbers ~f:(test bswap16);
  [%expect
    {|
    0x1020 --> 0x2010
    0x11_2233 --> 0x3322
    0x1122_331f --> 0x1f33
    0x1122_3344 --> 0x4433
    |}]
;;

let%expect_test "bswap32" =
  List.iter numbers ~f:(test bswap32);
  [%expect
    {|
    0x1020 --> 0x2010_0000
    0x11_2233 --> 0x3322_1100
    0x1122_331f --> 0x1f33_2211
    0x1122_3344 --> 0x4433_2211
    |}]
;;

let%expect_test "binary" =
  quickcheck_m
    [%here]
    (module struct
      type t = int32 [@@deriving quickcheck, sexp_of]
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
  test_binary 0b01l;
  [%expect {|
    0b1
    0b1
    0b1
    |}];
  test_binary 0b100l;
  [%expect {|
    0b100
    0b100
    0b100
    |}];
  test_binary 0b101l;
  [%expect {|
    0b101
    0b101
    0b101
    |}];
  test_binary 0b10_1010_1010_1010l;
  [%expect {|
    0b10_1010_1010_1010
    0b10101010101010
    0b10_1010_1010_1010
    |}];
  test_binary 0b11_1111_0000_0000l;
  [%expect {|
    0b11_1111_0000_0000
    0b11111100000000
    0b11_1111_0000_0000
    |}]
;;
