open! Import
open! Int64

let%expect_test "hash coherence" =
  check_int_hash_coherence [%here] (module Int64);
  [%expect {| |}]
;;

let numbers =
  [ 0x0000_0000_0000_1020L
  ; 0x0000_0000_0011_2233L
  ; 0x0000_0000_1122_3344L
  ; 0x0000_0011_2233_4455L
  ; 0x0000_1122_3344_5566L
  ; 0x0011_2233_4455_6677L
  ; 0x1122_3344_5566_7788L
  ]
;;

let test = test_conversion ~to_string:Int64.Hex.to_string_hum

let%expect_test "bswap16" =
  List.iter numbers ~f:(test bswap16);
  [%expect
    {|
    0x1020 --> 0x2010
    0x11_2233 --> 0x3322
    0x1122_3344 --> 0x4433
    0x11_2233_4455 --> 0x5544
    0x1122_3344_5566 --> 0x6655
    0x11_2233_4455_6677 --> 0x7766
    0x1122_3344_5566_7788 --> 0x8877
    |}]
;;

let%expect_test "bswap32" =
  List.iter numbers ~f:(test bswap32);
  [%expect
    {|
    0x1020 --> 0x2010_0000
    0x11_2233 --> 0x3322_1100
    0x1122_3344 --> 0x4433_2211
    0x11_2233_4455 --> 0x5544_3322
    0x1122_3344_5566 --> 0x6655_4433
    0x11_2233_4455_6677 --> 0x7766_5544
    0x1122_3344_5566_7788 --> 0x8877_6655
    |}]
;;

let%expect_test "bswap48" =
  List.iter numbers ~f:(test bswap48);
  [%expect
    {|
    0x1020 --> 0x2010_0000_0000
    0x11_2233 --> 0x3322_1100_0000
    0x1122_3344 --> 0x4433_2211_0000
    0x11_2233_4455 --> 0x5544_3322_1100
    0x1122_3344_5566 --> 0x6655_4433_2211
    0x11_2233_4455_6677 --> 0x7766_5544_3322
    0x1122_3344_5566_7788 --> 0x8877_6655_4433
    |}]
;;

let%expect_test "bswap64" =
  List.iter numbers ~f:(test bswap64);
  [%expect
    {|
    0x1020 --> 0x2010_0000_0000_0000
    0x11_2233 --> 0x3322_1100_0000_0000
    0x1122_3344 --> 0x4433_2211_0000_0000
    0x11_2233_4455 --> 0x5544_3322_1100_0000
    0x1122_3344_5566 --> 0x6655_4433_2211_0000
    0x11_2233_4455_6677 --> 0x7766_5544_3322_1100
    0x1122_3344_5566_7788 --> -0x7788_99aa_bbcc_ddef
    |}]
;;

let%expect_test "binary" =
  quickcheck_m
    [%here]
    (module struct
      type t = int64 [@@deriving quickcheck, sexp_of]
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
