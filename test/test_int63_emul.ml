open! Import
module Int63_emul = Base.Int63.Private.Emul

let%expect_test _ =
  let s63 = Int63.(Hex.to_string min_value) in
  let s63_emul = Int63_emul.(Hex.to_string min_value) in
  print_s [%message (s63 : string) (s63_emul : string)];
  require (String.equal s63 s63_emul);
  [%expect
    {|
    ((s63      -0x4000000000000000)
     (s63_emul -0x4000000000000000))
    |}]
;;

let%expect_test "log2" =
  let open Int63_emul in
  let t x =
    let x = x |> of_int in
    let flg = floor_log2 x in
    let clg = ceil_log2 x in
    print_s [%message (x : t) (flg : t) (clg : t)]
  in
  t 1023;
  [%expect
    {|
    ((x   1_023)
     (flg 9)
     (clg 10))
    |}];
  t 1024;
  [%expect
    {|
    ((x   1_024)
     (flg 10)
     (clg 10))
    |}];
  t 1025;
  [%expect
    {|
    ((x   1_025)
     (flg 10)
     (clg 11))
    |}]
;;
