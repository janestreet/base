open! Import
open! Sign

let%test _ = of_int 37 = Pos && of_int (-22) = Neg && of_int 0 = Zero

let%expect_test "hash coherence" [@tags "64-bits-only"] =
  check_hash_coherence [%here] (module Sign) all;
  [%expect {|
    ((value Neg)
     (hash1 -1)
     (hash2 1_058_613_066))
    ((value Zero)
     (hash1 0)
     (hash2 129_913_994))
    ((value Pos)
     (hash1 1)
     (hash2 462_777_137)) |}];
;;
