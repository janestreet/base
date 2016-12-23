open! Import

let%expect_test "hash coherence" =
  check_hash_coherence [%here] (module Bool) [ false; true ];
  [%expect {|
    ((value false)
     (hash1 0)
     (hash2 1_058_613_066))
    ((value true)
     (hash1 1)
     (hash2 129_913_994)) |}]
;;
