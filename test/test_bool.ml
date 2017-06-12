open! Import

let%expect_test "hash coherence" =
  check_hash_coherence [%here] (module Bool) [ false; true ];
  [%expect {| |}]
;;
