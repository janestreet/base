open! Base
open! Import

let%expect_test "[%here]" =
  print_s [%sexp [%here]];
  [%expect {| lib/base/test/test_source_code_position.ml:5:17 |}]
;;

let%expect_test "of_pos __POS__" =
  let here = Source_code_position.of_pos Stdlib.__POS__ in
  print_s [%sexp (here : Source_code_position.t)];
  [%expect {| test_source_code_position.ml:10:41 |}]
;;

let%expect_test "here_or_there" =
  print_s [%sexp (Source_code_position.here_or_there None : Source_code_position.t)];
  [%expect {| lib/base/test/test_source_code_position.ml:16:18 |}];
  let here = Some [%here] in
  print_s [%sexp (Source_code_position.here_or_there here : Source_code_position.t)];
  [%expect {| lib/base/test/test_source_code_position.ml:18:18 |}]
;;
