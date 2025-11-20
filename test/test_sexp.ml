open! Base
open Expect_test_helpers_base

[@@@alert "-base_sexp_private"]

let%expect_test "escaped_with_utf8 quotes" =
  "hV\"" |> Sexp.Private.Utf8.escaped ~quoted:false |> print_endline;
  [%expect {| hV\" |}]
;;

(* Make sure we avoid turning "\195\188" ("ü") into "\252" which would be an invalid UTF-8
   string, but is what we'd get if we naively do this, because it's a valid latin-1
   encoding:

   {v
     utop # Uchar.Utf8.of_string "ü" |> Uchar.to_char;;
     - : char option = Some '\252'
   v}
*)
let%expect_test "preserve valid utf8 despite alternative single char encoding" =
  Sexp.Private.Utf8.escaped ~quoted:false "d\195\188" |> print_endline;
  [%expect {| dü |}]
;;

let%expect_test "Test normal chars requiring escaping" =
  print_endline (Sexp.Private.Utf8.escaped ~quoted:true "foo bar");
  [%expect {| "foo bar" |}]
;;

let%expect_test "Test utf-8 chars" =
  print_endline (Sexp.Private.Utf8.escaped ~quoted:true "こんにちは");
  [%expect {| "こんにちは" |}]
;;

let%expect_test "Test normal chars requiring escaping" =
  print_endline (Sexp.Utf8.to_string_hum [%sexp "foo bar"] :> string);
  [%expect {| "foo bar" |}]
;;

let%expect_test "Test utf-8 atom" =
  print_endline (Sexp.Utf8.to_string_hum [%sexp "こんにちは"] :> string);
  [%expect {| こんにちは |}]
;;

let%expect_test "Test utf-8 list" =
  print_endline (Sexp.Utf8.to_string_hum [%sexp [ "こんにちは"; "你好" ]] :> string);
  [%expect {| (こんにちは 你好) |}]
;;

let%expect_test "Test utf-8 list with indent" =
  Sexp.List (List.init 20 ~f:(fun _ -> Sexp.Atom "こんにちは"))
  |> Sexp.Utf8.to_string_hum
  |> String.Utf8.to_string
  |> print_endline;
  [%expect
    {|
    (こんにちは こんにちは こんにちは こんにちは
     こんにちは こんにちは こんにちは こんにちは
     こんにちは こんにちは こんにちは こんにちは
     こんにちは こんにちは こんにちは こんにちは
     こんにちは こんにちは こんにちは こんにちは)
    |}]
;;
