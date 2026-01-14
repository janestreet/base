open! Import
open Invariant

let%expect_test "[invariant]" =
  require_does_raise ~hide_positions:true (fun () ->
    invariant 32 sexp_of_int (fun () -> failwith "deliberate error"));
  [%expect
    {|
    ("invariant failed"
     lib/base/test/test_invariant.ml:LINE:COL
     (exn (Failure "deliberate error"))
     32)
    |}]
;;

let%expect_test "[invariant] handles [Lexing.dummy_pos]" =
  (* Location information should be excluded when [here] is [Lexing.dummy_pos], which is
     the default value of [here] in the external version of Base for [%call_pos] arguments *)
  require_does_raise (fun () ->
    invariant ~here:Lexing.dummy_pos 32 sexp_of_int (fun () ->
      failwith "deliberate error"));
  [%expect {| ("invariant failed" (exn (Failure "deliberate error")) 32) |}]
;;
