open! Base
open! Import

let%expect_test "[Base.Hash.state] is still immediate" =
  require_no_allocation [%here] (fun () ->
    ignore (Sys.opaque_identity (Base.Hash.create ())));
  [%expect {| |}]

let%expect_test "[Base_boot.Hash.state] is still immediate" =
  require_no_allocation [%here] (fun () ->
    ignore (Sys.opaque_identity (Base_boot.Hash.create ())));
  [%expect {| |}]

type t = { mutable state : Base.Hash.state; mutable list : unit list }

let%expect_test _ =
  let count_caml_modify f =
    Replace_caml_modify_for_testing.reset ();
    f ();
    print_s [%sexp (Replace_caml_modify_for_testing.count () : int)];
  in
  let t = { state = Base.Hash.create ~seed:1 (); list = [] } in
  let list = [ (); () ] (* not an immediate type, requires caml_modify *) in
  count_caml_modify (fun () -> t.list <- list);
  [%expect {| 1 |}];
  let state = Base.Hash.create ~seed:2 () (* immediate, I hope *) in
  count_caml_modify (fun () -> t.state <- state);
  [%expect {| 0 |}];
;;
