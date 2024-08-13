open! Base
open! Import

let%expect_test "[Base.Hash.state] is still immediate" =
  require_no_allocation (fun () -> ignore (Sys.opaque_identity (Base.Hash.create ())));
  [%expect {| |}]
;;

let%expect_test _ =
  print_s
    [%sexp (Stdlib.Obj.is_int (Stdlib.Obj.repr (Base.Hash.create ~seed:1 ())) : bool)];
  [%expect {| true |}]
;;
