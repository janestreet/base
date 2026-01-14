open! Base
open! Import

module%test [@name "Result.Error"] _ = struct
  open Result.Error.Let_syntax

  module Int_or_string = struct
    type t = (int, string) Result.t [@@deriving equal, sexp_of]
  end

  let%expect_test "return" =
    require_equal (module Int_or_string) (return "error") (Error "error");
    [%expect {| |}]
  ;;

  let%expect_test "bind Error" =
    let result =
      let%bind e1 = Error "e1" in
      let%bind e2 = Error "e2" in
      let%bind e3 = Error "e3" in
      return (String.concat ~sep:"," [ e1; e2; e3 ])
    in
    require_equal (module Int_or_string) result (Error "e1,e2,e3");
    [%expect {| |}]
  ;;

  let%expect_test "bind Ok" =
    let result =
      let%bind e1 = Error "e1" in
      let%bind e2 = Ok 1 in
      let%bind e3 = Error "e3" in
      return (String.concat ~sep:"," [ e1; e2; e3 ])
    in
    require_equal (module Int_or_string) result (Ok 1);
    [%expect {| |}]
  ;;

  let%expect_test "map Error" =
    let result =
      let%map e1 = Error "e1" in
      e1 ^ "!"
    in
    require_equal (module Int_or_string) result (Error "e1!");
    [%expect {| |}]
  ;;

  let%expect_test "map Ok" =
    let result =
      let%map e1 = Ok 1 in
      e1 ^ "!"
    in
    require_equal (module Int_or_string) result (Ok 1);
    [%expect {| |}]
  ;;

  (* The rest of the Monad functions are derived using the Monad.Make functor, which is
     well-tested. *)
end

module%test [@name "can contain nulls"] _ = struct
  (* no annotation *)
  let _ = Ok Null
  let _ = Error Null

  (* toplevel alias in [base.ml] *)
  let _ : _ result = Ok Null
  let _ : _ result = Error Null
  let _ : _ Result.t = Ok Null
  let _ : _ Result.t = Error Null

  (* This isn't directly exposed anywhere by Base, but it gets re-exported by Core. *)
  let _ : _ Result.Export._result = Ok Null
  let _ : _ Result.Export._result = Error Null

  (* check that [open]ing [Result] doesn't ruin the visible constructors *)
  open! Result

  let _ = Ok Null
  let _ = Error Null
  let _ : _ t = Ok Null
  let _ : _ t = Error Null
end
