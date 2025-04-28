open! Base
open! Import

let errors =
  [ Error.of_string "ABC"
  ; Error.tag ~tag:"DEF" (Error.of_thunk (fun () -> "GHI"))
  ; Error.create_s [%message "foo" ~bar:(31 : int)]
  ]
;;

let%expect_test _ =
  List.iter errors ~f:(fun error -> show_raise (fun () -> Error.raise error));
  [%expect
    {|
    (raised ABC)
    (raised (DEF GHI))
    (raised (foo (bar 31)))
    |}]
;;

let%expect_test _ =
  List.iter errors ~f:(fun error ->
    show_raise (fun () -> Error.raise_s [%sexp (error : Error.t)]));
  [%expect
    {|
    (raised ABC)
    (raised (DEF GHI))
    (raised (foo (bar 31)))
    |}]
;;

let%expect_test "reraise_uncaught" =
  require_does_raise (fun () ->
    Error.reraise_uncaught (Error.of_string "my bad") ~f:(fun () ->
      raise_s [%message "oops"]));
  [%expect {| ("my bad" oops) |}];
  require_does_raise (fun () ->
    Error.reraise_uncaught
      (Error.of_lazy_sexp (lazy (raise_s [%message "ceci n'est pas une erreur"])))
      ~f:(fun () -> raise_s [%message "oops"]));
  [%expect {| ((Could_not_construct "ceci n'est pas une erreur") oops) |}];
  require_does_not_raise (fun () ->
    Error.reraise_uncaught (Error.of_string "no problem") ~f:(fun () -> ()));
  [%expect {| |}]
;;
