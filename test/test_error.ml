open! Base
open! Import

let errors =
  [ Error.of_string "ABC"
  ; Error.tag ~tag:"DEF" (Error.of_thunk (fun () -> "GHI"))
  ; Error.create_s [%message "foo" ~bar:(31 : int)]
  ; (* A utf8 string inside *)
    Error.create_s [%message "こんにちは、世界" ~bar:(31 : int)]
  ]
;;

let%expect_test _ =
  List.iter errors ~f:(fun error -> show_raise (fun () -> Error.raise error));
  [%expect
    {|
    (raised ABC)
    (raised (DEF GHI))
    (raised (foo (bar 31)))
    (raised (
      "\227\129\147\227\130\147\227\129\171\227\129\161\227\129\175\227\128\129\228\184\150\231\149\140" (
        bar 31)))
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
    (raised (
      "\227\129\147\227\130\147\227\129\171\227\129\161\227\129\175\227\128\129\228\184\150\231\149\140" (
        bar 31)))
    |}]
;;

let%expect_test "errors printed as utf8" =
  List.iter errors ~f:(fun error -> error |> Error.Utf8.to_string_hum |> print_endline);
  [%expect
    {|
    ABC
    (DEF GHI)
    (foo (bar 31))
    (こんにちは、世界 (bar 31))
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
  require_does_raise (fun () ->
    Error.reraise_uncaught
      (Error.of_portable_lazy_sexp
         (Portable_lazy.from_fun (fun () ->
            raise_s [%message "ceci n'est pas une erreur portable"])))
      ~f:(fun () -> raise_s [%message "oops"]));
  [%expect {| ((Could_not_construct "ceci n'est pas une erreur portable") oops) |}];
  require_does_not_raise (fun () ->
    Error.reraise_uncaught (Error.of_string "no problem") ~f:(fun () -> ()));
  [%expect {| |}]
;;

module%test Avoid_stack_overflow = struct
  let n = 500_000

  let%expect_test "" =
    let err = Error.of_string "A" in
    List.init n ~f:(fun _ -> err)
    |> List.fold_right ~init:err ~f:(fun err1 err2 -> Error.of_list [ err1; err2 ])
    |> Error.to_info
    |> (ignore : Info.t -> unit)
  ;;

  (* The implementation of portable [of_list] is sufficiently different that it seems
     worth having a separate test just for this case.
  *)
  let%expect_test "portable" =
    let err = Error.of_string "A" |> Modes.Portable.wrap in
    List.init n ~f:(fun _ -> err)
    |> List.fold_right ~init:err ~f:(fun err1 err2 ->
      (Error.of_list [@mode portable]) [ err1.portable; err2.portable ]
      |> Modes.Portable.wrap)
    |> Modes.Portable.unwrap
    |> Error.to_info
    |> (ignore : Info.t -> unit)
  ;;
end

(* Most tests are for the default, non-portable version of error functions. This test
   helps us gain confidence that the portable versions of error functions are equivalent.
*)
module%test Portable_feature_parity = struct
  module Operation = struct
    module Ab = struct
      type t =
        | A
        | B
      [@@deriving quickcheck, sexp_of, string]
    end

    type t =
      | Create_s of Ab.t
      | Of_string of Ab.t
      | Of_lazy_t of t
      | Of_list of t list
      | Tag of t * Ab.t
      | Tag_s of t * Ab.t
      | Tag_arg of t * Ab.t * Ab.t
      | Tag_portable_lazy of t * Ab.t
      | Of_thunk of Ab.t
    [@@deriving quickcheck, sexp_of]

    let rec interpret : t -> Error.t = function
      | Create_s x -> Error.create_s (Ab.sexp_of_t x)
      | Of_string x -> Error.of_string (Ab.to_string x)
      | Of_lazy_t x -> Error.of_lazy_t (lazy (interpret x))
      | Of_list xs -> Error.of_list (List.map xs ~f:interpret)
      | Tag (x, tag) -> Error.tag (interpret x) ~tag:(Ab.to_string tag)
      | Tag_s (x, tag) -> Error.tag_s (interpret x) ~tag:(Ab.sexp_of_t tag)
      | Tag_arg (x, message, v) ->
        Error.tag_arg (interpret x) (Ab.to_string message) v Ab.sexp_of_t
      | Tag_portable_lazy (x, tag) ->
        Error.tag_s_portable_lazy
          (interpret x)
          ~tag:(Portable_lazy.from_fun (fun () -> Ab.sexp_of_t tag))
      | Of_thunk ab -> Error.of_thunk (fun () -> Ab.to_string ab)
    ;;

    let rec interpret_portable : t -> Error.t @ portable = function
      | Create_s x -> Error.create_s (Ab.sexp_of_t x)
      | Of_string x -> Error.of_string (Ab.to_string x)
      | Of_lazy_t x ->
        Error.of_portable_lazy_t (Portable_lazy.from_fun (fun () -> interpret_portable x))
      | Of_list xs ->
        (Error.of_list [@mode portable])
          (List.map (Modes.Portable.wrap_list xs) ~f:(fun x ->
             x |> Modes.Portable.unwrap |> interpret_portable |> Modes.Portable.wrap)
           |> Modes.Portable.unwrap_list)
      | Tag (x, tag) ->
        (Error.tag [@mode portable]) (interpret_portable x) ~tag:(Ab.to_string tag)
      | Tag_s (x, tag) ->
        (Error.tag_s [@mode portable]) (interpret_portable x) ~tag:(Ab.sexp_of_t tag)
      | Tag_arg (x, message, v) ->
        (Error.tag_arg [@mode portable])
          (interpret_portable x)
          (Ab.to_string message)
          v
          (fun x -> Ab.sexp_of_t x)
      | Tag_portable_lazy (x, tag) ->
        (Error.tag_s_portable_lazy [@mode portable])
          (interpret_portable x)
          ~tag:(Portable_lazy.from_fun (fun () -> Ab.sexp_of_t tag))
      | Of_thunk ab -> (Error.of_thunk [@mode portable]) (fun () -> Ab.to_string ab)
    ;;
  end

  let test operation =
    let err_via_nonportable = Operation.interpret operation |> Error.to_info in
    let err_via_portable = Operation.interpret_portable operation |> Error.to_info in
    if not (Info.equal err_via_nonportable err_via_portable)
    then
      raise_s
        [%message
          "Operation did not produce equivalent error via portable/nonportable code paths"
            (operation : Operation.t)
            (err_via_nonportable : Info.t)
            (err_via_portable : Info.t)]
  ;;

  let%expect_test _ = Base_quickcheck.Test.run_exn (module Operation) ~f:test
end
