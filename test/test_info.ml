open! Import
open! Info

let%expect_test _ =
  print_endline (to_string_hum (of_exn (Failure "foo")));
  [%expect {| (Failure foo) |}]
;;

let%expect_test _ =
  print_endline (to_string_hum (tag (of_string "b") ~tag:"a"));
  [%expect {| (a b) |}]
;;

let%expect_test _ =
  print_endline (to_string_hum (of_list (List.map ~f:of_string [ "a"; "b"; "c" ])));
  [%expect {| (a b c) |}]
;;

let%expect_test _ =
  print_endline (to_string_hum (tag_s ~tag:[%message "tag"] (create_s [%message "info"])));
  [%expect {| (tag info) |}]
;;

let%expect_test _ =
  print_endline
    (to_string_hum
       ((tag_s [@mode portable]) ~tag:[%message "tag"] (create_s [%message "info"])));
  [%expect {| (tag info) |}]
;;

let of_strings strings = of_list (List.map ~f:of_string strings)

let nested =
  of_list
    (List.map ~f:of_strings [ [ "a"; "b"; "c" ]; [ "d"; "e"; "f" ]; [ "g"; "h"; "i" ] ])
;;

let%expect_test _ =
  print_endline (to_string_hum nested);
  [%expect {| (a b c d e f g h i) |}]
;;

let%expect_test _ =
  require_equal
    (module Sexp)
    (sexp_of_t nested)
    (sexp_of_t (of_strings [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i" ]));
  [%expect {| |}]
;;

let%expect_test _ =
  match to_exn (of_exn (Failure "foo")) with
  | ((Failure "foo") [@warning "-52"]) -> ()
  | exn -> raise_s [%sexp { got = (exn : exn); expected = Failure "foo" }]
;;

let round t =
  let sexp = sexp_of_t t in
  require (Sexp.( = ) sexp (sexp_of_t (t_of_sexp sexp)))
;;

let%expect_test "non-empty tag" =
  tag_arg (of_string "hello") "tag" 13 [%sexp_of: int] |> sexp_of_t |> print_s;
  [%expect {| (tag 13 hello) |}]
;;

let%expect_test "empty tag" =
  tag_arg (of_string "hello") "" 13 [%sexp_of: int] |> sexp_of_t |> print_s;
  [%expect {| (13 hello) |}]
;;

let%expect_test _ = round (of_string "hello")
let%expect_test _ = round (of_thunk (fun () -> "hello"))
let%expect_test _ = round (create "tag" 13 [%sexp_of: int])
let%expect_test _ = round (tag (of_string "hello") ~tag:"tag")
let%expect_test _ = round ((tag [@mode portable]) (of_string "hello") ~tag:"tag")
let%expect_test _ = round (tag_arg (of_string "hello") "tag" 13 [%sexp_of: int])
let%expect_test _ = round (tag_arg (of_string "hello") "" 13 [%sexp_of: int])
let%expect_test _ = round (of_list [ of_string "hello"; of_string "goodbye" ])

let%expect_test _ =
  round (t_of_sexp (Sexplib.Sexp.of_string "((random sexp 1)(b 2)((c (1 2 3))))"))
;;

let%expect_test _ =
  require_equal (module String) (to_string_hum (of_string "a\nb")) "a\nb";
  [%expect {| |}]
;;

let%expect_test "stack overflow" =
  (* [Info.of_list [info]] and [Info.of_lazy_t (lazy info)] produce an [Info.t] with the
     same sexp as the original. Ideally, deep nesting of these should not yield a stack
     overflow just to produce a small value. *)
  let depth =
    match Word_size.word_size with
    | W64 -> 1_000_000
    | W32 -> 100_000
  in
  let%template[@mode __ = (portable, nonportable)] test f ~wrap ~unwrap =
    let info = ref (wrap (Info.of_string "info")) in
    for _ = 1 to depth do
      info := f (unwrap !info) |> wrap
    done;
    print_s (Info.sexp_of_t (unwrap !info))
  in
  test (fun info -> Info.of_list [ info ]) ~wrap:Fn.id ~unwrap:Fn.id;
  [%expect {| info |}];
  test (fun info -> Info.of_lazy_t (lazy info)) ~wrap:Fn.id ~unwrap:Fn.id;
  [%expect {| info |}];
  test
    (fun info -> Info.of_lazy_t (lazy (Info.of_list [ info ])))
    ~wrap:Fn.id
    ~unwrap:Fn.id;
  [%expect {| info |}];
  (test [@mode portable])
    ~wrap:Modes.Portable.wrap
    ~unwrap:Modes.Portable.unwrap
    (fun info ->
       Info.of_portable_lazy_t
         (Portable_lazy.from_fun (fun () -> (Info.of_list [@mode portable]) [ info ])));
  [%expect {| info |}]
;;

let%expect_test "cyclic info computation" =
  let info =
    let rec lazy_info = lazy (Info.of_lazy_t lazy_info) in
    Info.of_lazy_t lazy_info
  in
  print_s (Info.sexp_of_t info);
  [%expect {| (Could_not_construct "cycle while computing message") |}];
  let info =
    let lazy_info =
      Portable_lazy.from_fun_fixed (fun lazy_info -> Info.of_portable_lazy_t lazy_info)
    in
    Info.of_portable_lazy_t lazy_info
  in
  print_s (Info.sexp_of_t info);
  [%expect {| (Could_not_construct "cycle while computing message") |}]
;;

let%expect_test "show how backtraces are printed" =
  (* This is a real backtrace from some random OCaml program.

     The words [Raised] and [Called] have been lowercased to fool the expect test
     collector and prevent it from complaining about the presence of a backtrace.  This is
     fine for this test because we are using a static string as the source for the
     backtrace, rather than actually raising (which might change output between compiler
     versions). *)
  let backtrace =
    "raised at Base__Error.raise in file \"error.ml\" (inlined), line 9, characters 14-30\n\
     called from Base__Error.raise_s in file \"error.ml\", line 10, characters 19-40\n\
     called from Floops_interfaces__Registrant.register_exn in file \"registrant.ml\", \
     line 18, characters 4-241\n\
     called from Floops_interfaces_test__Registrant_test.Test_brick.create_exn.(fun) in \
     file \"registrant_test.ml\", line 25, characters 6-67\n\
     called from Base__Or_error.try_with in file \"or_error.ml\", line 84, characters 9-15\n"
  in
  let exn = of_exn ~backtrace:(`This backtrace) (Failure "foo") in
  print_s [%sexp (exn : t)];
  [%expect
    {|
    ((Failure foo)
     ("raised at Base__Error.raise in file \"error.ml\" (inlined), line 9, characters 14-30"
      "called from Base__Error.raise_s in file \"error.ml\", line 10, characters 19-40"
      "called from Floops_interfaces__Registrant.register_exn in file \"registrant.ml\", line 18, characters 4-241"
      "called from Floops_interfaces_test__Registrant_test.Test_brick.create_exn.(fun) in file \"registrant_test.ml\", line 25, characters 6-67"
      "called from Base__Or_error.try_with in file \"or_error.ml\", line 84, characters 9-15"))
    |}];
  print_endline (Info.to_string_hum exn);
  [%expect
    {|
    ((Failure foo)
     ("raised at Base__Error.raise in file \"error.ml\" (inlined), line 9, characters 14-30"
      "called from Base__Error.raise_s in file \"error.ml\", line 10, characters 19-40"
      "called from Floops_interfaces__Registrant.register_exn in file \"registrant.ml\", line 18, characters 4-241"
      "called from Floops_interfaces_test__Registrant_test.Test_brick.create_exn.(fun) in file \"registrant_test.ml\", line 25, characters 6-67"
      "called from Base__Or_error.try_with in file \"or_error.ml\", line 84, characters 9-15"))
    |}]
;;

let%expect_test "portabilize" =
  let portable_lazy_status = Atomic.make "<unforced>" in
  let examples =
    [ create_s [%message "sexp"]
    ; of_string "string"
    ; of_lazy
        (lazy
          (print_endline "forcing lazy";
           "lazy"))
    ; of_thunk (fun () ->
        print_endline "forcing thunk";
        "thunk")
    ; of_portable_lazy
        (Portable_lazy.from_fun (fun () ->
           Atomic.set portable_lazy_status "portable lazy was forced";
           "portable lazy"))
    ]
  in
  let examples = List.map examples ~f:portabilize in
  [%expect
    {|
    forcing lazy
    forcing thunk
    |}];
  (* The info constructed with [of_portable_lazy] wasn't forced
     by [portabilize] because it's already portable. *)
  print_endline (Atomic.get portable_lazy_status);
  [%expect {| <unforced> |}];
  List.iter examples ~f:(fun info -> print_s [%sexp (info : t)]);
  [%expect
    {|
    sexp
    string
    lazy
    thunk
    "portable lazy"
    |}];
  (* But sexpifying the portable lazy has forced it. *)
  print_endline (Atomic.get portable_lazy_status);
  [%expect {| portable lazy was forced |}]
;;
