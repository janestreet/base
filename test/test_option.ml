open! Import
open! Option

let f = ( + )
let%test _ = [%compare.equal: int t] (merge None None ~f) None
let%test _ = [%compare.equal: int t] (merge (Some 3) None ~f) (Some 3)
let%test _ = [%compare.equal: int t] (merge None (Some 3) ~f) (Some 3)
let%test _ = [%compare.equal: int t] (merge (Some 1) (Some 3) ~f) (Some 4)

let%expect_test "[value_exn]" =
  (* Location information should be excluded when [here] is [Lexing.dummy_pos], which is
     the default value of [here] in the external version of Base *)
  Expect_test_helpers_base.require_does_raise ~hide_positions:true (fun () ->
    value_exn None);
  [%expect {| ("Option.value_exn None" lib/base/test/test_option.ml:LINE:COL) |}];
  Expect_test_helpers_base.require_does_raise (fun () ->
    value_exn None ~here:Lexing.dummy_pos);
  [%expect {| "Option.value_exn None" |}]
;;

let%expect_test "[value_or_thunk]" =
  let default () =
    print_endline "THUNK!";
    0
  in
  let value_or_thunk = value_or_thunk ~default in
  let test t = print_s [%sexp (value_or_thunk t : int)] in
  (* trigger the thunk *)
  test None;
  [%expect
    {|
    THUNK!
    0
    |}];
  (* same value, no trigger *)
  test (Some 0);
  [%expect {| 0 |}];
  (* different value *)
  test (Some 1);
  [%expect {| 1 |}];
  (* trigger the thunk again: no memoization *)
  test None;
  [%expect
    {|
    THUNK!
    0
    |}]
;;

let%expect_test "map2" =
  let m t1 t2 =
    let result = Option.map2 ~f:(fun x y -> x + y) t1 t2 in
    print_s [%sexp (result : int Option.t)]
  in
  m None None;
  [%expect {| () |}];
  m (Some 1) (Some 2);
  [%expect {| (3) |}];
  m None (Some 1);
  [%expect {| () |}];
  m (Some 1) None;
  [%expect {| () |}]
;;

[%%template
let%expect_test "some_if_thunk{,_local}" =
  let print_opt = (Option.iter [@mode local]) ~f:(fun x -> Stdlib.print_int x) in
  let run_test some_if_thunk =
    (* In the [false] case, don't run the thunk *)
    print_opt (some_if_thunk false (fun () -> assert false));
    [%expect {| |}];
    (* In the [true] case, do run the thunk *)
    some_if_thunk true (fun () ->
      print_endline "running";
      1)
    |> print_opt;
    [%expect
      {|
      running
      1
      |}]
  in
  run_test (fun b f -> exclave_ [%template some_if_thunk [@mode local]] b f);
  run_test some_if_thunk
;;

let%expect_test "first_some_thunk" =
  let print_opt = (Option.iter [@mode local]) ~f:(fun x -> Stdlib.print_int x) in
  print_opt (first_some_thunk None (Fn.const None));
  [%expect {| |}];
  print_opt (first_some_thunk (Some 1) (fun () -> failwith "should not be run"));
  [%expect {| 1 |}];
  print_opt (first_some_thunk None (Fn.const (Some 2)));
  [%expect {| 2 |}]
;;]
