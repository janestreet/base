open! Import
open! Or_error

let%test _ = [%compare.equal: string t] (errorf "foo %d" 13) (error_string "foo 13")

let%test_unit _ =
  for i = 0 to 10 do
    assert (
      [%compare.equal: unit list t]
        (combine_errors (List.init i ~f:(fun _ -> Ok ())))
        (Ok (List.init i ~f:(fun _ -> ()))))
  done
;;

let%test _ = Result.is_error (combine_errors [ error_string "" ])
let%test _ = Result.is_error (combine_errors [ Ok (); error_string "" ])
let ( = ) = [%compare.equal: unit t]
let%test _ = combine_errors_unit [ Ok (); Ok () ] = Ok ()
let%test _ = combine_errors_unit [] = Ok ()

let%test _ =
  let a = Error.of_string "a"
  and b = Error.of_string "b" in
  match combine_errors_unit [ Ok (); Error a; Ok (); Error b ] with
  | Ok _ -> false
  | Error e ->
    String.equal (Error.to_string_hum e) (Error.to_string_hum (Error.of_list [ a; b ]))
;;

let%expect_test "map2" =
  let m t1 t2 =
    let result = Or_error.map2 ~f:(fun x y -> x + y) t1 t2 in
    print_s [%sexp (result : int Or_error.t)]
  in
  let foo = Error.of_string "foo" in
  let bar = Error.of_string "bar" in
  m (Error foo) (Error bar);
  [%expect {| (Error (foo bar)) |}];
  m (Ok 1) (Ok 2);
  [%expect {| (Ok 3) |}];
  m (Error foo) (Ok 1);
  [%expect {| (Error foo) |}];
  m (Ok 1) (Error bar);
  [%expect {| (Error bar) |}]
;;

(* These tests check for stack overflow, and that we don't time out, when given large
   lists. We also test that we preserve all errors, in order, so that performance-related
   changes don't accidentally change behavior.

   History: in [2023-02], [all] and [all_unit] had O(N) stack usage and O(N^2) time for
   lists of length N. These costs were hidden behind the [lazy] inside [Error] values, so
   they could occur far from where the error was constructed. *)
let%expect_test "behavior and performance on lists of or_error's" =
  let make_list len =
    (* We construct atoms with spaces in them to show sexp rendering with quotes, which is
       significant to [Error.to_string_hum]'s behavior below. *)
    List.init len ~f:(Or_error.errorf "at %d")
  in
  let short_lists = List.map ~f:make_list [ 0; 1; 2; 10 ] in
  let long_list = make_list 500_000 in
  let to_string = function
    | Ok _ -> "ok"
    | Error error ->
      (* Converting to string forces the [lazy] inside [Error.t]. Using [to_string_hum]
         also happens to observe whether the error was created via [Error.of_string]. *)
      Error.to_string_hum error
  in
  let test f =
    (* Show behavior on short lists. *)
    List.iter short_lists ~f:(fun list -> print_endline (to_string (f list)));
    (* Test for timeout / stack overflow on a long list. *)
    match to_string (f long_list) with
    | (_ : string) -> ()
    | exception Stack_overflow -> print_cr [%here] [%message "stack overflow"]
  in
  (* test functions that combine a list of or_errors *)
  test all;
  [%expect
    {|
    ok
    at 0
    ("at 0" "at 1")
    ("at 0" "at 1" "at 2" "at 3" "at 4" "at 5" "at 6" "at 7" "at 8" "at 9")
    |}];
  test all_unit;
  [%expect
    {|
    ok
    at 0
    ("at 0" "at 1")
    ("at 0" "at 1" "at 2" "at 3" "at 4" "at 5" "at 6" "at 7" "at 8" "at 9")
    |}];
  test combine_errors;
  [%expect
    {|
    ok
    "at 0"
    ("at 0" "at 1")
    ("at 0" "at 1" "at 2" "at 3" "at 4" "at 5" "at 6" "at 7" "at 8" "at 9")
    |}];
  test combine_errors_unit;
  [%expect
    {|
    ok
    "at 0"
    ("at 0" "at 1")
    ("at 0" "at 1" "at 2" "at 3" "at 4" "at 5" "at 6" "at 7" "at 8" "at 9")
    |}];
  test find_ok;
  [%expect
    {|
    ()
    "at 0"
    ("at 0" "at 1")
    ("at 0" "at 1" "at 2" "at 3" "at 4" "at 5" "at 6" "at 7" "at 8" "at 9")
    |}];
  test (find_map_ok ~f:Fn.id);
  [%expect
    {|
    ()
    "at 0"
    ("at 0" "at 1")
    ("at 0" "at 1" "at 2" "at 3" "at 4" "at 5" "at 6" "at 7" "at 8" "at 9")
    |}];
  test filter_ok_at_least_one;
  [%expect
    {|
    ()
    "at 0"
    ("at 0" "at 1")
    ("at 0" "at 1" "at 2" "at 3" "at 4" "at 5" "at 6" "at 7" "at 8" "at 9")
    |}]
;;
