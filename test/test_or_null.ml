open! Import
open! Or_null

module%test _ = struct
  let () = Dynamic.set_root sexp_style Sexp_style.simple_pretty
end

module _ : module type of struct
  include Or_null
end = struct
  type 'a t = 'a or_null [@@or_null_reexport] [@@deriving sexp ~stackify]

  let globalize = globalize_or_null

  let%expect_test "globalize" =
    let t @ local = Null in
    print_s [%sexp ([%globalize: int t] t : int t)];
    [%expect {| () |}];
    let t' @ local = This "hello world" in
    print_s [%sexp ([%globalize: string t] t' : string t)];
    [%expect {| ("hello world") |}]
  ;;

  let hash_fold_t = hash_fold_t

  let%expect_test "hash" =
    let t = Null in
    print_s [%sexp ([%hash: int t] t : int)];
    [%expect {| 1_058_613_066 |}];
    let t' = This "my string" in
    print_s [%sexp ([%hash: string t] t' : int)];
    [%expect {| 333_169_405 |}]
  ;;

  let%template[@mode m = (global, local)] compare = (Or_null.compare [@mode m])

  let%expect_test "compare" =
    let test t1 t2 =
      let result = compare Int.compare t1 t2 in
      print_s [%message (t1 : int t) (t2 : int t) (result : int)]
    in
    test Null Null;
    [%expect {| ((t1 ()) (t2 ()) (result 0)) |}];
    test Null (This 42);
    [%expect {| ((t1 ()) (t2 (42)) (result -1)) |}];
    test (This 42) Null;
    [%expect {| ((t1 (42)) (t2 ()) (result 1)) |}];
    test (This 42) (This 42);
    [%expect {| ((t1 (42)) (t2 (42)) (result 0)) |}];
    test (This 10) (This 20);
    [%expect {| ((t1 (10)) (t2 (20)) (result -1)) |}];
    test (This 20) (This 10);
    [%expect {| ((t1 (20)) (t2 (10)) (result 1)) |}];
    (* Test with [%compare: int t] *)
    let test_ppx_compare t1 t2 =
      let result = [%compare: int t] t1 t2 in
      print_s [%message (t1 : int t) (t2 : int t) (result : int)]
    in
    test_ppx_compare Null Null;
    [%expect {| ((t1 ()) (t2 ()) (result 0)) |}];
    test_ppx_compare Null (This 42);
    [%expect {| ((t1 ()) (t2 (42)) (result -1)) |}];
    test_ppx_compare (This 42) Null;
    [%expect {| ((t1 (42)) (t2 ()) (result 1)) |}];
    test_ppx_compare (This 10) (This 20);
    [%expect {| ((t1 (10)) (t2 (20)) (result -1)) |}]
  ;;

  let%template[@mode m = (global, local)] equal = (Or_null.equal [@mode m])

  let%expect_test "equal" =
    let test t1 t2 =
      let result = equal Int.equal t1 t2 in
      print_s [%message (t1 : int t) (t2 : int t) (result : bool)]
    in
    test Null Null;
    [%expect {| ((t1 ()) (t2 ()) (result true)) |}];
    test Null (This 42);
    [%expect {| ((t1 ()) (t2 (42)) (result false)) |}];
    test (This 42) Null;
    [%expect {| ((t1 (42)) (t2 ()) (result false)) |}];
    test (This 42) (This 42);
    [%expect {| ((t1 (42)) (t2 (42)) (result true)) |}];
    test (This 10) (This 20);
    [%expect {| ((t1 (10)) (t2 (20)) (result false)) |}];
    (* Test with [%equal: int t] *)
    let test_ppx_equal t1 t2 =
      let result = [%equal: int t] t1 t2 in
      print_s [%message (t1 : int t) (t2 : int t) (result : bool)]
    in
    test_ppx_equal Null Null;
    [%expect {| ((t1 ()) (t2 ()) (result true)) |}];
    test_ppx_equal Null (This 42);
    [%expect {| ((t1 ()) (t2 (42)) (result false)) |}];
    test_ppx_equal (This 42) (This 42);
    [%expect {| ((t1 (42)) (t2 (42)) (result true)) |}];
    test_ppx_equal (This 10) (This 20);
    [%expect {| ((t1 (10)) (t2 (20)) (result false)) |}]
  ;;

  let is_null t = Or_null.is_null t
  let is_this t = Or_null.is_this t

  let%expect_test "is_null and is_this" =
    let test_predicates t =
      Stdio.print_s [%message (is_null t : bool) (is_this t : bool)]
    in
    test_predicates Null;
    [%expect {| (("is_null t" true) ("is_this t" false)) |}];
    test_predicates (This 42);
    [%expect {| (("is_null t" false) ("is_this t" true)) |}]
  ;;

  let length = Or_null.length

  let%expect_test "length" =
    print_s [%message (length Null : int)];
    [%expect {| ("length Null" 0) |}];
    print_s [%message (length (This 42) : int)];
    [%expect {| ("length (This 42)" 1) |}]
  ;;

  let%template[@mode m = (global, local)] to_option t =
    (Or_null.to_option [@mode m]) t [@exclave_if_local m]
  ;;

  let%expect_test "to_option" =
    print_s [%sexp (to_option Null : int option)];
    [%expect {| () |}];
    print_s [%sexp (to_option (This 42) : int option)];
    [%expect {| (42) |}]
  ;;

  let%template[@alloc a = (heap, stack)] map_to_option =
    (Or_null.map_to_option [@alloc a])
  ;;

  let%expect_test "map_to_option" =
    let f x = x + 1 in
    print_s [%sexp (map_to_option ~f Null : int option)];
    [%expect {| () |}];
    print_s [%sexp (map_to_option ~f (This 42) : int option)];
    [%expect {| (43) |}]
  ;;

  let%template[@mode m = (global, local)] of_option o =
    (Or_null.of_option [@mode m]) o [@exclave_if_local m]
  ;;

  let%expect_test "of_option" =
    print_s [%sexp (of_option None : int or_null)];
    [%expect {| () |}];
    print_s [%sexp (of_option (Some 42) : int or_null)];
    [%expect {| (42) |}]
  ;;

  let%template[@mode m = (global, local)] value t ~default =
    (Or_null.value [@mode m]) t ~default [@exclave_if_local m]
  ;;

  let%expect_test "value" =
    print_s [%sexp (value Null ~default:42 : int)];
    [%expect {| 42 |}];
    print_s [%sexp (value (This 5) ~default:42 : int)];
    [%expect {| 5 |}]
  ;;

  let%template[@mode m = (global, local)] value_exn ~(here : [%call_pos]) t =
    (Or_null.value_exn [@mode m]) ~here t [@exclave_if_local m]
  ;;

  let%expect_test "value_exn" =
    (* Location information should be excluded when [here] is [Lexing.dummy_pos], which is
       the default value of [here] in the external version of Base *)
    Expect_test_helpers_base.require_does_raise ~hide_positions:true (fun () ->
      value_exn Null ~here:[%here]);
    [%expect {| ("Or_null.value_exn Null" lib/base/test/test_or_null.ml:LINE:COL) |}];
    Expect_test_helpers_base.require_does_raise (fun () ->
      value_exn Null ~here:Lexing.dummy_pos);
    [%expect {| "Or_null.value_exn Null" |}]
  ;;

  let%template[@mode m = (global, local)] unsafe_value = (Or_null.unsafe_value [@mode m])

  let%expect_test "unsafe_value" =
    print_s [%sexp (unsafe_value (This 42) : int)];
    [%expect {| 42 |}]
  ;;

  let%template[@mode m = (global, local)] value_or_thunk =
    (Or_null.value_or_thunk [@mode m])
  ;;

  let%expect_test "value_or_thunk" =
    let default () =
      print_endline "THUNK!";
      0
    in
    let value_or_thunk = value_or_thunk ~default in
    let test t = print_s [%sexp (value_or_thunk t : int)] in
    (* trigger the thunk *)
    test Null;
    [%expect
      {|
      THUNK!
      0
      |}];
    (* same value, no trigger *)
    test (This 0);
    [%expect {| 0 |}];
    (* different value *)
    test (This 1);
    [%expect {| 1 |}];
    (* trigger the thunk again: no memoization *)
    test Null;
    [%expect
      {|
      THUNK!
      0
      |}]
  ;;

  let%template[@mode m = (global, local)] value_map = (Or_null.value_map [@mode m])

  let%expect_test "value_map" =
    print_s [%sexp (value_map Null ~default:42 ~f:(fun x -> x * 2) : int)];
    [%expect {| 42 |}];
    print_s [%sexp (value_map (This 5) ~default:42 ~f:(fun x -> x * 2) : int)];
    [%expect {| 10 |}]
  ;;

  let%template[@mode m = (global, local)] this x =
    (Or_null.this [@mode m]) x [@exclave_if_local m]
  ;;

  let%expect_test "this" =
    print_s [%sexp (this 42 : int or_null)];
    [%expect {| (42) |}]
  ;;

  let%template[@mode m = (global, local)] both = (Or_null.both [@mode m])

  let%expect_test "both" =
    print_s [%sexp (both Null Null : (int * int) or_null)];
    [%expect {| () |}];
    print_s [%sexp (both (This 1) Null : (int * int) or_null)];
    [%expect {| () |}];
    print_s [%sexp (both Null (This 2) : (int * int) or_null)];
    [%expect {| () |}];
    print_s [%sexp (both (This 1) (This 2) : (int * int) or_null)];
    [%expect {| ((1 2)) |}]
  ;;

  let%template[@mode m = (global, local)] this_if b a =
    (Or_null.this_if [@mode m]) b a [@exclave_if_local m]
  ;;

  let%expect_test "this_if" =
    print_s [%sexp (this_if true 42 : int or_null)];
    [%expect {| (42) |}];
    print_s [%sexp (this_if false 42 : int or_null)];
    [%expect {| () |}]
  ;;

  let%template[@mode m = (global, local)] this_if_thunk =
    (Or_null.this_if_thunk [@mode m])
  ;;

  let%expect_test "this_if_thunk" =
    (* In the [false] case, don't run the thunk *)
    print_s
      [%sexp (this_if_thunk false (fun () -> failwith "should not be run") : int or_null)];
    [%expect {| () |}];
    (* In the [true] case, do run the thunk *)
    print_s
      [%sexp
        (this_if_thunk true (fun () ->
           print_endline "running";
           42)
         : int or_null)];
    [%expect
      {|
      running
      (42)
      |}]
  ;;

  let%template[@mode m = (global, local)] iter = (Or_null.iter [@mode m])

  let%expect_test "iter" =
    iter Null ~f:(fun _ -> print_cr [%sexp "[iter Null ~f] should not call [f]"]);
    [%expect {| |}];
    iter (This 42) ~f:(fun value -> print_s [%message (value : int)]);
    [%expect {| (value 42) |}]
  ;;

  let%template[@mode m = (global, local)] call = (Or_null.call [@mode m])

  let%expect_test "call" =
    call 42 ~f:Null;
    [%expect {| |}];
    call 42 ~f:(This (fun value -> print_s [%message (value : int)]));
    [%expect {| (value 42) |}]
  ;;

  let%template[@mode m = (global, local)] first_this a b =
    (Or_null.first_this [@mode m]) a b [@exclave_if_local m]
  ;;

  let%expect_test "first_this" =
    print_s [%sexp (first_this Null Null : int or_null)];
    [%expect {| () |}];
    print_s [%sexp (first_this (This 1) (This 2) : int or_null)];
    [%expect {| (1) |}];
    print_s [%sexp (first_this Null (This 2) : int or_null)];
    [%expect {| (2) |}];
    print_s [%sexp (first_this (This 1) Null : int or_null)];
    [%expect {| (1) |}]
  ;;

  let%template[@mode m = (global, local)] first_this_thunk =
    (Or_null.first_this_thunk [@mode m])
  ;;

  let%expect_test "first_this_thunk" =
    print_s [%sexp (first_this_thunk Null (fun () -> Null) : int or_null)];
    [%expect {| () |}];
    print_s
      [%sexp
        (first_this_thunk (This 1) (fun () -> failwith "should not be run") : int or_null)];
    [%expect {| (1) |}];
    print_s [%sexp (first_this_thunk Null (fun () -> This 2) : int or_null)];
    [%expect {| (2) |}]
  ;;

  let%template[@mode m = (global, local)] map = (Or_null.map [@mode m])

  let%expect_test "map" =
    let double x = x * 2 in
    print_s [%sexp (map Null ~f:double : int or_null)];
    [%expect {| () |}];
    print_s [%sexp (map (This 21) ~f:double : int or_null)];
    [%expect {| (42) |}]
  ;;

  let%template[@mode m = (global, local)] bind = (Or_null.bind [@mode m])

  let%expect_test "bind" =
    let maybe_double x = if x > 0 then This (x * 2) else Null in
    print_s [%sexp (bind Null ~f:maybe_double : int or_null)];
    [%expect {| () |}];
    print_s [%sexp (bind (This 21) ~f:maybe_double : int or_null)];
    [%expect {| (42) |}];
    print_s [%sexp (bind (This (-1)) ~f:maybe_double : int or_null)];
    [%expect {| () |}]
  ;;

  let%template[@mode m = (global, local), n = (global, local)] fold =
    (Or_null.fold [@mode m n])
  ;;

  let%expect_test "fold" =
    let fold orn = fold orn ~init:42 ~f:(fun acc x -> acc + x) in
    print_s [%message (fold Null : int)];
    [%expect {| ("fold Null" 42) |}];
    print_s [%message (fold (This 100) : int)];
    [%expect {| ("fold (This 100)" 142) |}]
  ;;

  let%template[@mode m = (global, local)] to_list t =
    (Or_null.to_list [@mode m]) t [@exclave_if_local m]
  ;;

  let%template[@mode m = (global, local)] to_array = (Or_null.to_array [@mode m])

  let%expect_test "to_list and to_array" =
    let test_conversions t =
      Stdio.print_s [%message (to_list t : int list) (to_array t : int array)]
    in
    test_conversions Null;
    [%expect {| (("to_list t" ()) ("to_array t" ())) |}];
    test_conversions (This 42);
    [%expect {| (("to_list t" (42)) ("to_array t" (42))) |}]
  ;;

  let%template[@mode m = (global, local)] mem = (Or_null.mem [@mode m])

  let%expect_test "mem" =
    let mem orn value = mem orn value ~equal:Int.equal in
    print_s [%message (mem Null 42 : bool)];
    [%expect {| ("mem Null 42" false) |}];
    print_s [%message (mem (This 42) 42 : bool)];
    [%expect {| ("mem (This 42) 42" true) |}];
    print_s [%message (mem (This 42) 100 : bool)];
    [%expect {| ("mem (This 42) 100" false) |}]
  ;;

  let%template[@mode m = (global, local)] exists = (Or_null.exists [@mode m])
  let%template[@mode m = (global, local)] for_all = (Or_null.for_all [@mode m])

  let%expect_test "exists and for_all" =
    let is_positive x = x > 0 in
    let test_predicates t =
      Stdio.print_s
        [%message (exists t ~f:is_positive : bool) (for_all t ~f:is_positive : bool)]
    in
    test_predicates Null;
    [%expect {| (("exists t ~f:is_positive" false) ("for_all t ~f:is_positive" true)) |}];
    test_predicates (This 42);
    [%expect {| (("exists t ~f:is_positive" true) ("for_all t ~f:is_positive" true)) |}];
    test_predicates (This (-1));
    [%expect {| (("exists t ~f:is_positive" false) ("for_all t ~f:is_positive" false)) |}]
  ;;

  let%template[@mode m = (global, local)] find = (Or_null.find [@mode m])

  let%expect_test "find" =
    let is_even x = x % 2 = 0 in
    print_s [%sexp (find Null ~f:is_even : int or_null)];
    [%expect {| () |}];
    print_s [%sexp (find (This 1) ~f:is_even : int or_null)];
    [%expect {| () |}];
    print_s [%sexp (find (This 2) ~f:is_even : int or_null)];
    [%expect {| (2) |}]
  ;;

  let%template[@mode m = (global, local)] try_with = (Or_null.try_with [@mode m])

  let%expect_test "try_with" =
    print_s [%sexp (try_with (fun () -> 42) : int or_null)];
    [%expect {| (42) |}];
    print_s [%sexp (try_with (fun () -> failwith "error") : int or_null)];
    [%expect {| () |}]
  ;;

  let%template[@mode m = (global, local)] try_with_join =
    (Or_null.try_with_join [@mode m])
  ;;

  let%expect_test "try_with_join" =
    print_s [%sexp (try_with_join (fun () -> This 42) : int or_null)];
    [%expect {| (42) |}];
    print_s [%sexp (try_with_join (fun () -> Null) : int or_null)];
    [%expect {| () |}];
    print_s [%sexp (try_with_join (fun () -> failwith "error") : int or_null)];
    [%expect {| () |}]
  ;;

  let%template[@mode m = (global, local)] merge = (Or_null.merge [@mode m])

  let%expect_test "merge" =
    let f = ( + ) in
    print_s [%sexp (merge Null Null ~f : int or_null)];
    [%expect {| () |}];
    print_s [%sexp (merge (This 3) Null ~f : int or_null)];
    [%expect {| (3) |}];
    print_s [%sexp (merge Null (This 3) ~f : int or_null)];
    [%expect {| (3) |}];
    print_s [%sexp (merge (This 1) (This 3) ~f : int or_null)];
    [%expect {| (4) |}]
  ;;

  module Let_syntax = Or_null.Let_syntax

  let%expect_test "Let_syntax return" =
    let open Let_syntax in
    print_s [%sexp (return 42 : int or_null)];
    [%expect {| (42) |}]
  ;;

  let%expect_test "Let_syntax bind and map operators" =
    let open Let_syntax in
    let double x = This (x * 2) in
    print_s [%sexp (Null >>= double : int or_null)];
    [%expect {| () |}];
    print_s [%sexp (This 21 >>= double : int or_null)];
    [%expect {| (42) |}];
    print_s [%sexp (Null >>| Int.succ : int or_null)];
    [%expect {| () |}];
    print_s [%sexp (This 41 >>| Int.succ : int or_null)];
    [%expect {| (42) |}]
  ;;

  let%expect_test "let%bind syntax" =
    let maybe_double x = if x > 0 then This (x * 2) else Null in
    let test_bind o =
      let res =
        let%bind.Or_null x = o in
        maybe_double x
      in
      print_s [%sexp (res : int or_null)]
    in
    test_bind Null;
    [%expect {| () |}];
    test_bind (This 5);
    [%expect {| (10) |}];
    test_bind (This (-3));
    [%expect {| () |}]
  ;;

  let%expect_test "let%map syntax" =
    let test_map o =
      let res =
        let%map.Or_null x = o in
        x * 2
      in
      print_s [%sexp (res : int or_null)]
    in
    test_map Null;
    [%expect {| () |}];
    test_map (This 21);
    [%expect {| (42) |}]
  ;;

  let%expect_test "let%map with multiple bindings" =
    let test_map o1 o2 =
      let res =
        let%map.Or_null x = o1
        and y = o2 in
        x + y
      in
      print_s [%sexp (res : int or_null)]
    in
    test_map Null Null;
    [%expect {| () |}];
    test_map (This 10) Null;
    [%expect {| () |}];
    test_map Null (This 20);
    [%expect {| () |}];
    test_map (This 10) (This 20);
    [%expect {| (30) |}]
  ;;

  let%expect_test "nested let%bind and let%map" =
    let test_nested o1 o2 =
      let res =
        let%bind.Or_null x = o1 in
        let%map.Or_null y = o2 in
        x * y
      in
      print_s [%sexp (res : int or_null)]
    in
    test_nested Null Null;
    [%expect {| () |}];
    test_nested (This 6) Null;
    [%expect {| () |}];
    test_nested Null (This 7);
    [%expect {| () |}];
    test_nested (This 6) (This 7);
    [%expect {| (42) |}]
  ;;

  let%expect_test "complex nested ppx_let syntax" =
    let halve x = if x % 2 = 0 then This (x / 2) else Null in
    let test_complex o =
      let res =
        let%bind.Or_null a = o in
        let%bind.Or_null b = halve a in
        let%map.Or_null c = This (b + 1) in
        a + b + c
      in
      print_s [%sexp (res : int or_null)]
    in
    test_complex Null;
    [%expect {| () |}];
    test_complex (This 5);
    [%expect {| () |}];
    test_complex (This 10);
    [%expect {| (21) |}]
  ;;

  module Local = Or_null.Local

  let%expect_test "let%bindl local syntax" =
    let maybe_double (local_ x) = exclave_ if x > 0 then This (x * 2) else Null in
    let test_bind (local_ o) =
      let res =
        let%bindl.Or_null.Local x = o in
        maybe_double x
      in
      print_s [%sexp (res : int or_null)]
    in
    test_bind Null;
    [%expect {| () |}];
    test_bind (This 5);
    [%expect {| (10) |}];
    test_bind (This (-3));
    [%expect {| () |}]
  ;;

  let%expect_test "let%mapl local syntax" =
    let test_map (local_ o) =
      let res =
        let%mapl.Or_null.Local x = o in
        x * 2
      in
      print_s [%sexp (res : int or_null)]
    in
    test_map Null;
    [%expect {| () |}];
    test_map (This 21);
    [%expect {| (42) |}]
  ;;

  let%expect_test "let%mapl with multiple bindings" =
    let test_map (local_ o1) (local_ o2) =
      let res =
        let%mapl.Or_null.Local x = o1
        and y = o2 in
        x + y
      in
      print_s [%sexp (res : int or_null)]
    in
    test_map Null Null;
    [%expect {| () |}];
    test_map (This 10) Null;
    [%expect {| () |}];
    test_map Null (This 20);
    [%expect {| () |}];
    test_map (This 10) (This 20);
    [%expect {| (30) |}]
  ;;

  let%expect_test "nested let%bindl and let%mapl" =
    let test_nested (local_ o1) (local_ o2) =
      let res =
        let%bindl.Or_null.Local x = o1 in
        let%mapl.Or_null.Local y = o2 in
        x * y
      in
      print_s [%sexp (res : int or_null)]
    in
    test_nested Null Null;
    [%expect {| () |}];
    test_nested (This 6) Null;
    [%expect {| () |}];
    test_nested Null (This 7);
    [%expect {| () |}];
    test_nested (This 6) (This 7);
    [%expect {| (42) |}]
  ;;

  let%expect_test "complex nested local ppx_let syntax" =
    let halve (local_ x) = exclave_ if x % 2 = 0 then This (x / 2) else Null in
    let test_complex (local_ o) =
      let res =
        let%bindl.Or_null.Local a = o in
        let%bindl.Or_null.Local b = halve a in
        let%mapl.Or_null.Local c = This (b + 1) in
        a + b + c
      in
      print_s [%sexp (res : int or_null)]
    in
    test_complex Null;
    [%expect {| () |}];
    test_complex (This 5);
    [%expect {| () |}];
    test_complex (This 10);
    [%expect {| (21) |}]
  ;;

  module Optional_syntax = Optional_syntax

  let%expect_test "optional syntax" =
    let () =
      match%optional.Or_null This 3 with
      | None -> failwith "wrong"
      | Some x -> require_equal (module Int) x 3
    in
    let () =
      match%optional.Or_null Null with
      | Some x -> raise_s [%message "wrong" (x : int)]
      | None -> ()
    in
    [%expect {| |}]
  ;;
end
