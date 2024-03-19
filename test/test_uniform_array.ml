open! Import
open Uniform_array

let does_raise = Exn.does_raise
let zero_obj = Stdlib.Obj.repr (0 : int)

(* [create_obj_array] *)
let%test_unit _ =
  let t = create_obj_array ~len:0 in
  assert (length t = 0)
;;

(* [create] *)
let%test_unit _ =
  let str = Stdlib.Obj.repr "foo" in
  let t = create ~len:2 str in
  assert (phys_equal (get t 0) str);
  assert (phys_equal (get t 1) str)
;;

let%test_unit _ =
  let float = Stdlib.Obj.repr 3.5 in
  let t = create ~len:2 float in
  assert (Stdlib.Obj.tag (Stdlib.Obj.repr t) = 0);
  (* not a double array *)
  assert (phys_equal (get t 0) float);
  assert (phys_equal (get t 1) float);
  set t 1 (Stdlib.Obj.repr 4.);
  assert (Float.( = ) (Stdlib.Obj.obj (get t 1)) 4.)
;;

(* [empty] *)
let%test _ = length empty = 0
let%test _ = does_raise (fun () -> get empty 0)

(* [singleton] *)
let%test _ = length (singleton zero_obj) = 1
let%test _ = phys_equal (get (singleton zero_obj) 0) zero_obj
let%test _ = does_raise (fun () -> get (singleton zero_obj) 1)

let%test_unit _ =
  let f = 13. in
  let t = singleton (Stdlib.Obj.repr f) in
  invariant t;
  assert (Poly.equal (Stdlib.Obj.repr f) (get t 0))
;;

(* [get], [unsafe_get], [set], [unsafe_set], [unsafe_set_assuming_currently_int],
   [set_with_caml_modify] *)
let%test_unit _ =
  let t = create_obj_array ~len:1 in
  assert (length t = 1);
  assert (phys_equal (get t 0) zero_obj);
  assert (phys_equal (unsafe_get t 0) zero_obj);
  let one_obj = Stdlib.Obj.repr (1 : int) in
  let check_get expect =
    assert (phys_equal (get t 0) expect);
    assert (phys_equal (unsafe_get t 0) expect)
  in
  set t 0 one_obj;
  check_get one_obj;
  unsafe_set t 0 zero_obj;
  check_get zero_obj;
  unsafe_set_assuming_currently_int t 0 one_obj;
  check_get one_obj;
  set_with_caml_modify t 0 zero_obj;
  check_get zero_obj
;;

let%expect_test "exists" =
  let test arr f = of_list arr |> exists ~f in
  let r here = require_equal here (module Bool) in
  r [%here] false (test [] Fn.id);
  r [%here] true (test [ true ] Fn.id);
  r [%here] true (test [ false; false; false; false; true ] Fn.id);
  r [%here] true (test [ 0; 1; 2; 3; 4 ] (fun i -> i % 2 = 1));
  r [%here] false (test [ 0; 2; 4; 6; 8 ] (fun i -> i % 2 = 1));
  [%expect {| |}]
;;

let%expect_test "for_all" =
  let test arr f = of_list arr |> for_all ~f in
  let r here = require_equal here (module Bool) in
  r [%here] true (test [] Fn.id);
  r [%here] true (test [ true ] Fn.id);
  r [%here] false (test [ false; false; false; false; true ] Fn.id);
  r [%here] false (test [ 0; 1; 2; 3; 4 ] (fun i -> i % 2 = 1));
  r [%here] true (test [ 0; 2; 4; 6; 8 ] (fun i -> i % 2 = 0));
  [%expect {| |}]
;;

let%expect_test "iteri" =
  let test arr = of_list arr |> iteri ~f:(printf "(%d %c)") in
  test [];
  [%expect {| |}];
  test [ 'a' ];
  [%expect {| (0 a) |}];
  test [ 'a'; 'b'; 'c'; 'd' ];
  [%expect {| (0 a)(1 b)(2 c)(3 d) |}]
;;

module Sequence = struct
  type nonrec 'a t = 'a t
  type 'a z = 'a

  let length = length
  let get = get
  let set = set
  let create_bool ~len = create ~len false
end

include Base_for_tests.Test_blit.Test1 (Sequence) (Uniform_array)

let%expect_test "map2_exn" =
  let test a1 a2 f =
    let result = map2_exn ~f (of_list a1) (of_list a2) in
    print_s [%message (result : int Uniform_array.t)]
  in
  test [] [] (fun _ -> failwith "don't call me");
  [%expect {| (result ()) |}];
  test [ 1; 2; 3 ] [ 100; 200; 300 ] ( + );
  [%expect {| (result (101 202 303)) |}];
  require_does_raise [%here] (fun () -> test [ 1 ] [] (fun _ _ -> 0));
  [%expect {| (Invalid_argument Array.map2_exn) |}]
;;

let%expect_test "fold2_exn" =
  let test a1 a2 =
    let result =
      fold2_exn ~init:0 ~f:(fun acc x y -> acc + x + (1000 * y)) (of_list a1) (of_list a2)
    in
    print_s [%sexp (result : int)]
  in
  test [] [];
  [%expect {| 0 |}];
  test [ 1; 2; 3 ] [ 7; 8; 9 ];
  [%expect {| 24_006 |}];
  require_does_raise [%here] (fun () -> test [ 1 ] []);
  [%expect {| (Invalid_argument Array.fold2_exn) |}]
;;

let%expect_test "mapi" =
  let test arr =
    let mapped = of_list arr |> mapi ~f:(fun i str -> i, String.capitalize str) in
    print_s [%sexp (mapped : (int * string) t)]
  in
  test [];
  [%expect {| () |}];
  test [ "foo"; "bar" ];
  [%expect {|
    ((0 Foo)
     (1 Bar))
    |}]
;;

let%expect_test "of_list_rev" =
  let test a = print_s [%sexp (of_list_rev a : int t)] in
  test [];
  [%expect {| () |}];
  test [ 1; 2; 3; 4 ];
  [%expect {| (4 3 2 1) |}]
;;

let%expect_test "concat" =
  let test ts = print_s [%sexp (concat ts : int t)] in
  test [];
  [%expect {| () |}];
  test [ of_list [ 1; 2; 3 ]; of_list [ 4; 5; 6 ]; empty; of_list [ 7 ] ];
  [%expect {| (1 2 3 4 5 6 7) |}]
;;

let%expect_test "concat_map" =
  let test t =
    print_s
      [%sexp
        (concat_map t ~f:(fun i -> of_list [ i * 10; (i * 10) + 1; (i * 10) + 2 ])
          : int t)]
  in
  test empty;
  [%expect {| () |}];
  test (of_list [ 1; 2; 3; 4 ]);
  [%expect {| (10 11 12 20 21 22 30 31 32 40 41 42) |}]
;;

let%expect_test "concat_mapi" =
  let test t =
    print_s
      [%sexp
        (concat_mapi t ~f:(fun idx i ->
           if idx = 1 then empty else of_list [ i * 10; (i * 10) + 1; (i * 10) + 2 ])
          : int t)]
  in
  test empty;
  [%expect {| () |}];
  test (of_list [ 1; 2; 3; 4 ]);
  [%expect {| (10 11 12 30 31 32 40 41 42) |}]
;;

let%expect_test "partition_map" =
  let test t =
    let first, second =
      partition_map t ~f:(fun i ->
        match i % 2 = 0 with
        | true -> First i
        | false -> Second i)
    in
    print_s [%sexp (first : int t)];
    print_s [%sexp (second : int t)]
  in
  test empty;
  [%expect {|
    ()
    ()
    |}];
  test (of_list [ 0; 1; 2; 3 ]);
  [%expect {|
    (0 2)
    (1 3)
    |}];
  test (of_list [ 0; 2; 4; 6 ]);
  [%expect {|
    (0 2 4 6)
    ()
    |}];
  test (of_list [ 1; 3; 5; 7 ]);
  [%expect {|
    ()
    (1 3 5 7)
    |}]
;;

let%expect_test "filter" =
  let test t = print_s [%sexp (filter t ~f:(fun i -> i % 2 = 0) : int t)] in
  test empty;
  [%expect {| () |}];
  test (of_list [ 1; 2; 3; 4; 5; 6; 7; 8 ]);
  [%expect {| (2 4 6 8) |}]
;;

let%expect_test "filteri" =
  let test t =
    print_s [%sexp (filteri t ~f:(fun idx i -> idx = 0 || i % 2 = 0) : int t)]
  in
  test empty;
  [%expect {| () |}];
  test (of_list [ 1; 2; 3; 4; 5; 6; 7; 8 ]);
  [%expect {| (1 2 4 6 8) |}]
;;

let%expect_test "filter_map" =
  let test t =
    print_s
      [%sexp
        (filter_map t ~f:(fun i ->
           if i % 2 = 0 then None else Some (Char.of_int_exn (Char.to_int 'a' + i)))
          : char t)]
  in
  test empty;
  [%expect {| () |}];
  test (of_list [ 1; 2; 3; 4; 5; 6; 7; 8 ]);
  [%expect {| (b d f h) |}]
;;

let%expect_test "filter_mapi" =
  let test t =
    print_s
      [%sexp
        (filter_mapi t ~f:(fun idx i ->
           if idx = 0 || i % 2 = 0
           then None
           else
             Some
               (Int.to_string idx
                ^ ": "
                ^ Char.to_string (Char.of_int_exn (Char.to_int 'a' + i))))
          : string t)]
  in
  test empty;
  [%expect {| () |}];
  test (of_list [ 1; 2; 3; 4; 5; 6; 7; 8 ]);
  [%expect {| ("2: d" "4: f" "6: h") |}]
;;

let%expect_test "find" =
  let test t = print_s [%sexp (find t ~f:(fun i -> i >= 6) : int option)] in
  test empty;
  [%expect {| () |}];
  test (of_list [ 1; 5; 2; 6; 7; 3; 0; -8; 10 ]);
  [%expect {| (6) |}]
;;

let%expect_test "findi" =
  let test t =
    print_s [%sexp (findi t ~f:(fun idx i -> idx % 2 = 0 && i >= 6) : (int * int) option)]
  in
  test empty;
  [%expect {| () |}];
  test (of_list [ 1; 5; 2; 6; 7; 3; 0; -8; 10 ]);
  [%expect {| ((4 7)) |}]
;;

let%expect_test "find_map" =
  let test t = print_s [%sexp (find_map t ~f:Char.of_int : char option)] in
  test empty;
  [%expect {| () |}];
  test (of_list [ 500; 1000; -3; 65; 66; 7000 ]);
  [%expect {| (A) |}]
;;

let%expect_test "find_mapi" =
  let test t =
    print_s
      [%sexp
        (find_mapi t ~f:(fun idx i -> if idx % 2 = 1 then None else Char.of_int i)
          : char option)]
  in
  test empty;
  [%expect {| () |}];
  test (of_list [ 500; 1000; -3; 65; 66; 7000 ]);
  [%expect {| (B) |}]
;;

let%expect_test "unsafe_to_array_inplace__promise_not_a_float" =
  let arr = of_list [ 1; 2; 3; 4; 5 ] in
  print_s [%sexp (arr : int t)];
  [%expect {| (1 2 3 4 5) |}];
  let arr = unsafe_to_array_inplace__promise_not_a_float arr in
  print_s [%sexp (arr : int array)];
  [%expect {| (1 2 3 4 5) |}]
;;
