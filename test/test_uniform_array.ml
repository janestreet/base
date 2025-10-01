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
  let r here = require_equal ~here (module Bool) in
  r [%here] false (test [] Fn.id);
  r [%here] true (test [ true ] Fn.id);
  r [%here] true (test [ false; false; false; false; true ] Fn.id);
  r [%here] true (test [ 0; 1; 2; 3; 4 ] (fun i -> i % 2 = 1));
  r [%here] false (test [ 0; 2; 4; 6; 8 ] (fun i -> i % 2 = 1));
  [%expect {| |}]
;;

let%expect_test "for_all" =
  let test arr f = of_list arr |> for_all ~f in
  let r here = require_equal ~here (module Bool) in
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

  let length = length
  let get t i = get t i
  let set t i x = set t i x
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
  require_does_raise (fun () -> test [ 1 ] [] (fun _ _ -> 0));
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
  require_does_raise (fun () -> test [ 1 ] []);
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
  [%expect
    {|
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
  [%expect
    {|
    ()
    ()
    |}];
  test (of_list [ 0; 1; 2; 3 ]);
  [%expect
    {|
    (0 2)
    (1 3)
    |}];
  test (of_list [ 0; 2; 4; 6 ]);
  [%expect
    {|
    (0 2 4 6)
    ()
    |}];
  test (of_list [ 1; 3; 5; 7 ]);
  [%expect
    {|
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

let%expect_test "or_null elements" =
  let arr = create ~len:5 Null in
  print_s [%sexp (arr : int or_null t)];
  [%expect
    {|
    (()
     ()
     ()
     ()
     ())
    |}];
  for i = 0 to 4 do
    set arr i (This i)
  done;
  print_s [%sexp (arr : int or_null t)];
  [%expect
    {|
    ((0)
     (1)
     (2)
     (3)
     (4))
    |}];
  let arr2 = concat [ arr; create ~len:3 Null ] in
  let arr3 =
    map arr2 ~f:(function
      | This i when i % 2 = 0 -> This (i / 2)
      | This _ -> Null
      | Null -> This (-1))
  in
  print_s [%sexp (arr3 : int or_null t)];
  [%expect
    {|
    ((0)
     ()
     (1)
     ()
     (2)
     (-1)
     (-1)
     (-1))
    |}];
  let arr4 = filter_map arr3 ~f:Or_null.to_option in
  print_s [%sexp (arr4 : int t)];
  [%expect {| (0 1 2 -1 -1 -1) |}]
;;

(* Invariant tests *)
module%test [@tags "no-js"] _ : module type of struct
  include Uniform_array
end = struct
  type ('a : value_or_null) t = 'a Uniform_array.t [@@deriving compare ~localize, sexp_of]

  let t_sexp_grammar = Uniform_array.t_sexp_grammar
  let invariant = Uniform_array.invariant

  (* We test that constructors satisfy the invariant, especially when given floats. *)

  open struct
    let test_poly ?(allow_empty = false) ?cr ~(here : [%call_pos]) t =
      assert (allow_empty || length t > 0);
      require_does_not_raise ~here ?cr (fun () -> invariant t)
    ;;

    let test ?allow_empty ?cr ~(here : [%call_pos]) (t : float t) =
      test_poly ?allow_empty ?cr ~here t
    ;;
  end

  let t_of_sexp = Uniform_array.t_of_sexp
  let%expect_test _ = test (t_of_sexp Float.t_of_sexp (List [ Atom "0" ]))
  let empty = Uniform_array.empty
  let%expect_test _ = test empty ~allow_empty:true
  let get_empty = Uniform_array.get_empty
  let%expect_test _ = test (get_empty ()) ~allow_empty:true
  let create = Uniform_array.create
  let%expect_test _ = test (create ~len:1 0.)
  let singleton = Uniform_array.singleton
  let%expect_test _ = test (singleton 0.)
  let init = Uniform_array.init
  let%expect_test _ = test (init 1 ~f:(fun _ -> 0.))
  let map = Uniform_array.map
  let%expect_test _ = test (map (singleton 0) ~f:(fun _ -> 0.))
  let mapi = Uniform_array.mapi
  let%expect_test _ = test (mapi (singleton 0) ~f:(fun _ _ -> 0.))
  let of_array = Uniform_array.of_array
  let%expect_test _ = test (of_array [| 0. |])
  let of_list = Uniform_array.of_list
  let%expect_test _ = test (of_list [ 0. ])
  let of_list_rev = Uniform_array.of_list_rev
  let%expect_test _ = test (of_list_rev [ 0. ])
  let sub = Uniform_array.sub
  let%expect_test _ = test (sub (singleton 0.) ~pos:0 ~len:1)
  let subo = Uniform_array.subo
  let%expect_test _ = test (subo (singleton 0.))
  let copy = Uniform_array.copy
  let%expect_test _ = test (copy (singleton 0.))
  let concat = Uniform_array.concat
  let%expect_test _ = test (concat [ singleton 0. ])
  let concat_map = Uniform_array.concat_map
  let%expect_test _ = test (concat_map (singleton 0) ~f:(fun _ -> singleton 0.))
  let concat_mapi = Uniform_array.concat_mapi
  let%expect_test _ = test (concat_mapi (singleton 0) ~f:(fun _ _ -> singleton 0.))
  let partition_map = Uniform_array.partition_map

  let%expect_test _ =
    let ts, fs = partition_map (of_list [ Either.First 0.; Either.Second 0. ]) ~f:Fn.id in
    test ts;
    test fs
  ;;

  let filter = Uniform_array.filter
  let%expect_test _ = test (filter (singleton 0.) ~f:(fun _ -> true))
  let filteri = Uniform_array.filteri
  let%expect_test _ = test (filteri (singleton 0.) ~f:(fun _ _ -> true))
  let filter_map = Uniform_array.filter_map
  let%expect_test _ = test (filter_map (singleton 0) ~f:(fun _ -> Some 0.))
  let filter_mapi = Uniform_array.filter_mapi
  let%expect_test _ = test (filter_mapi (singleton 0) ~f:(fun _ _ -> Some 0.))
  let map2_exn = Uniform_array.map2_exn
  let%expect_test _ = test (map2_exn (singleton 0) (singleton 0) ~f:(fun _ _ -> 0.))
  let unsafe_create_uninitialized = Uniform_array.unsafe_create_uninitialized

  let%expect_test _ =
    let t = unsafe_create_uninitialized ~len:1 in
    set t 0 0.;
    test t
  ;;

  let create_obj_array = Uniform_array.create_obj_array

  let%expect_test _ =
    let t = create_obj_array ~len:1 in
    set t 0 (Stdlib.Obj.repr 0.);
    test_poly t
  ;;

  (* Accessors, no invariant to test here *)

  let length = Uniform_array.length
  let get = Uniform_array.get
  let unsafe_get = Uniform_array.unsafe_get
  let set = Uniform_array.set
  let unsafe_set = Uniform_array.unsafe_set
  let swap = Uniform_array.swap
  let unsafe_set_omit_phys_equal_check = Uniform_array.unsafe_set_omit_phys_equal_check
  let unsafe_set_with_caml_modify = Uniform_array.unsafe_set_with_caml_modify
  let set_with_caml_modify = Uniform_array.set_with_caml_modify
  let iter = Uniform_array.iter
  let iteri = Uniform_array.iteri
  let fold = Uniform_array.fold
  let foldi = Uniform_array.foldi

  let unsafe_to_array_inplace__promise_not_a_float =
    Uniform_array.unsafe_to_array_inplace__promise_not_a_float
  ;;

  let%template to_array = (Uniform_array.to_array [@alloc a]) [@@alloc a = (heap, stack)]
  let to_list = Uniform_array.to_list
  let blit = Uniform_array.blit
  let blito = Uniform_array.blito
  let unsafe_blit = Uniform_array.unsafe_blit
  let exists = Uniform_array.exists
  let existsi = Uniform_array.existsi
  let for_all = Uniform_array.for_all
  let for_alli = Uniform_array.for_alli
  let find = Uniform_array.find
  let findi = Uniform_array.findi
  let find_map = Uniform_array.find_map
  let find_mapi = Uniform_array.find_mapi
  let fold2_exn = Uniform_array.fold2_exn
  let min_elt = Uniform_array.min_elt
  let max_elt = Uniform_array.max_elt
  let sort = Uniform_array.sort
  let binary_search = Uniform_array.binary_search
  let binary_search_segmented = Uniform_array.binary_search_segmented
  let unsafe_set_assuming_currently_int = Uniform_array.unsafe_set_assuming_currently_int

  let unsafe_set_int_assuming_currently_int =
    Uniform_array.unsafe_set_int_assuming_currently_int
  ;;

  let unsafe_set_int = Uniform_array.unsafe_set_int
  let unsafe_clear_if_pointer = Uniform_array.unsafe_clear_if_pointer
end
