open! Import
open Uniform_array

let%expect_test "[create] with [Null] elements" =
  let t = create ~len:2 Null in
  print_s [%sexp (t : int or_null t)];
  [%expect
    {|
    (()
     ())
    |}]
;;

let%expect_test "uniform array representation" =
  let value : float or_null = This 42. in
  let t = create ~len:3 value in
  [%test_result: int] (Stdlib.Obj.tag (Stdlib.Obj.repr t)) ~expect:0;
  (* not a double array *)
  [%test_result: bool] (phys_equal (get t 0) value) ~expect:true;
  [%test_result: bool] (phys_equal (get t 1) value) ~expect:true;
  set t 1 (This 84.);
  set t 2 Null;
  print_s [%sexp (t : float or_null t)];
  [%expect
    {|
    ((42)
     (84)
     ())
    |}]
;;

let%expect_test "[empty]" =
  print_s [%sexp (empty : int or_null t)];
  [%expect {| () |}]
;;

let%expect_test "[get] on [empty]" =
  require_does_raise (fun () -> ignore (get empty 0 : int or_null));
  [%expect {| (Invalid_argument "index out of bounds") |}]
;;

let%expect_test "[singleton] [Null]" =
  let null = Null in
  let t = singleton null in
  print_s [%sexp (t : string or_null t)];
  [%expect {| (()) |}];
  require_does_raise (fun () -> ignore (get t 1 : string or_null));
  [%expect {| (Invalid_argument "index out of bounds") |}]
;;

let%expect_test "[singleton] [This]" =
  let value = This "hello world" in
  let t = singleton value in
  print_s [%sexp (t : string or_null t)];
  [%expect {| (("hello world")) |}];
  require_does_raise (fun () -> ignore (get t 1 : string or_null));
  [%expect {| (Invalid_argument "index out of bounds") |}]
;;

let%expect_test "[length], [get], [unsafe_get], [set], [unsafe_set] with [or_null] \
                 elements"
  =
  let t = create ~len:1 Null in
  print_s
    [%message (length t : int) (get t 0 : int or_null) (unsafe_get t 0 : int or_null)];
  [%expect
    {|
    (("length t" 1)
     ("get t 0"        ())
     ("unsafe_get t 0" ()))
    |}];
  let value : int or_null = This 42 in
  let check_get expect =
    [%test_result: bool] ([%equal: int Or_null.t] (get t 0) expect) ~expect:true;
    [%test_result: bool] ([%equal: int Or_null.t] (unsafe_get t 0) expect) ~expect:true
  in
  set t 0 value;
  check_get value;
  unsafe_set t 0 Null;
  check_get Null;
  set_with_caml_modify t 0 value;
  check_get value
;;

let%expect_test "[map] with [or_null]" =
  let t = of_list [ This 1; This 2; Null; This 4 ] in
  let mapped =
    map t ~f:(function
      | This x -> This (x * 2)
      | Null -> This (-1))
  in
  print_s [%sexp (mapped : int or_null t)];
  [%expect
    {|
    ((2)
     (4)
     (-1)
     (8))
    |}]
;;

let%expect_test "[mapi] with [or_null]" =
  let t = of_list [ This 1; This 2; Null; This 4 ] in
  let mapped =
    mapi t ~f:(fun i x ->
      match x with
      | This v -> This (i + v)
      | Null -> This (-i))
  in
  print_s [%sexp (mapped : int or_null t)];
  [%expect
    {|
    ((1)
     (3)
     (-2)
     (7))
    |}]
;;

let%expect_test "[iter] with [or_null]" =
  let t = of_list [ This "a"; Null; This "c" ] in
  iter t ~f:(function
    | This x -> printf "%s; " x
    | Null -> printf "null; ");
  [%expect {| a; null; c; |}]
;;

let%expect_test "[iteri] with [or_null]" =
  let t = of_list [ This "a"; Null; This "c" ] in
  iteri t ~f:(fun i x ->
    match x with
    | This v -> printf "(%d %s); " i v
    | Null -> printf "(%d null); " i);
  [%expect {| (0 a); (1 null); (2 c); |}]
;;

let%expect_test "[fold] with [or_null]" =
  let t = of_list [ This 1; This 2; Null; This 4 ] in
  let sum =
    fold t ~init:0 ~f:(fun acc x ->
      match x with
      | This v -> acc + v
      | Null -> acc)
  in
  print_s [%sexp (sum : int)];
  [%expect {| 7 |}]
;;

let%expect_test "[foldi] with [or_null]" =
  let t = of_list [ This 1; This 2; Null; This 4 ] in
  let result =
    foldi t ~init:[] ~f:(fun i acc x ->
      match x with
      | This v -> (i, v) :: acc
      | Null -> acc)
  in
  print_s [%sexp (result : (int * int) list)];
  [%expect
    {|
    ((3 4)
     (1 2)
     (0 1))
    |}]
;;

let%expect_test "[exists] with [or_null]" =
  let test arr f = of_list arr |> exists ~f in
  let r here = require_equal ~here (module Bool) in
  r [%here] false (test [] (function _ -> true));
  r
    [%here]
    true
    (test [ This true ] (function
      | This x -> x
      | Null -> false));
  r
    [%here]
    false
    (test [ This false; This false; Null; This false ] (function
      | This x -> x
      | Null -> false));
  r
    [%here]
    true
    (test [ This 0; This 1; This 2; Null; This 4 ] (function
      | This x -> x % 2 = 1
      | Null -> false));
  r
    [%here]
    false
    (test [ This 0; This 2; This 4; Null; This 8 ] (function
      | This x -> x % 2 = 1
      | Null -> false));
  r
    [%here]
    true
    (test [ This 0; Null; This 4 ] (function
      | Null -> true
      | _ -> false));
  [%expect {| |}]
;;

let%expect_test "[for_all] with [or_null]" =
  let test arr f = of_list arr |> for_all ~f in
  let r here = require_equal ~here (module Bool) in
  r [%here] true (test [] (function _ -> false));
  r
    [%here]
    true
    (test [ This true ] (function
      | This x -> x
      | Null -> false));
  r
    [%here]
    false
    (test [ This false; This false; Null; This false ] (function
      | This x -> x
      | Null -> false));
  r
    [%here]
    false
    (test [ This 0; This 1; This 2; Null; This 4 ] (function
      | This x -> x % 2 = 0
      | Null -> false));
  r
    [%here]
    false
    (test [ This 0; This 2; This 4; Null; This 8 ] (function
      | This x -> x % 2 = 0
      | Null -> false));
  r
    [%here]
    true
    (test [ This 0; This 2; This 4; This 8 ] (function
      | This x -> x % 2 = 0
      | Null -> false));
  [%expect {| |}]
;;

let%expect_test "[filter] with [or_null]" =
  let t = of_list [ This 1; This 2; Null; This 3; This 4; Null ] in
  let filtered =
    filter t ~f:(function
      | This x -> x % 2 = 0
      | Null -> true)
  in
  print_s [%sexp (filtered : int or_null t)];
  [%expect {| ((2) () (4) ()) |}]
;;

let%expect_test "[filteri] with [or_null]" =
  let t = of_list [ This 1; This 2; Null; This 3; This 4; Null ] in
  let filtered =
    filteri t ~f:(fun i x ->
      match x with
      | This v -> i % 2 = 0 || v % 2 = 0
      | Null -> true)
  in
  print_s [%sexp (filtered : int or_null t)];
  [%expect
    {|
    ((1)
     (2)
     ()
     (4)
     ())
    |}]
;;

let%expect_test "[filter_map] with [or_null]" =
  let t = of_list [ This 1; This 2; Null; This 3; This 4 ] in
  let result =
    filter_map t ~f:(function
      | This x when x % 2 = 0 -> Some (x * 2)
      | _ -> None)
  in
  print_s [%sexp (result : int t)];
  [%expect {| (4 8) |}]
;;

let%expect_test "[filter_mapi] with [or_null]" =
  let t = of_list [ This 1; This 2; Null; This 3; This 4 ] in
  let result =
    filter_mapi t ~f:(fun i x ->
      match x with
      | This v when i % 2 = 0 -> Some (i + v)
      | _ -> None)
  in
  print_s [%sexp (result : int t)];
  [%expect {| (1 8) |}]
;;

let%expect_test "[find] with [or_null]" =
  let t = of_list [ This 1; This 2; Null; This 3; This 4 ] in
  let result =
    find t ~f:(function
      | This x -> x > 2
      | Null -> false)
    |> function
    | Some x -> x
    | None -> Null
  in
  print_s [%sexp (result : int or_null)];
  [%expect {| (3) |}]
;;

let%expect_test "[findi] with [or_null]" =
  let t = of_list [ This 1; This 2; Null; This 3; This 4 ] in
  let result =
    findi t ~f:(fun i x ->
      match x with
      | This v -> i > 2 && v > 2
      | Null -> false)
  in
  print_s [%sexp (result : (int * int or_null) option)];
  [%expect {| ((3 (3))) |}]
;;

let%expect_test "[find_map] with [or_null]" =
  let t = of_list [ This 1; This 2; Null; This 3; This 4 ] in
  let result =
    find_map t ~f:(function
      | This x when x > 2 -> Some (x * 2)
      | _ -> None)
  in
  print_s [%sexp (result : int option)];
  [%expect {| (6) |}]
;;

let%expect_test "[find_mapi] with [or_null]" =
  let t = of_list [ This 1; This 2; Null; This 3; This 4 ] in
  let result =
    find_mapi t ~f:(fun i x ->
      match x with
      | This v when i % 2 = 1 && v > 1 -> Some (i + v)
      | _ -> None)
  in
  print_s [%sexp (result : int option)];
  [%expect {| (3) |}]
;;

let%expect_test "[partition_map] with [or_null]" =
  let t = of_list [ This 1; This 2; Null; This 3; This 4 ] in
  let first, second =
    partition_map t ~f:(function
      | This x when x % 2 = 0 -> First x
      | This x -> Second x
      | Null -> Second (-1))
  in
  print_s [%sexp (first : int t)];
  print_s [%sexp (second : int t)];
  [%expect
    {|
    (2 4)
    (1 -1 3)
    |}]
;;

let%expect_test "[map2_exn] with [or_null]" =
  let test a1 a2 f =
    let result = map2_exn ~f (of_list a1) (of_list a2) in
    print_s [%message (result : int or_null t)]
  in
  test [] [] (fun _ _ -> failwith "don't call me");
  [%expect {| (result ()) |}];
  test [ This 1; This 2; Null ] [ This 10; Null; This 30 ] (fun a b ->
    match a, b with
    | This x, This y -> This (x + y)
    | This x, Null -> This x
    | Null, This y -> This y
    | Null, Null -> Null);
  [%expect
    {|
    (result (
      (11)
      (2)
      (30)))
    |}];
  require_does_raise (fun () -> test [ This 1 ] [] (fun _ _ -> This 0));
  [%expect {| (Invalid_argument Array.map2_exn) |}]
;;

let%expect_test "[fold2_exn] with [or_null]" =
  let test a1 a2 =
    let result =
      fold2_exn
        ~init:0
        ~f:(fun acc x y ->
          match x, y with
          | This x, This y -> acc + x + (1000 * y)
          | This x, Null -> acc + x
          | Null, This y -> acc + (1000 * y)
          | Null, Null -> acc)
        (of_list a1)
        (of_list a2)
    in
    print_s [%sexp (result : int)]
  in
  test [] [];
  [%expect {| 0 |}];
  test [ This 1; This 2; Null ] [ This 7; Null; This 9 ];
  [%expect {| 16_003 |}];
  require_does_raise (fun () -> test [ This 1 ] []);
  [%expect {| (Invalid_argument Array.fold2_exn) |}]
;;

let%expect_test "[of_list_rev] with [or_null]" =
  let test a = print_s [%sexp (of_list_rev a : int or_null t)] in
  test [];
  [%expect {| () |}];
  test [ This 1; This 2; Null; This 4 ];
  [%expect
    {|
    ((4)
     ()
     (2)
     (1))
    |}]
;;

let%expect_test "[concat] with [or_null]" =
  let test ts = print_s [%sexp (concat ts : int or_null t)] in
  test [];
  [%expect {| () |}];
  test [ of_list [ This 1; This 2 ]; of_list [ This 3; Null ]; empty; of_list [ This 7 ] ];
  [%expect
    {|
    ((1)
     (2)
     (3)
     ()
     (7))
    |}]
;;

let%expect_test "[concat_map] with [or_null]" =
  let test t =
    print_s
      [%sexp
        (concat_map t ~f:(function
           | This i -> of_list [ This (i * 10); This ((i * 10) + 1); This ((i * 10) + 2) ]
           | Null -> of_list [ This 0; Null; This 0 ])
         : int or_null t)]
  in
  test empty;
  [%expect {| () |}];
  test (of_list [ This 1; Null; This 3 ]);
  [%expect
    {|
    ((10)
     (11)
     (12)
     (0)
     ()
     (0)
     (30)
     (31)
     (32))
    |}]
;;

let%expect_test "[concat_mapi] with [or_null]" =
  let test t =
    print_s
      [%sexp
        (concat_mapi t ~f:(fun idx x ->
           match x with
           | This i ->
             if idx = 1
             then empty
             else of_list [ This (i * 10); This ((i * 10) + 1); This ((i * 10) + 2) ]
           | Null -> of_list [ This idx; Null ])
         : int or_null t)]
  in
  test empty;
  [%expect {| () |}];
  test (of_list [ This 1; This 2; Null; This 4 ]);
  [%expect
    {|
    ((10)
     (11)
     (12)
     (2)
     ()
     (40)
     (41)
     (42))
    |}]
;;

let%expect_test "conversion between [or_null] and non-null uniform arrays" =
  let null_array = create ~len:3 (Null : int or_null) in
  set null_array 1 (This 42);
  let regular_array = filter_map null_array ~f:Or_null.to_option in
  print_s [%sexp (regular_array : int t)];
  [%expect {| (42) |}];
  let regular_array = of_list [ 1; 2; 3 ] in
  let nullable_array = map regular_array ~f:Or_null.this in
  print_s [%sexp (nullable_array : int or_null t)];
  [%expect
    {|
    ((1)
     (2)
     (3))
    |}]
;;
