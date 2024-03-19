open! Base
open Expect_test_helpers_base

module type S = sig
  type t [@@deriving sexp_of]

  include Comparable.Comparisons with type t := t
end

(* Test the consistency of derived comparison operators with [compare] because many of
   them are hand-optimized in [Base]. *)
let test (type a) here (module T : S with type t = a) list =
  let op (type b) (module Result : S with type t = b) operator ~actual ~expect =
    With_return.with_return (fun failed ->
      List.iter list ~f:(fun arg1 ->
        List.iter list ~f:(fun arg2 ->
          let actual = actual arg1 arg2 in
          let expect = expect arg1 arg2 in
          if not (Result.compare actual expect = 0)
          then (
            print_cr
              here
              [%message
                "comparison failed"
                  (operator : string)
                  (arg1 : T.t)
                  (arg2 : T.t)
                  (actual : Result.t)
                  (expect : Result.t)];
            failed.return ()))))
  in
  let module C = Comparable.Make (T) in
  op (module Bool) "equal" ~actual:T.equal ~expect:C.equal;
  op (module T) "min" ~actual:T.min ~expect:C.min;
  op (module T) "max" ~actual:T.max ~expect:C.max;
  op (module Bool) "(=)" ~actual:T.( = ) ~expect:C.( = );
  op (module Bool) "(<)" ~actual:T.( < ) ~expect:C.( < );
  op (module Bool) "(>)" ~actual:T.( > ) ~expect:C.( > );
  op (module Bool) "(<>)" ~actual:T.( <> ) ~expect:C.( <> );
  op (module Bool) "(<=)" ~actual:T.( <= ) ~expect:C.( <= );
  op (module Bool) "(>=)" ~actual:T.( >= ) ~expect:C.( >= );
  op
    (module Bool)
    "Comparable.equal"
    ~actual:(fun a b -> Comparable.equal T.compare a b)
    ~expect:C.equal;
  op
    (module T)
    "Comparable.min"
    ~actual:(fun a b -> Comparable.min T.compare a b)
    ~expect:C.min;
  op
    (module T)
    "Comparable.max"
    ~actual:(fun a b -> Comparable.max T.compare a b)
    ~expect:C.max
;;

let%expect_test "Base" =
  test
    [%here]
    (module struct
      include Base

      type t = int [@@deriving sexp_of]
    end)
    Int.[ min_value; minus_one; zero; one; max_value ];
  [%expect {| |}]
;;

let%expect_test "Unit" =
  test [%here] (module Unit) Unit.all;
  [%expect {| |}]
;;

let%expect_test "Bool" =
  test [%here] (module Bool) Bool.all;
  [%expect {| |}]
;;

let%expect_test "Char" =
  test [%here] (module Char) Char.all;
  [%expect {| |}]
;;

let%expect_test "Float" =
  test [%here] (module Float) Float.[ min_value; minus_one; zero; one; max_value ];
  [%expect {| |}]
;;

let%expect_test "Int" =
  test [%here] (module Int) Int.[ min_value; minus_one; zero; one; max_value ];
  [%expect {| |}]
;;

let%expect_test "Int32" =
  test [%here] (module Int32) Int32.[ min_value; minus_one; zero; one; max_value ];
  [%expect {| |}]
;;

let%expect_test "Int64" =
  test [%here] (module Int64) Int64.[ min_value; minus_one; zero; one; max_value ];
  [%expect {| |}]
;;

let%expect_test "Nativeint" =
  test [%here] (module Nativeint) Nativeint.[ min_value; minus_one; zero; one; max_value ];
  [%expect {| |}]
;;

let%expect_test "Int63" =
  test [%here] (module Int63) Int63.[ min_value; minus_one; zero; one; max_value ];
  [%expect {| |}]
;;

let%test_module "lexicographic" =
  (module struct
    let%expect_test "single" =
      Ref.set_temporarily sexp_style To_string_hum ~f:(fun () ->
        List.iter
          [ 1, 2; 1, 1; 2, 1 ]
          ~f:(fun (a, b) ->
            let ordering = Ordering.of_int (compare a b) in
            print_s [%message (a : int) (b : int) (ordering : Ordering.t)];
            require_equal
              [%here]
              (module Ordering)
              (Ordering.of_int (compare a b))
              (Ordering.of_int (Comparable.lexicographic [ compare ] a b)));
        [%expect
          {|
          ((a 1) (b 2) (ordering Less))
          ((a 1) (b 1) (ordering Equal))
          ((a 2) (b 1) (ordering Greater))
          |}])
    ;;

    let%expect_test "three comparisons" =
      Ref.set_temporarily sexp_style To_string_hum ~f:(fun () ->
        let compare_first_three_elts a_1 b_1 =
          Comparable.lexicographic
            (List.init 3 ~f:(fun i a b -> compare a.(i) b.(i)))
            a_1
            b_1
        in
        let test a b =
          let a = Array.of_list a in
          let b = Array.of_list b in
          let ordering = Ordering.of_int (compare_first_three_elts a b) in
          print_s [%message (a : int array) (b : int array) (ordering : Ordering.t)]
        in
        test [ 1; 2; 3; 4 ] [ 1; 2; 4; 9 ];
        [%expect {| ((a (1 2 3 4)) (b (1 2 4 9)) (ordering Less)) |}];
        test [ 1; 2; 3; 4 ] [ 1; 2; 3; 9 ];
        [%expect {| ((a (1 2 3 4)) (b (1 2 3 9)) (ordering Equal)) |}];
        test [ 1; 2; 3; 4 ] [ 1; 1; 4; 9 ];
        [%expect {| ((a (1 2 3 4)) (b (1 1 4 9)) (ordering Greater)) |}])
    ;;
  end)
;;

let%expect_test "reversed" =
  let list = [ 3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5; 9 ] in
  let sort_asc1 = List.sort ~compare:[%compare: int] list in
  let sort_desc = List.sort ~compare:[%compare: int Comparable.reversed] list in
  let sort_asc2 =
    List.sort ~compare:[%compare: int Comparable.reversed Comparable.reversed] list
  in
  print_s [%message (sort_asc1 : int list) (sort_desc : int list) (sort_asc2 : int list)];
  [%expect
    {|
    ((sort_asc1 (1 1 2 3 3 4 5 5 5 6 9 9))
     (sort_desc (9 9 6 5 5 5 4 3 3 2 1 1))
     (sort_asc2 (1 1 2 3 3 4 5 5 5 6 9 9)))
    |}]
;;
