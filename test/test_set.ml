open! Import
open! Set

type int_set = Set.M(Int).t [@@deriving compare, equal, hash, sexp]

let%test _ = invariants (of_increasing_iterator_unchecked (module Int) ~len:20 ~f:Fn.id)
let%test _ = invariants (Poly.of_increasing_iterator_unchecked ~len:20 ~f:Fn.id)
let of_list = of_list (module Int)

let%expect_test "split_le_gt" =
  for len = 1 to 4 do
    print_endline "";
    for key = 0 to len + 1 do
      let le, gt = split_le_gt (of_list (List.init len ~f:Int.succ)) key in
      Core.print_s [%sexp (le : int_set), "<=", (key : int), "<", (gt : int_set)]
    done
  done;
  [%expect
    {|
    (() <= 0 < (1))
    ((1) <= 1 < ())
    ((1) <= 2 < ())

    (() <= 0 < (1 2))
    ((1) <= 1 < (2))
    ((1 2) <= 2 < ())
    ((1 2) <= 3 < ())

    (() <= 0 < (1 2 3))
    ((1) <= 1 < (2 3))
    ((1 2) <= 2 < (3))
    ((1 2 3) <= 3 < ())
    ((1 2 3) <= 4 < ())

    (() <= 0 < (1 2 3 4))
    ((1) <= 1 < (2 3 4))
    ((1 2) <= 2 < (3 4))
    ((1 2 3) <= 3 < (4))
    ((1 2 3 4) <= 4 < ())
    ((1 2 3 4) <= 5 < ())
    |}]
;;

let%expect_test "split_lt_ge" =
  for len = 1 to 4 do
    print_endline "";
    for key = 0 to len + 1 do
      let lt, ge = split_lt_ge (of_list (List.init len ~f:Int.succ)) key in
      Core.print_s [%sexp (lt : int_set), "<", (key : int), "<=", (ge : int_set)]
    done
  done;
  [%expect
    {|
    (() < 0 <= (1))
    (() < 1 <= (1))
    ((1) < 2 <= ())

    (() < 0 <= (1 2))
    (() < 1 <= (1 2))
    ((1) < 2 <= (2))
    ((1 2) < 3 <= ())

    (() < 0 <= (1 2 3))
    (() < 1 <= (1 2 3))
    ((1) < 2 <= (2 3))
    ((1 2) < 3 <= (3))
    ((1 2 3) < 4 <= ())

    (() < 0 <= (1 2 3 4))
    (() < 1 <= (1 2 3 4))
    ((1) < 2 <= (2 3 4))
    ((1 2) < 3 <= (3 4))
    ((1 2 3) < 4 <= (4))
    ((1 2 3 4) < 5 <= ())
    |}]
;;

let%test_module "Poly" =
  (module struct
    let%test _ = length Poly.empty = 0
    let%test _ = Poly.equal (Poly.of_list []) Poly.empty

    let%test _ =
      let a = Poly.of_list [ 1; 1 ] in
      let b = Poly.of_list [ "a" ] in
      length a = length b
    ;;
  end)
;;
