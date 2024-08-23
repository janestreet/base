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
      print_s [%sexp (le : int_set), "<=", (key : int), "<", (gt : int_set)]
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
      print_s [%sexp (lt : int_set), "<", (key : int), "<=", (ge : int_set)]
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

(* Shows which element is selected, for functions that choose/scan among elements. Some of
   the functions do not, or did not, guarantee which element or which order, so it helps
   to have a view of their choice and whether it is deterministic based on the elements or
   whether it depends on the specific "shape" or "balance" of the tree. *)
let%test_module "element selection" =
  (module struct
    let create_balanced array = Set.of_sorted_array_unchecked (module Int) array

    let create_left_to_right array =
      Array.fold array ~init:(Set.empty (module Int)) ~f:(fun set elt -> Set.add set elt)
    ;;

    let create_right_to_left array =
      Array.fold_right
        array
        ~init:(Set.empty (module Int))
        ~f:(fun elt set -> Set.add set elt)
    ;;

    let create ~len =
      let array = Array.init len ~f:Int.succ in
      [ "balanced", create_balanced array
      ; "left-heavy", create_left_to_right array
      ; "right-heavy", create_right_to_left array
      ]
    ;;

    module type S = sig
      type t [@@deriving compare, sexp_of]
    end

    let test1 (type a) (module M : S with type t = a) fn =
      Ref.set_temporarily sexp_style Sexp_style.simple_pretty ~f:(fun () ->
        for len = 0 to 8 do
          let results =
            create ~len |> List.map ~f:(fun (set_name, set) -> fn set, set_name)
          in
          match results |> List.Assoc.sort_and_group ~compare:[%compare: M.t] with
          | [ (singleton, _) ] ->
            print_s [%message "" ~_:(len : int) ~_:(singleton : M.t)]
          | _ ->
            print_s
              [%message "" ~_:(len : int) "multiple" ~_:(results : (M.t * string) list)]
        done)
    ;;

    let test2 fn_opt fn_exn =
      test1
        (module struct
          type t = int Or_error.t [@@deriving compare, sexp_of]
        end)
        (fun set ->
          let option = fn_opt set in
          let result = Or_error.try_with (fun () -> fn_exn set) in
          require
            ([%equal: int option] option (Or_error.ok result))
            ~if_false_then_print_s:
              [%lazy_message
                "output mismatch" (option : int option) (result : int Or_error.t)];
          result)
    ;;

    let%expect_test "min_elt" =
      test2 Set.min_elt Set.min_elt_exn;
      [%expect
        {|
        (0 (Error set.ml.Tree0.Set_min_elt_exn_of_empty_set))
        (1 (Ok 1))
        (2 (Ok 1))
        (3 (Ok 1))
        (4 (Ok 1))
        (5 (Ok 1))
        (6 (Ok 1))
        (7 (Ok 1))
        (8 (Ok 1))
        |}]
    ;;

    let%expect_test "max_elt" =
      test2 Set.max_elt Set.max_elt_exn;
      [%expect
        {|
        (0 (Error set.ml.Tree0.Set_max_elt_exn_of_empty_set))
        (1 (Ok 1))
        (2 (Ok 2))
        (3 (Ok 3))
        (4 (Ok 4))
        (5 (Ok 5))
        (6 (Ok 6))
        (7 (Ok 7))
        (8 (Ok 8))
        |}]
    ;;

    let%expect_test "choose" =
      test2 Set.choose Set.choose_exn;
      [%expect
        {|
        (0 (Error (Not_found_s "Set.choose_exn: empty set")))
        (1 (Ok 1))
        (2 multiple (((Ok 2) balanced) ((Ok 1) left-heavy) ((Ok 2) right-heavy)))
        (3 multiple (((Ok 2) balanced) ((Ok 1) left-heavy) ((Ok 3) right-heavy)))
        (4 multiple (((Ok 3) balanced) ((Ok 2) left-heavy) ((Ok 3) right-heavy)))
        (5 multiple (((Ok 3) balanced) ((Ok 2) left-heavy) ((Ok 4) right-heavy)))
        (6 multiple (((Ok 4) balanced) ((Ok 2) left-heavy) ((Ok 5) right-heavy)))
        (7 (Ok 4))
        (8 multiple (((Ok 5) balanced) ((Ok 4) left-heavy) ((Ok 5) right-heavy)))
        |}]
    ;;

    let%expect_test "find" =
      let f x = x land 1 = 0 in
      test2 (Set.find ~f) (Set.find_exn ~f);
      [%expect
        {|
        (0 (Error (Failure "Set.find_exn failed to find a matching element")))
        (1 (Error (Failure "Set.find_exn failed to find a matching element")))
        (2 (Ok 2))
        (3 (Ok 2))
        (4 (Ok 2))
        (5 multiple (((Ok 2) balanced) ((Ok 2) left-heavy) ((Ok 4) right-heavy)))
        (6 multiple (((Ok 4) balanced) ((Ok 2) left-heavy) ((Ok 2) right-heavy)))
        (7 (Ok 4))
        (8 multiple (((Ok 2) balanced) ((Ok 4) left-heavy) ((Ok 2) right-heavy)))
        |}]
    ;;

    let%expect_test "find_map" =
      let f x = if x land 1 = 0 then Some (x asr 1) else None in
      test1
        (module struct
          type t = int option [@@deriving compare, sexp_of]
        end)
        (Set.find_map ~f);
      [%expect
        {|
        (0 ())
        (1 ())
        (2 (1))
        (3 (1))
        (4 (1))
        (5 multiple (((1) balanced) ((1) left-heavy) ((2) right-heavy)))
        (6 multiple (((2) balanced) ((1) left-heavy) ((1) right-heavy)))
        (7 (2))
        (8 multiple (((1) balanced) ((2) left-heavy) ((1) right-heavy)))
        |}]
    ;;

    let%expect_test "group_by" =
      test1
        (module struct
          type t = Set.M(Int).t list [@@deriving compare, sexp_of]
        end)
        (Set.group_by ~equiv:(fun x y -> Int.popcount x = Int.popcount y));
      [%expect
        {|
        (0 ())
        (1 ((1)))
        (2 ((1 2)))
        (3
         multiple
         ((((3) (1 2)) balanced) (((3) (1 2)) left-heavy) (((1 2) (3)) right-heavy)))
        (4
         multiple
         ((((1 2 4) (3)) balanced)
          (((3) (1 2 4)) left-heavy)
          (((1 2 4) (3)) right-heavy)))
        (5
         multiple
         ((((1 2 4) (3 5)) balanced)
          (((3 5) (1 2 4)) left-heavy)
          (((3 5) (1 2 4)) right-heavy)))
        (6
         multiple
         ((((3 5 6) (1 2 4)) balanced)
          (((3 5 6) (1 2 4)) left-heavy)
          (((1 2 4) (3 5 6)) right-heavy)))
        (7 ((7) (3 5 6) (1 2 4)))
        (8
         multiple
         ((((7) (1 2 4 8) (3 5 6)) balanced)
          (((7) (3 5 6) (1 2 4 8)) left-heavy)
          (((7) (1 2 4 8) (3 5 6)) right-heavy)))
        |}]
    ;;
  end)
;;
