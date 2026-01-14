open! Base
open Base_quickcheck
open Expect_test_helpers_base
open Generator.Let_syntax

open struct
  (* Testing helpers. Not part of [Enum]'s interface, so not exported. *)

  module Tree = Set.Tree
  module Enum = Set.Private.Enum

  let rec fold_right_increasing t ~init:acc ~f =
    match (t : (_, _, Enum.increasing) Enum.t) with
    | Null -> acc
    | This (More (elt, tree, tail)) ->
      let acc = fold_right_increasing tail ~init:acc ~f in
      let acc = Set.Tree.fold_right tree ~init:acc ~f in
      f elt acc
  ;;

  let rec fold_right_decreasing t ~init:acc ~f =
    match (t : (_, _, Enum.decreasing) Enum.t) with
    | Null -> acc
    | This (More (elt, tree, tail)) ->
      let acc = fold_right_decreasing tail ~init:acc ~f in
      let acc = Set.Tree.fold tree ~init:acc ~f:(fun acc elt -> f elt acc) in
      f elt acc
  ;;

  let to_list_increasing t =
    fold_right_increasing t ~init:[] ~f:(fun elt acc -> elt :: acc)
  ;;

  let to_list_decreasing t =
    fold_right_decreasing t ~init:[] ~f:(fun elt acc -> elt :: acc)
  ;;

  let elts_gen =
    Generator.small_strictly_positive_int
    |> Generator.list
    |> Generator.map ~f:(fun list ->
      List.dedup_and_sort list ~compare:[%compare: int] |> Iarray.of_list)
  ;;

  (* Custom generator to cover arbitrary internal structure of trees. *)
  let tree_of_elts_gen elts ~pos ~len =
    let rec loop ~pos ~len =
      match len with
      | 0 -> Generator.return Tree.Expert.empty
      | 1 ->
        let elt = Iarray.get elts pos in
        Generator.return (Tree.Expert.singleton elt)
      | _ ->
        (* Randomly balance between left and right. *)
        let%bind left_len = Generator.int_uniform_inclusive 0 (len - 1) in
        let%map left = loop ~pos ~len:left_len
        and right = loop ~pos:(pos + 1 + left_len) ~len:(len - left_len - 1) in
        let elt = Iarray.get elts (pos + left_len) in
        (* Rather than try to construct only valid left/right splits, we pick arbitrary
           ones, and anything unbalanced will be fixed up by this constructor. This is
           easier than trying to be super precise, and should still give us decent
           coverage. *)
        Tree.Expert.create_and_rebalance_unchecked left elt right
    in
    let%map.Generator tree = loop ~pos ~len in
    assert (
      [%equal: int iarray]
        (Iarray.of_list (Tree.to_list tree))
        (Iarray.sub elts ~pos ~len));
    tree
  ;;

  let tree_gen () =
    let%bind elts = elts_gen in
    tree_of_elts_gen elts ~pos:0 ~len:(Iarray.length elts)
  ;;

  (* Custom generator to cover arbitrary internal structure of enums. *)
  let enum_increasing_of_elts_gen elts ~pos ~len =
    let rec loop ~pos ~len tail =
      match len with
      | 0 -> Generator.return tail
      | 1 ->
        Generator.return (This (Enum.More (Iarray.get elts pos, Tree.Expert.empty, tail)))
      | _ ->
        (* Choose where to split between [More] nodes. *)
        (match%bind Generator.int_uniform_inclusive 0 (len - 1) with
         | 0 ->
           (* Put everything in one node. *)
           let%map tree = tree_of_elts_gen elts ~pos:(pos + 1) ~len:(len - 1) in
           This (Enum.More (Iarray.get elts pos, tree, tail))
         | idx ->
           (* Put suffix in a final [More] node, recursively split up the prefix. *)
           let%bind tree =
             tree_of_elts_gen elts ~pos:(pos + idx + 1) ~len:(len - idx - 1)
           in
           loop ~pos ~len:idx (This (Enum.More (Iarray.get elts (pos + idx), tree, tail))))
    in
    let%map.Generator enum = loop ~pos ~len Null in
    assert (
      [%equal: int iarray]
        (Iarray.of_list (to_list_increasing enum))
        (Iarray.sub elts ~pos ~len));
    enum
  ;;

  (* As above, for decreasing enums. The trees do not change internal order. *)
  let enum_decreasing_of_elts_gen elts ~pos ~len =
    let rec loop ~pos ~len tail =
      match len with
      | 0 -> Generator.return tail
      | 1 ->
        Generator.return (This (Enum.More (Iarray.get elts pos, Tree.Expert.empty, tail)))
      | _ ->
        (* Choose where to split between [More] nodes. *)
        (match%bind Generator.int_uniform_inclusive 0 (len - 1) with
         | 0 ->
           (* Put everything in one node. *)
           let%map tree = tree_of_elts_gen elts ~pos ~len:(len - 1) in
           This (Enum.More (Iarray.get elts (pos + len - 1), tree, tail))
         | idx ->
           (* Put suffix in a final [More] node, recursively split up the prefix. *)
           let%bind tree = tree_of_elts_gen elts ~pos ~len:idx in
           loop
             ~pos:(pos + idx + 1)
             ~len:(len - idx - 1)
             (This (Enum.More (Iarray.get elts (pos + idx), tree, tail))))
    in
    let%map.Generator enum = loop ~pos ~len Null in
    assert (
      [%equal: int iarray]
        (Iarray.of_list_rev (to_list_decreasing enum))
        (Iarray.sub elts ~pos ~len));
    enum
  ;;

  let enum_increasing_gen () =
    let%bind elts = elts_gen in
    enum_increasing_of_elts_gen elts ~pos:0 ~len:(Iarray.length elts)
  ;;

  let tree_and_enum_increasing_gen () =
    let%bind elts = elts_gen in
    let len = Iarray.length elts in
    let%bind idx = Generator.int_uniform_inclusive 0 len in
    let%bind tree = tree_of_elts_gen elts ~pos:0 ~len:idx in
    let%map enum = enum_increasing_of_elts_gen elts ~pos:idx ~len:(len - idx) in
    tree, enum
  ;;

  let tree_and_enum_decreasing_gen () =
    let%bind elts = elts_gen in
    let len = Iarray.length elts in
    let%bind idx = Generator.int_uniform_inclusive 0 len in
    let%bind tree = tree_of_elts_gen elts ~pos:idx ~len:(len - idx) in
    let%map enum = enum_decreasing_of_elts_gen elts ~pos:0 ~len:idx in
    tree, enum
  ;;

  let add_elts_to_tree elts tree =
    Iarray.fold elts ~init:tree ~f:(fun acc elt ->
      Set.Tree.add ~comparator:Int.comparator acc elt)
  ;;

  let rec add_to_enum enum x : (_, _, Enum.increasing) Enum.t =
    match (enum : (_, _, Enum.increasing) Enum.t) with
    | Null -> This (More (x, Tree.Expert.empty, Null))
    | This (More (elt, tree, tail)) ->
      (match Int.compare x elt with
       | c when c < 0 -> This (More (x, Tree.Expert.empty, enum))
       | c when c > 0 ->
         (match tail with
          | This (More (other, _, _)) when Int.compare x other >= 0 ->
            This (More (elt, tree, add_to_enum tail x))
          | _ -> This (More (elt, Set.Tree.add ~comparator:Int.comparator tree x, tail)))
       | _ -> enum)
  ;;

  let add_elts_to_enum elts enum =
    Iarray.fold elts ~init:enum ~f:(fun acc elt -> add_to_enum acc elt)
  ;;

  let pair_of_tree_gen () =
    Generator.union
      [ Generator.both (tree_gen ()) (tree_gen ())
      ; (let%map tree0 = tree_gen ()
         and elts1 = elts_gen
         and elts2 = elts_gen in
         add_elts_to_tree elts1 tree0, add_elts_to_tree elts2 tree0)
      ]
  ;;

  let pair_of_enum_increasing_gen () =
    Generator.union
      [ Generator.both (enum_increasing_gen ()) (enum_increasing_gen ())
      ; (let%map enum0 = enum_increasing_gen ()
         and elts1 = elts_gen
         and elts2 = elts_gen in
         add_elts_to_enum elts1 enum0, add_elts_to_enum elts2 enum0)
      ]
  ;;

  module Enum_with_sexp = struct
    type ('a, 'cmp, 'direction) nonempty = ('a, 'cmp, 'direction) Enum.nonempty =
      | More of 'a * ('a, 'cmp) Tree.Expert.t * ('a, 'cmp, 'direction) t

    and ('a, 'cmp, 'direction) t = ('a, 'cmp, 'direction) nonempty or_null
    [@@deriving sexp_of]
  end

  module Int_list = struct
    type t = int list [@@deriving equal, sexp_of]
  end

  module Tree_and_enum_increasing = struct
    type nonrec t =
      (int, (Int.comparator_witness[@sexp.opaque])) Set.Tree.Expert.t
      * ( int
          , (Int.comparator_witness[@sexp.opaque])
          , (Enum.increasing[@sexp.opaque]) )
          Enum_with_sexp.t
    [@@deriving sexp_of]

    let quickcheck_generator = tree_and_enum_increasing_gen ()
    let quickcheck_shrinker = Shrinker.atomic
  end

  module Tree_and_enum_decreasing = struct
    type nonrec t =
      (int, (Int.comparator_witness[@sexp.opaque])) Set.Tree.Expert.t
      * ( int
          , (Int.comparator_witness[@sexp.opaque])
          , (Enum.decreasing[@sexp.opaque]) )
          Enum_with_sexp.t
    [@@deriving sexp_of]

    let quickcheck_generator = tree_and_enum_decreasing_gen ()
    let quickcheck_shrinker = Shrinker.atomic
  end

  module Tree_increasing = struct
    type t = (int, (Int.comparator_witness[@sexp.opaque])) Set.Tree.Expert.t
    [@@deriving sexp_of]

    let quickcheck_generator = tree_gen ()
    let quickcheck_shrinker = Shrinker.atomic
  end

  module Tree_and_key = struct
    type nonrec t = (int, (Int.comparator_witness[@sexp.opaque])) Set.Tree.Expert.t * int
    [@@deriving sexp_of]

    let quickcheck_generator =
      Generator.both (tree_gen ()) Generator.small_positive_or_zero_int
    ;;

    let quickcheck_shrinker = Shrinker.atomic
  end

  module Enum_increasing = struct
    type nonrec t =
      ( int
        , (Int.comparator_witness[@sexp.opaque])
        , (Enum.increasing[@sexp.opaque]) )
        Enum_with_sexp.t
    [@@deriving sexp_of]

    let quickcheck_generator = enum_increasing_gen ()
    let quickcheck_shrinker = Shrinker.atomic
  end

  module Pair_of_tree = struct
    type nonrec t =
      (int, (Int.comparator_witness[@sexp.opaque])) Tree.Expert.t
      * (int, (Int.comparator_witness[@sexp.opaque])) Tree.Expert.t
    [@@deriving sexp_of]

    let quickcheck_generator = pair_of_tree_gen ()
    let quickcheck_shrinker = Shrinker.atomic
  end

  module Pair_of_enum = struct
    type nonrec t =
      ( int
        , (Int.comparator_witness[@sexp.opaque])
        , (Enum.increasing[@sexp.opaque]) )
        Enum_with_sexp.t
      * ( int
          , (Int.comparator_witness[@sexp.opaque])
          , (Enum.increasing[@sexp.opaque]) )
          Enum_with_sexp.t
    [@@deriving sexp_of]

    let quickcheck_generator = pair_of_enum_increasing_gen ()
    let quickcheck_shrinker = Shrinker.atomic
  end
end

(* Testing exports of [Enum]: *)

type increasing = Enum.increasing
type decreasing = Enum.decreasing

type ('a, 'cmp, 'direction) nonempty = ('a, 'cmp, 'direction) Enum.nonempty =
  | More of 'a * ('a, 'cmp) Tree.t * ('a, 'cmp, 'direction) t

and ('a, 'cmp, 'direction) t = ('a, 'cmp, 'direction) nonempty or_null
[@@deriving sexp_of]

let cons = Enum.cons

let%expect_test "cons" =
  quickcheck_m (module Tree_and_enum_increasing) ~f:(fun (tree, enum) ->
    require_equal
      (module Int_list)
      (to_list_increasing (Enum.cons tree enum))
      (Set.Tree.to_list tree @ to_list_increasing enum))
;;

let cons_right = Enum.cons_right

let%expect_test "cons_right" =
  quickcheck_m (module Tree_and_enum_decreasing) ~f:(fun (tree, enum) ->
    require_equal
      (module Int_list)
      (to_list_decreasing (Enum.cons_right tree enum))
      (List.rev (Set.Tree.to_list tree) @ to_list_decreasing enum));
  [%expect {| |}]
;;

let of_set = Enum.of_set

let%expect_test "of_set" =
  quickcheck_m (module Tree_increasing) ~f:(fun tree ->
    require_equal
      (module Int_list)
      (to_list_increasing (Enum.of_set tree))
      (Set.Tree.to_list tree))
;;

let of_set_right = Enum.of_set_right

let%expect_test "of_set_right" =
  quickcheck_m (module Tree_increasing) ~f:(fun tree ->
    require_equal
      (module Int_list)
      (to_list_decreasing (Enum.of_set_right tree))
      (List.rev (Set.Tree.to_list tree)));
  [%expect {| |}]
;;

let starting_at_increasing = Enum.starting_at_increasing

let%expect_test "starting_at_increasing" =
  quickcheck_m (module Tree_and_key) ~f:(fun (tree, pos) ->
    require_equal
      (module Int_list)
      (to_list_increasing (Enum.starting_at_increasing tree pos Int.compare))
      (Sequence.to_list
         (Set.Tree.to_sequence
            tree
            ~comparator:Int.comparator
            ~order:`Increasing
            ~greater_or_equal_to:pos)))
;;

let starting_at_decreasing = Enum.starting_at_decreasing

let%expect_test "starting_at_decreasing" =
  quickcheck_m (module Tree_and_key) ~f:(fun (tree, pos) ->
    require_equal
      (module Int_list)
      (to_list_decreasing (Enum.starting_at_decreasing tree pos Int.compare))
      (Sequence.to_list
         (Set.Tree.to_sequence
            tree
            ~comparator:Int.comparator
            ~order:`Decreasing
            ~less_or_equal_to:pos)));
  [%expect {| |}]
;;

let compare = Enum.compare

let%expect_test "compare" =
  quickcheck_m (module Pair_of_enum) ~f:(fun (enum1, enum2) ->
    require_equal
      (module Ordering)
      (Ordering.of_int (Enum.compare Int.compare enum1 enum2))
      (Ordering.of_int
         ([%compare: int list] (to_list_increasing enum1) (to_list_increasing enum2))))
;;

let iter = Enum.iter

let%expect_test "iter" =
  quickcheck_m (module Enum_increasing) ~f:(fun enum ->
    require_equal
      (module Int_list)
      (let queue = Queue.create () in
       Enum.iter enum ~f:(Queue.enqueue queue);
       Queue.to_list queue)
      (to_list_increasing enum))
;;

let iter2 = Enum.iter2

let%expect_test "iter2" =
  quickcheck_m (module Pair_of_enum) ~f:(fun (enum1, enum2) ->
    require_equal
      (module struct
        type t = [ `Both of int * int | `Left of int | `Right of int ] list
        [@@deriving equal, sexp_of]
      end)
      (let queue = Queue.create () in
       Enum.iter2 Int.compare enum1 enum2 ~f:(Queue.enqueue queue);
       Queue.to_list queue)
      (let queue = Queue.create () in
       Set.Tree.iter2
         ~comparator:Int.comparator
         (Set.Tree.of_list ~comparator:Int.comparator (to_list_increasing enum1))
         (Set.Tree.of_list ~comparator:Int.comparator (to_list_increasing enum2))
         ~f:(Queue.enqueue queue);
       Queue.to_list queue))
;;

let symmetric_diff = Enum.symmetric_diff

let%expect_test "symmetric_diff" =
  quickcheck_m (module Pair_of_tree) ~f:(fun (tree1, tree2) ->
    require_equal
      (module struct
        type t = (int, int) Either.t Sequence.t [@@deriving equal, sexp_of]
      end)
      (Enum.symmetric_diff tree1 tree2 ~compare_elt:Int.compare)
      (Set.Tree.symmetric_diff tree1 tree2 ~comparator:Int.comparator))
;;
