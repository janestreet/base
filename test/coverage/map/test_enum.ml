open! Base
open Base_quickcheck
open Expect_test_helpers_base
open Generator.Let_syntax

open struct
  (* Testing helpers. Not part of [Enum]'s interface, so not exported. *)

  module Tree = Map.Tree
  module Enum = Map.Private.Enum

  let rec fold_right_increasing t ~init:acc ~f =
    match (t : (_, _, _, Enum.increasing) Enum.t) with
    | Null -> acc
    | This (More (key, data, tree, tail)) ->
      let acc = fold_right_increasing tail ~init:acc ~f in
      let acc = Map.Tree.fold_right tree ~init:acc ~f in
      f ~key ~data acc
  ;;

  let rec fold_right_decreasing t ~init:acc ~f =
    match (t : (_, _, _, Enum.decreasing) Enum.t) with
    | Null -> acc
    | This (More (key, data, tree, tail)) ->
      let acc = fold_right_decreasing tail ~init:acc ~f in
      let acc = Map.Tree.fold tree ~init:acc ~f in
      f ~key ~data acc
  ;;

  let to_list_increasing t =
    fold_right_increasing t ~init:[] ~f:(fun ~key ~data acc -> (key, data) :: acc)
  ;;

  let to_list_decreasing t =
    fold_right_decreasing t ~init:[] ~f:(fun ~key ~data acc -> (key, data) :: acc)
  ;;

  let pairs_gen =
    Generator.both
      Generator.small_strictly_positive_int
      Generator.small_strictly_positive_int
    |> Generator.list
    |> Generator.map ~f:(fun list ->
      List.dedup_and_sort list ~compare:[%compare: int * _] |> Iarray.of_list)
  ;;

  (* Custom generator to cover arbitrary internal structure of trees. *)
  let tree_of_pairs_gen pairs ~pos ~len =
    let rec loop ~pos ~len =
      match len with
      | 0 -> Generator.return Tree.Expert.empty
      | 1 ->
        let key, data = Iarray.get pairs pos in
        Generator.return (Tree.Expert.singleton key data)
      | _ ->
        (* Randomly balance between left and right. *)
        let%bind left_len = Generator.int_uniform_inclusive 0 (len - 1) in
        let%map left = loop ~pos ~len:left_len
        and right = loop ~pos:(pos + 1 + left_len) ~len:(len - left_len - 1) in
        let key, data = Iarray.get pairs (pos + left_len) in
        (* Rather than try to construct only valid left/right splits, we pick arbitrary
           ones, and anything unbalanced will be fixed up by this constructor. This is
           easier than trying to be super precise, and should still give us decent
           coverage. *)
        Tree.Expert.create_and_rebalance_unchecked left key data right
    in
    let%map.Generator tree = loop ~pos ~len in
    assert (
      [%equal: (int * int) iarray]
        (Iarray.of_list (Tree.to_alist tree))
        (Iarray.sub pairs ~pos ~len));
    tree
  ;;

  let tree_gen () =
    let%bind pairs = pairs_gen in
    tree_of_pairs_gen pairs ~pos:0 ~len:(Iarray.length pairs)
  ;;

  (* Custom generator to cover arbitrary internal structure of enums. *)
  let enum_increasing_of_pairs_gen pairs ~pos ~len =
    let rec loop ~pos ~len tail =
      match len with
      | 0 -> Generator.return tail
      | 1 ->
        let key, data = Iarray.get pairs pos in
        Generator.return (This (Enum.More (key, data, Tree.Expert.empty, tail)))
      | _ ->
        (* Choose where to split between [More] nodes. *)
        (match%bind Generator.int_uniform_inclusive 0 (len - 1) with
         | 0 ->
           (* Put everything in one node. *)
           let%map tree = tree_of_pairs_gen pairs ~pos:(pos + 1) ~len:(len - 1) in
           let key, data = Iarray.get pairs pos in
           This (Enum.More (key, data, tree, tail))
         | idx ->
           (* Put suffix in a final [More] node, recursively split up the prefix. *)
           let%bind tree =
             tree_of_pairs_gen pairs ~pos:(pos + idx + 1) ~len:(len - idx - 1)
           in
           let key, data = Iarray.get pairs (pos + idx) in
           loop ~pos ~len:idx (This (Enum.More (key, data, tree, tail))))
    in
    let%map.Generator enum = loop ~pos ~len Null in
    assert (
      [%equal: (int * int) iarray]
        (Iarray.of_list (to_list_increasing enum))
        (Iarray.sub pairs ~pos ~len));
    enum
  ;;

  (* As above, for decreasing enums. The trees do not change internal order. *)
  let enum_decreasing_of_pairs_gen pairs ~pos ~len =
    let rec loop ~pos ~len tail =
      match len with
      | 0 -> Generator.return tail
      | 1 ->
        let key, data = Iarray.get pairs pos in
        Generator.return (This (Enum.More (key, data, Tree.Expert.empty, tail)))
      | _ ->
        (* Choose where to split between [More] nodes. *)
        (match%bind Generator.int_uniform_inclusive 0 (len - 1) with
         | 0 ->
           (* Put everything in one node. *)
           let%map tree = tree_of_pairs_gen pairs ~pos ~len:(len - 1) in
           let key, data = Iarray.get pairs (pos + len - 1) in
           This (Enum.More (key, data, tree, tail))
         | idx ->
           (* Put suffix in a final [More] node, recursively split up the prefix. *)
           let%bind tree = tree_of_pairs_gen pairs ~pos ~len:idx in
           let key, data = Iarray.get pairs (pos + idx) in
           loop
             ~pos:(pos + idx + 1)
             ~len:(len - idx - 1)
             (This (Enum.More (key, data, tree, tail))))
    in
    let%map.Generator enum = loop ~pos ~len Null in
    assert (
      [%equal: (int * int) iarray]
        (Iarray.of_list_rev (to_list_decreasing enum))
        (Iarray.sub pairs ~pos ~len));
    enum
  ;;

  let enum_increasing_gen () =
    let%bind pairs = pairs_gen in
    enum_increasing_of_pairs_gen pairs ~pos:0 ~len:(Iarray.length pairs)
  ;;

  let tree_and_enum_increasing_gen () =
    let%bind pairs = pairs_gen in
    let len = Iarray.length pairs in
    let%bind idx = Generator.int_uniform_inclusive 0 len in
    let%bind tree = tree_of_pairs_gen pairs ~pos:0 ~len:idx in
    let%map enum = enum_increasing_of_pairs_gen pairs ~pos:idx ~len:(len - idx) in
    tree, enum
  ;;

  let tree_and_enum_decreasing_gen () =
    let%bind pairs = pairs_gen in
    let len = Iarray.length pairs in
    let%bind idx = Generator.int_uniform_inclusive 0 len in
    let%bind tree = tree_of_pairs_gen pairs ~pos:idx ~len:(len - idx) in
    let%map enum = enum_decreasing_of_pairs_gen pairs ~pos:0 ~len:idx in
    tree, enum
  ;;

  (* When generating pairs of trees and/or enums, half the time we generate one and modify
     it a bit on both sides, so that we test shared structure. *)

  let add_pairs_to_tree pairs tree =
    Iarray.fold pairs ~init:tree ~f:(fun acc (key, data) ->
      Map.Tree.set ~comparator:Int.comparator acc ~key ~data)
  ;;

  let rec add_to_enum enum ~key:k ~data:v : (_, _, _, Enum.increasing) Enum.t =
    match (enum : (_, _, _, Enum.increasing) Enum.t) with
    | Null -> This (More (k, v, Tree.Expert.empty, Null))
    | This (More (key, data, tree, tail)) ->
      (match Int.compare k key with
       | c when c < 0 -> This (More (k, v, Tree.Expert.empty, enum))
       | c when c > 0 ->
         let tree, tail = add_to_tree_and_enum tree tail ~key:k ~data:v in
         This (More (key, data, tree, tail))
       | _ -> enum)

  and add_to_tree_and_enum tree enum ~key:k ~data:v =
    match enum with
    | This (More (other, _, _, _)) when Int.compare k other >= 0 ->
      tree, add_to_enum enum ~key:k ~data:v
    | _ -> Map.Tree.set ~comparator:Int.comparator tree ~key:k ~data:v, enum
  ;;

  let add_pairs_to_enum pairs enum =
    Iarray.fold pairs ~init:enum ~f:(fun acc (key, data) -> add_to_enum acc ~key ~data)
  ;;

  let add_pairs_to_tree_and_enum pairs tree enum =
    Iarray.fold pairs ~init:(tree, enum) ~f:(fun (tree, enum) (key, data) ->
      add_to_tree_and_enum tree enum ~key ~data)
  ;;

  let pair_of_tree_gen () =
    Generator.union
      [ Generator.both (tree_gen ()) (tree_gen ())
      ; (let%map tree0 = tree_gen ()
         and pairs1 = pairs_gen
         and pairs2 = pairs_gen in
         add_pairs_to_tree pairs1 tree0, add_pairs_to_tree pairs2 tree0)
      ]
  ;;

  let pair_of_enum_increasing_gen () =
    Generator.union
      [ Generator.both (enum_increasing_gen ()) (enum_increasing_gen ())
      ; (let%map enum0 = enum_increasing_gen ()
         and pairs1 = pairs_gen
         and pairs2 = pairs_gen in
         add_pairs_to_enum pairs1 enum0, add_pairs_to_enum pairs2 enum0)
      ]
  ;;

  let pair_of_tree_and_enum_increasing_gen () =
    Generator.union
      [ Generator.both (tree_and_enum_increasing_gen ()) (tree_and_enum_increasing_gen ())
      ; (let%map tree0, enum0 = tree_and_enum_increasing_gen ()
         and pairs1 = pairs_gen
         and pairs2 = pairs_gen in
         ( add_pairs_to_tree_and_enum pairs1 tree0 enum0
         , add_pairs_to_tree_and_enum pairs2 tree0 enum0 ))
      ]
  ;;

  module Enum_with_sexp = struct
    type ('k, 'v, 'cmp, 'direction) nonempty = ('k, 'v, 'cmp, 'direction) Enum.nonempty =
      | More of 'k * 'v * ('k, 'v, 'cmp) Tree.Expert.t * ('k, 'v, 'cmp, 'direction) t

    and ('k, 'v, 'cmp, 'direction) t = ('k, 'v, 'cmp, 'direction) nonempty or_null
    [@@deriving sexp_of]
  end

  module Int_alist = struct
    type t = (int * int) list [@@deriving equal, sexp_of]
  end

  module Tree_and_enum_increasing = struct
    type nonrec t =
      (int, int, (Int.comparator_witness[@sexp.opaque])) Map.Tree.Expert.t
      * ( int
          , int
          , (Int.comparator_witness[@sexp.opaque])
          , (Enum.increasing[@sexp.opaque]) )
          Enum_with_sexp.t
    [@@deriving sexp_of]

    let quickcheck_generator = tree_and_enum_increasing_gen ()
    let quickcheck_shrinker = Shrinker.atomic
  end

  module Tree_and_enum_decreasing = struct
    type nonrec t =
      (int, int, (Int.comparator_witness[@sexp.opaque])) Map.Tree.Expert.t
      * ( int
          , int
          , (Int.comparator_witness[@sexp.opaque])
          , (Enum.decreasing[@sexp.opaque]) )
          Enum_with_sexp.t
    [@@deriving sexp_of]

    let quickcheck_generator = tree_and_enum_decreasing_gen ()
    let quickcheck_shrinker = Shrinker.atomic
  end

  module Tree_increasing = struct
    type t = (int, int, (Int.comparator_witness[@sexp.opaque])) Map.Tree.Expert.t
    [@@deriving sexp_of]

    let quickcheck_generator = tree_gen ()
    let quickcheck_shrinker = Shrinker.atomic
  end

  module Tree_and_key = struct
    type nonrec t =
      (int, int, (Int.comparator_witness[@sexp.opaque])) Map.Tree.Expert.t * int
    [@@deriving sexp_of]

    let quickcheck_generator =
      Generator.both (tree_gen ()) Generator.small_positive_or_zero_int
    ;;

    let quickcheck_shrinker = Shrinker.atomic
  end

  module Enum_increasing = struct
    type nonrec t =
      ( int
        , int
        , (Int.comparator_witness[@sexp.opaque])
        , (Enum.increasing[@sexp.opaque]) )
        Enum_with_sexp.t
    [@@deriving sexp_of]

    let quickcheck_generator = enum_increasing_gen ()
    let quickcheck_shrinker = Shrinker.atomic
  end

  module Pair_of_tree = struct
    type nonrec t =
      (int, int, (Int.comparator_witness[@sexp.opaque])) Tree.Expert.t
      * (int, int, (Int.comparator_witness[@sexp.opaque])) Tree.Expert.t
    [@@deriving sexp_of]

    let quickcheck_generator = pair_of_tree_gen ()
    let quickcheck_shrinker = Shrinker.atomic
  end

  module Pair_of_enum = struct
    type nonrec t =
      ( int
        , int
        , (Int.comparator_witness[@sexp.opaque])
        , (Enum.increasing[@sexp.opaque]) )
        Enum_with_sexp.t
      * ( int
          , int
          , (Int.comparator_witness[@sexp.opaque])
          , (Enum.increasing[@sexp.opaque]) )
          Enum_with_sexp.t
    [@@deriving sexp_of]

    let quickcheck_generator = pair_of_enum_increasing_gen ()
    let quickcheck_shrinker = Shrinker.atomic
  end

  module Pair_of_tree_and_enum = struct
    type nonrec t =
      ((int, int, (Int.comparator_witness[@sexp.opaque])) Map.Tree.Expert.t
      * ( int
          , int
          , (Int.comparator_witness[@sexp.opaque])
          , (Enum.increasing[@sexp.opaque]) )
          Enum_with_sexp.t)
      * ((int, int, (Int.comparator_witness[@sexp.opaque])) Map.Tree.Expert.t
        * ( int
            , int
            , (Int.comparator_witness[@sexp.opaque])
            , (Enum.increasing[@sexp.opaque]) )
            Enum_with_sexp.t)
    [@@deriving sexp_of]

    let quickcheck_generator = pair_of_tree_and_enum_increasing_gen ()
    let quickcheck_shrinker = Shrinker.atomic
  end
end

(* Testing exports of [Enum]: *)

type increasing = Enum.increasing
type decreasing = Enum.decreasing

type ('k, 'v, 'cmp, 'direction) nonempty = ('k, 'v, 'cmp, 'direction) Enum.nonempty =
  | More of 'k * 'v * ('k, 'v, 'cmp) Tree.Expert.t * ('k, 'v, 'cmp, 'direction) t

and ('k, 'v, 'cmp, 'direction) t = ('k, 'v, 'cmp, 'direction) nonempty or_null
[@@deriving sexp_of]

let cons = Enum.cons

let%expect_test "cons" =
  quickcheck_m (module Tree_and_enum_increasing) ~f:(fun (tree, enum) ->
    require_equal
      (module Int_alist)
      (to_list_increasing (Enum.cons tree enum))
      (Map.Tree.to_alist tree @ to_list_increasing enum))
;;

let cons_right = Enum.cons_right

let%expect_test "cons_right" =
  quickcheck_m (module Tree_and_enum_decreasing) ~f:(fun (tree, enum) ->
    require_equal
      (module Int_alist)
      (to_list_decreasing (Enum.cons_right tree enum))
      (List.rev (Map.Tree.to_alist tree) @ to_list_decreasing enum));
  [%expect {| |}]
;;

let of_tree = Enum.of_tree

let%expect_test "of_tree" =
  quickcheck_m (module Tree_increasing) ~f:(fun tree ->
    require_equal
      (module Int_alist)
      (to_list_increasing (Enum.of_tree tree))
      (Map.Tree.to_alist tree))
;;

let of_tree_right = Enum.of_tree_right

let%expect_test "of_tree_right" =
  quickcheck_m (module Tree_increasing) ~f:(fun tree ->
    require_equal
      (module Int_alist)
      (to_list_decreasing (Enum.of_tree_right tree))
      (List.rev (Map.Tree.to_alist tree)));
  [%expect {| |}]
;;

let starting_at_increasing = Enum.starting_at_increasing

let%expect_test "starting_at_increasing" =
  quickcheck_m (module Tree_and_key) ~f:(fun (tree, pos) ->
    require_equal
      (module Int_alist)
      (to_list_increasing (Enum.starting_at_increasing tree pos Int.compare))
      (Sequence.to_list
         (Map.Tree.to_sequence
            tree
            ~comparator:Int.comparator
            ~order:`Increasing_key
            ~keys_greater_or_equal_to:pos)))
;;

let starting_at_decreasing = Enum.starting_at_decreasing

let%expect_test "starting_at_decreasing" =
  quickcheck_m (module Tree_and_key) ~f:(fun (tree, pos) ->
    require_equal
      (module Int_alist)
      (to_list_decreasing (Enum.starting_at_decreasing tree pos Int.compare))
      (Sequence.to_list
         (Map.Tree.to_sequence
            tree
            ~comparator:Int.comparator
            ~order:`Decreasing_key
            ~keys_less_or_equal_to:pos)));
  [%expect {| |}]
;;

let compare = Enum.compare

let%expect_test "compare" =
  quickcheck_m (module Pair_of_enum) ~f:(fun (enum1, enum2) ->
    require_equal
      (module Ordering)
      (Ordering.of_int (Enum.compare Int.compare Int.compare enum1 enum2))
      (Ordering.of_int
         ([%compare: (int * int) list]
            (to_list_increasing enum1)
            (to_list_increasing enum2))))
;;

let equal = Enum.equal

let%expect_test "equal" =
  quickcheck_m (module Pair_of_enum) ~f:(fun (enum1, enum2) ->
    require_equal
      (module Bool)
      (Enum.equal Int.compare Int.equal enum1 enum2)
      ([%equal: (int * int) list] (to_list_increasing enum1) (to_list_increasing enum2)))
;;

let fold = Enum.fold

let%expect_test "fold" =
  quickcheck_m (module Enum_increasing) ~f:(fun enum ->
    require_equal
      (module Int_alist)
      (Enum.fold enum ~init:[] ~f:(fun ~key ~data acc -> (key, data) :: acc))
      (to_list_increasing enum |> List.rev))
;;

let fold2 = Enum.fold2

let%expect_test "fold2" =
  quickcheck_m (module Pair_of_enum) ~f:(fun (enum1, enum2) ->
    require_equal
      (module struct
        type t = (int * [ `Both of int * int | `Left of int | `Right of int ]) list
        [@@deriving equal, sexp_of]
      end)
      (Enum.fold2 Int.compare enum1 enum2 ~init:[] ~f:(fun ~key ~data acc ->
         (key, data) :: acc))
      (Map.Tree.fold2
         ~comparator:Int.comparator
         (Map.Tree.of_alist_exn ~comparator:Int.comparator (to_list_increasing enum1))
         (Map.Tree.of_alist_exn ~comparator:Int.comparator (to_list_increasing enum2))
         ~init:[]
         ~f:(fun ~key ~data acc -> (key, data) :: acc)))
;;

let symmetric_diff = Enum.symmetric_diff

let%expect_test "symmetric_diff" =
  quickcheck_m (module Pair_of_tree) ~f:(fun (tree1, tree2) ->
    require_equal
      (module struct
        type t =
          (int * [ `Left of int | `Right of int | `Unequal of int * int ]) Sequence.t
        [@@deriving equal, sexp_of]
      end)
      (Enum.symmetric_diff tree1 tree2 ~compare_key:Int.compare ~data_equal:Int.equal)
      (Map.Tree.symmetric_diff
         tree1
         tree2
         ~comparator:Int.comparator
         ~data_equal:Int.equal))
;;

let fold_symmetric_diff = Enum.fold_symmetric_diff

let%expect_test "fold_symmetric_diff" =
  quickcheck_m (module Pair_of_tree) ~f:(fun (tree1, tree2) ->
    require_equal
      (module struct
        type t = (int * [ `Left of int | `Right of int | `Unequal of int * int ]) list
        [@@deriving equal, sexp_of]
      end)
      (Enum.fold_symmetric_diff
         tree1
         tree2
         ~compare_key:Int.compare
         ~data_equal:Int.equal
         ~init:[]
         ~f:(fun acc pair -> pair :: acc))
      (Map.Tree.fold_symmetric_diff
         tree1
         tree2
         ~comparator:Int.comparator
         ~data_equal:Int.equal
         ~init:[]
         ~f:(fun acc pair -> pair :: acc)))
;;

let drop_phys_equal_prefix = Enum.drop_phys_equal_prefix

let%expect_test "drop_phys_equal_prefix" =
  quickcheck_m (module Pair_of_tree_and_enum) ~f:(fun ((tree1, enum1), (tree2, enum2)) ->
    let list1 = to_list_increasing (cons tree1 enum1) in
    let list2 = to_list_increasing (cons tree2 enum2) in
    let without_prefix1, without_prefix2 =
      Enum.drop_phys_equal_prefix tree1 enum1 tree2 enum2
    in
    let suffix1 = to_list_increasing without_prefix1 in
    let suffix2 = to_list_increasing without_prefix2 in
    let prefix1 = List.take list1 (List.length list1 - List.length suffix1) in
    let prefix2 = List.take list2 (List.length list2 - List.length suffix2) in
    let if_false_then_print_s =
      [%lazy_sexp
        { list1 : (int * int) list
        ; list2 : (int * int) list
        ; prefix1 : (int * int) list
        ; prefix2 : (int * int) list
        ; suffix1 : (int * int) list
        ; suffix2 : (int * int) list
        ; without_prefix1 : (int, int, _, _) t
        ; without_prefix2 : (int, int, _, _) t
        }]
    in
    require_equal
      (module Int_alist)
      prefix1
      prefix2
      ~message:"prefixes must be equal"
      ~if_false_then_print_s;
    require_equal
      (module Int_alist)
      list1
      (prefix1 @ suffix1)
      ~message:"first list must round-trip"
      ~if_false_then_print_s;
    require_equal
      (module Int_alist)
      list2
      (prefix2 @ suffix2)
      ~message:"second list must round-trip"
      ~if_false_then_print_s);
  [%expect {| |}]
;;
