open! Base
open Base_quickcheck
open Expect_test_helpers_base
open Base_test_coverage_helpers
open Functor_intf.Definitions
open Overrides

let sort_either_direction list ~compare =
  match list with
  | [] | [ _ ] -> list
  | a :: b :: _ ->
    if compare a b > 0
    then List.sort list ~compare:(Comparable.reverse compare)
    else List.sort list ~compare
;;

module Side = struct
  type t =
    | Left
    | Right
  [@@deriving equal, quickcheck, sexp_of]

  let select t (a, b) =
    match t with
    | Left -> a
    | Right -> b
  ;;

  let select3 t (a, _, b) =
    match t with
    | Left -> a
    | Right -> b
  ;;
end

module Constructor = struct
  let rec_weight = 4.

  type ('key, 'data) t =
    | Empty
    | Singleton of 'key * 'data
    | Map_keys of ('key, 'key) Func.t * ('key, 'data) t [@weight rec_weight]
    | Map_keys_exn of ('key, 'key) Func.t * ('key, 'data) t [@weight rec_weight]
    | Of_sorted_array of ('key * 'data) list
    | Of_sorted_array_unchecked of ('key * 'data) list
    | Of_increasing_iterator_unchecked of ('key * 'data) list
    | Of_increasing_sequence of ('key * 'data) list
    | Of_alist of ('key * 'data) list
    | Of_alist_or_error of ('key * 'data) list
    | Of_alist_exn of ('key * 'data) list
    | Of_alist_fold of 'data * ('data, ('data, 'data) Func.t) Func.t * ('key * 'data) list
    | Of_alist_reduce of ('data, ('data, 'data) Func.t) Func.t * ('key * 'data) list
    | Of_sequence of ('key * 'data) list
    | Of_sequence_or_error of ('key * 'data) list
    | Of_sequence_exn of ('key * 'data) list
    | Of_sequence_fold of
        'data * ('data, ('data, 'data) Func.t) Func.t * ('key * 'data) list
    | Of_sequence_reduce of ('data, ('data, 'data) Func.t) Func.t * ('key * 'data) list
    | Of_list_with_key of ('data, 'key) Func.t * 'data list
    | Of_list_with_key_or_error of ('data, 'key) Func.t * 'data list
    | Of_list_with_key_exn of ('data, 'key) Func.t * 'data list
    | Of_list_with_key_fold of
        'data * ('data, ('data, 'data) Func.t) Func.t * ('data, 'key) Func.t * 'data list
    | Of_list_with_key_reduce of
        ('data, ('data, 'data) Func.t) Func.t * ('data, 'key) Func.t * 'data list
    | Of_iteri of ('key * 'data) list
    | Of_iteri_exn of ('key * 'data) list
    | Add of 'key * 'data * ('key, 'data) t [@weight rec_weight]
    | Add_exn of 'key * 'data * ('key, 'data) t [@weight rec_weight]
    | Set of 'key * 'data * ('key, 'data) t [@weight rec_weight]
    | Change of 'key * ('data option, 'data option) Func.t * ('key, 'data) t
    [@weight rec_weight]
    | Update of 'key * ('data option, 'data) Func.t * ('key, 'data) t [@weight rec_weight]
    | Remove of 'key * ('key, 'data) t [@weight rec_weight]
    | Map of ('data, 'data) Func.t * ('key, 'data) t [@weight rec_weight]
    | Mapi of ('key, ('data, 'data) Func.t) Func.t * ('key, 'data) t [@weight rec_weight]
    | Filter_keys of ('key, bool) Func.t * ('key, 'data) t [@weight rec_weight]
    | Filter of ('data, bool) Func.t * ('key, 'data) t [@weight rec_weight]
    | Filteri of ('key, ('data, bool) Func.t) Func.t * ('key, 'data) t
    [@weight rec_weight]
    | Filter_map of ('data, 'data option) Func.t * ('key, 'data) t [@weight rec_weight]
    | Filter_mapi of ('key, ('data, 'data option) Func.t) Func.t * ('key, 'data) t
    [@weight rec_weight]
    | Partition_tf of Side.t * ('data, bool) Func.t * ('key, 'data) t [@weight rec_weight]
    | Partitioni_tf of Side.t * ('key, ('data, bool) Func.t) Func.t * ('key, 'data) t
    [@weight rec_weight]
    | Partition_map of Side.t * ('data, ('data, 'data) Either.t) Func.t * ('key, 'data) t
    [@weight rec_weight]
    | Partition_mapi of
        Side.t * ('key, ('data, ('data, 'data) Either.t) Func.t) Func.t * ('key, 'data) t
    [@weight rec_weight]
    | Merge of
        ('key, ('data, 'data option) Func.t) Func.t
        * ('key, ('data, 'data option) Func.t) Func.t
        * ('key, ('data, ('data, 'data option) Func.t) Func.t) Func.t
        * ('key, 'data) t
        * ('key, 'data) t [@weight rec_weight]
    | Merge_skewed of
        ('key, ('data, ('data, 'data) Func.t) Func.t) Func.t
        * ('key, 'data) t
        * ('key, 'data) t [@weight rec_weight]
    | Merge_disjoint_exn of ('key, 'data) t * ('key, 'data) t [@weight rec_weight]
    | Split of Side.t * 'key * ('key, 'data) t [@weight rec_weight]
    | Split_le_gt of Side.t * 'key * ('key, 'data) t [@weight rec_weight]
    | Split_lt_ge of Side.t * 'key * ('key, 'data) t [@weight rec_weight]
    | Append of ('key, 'data) t * ('key, 'data) t [@weight rec_weight]
    | Subrange of 'key Maybe_bound.t * 'key Maybe_bound.t * ('key, 'data) t
    [@weight rec_weight]
    | Make_applicative_traversals__mapi of
        ('key, ('data, 'data) Func.t) Func.t * ('key, 'data) t [@weight rec_weight]
    | Make_applicative_traversals__filter_mapi of
        ('key, ('data, 'data option) Func.t) Func.t * ('key, 'data) t [@weight rec_weight]
  [@@deriving equal, quickcheck ~generator ~observer, sexp_of]

  let nested = function
    | Empty -> []
    | Singleton (_, _) -> []
    | Map_keys (_, t) -> [ t ]
    | Map_keys_exn (_, t) -> [ t ]
    | Of_sorted_array _ -> []
    | Of_sorted_array_unchecked _ -> []
    | Of_increasing_iterator_unchecked _ -> []
    | Of_increasing_sequence _ -> []
    | Of_alist _ -> []
    | Of_alist_or_error _ -> []
    | Of_alist_exn _ -> []
    | Of_alist_fold (_, _, _) -> []
    | Of_alist_reduce (_, _) -> []
    | Of_sequence _ -> []
    | Of_sequence_or_error _ -> []
    | Of_sequence_exn _ -> []
    | Of_sequence_fold (_, _, _) -> []
    | Of_sequence_reduce (_, _) -> []
    | Of_list_with_key (_, _) -> []
    | Of_list_with_key_or_error (_, _) -> []
    | Of_list_with_key_exn (_, _) -> []
    | Of_list_with_key_fold (_, _, _, _) -> []
    | Of_list_with_key_reduce (_, _, _) -> []
    | Of_iteri _ -> []
    | Of_iteri_exn _ -> []
    | Add (_, _, t) -> [ t ]
    | Add_exn (_, _, t) -> [ t ]
    | Set (_, _, t) -> [ t ]
    | Change (_, _, t) -> [ t ]
    | Update (_, _, t) -> [ t ]
    | Remove (_, t) -> [ t ]
    | Map (_, t) -> [ t ]
    | Mapi (_, t) -> [ t ]
    | Filter_keys (_, t) -> [ t ]
    | Filter (_, t) -> [ t ]
    | Filteri (_, t) -> [ t ]
    | Filter_map (_, t) -> [ t ]
    | Filter_mapi (_, t) -> [ t ]
    | Partition_tf (_, _, t) -> [ t ]
    | Partitioni_tf (_, _, t) -> [ t ]
    | Partition_map (_, _, t) -> [ t ]
    | Partition_mapi (_, _, t) -> [ t ]
    | Merge (_, _, _, a, b) -> [ a; b ]
    | Merge_skewed (_, a, b) -> [ a; b ]
    | Merge_disjoint_exn (a, b) -> [ a; b ]
    | Split (_, _, t) -> [ t ]
    | Split_le_gt (_, _, t) -> [ t ]
    | Split_lt_ge (_, _, t) -> [ t ]
    | Append (a, b) -> [ a; b ]
    | Subrange (_, _, t) -> [ t ]
    | Make_applicative_traversals__mapi (_, t) -> [ t ]
    | Make_applicative_traversals__filter_mapi (_, t) -> [ t ]
  ;;

  let rec number_of_constructors t =
    1 + List.sum (module Int) (nested t) ~f:number_of_constructors
  ;;

  (* Simple shrinker to whittle away unnecessary enclosing constructors. *)
  let quickcheck_shrinker (type key data) (_ : key Shrinker.t) (_ : data Shrinker.t)
    : (key, data) t Shrinker.t
    =
    Shrinker.create (fun t -> Sequence.of_list (nested t))
  ;;

  let keys t ~compare =
    let rec keys = function
      | Empty -> []
      | Singleton (key, _) -> [ key ]
      | Map_keys (fn, t) -> Func.inputs fn @ Func.outputs fn @ keys t
      | Map_keys_exn (fn, t) -> Func.inputs fn @ Func.outputs fn @ keys t
      | Of_sorted_array alist -> List.map alist ~f:fst
      | Of_sorted_array_unchecked alist -> List.map alist ~f:fst
      | Of_increasing_iterator_unchecked alist -> List.map alist ~f:fst
      | Of_increasing_sequence alist -> List.map alist ~f:fst
      | Of_alist alist -> List.map alist ~f:fst
      | Of_alist_or_error alist -> List.map alist ~f:fst
      | Of_alist_exn alist -> List.map alist ~f:fst
      | Of_alist_fold (_, _, alist) -> List.map alist ~f:fst
      | Of_alist_reduce (_, alist) -> List.map alist ~f:fst
      | Of_sequence alist -> List.map alist ~f:fst
      | Of_sequence_or_error alist -> List.map alist ~f:fst
      | Of_sequence_exn alist -> List.map alist ~f:fst
      | Of_sequence_fold (_, _, alist) -> List.map alist ~f:fst
      | Of_sequence_reduce (_, alist) -> List.map alist ~f:fst
      | Of_list_with_key (get_key_fn, _) -> Func.outputs get_key_fn
      | Of_list_with_key_or_error (get_key_fn, _) -> Func.outputs get_key_fn
      | Of_list_with_key_exn (get_key_fn, _) -> Func.outputs get_key_fn
      | Of_list_with_key_fold (_, _, get_key_fn, _) -> Func.outputs get_key_fn
      | Of_list_with_key_reduce (_, get_key_fn, _) -> Func.outputs get_key_fn
      | Of_iteri alist -> List.map alist ~f:fst
      | Of_iteri_exn alist -> List.map alist ~f:fst
      | Add (key, _, t) -> key :: keys t
      | Add_exn (key, _, t) -> key :: keys t
      | Set (key, _, t) -> key :: keys t
      | Change (key, _, t) -> key :: keys t
      | Update (key, _, t) -> key :: keys t
      | Remove (key, t) -> key :: keys t
      | Map (_, t) -> keys t
      | Mapi (fn, t) -> Func.inputs fn @ keys t
      | Filter_keys (fn, t) -> Func.inputs fn @ keys t
      | Filter (_, t) -> keys t
      | Filteri (fn, t) -> Func.inputs fn @ keys t
      | Filter_map (_, t) -> keys t
      | Filter_mapi (fn, t) -> Func.inputs fn @ keys t
      | Partition_tf (_, _, t) -> keys t
      | Partitioni_tf (_, fn, t) -> Func.inputs fn @ keys t
      | Partition_map (_, _, t) -> keys t
      | Partition_mapi (_, fn, t) -> Func.inputs fn @ keys t
      | Merge (left_fn, right_fn, both_fn, a, b) ->
        Func.inputs left_fn @ Func.inputs right_fn @ Func.inputs both_fn @ keys a @ keys b
      | Merge_skewed (fn, a, b) -> Func.inputs fn @ keys a @ keys b
      | Merge_disjoint_exn (a, b) -> keys a @ keys b
      | Split (_, key, t) -> key :: keys t
      | Split_le_gt (_, key, t) -> key :: keys t
      | Split_lt_ge (_, key, t) -> key :: keys t
      | Append (a, b) -> keys a @ keys b
      | Subrange (lower_bound, upper_bound, t) ->
        Maybe_bound.to_list lower_bound @ Maybe_bound.to_list upper_bound @ keys t
      | Make_applicative_traversals__mapi (fn, t) -> Func.inputs fn @ keys t
      | Make_applicative_traversals__filter_mapi (fn, t) -> Func.inputs fn @ keys t
    in
    List.dedup_and_sort ~compare (keys t)
  ;;

  let rec map
    : type key1 key2 data1 data2.
      (key1, data1) t -> k:(key1 -> key2) -> d:(data1 -> data2) -> (key2, data2) t
    =
    fun t ~k ~d ->
    match t with
    | Empty -> Empty
    | Singleton (key, data) -> Singleton (k key, d data)
    | Map_keys (fn, t) -> Map_keys (Func.map ~i:k ~o:k fn, map ~k ~d t)
    | Map_keys_exn (fn, t) -> Map_keys_exn (Func.map ~i:k ~o:k fn, map ~k ~d t)
    | Of_sorted_array alist ->
      Of_sorted_array (List.map ~f:(fun (key, data) -> k key, d data) alist)
    | Of_sorted_array_unchecked alist ->
      Of_sorted_array_unchecked (List.map ~f:(fun (key, data) -> k key, d data) alist)
    | Of_increasing_iterator_unchecked alist ->
      Of_increasing_iterator_unchecked
        (List.map ~f:(fun (key, data) -> k key, d data) alist)
    | Of_increasing_sequence alist ->
      Of_increasing_sequence (List.map ~f:(fun (key, data) -> k key, d data) alist)
    | Of_alist alist -> Of_alist (List.map ~f:(fun (key, data) -> k key, d data) alist)
    | Of_alist_or_error alist ->
      Of_alist_or_error (List.map ~f:(fun (key, data) -> k key, d data) alist)
    | Of_alist_exn alist ->
      Of_alist_exn (List.map ~f:(fun (key, data) -> k key, d data) alist)
    | Of_alist_fold (init, fn, alist) ->
      Of_alist_fold
        ( d init
        , Func.map ~i:d ~o:(Func.map ~i:d ~o:d) fn
        , List.map ~f:(fun (key, data) -> k key, d data) alist )
    | Of_alist_reduce (fn, alist) ->
      Of_alist_reduce
        ( Func.map ~i:d ~o:(Func.map ~i:d ~o:d) fn
        , List.map ~f:(fun (key, data) -> k key, d data) alist )
    | Of_sequence alist ->
      Of_sequence (List.map ~f:(fun (key, data) -> k key, d data) alist)
    | Of_sequence_or_error alist ->
      Of_sequence_or_error (List.map ~f:(fun (key, data) -> k key, d data) alist)
    | Of_sequence_exn alist ->
      Of_sequence_exn (List.map ~f:(fun (key, data) -> k key, d data) alist)
    | Of_sequence_fold (init, fn, alist) ->
      Of_sequence_fold
        ( d init
        , Func.map ~i:d ~o:(Func.map ~i:d ~o:d) fn
        , List.map ~f:(fun (key, data) -> k key, d data) alist )
    | Of_sequence_reduce (fn, alist) ->
      Of_sequence_reduce
        ( Func.map ~i:d ~o:(Func.map ~i:d ~o:d) fn
        , List.map ~f:(fun (key, data) -> k key, d data) alist )
    | Of_list_with_key (get_key_fn, list) ->
      Of_list_with_key (Func.map ~i:d ~o:k get_key_fn, List.map ~f:d list)
    | Of_list_with_key_or_error (get_key_fn, list) ->
      Of_list_with_key_or_error (Func.map ~i:d ~o:k get_key_fn, List.map ~f:d list)
    | Of_list_with_key_exn (get_key_fn, list) ->
      Of_list_with_key_exn (Func.map ~i:d ~o:k get_key_fn, List.map ~f:d list)
    | Of_list_with_key_fold (init, fn, get_key_fn, list) ->
      Of_list_with_key_fold
        ( d init
        , Func.map ~i:d ~o:(Func.map ~i:d ~o:d) fn
        , Func.map ~i:d ~o:k get_key_fn
        , List.map ~f:d list )
    | Of_list_with_key_reduce (fn, get_key_fn, list) ->
      Of_list_with_key_reduce
        ( Func.map ~i:d ~o:(Func.map ~i:d ~o:d) fn
        , Func.map ~i:d ~o:k get_key_fn
        , List.map ~f:d list )
    | Of_iteri alist -> Of_iteri (List.map ~f:(fun (key, data) -> k key, d data) alist)
    | Of_iteri_exn alist ->
      Of_iteri_exn (List.map ~f:(fun (key, data) -> k key, d data) alist)
    | Add (key, data, t) -> Add (k key, d data, map ~k ~d t)
    | Add_exn (key, data, t) -> Add_exn (k key, d data, map ~k ~d t)
    | Set (key, data, t) -> Set (k key, d data, map ~k ~d t)
    | Change (key, fn, t) ->
      Change (k key, Func.map ~i:(Option.map ~f:d) ~o:(Option.map ~f:d) fn, map ~k ~d t)
    | Update (key, fn, t) ->
      Update (k key, Func.map ~i:(Option.map ~f:d) ~o:d fn, map ~k ~d t)
    | Remove (key, t) -> Remove (k key, map ~k ~d t)
    | Map (fn, t) -> Map (Func.map ~i:d ~o:d fn, map ~k ~d t)
    | Mapi (fn, t) -> Mapi (Func.map ~i:k ~o:(Func.map ~i:d ~o:d) fn, map ~k ~d t)
    | Filter_keys (fn, t) -> Filter_keys (Func.map ~i:k ~o:Fn.id fn, map ~k ~d t)
    | Filter (fn, t) -> Filter (Func.map ~i:d ~o:Fn.id fn, map ~k ~d t)
    | Filteri (fn, t) ->
      Filteri (Func.map ~i:k ~o:(Func.map ~i:d ~o:Fn.id) fn, map ~k ~d t)
    | Filter_map (fn, t) -> Filter_map (Func.map ~i:d ~o:(Option.map ~f:d) fn, map ~k ~d t)
    | Filter_mapi (fn, t) ->
      Filter_mapi (Func.map ~i:k ~o:(Func.map ~i:d ~o:(Option.map ~f:d)) fn, map ~k ~d t)
    | Partition_tf (side, fn, t) ->
      Partition_tf (side, Func.map ~i:d ~o:Fn.id fn, map ~k ~d t)
    | Partitioni_tf (side, fn, t) ->
      Partitioni_tf (side, Func.map ~i:k ~o:(Func.map ~i:d ~o:Fn.id) fn, map ~k ~d t)
    | Partition_map (side, fn, t) ->
      Partition_map
        (side, Func.map ~i:d ~o:(Either.map ~first:d ~second:d) fn, map ~k ~d t)
    | Partition_mapi (side, fn, t) ->
      Partition_mapi
        ( side
        , Func.map ~i:k ~o:(Func.map ~i:d ~o:(Either.map ~first:d ~second:d)) fn
        , map ~k ~d t )
    | Merge (left_fn, right_fn, both_fn, a, b) ->
      Merge
        ( Func.map ~i:k ~o:(Func.map ~i:d ~o:(Option.map ~f:d)) left_fn
        , Func.map ~i:k ~o:(Func.map ~i:d ~o:(Option.map ~f:d)) right_fn
        , Func.map ~i:k ~o:(Func.map ~i:d ~o:(Func.map ~i:d ~o:(Option.map ~f:d))) both_fn
        , map ~k ~d a
        , map ~k ~d b )
    | Merge_skewed (fn, a, b) ->
      Merge_skewed
        ( Func.map ~i:k ~o:(Func.map ~i:d ~o:(Func.map ~i:d ~o:d)) fn
        , map ~k ~d a
        , map ~k ~d b )
    | Merge_disjoint_exn (a, b) -> Merge_disjoint_exn (map ~k ~d a, map ~k ~d b)
    | Split (side, key, t) -> Split (side, k key, map ~k ~d t)
    | Split_le_gt (side, key, t) -> Split_le_gt (side, k key, map ~k ~d t)
    | Split_lt_ge (side, key, t) -> Split_lt_ge (side, k key, map ~k ~d t)
    | Append (a, b) -> Append (map ~k ~d a, map ~k ~d b)
    | Subrange (lower_bound, upper_bound, t) ->
      Subrange
        (Maybe_bound.map ~f:k lower_bound, Maybe_bound.map ~f:k upper_bound, map ~k ~d t)
    | Make_applicative_traversals__mapi (fn, t) ->
      Make_applicative_traversals__mapi
        (Func.map ~i:k ~o:(Func.map ~i:d ~o:d) fn, map ~k ~d t)
    | Make_applicative_traversals__filter_mapi (fn, t) ->
      Make_applicative_traversals__filter_mapi
        (Func.map ~i:k ~o:(Func.map ~i:d ~o:(Option.map ~f:d)) fn, map ~k ~d t)
  ;;

  module Make
      (Instance : Instance)
      (Impl : Impl with module Types := Instance.Types)
      (Data : Data.S) =
  struct
    module Key = struct
      include Instance.Key

      let get = to_int
      let set _ n = of_int n
    end

    module Data = struct
      include Data

      let get = to_int
      let set _ n = of_int n
    end

    let unique_key = Adjustable.unique (module Key)
    let non_overlapping_data = Adjustable.non_overlapping (module Data)

    let non_overlapping_alist =
      Adjustable.non_overlapping
        (module struct
          type t = Key.t * Data.t

          let get (key, _) = Key.to_int key
          let set (_, data) n = Key.of_int n, data
        end)
    ;;

    let keys t = keys t ~compare:Key.compare

    (* [value (normalize t)] is guaranteed not to raise, at least for [t] from
       [quickcheck_generator]. We rely on key values being small so we don't have to deal
       with integer overflow. *)
    let rec normalize = function
      | Empty -> Empty
      | Singleton (key, data) -> Singleton (key, data)
      | Map_keys (fn, t) ->
        let t = normalize t in
        let fn = Func.injective fn (module Key) (module Key) (keys t) in
        Map_keys (fn, t)
      | Map_keys_exn (fn, t) ->
        let t = normalize t in
        let fn = Func.injective fn (module Key) (module Key) (keys t) in
        Map_keys_exn (fn, t)
      | Of_sorted_array alist ->
        let alist =
          non_overlapping_alist alist
          |> sort_either_direction ~compare:[%compare: Key.t * _]
        in
        Of_sorted_array alist
      | Of_sorted_array_unchecked alist ->
        let alist =
          non_overlapping_alist alist
          |> sort_either_direction ~compare:[%compare: Key.t * _]
        in
        Of_sorted_array_unchecked alist
      | Of_increasing_iterator_unchecked alist ->
        let alist =
          non_overlapping_alist alist |> List.sort ~compare:[%compare: Key.t * _]
        in
        Of_increasing_iterator_unchecked alist
      | Of_increasing_sequence alist ->
        let alist =
          non_overlapping_alist alist |> List.sort ~compare:[%compare: Key.t * _]
        in
        Of_increasing_sequence alist
      | Of_alist alist ->
        let alist = non_overlapping_alist alist in
        Of_alist alist
      | Of_alist_or_error alist ->
        let alist = non_overlapping_alist alist in
        Of_alist_or_error alist
      | Of_alist_exn alist ->
        let alist = non_overlapping_alist alist in
        Of_alist_exn alist
      | Of_alist_fold (init, fn, alist) -> Of_alist_fold (init, fn, alist)
      | Of_alist_reduce (fn, alist) -> Of_alist_reduce (fn, alist)
      | Of_sequence alist ->
        let alist = non_overlapping_alist alist in
        Of_sequence alist
      | Of_sequence_or_error alist ->
        let alist = non_overlapping_alist alist in
        Of_sequence_or_error alist
      | Of_sequence_exn alist ->
        let alist = non_overlapping_alist alist in
        Of_sequence_exn alist
      | Of_sequence_fold (init, fn, alist) -> Of_sequence_fold (init, fn, alist)
      | Of_sequence_reduce (fn, alist) -> Of_sequence_reduce (fn, alist)
      | Of_list_with_key (fn, list) ->
        let list = non_overlapping_data list in
        let fn = Func.injective fn (module Data) (module Key) list in
        Of_list_with_key (fn, list)
      | Of_list_with_key_or_error (fn, list) ->
        let list = non_overlapping_data list in
        let fn = Func.injective fn (module Data) (module Key) list in
        Of_list_with_key_or_error (fn, list)
      | Of_list_with_key_exn (fn, list) ->
        let list = non_overlapping_data list in
        let fn = Func.injective fn (module Data) (module Key) list in
        Of_list_with_key_exn (fn, list)
      | Of_list_with_key_fold (init, fn, get_key_fn, list) ->
        Of_list_with_key_fold (init, fn, get_key_fn, list)
      | Of_list_with_key_reduce (fn, get_key_fn, list) ->
        Of_list_with_key_reduce (fn, get_key_fn, list)
      | Of_iteri alist -> Of_iteri (non_overlapping_alist alist)
      | Of_iteri_exn alist -> Of_iteri_exn (non_overlapping_alist alist)
      | Add (key, data, t) ->
        let t = normalize t in
        let key = unique_key key (keys t) in
        Add (key, data, t)
      | Add_exn (key, data, t) ->
        let t = normalize t in
        let key = unique_key key (keys t) in
        Add_exn (key, data, t)
      | Set (key, data, t) -> Set (key, data, normalize t)
      | Change (key, fn, t) -> Change (key, fn, normalize t)
      | Update (key, fn, t) -> Update (key, fn, normalize t)
      | Remove (key, t) -> Remove (key, normalize t)
      | Map (fn, t) -> Map (fn, normalize t)
      | Mapi (fn, t) -> Mapi (fn, normalize t)
      | Filter_keys (fn, t) -> Filter_keys (fn, normalize t)
      | Filter (fn, t) -> Filter (fn, normalize t)
      | Filteri (fn, t) -> Filteri (fn, normalize t)
      | Filter_map (fn, t) -> Filter_map (fn, normalize t)
      | Filter_mapi (fn, t) -> Filter_mapi (fn, normalize t)
      | Partition_tf (side, fn, t) -> Partition_tf (side, fn, normalize t)
      | Partitioni_tf (side, fn, t) -> Partitioni_tf (side, fn, normalize t)
      | Partition_map (side, fn, t) -> Partition_map (side, fn, normalize t)
      | Partition_mapi (side, fn, t) -> Partition_mapi (side, fn, normalize t)
      | Merge (left_fn, right_fn, both_fn, a, b) ->
        Merge (left_fn, right_fn, both_fn, normalize a, normalize b)
      | Merge_skewed (fn, a, b) -> Merge_skewed (fn, normalize a, normalize b)
      | Merge_disjoint_exn (a, b) ->
        let a = normalize a
        and b = normalize b in
        let rename_a, rename_b =
          List.concat
            [ List.map (keys a) ~f:(fun key -> Side.Left, (key, key))
            ; List.map (keys b) ~f:(fun key -> Side.Right, (key, key))
            ]
          |> Adjustable.non_overlapping
               (module struct
                 type t = Side.t * (Key.t * Key.t)

                 let get (_, (_, key)) = Key.to_int key
                 let set (side, (key, _)) int = side, (key, Key.of_int int)
               end)
          |> List.partition_map ~f:(function
            | Left, pair -> First pair
            | Right, pair -> Second pair)
        in
        let a =
          map a ~k:(fun k -> List.Assoc.find_exn rename_a k ~equal:Key.equal) ~d:Fn.id
        and b =
          map b ~k:(fun k -> List.Assoc.find_exn rename_b k ~equal:Key.equal) ~d:Fn.id
        in
        Merge_disjoint_exn (a, b)
      | Split (side, key, t) -> Split (side, key, normalize t)
      | Split_le_gt (side, key, t) -> Split_le_gt (side, key, normalize t)
      | Split_lt_ge (side, key, t) -> Split_lt_ge (side, key, normalize t)
      | Append (a, b) ->
        let a = normalize a
        and b = normalize b in
        let b =
          (* We rely on [Generator.small_positive_or_zero_int] so that we don't have to
             worry about overflow here. *)
          match
            ( List.max_elt ~compare:Key.compare (keys a)
            , List.min_elt ~compare:Key.compare (keys b) )
          with
          | Some x, Some y when Key.compare x y >= 0 ->
            let k key = Key.of_int (Key.to_int key + 1 + Key.to_int x - Key.to_int y) in
            map b ~k ~d:Fn.id
          | _ -> b
        in
        Append (a, b)
      | Subrange (lower_bound, upper_bound, t) ->
        Subrange (lower_bound, upper_bound, normalize t)
      | Make_applicative_traversals__mapi (fn, t) ->
        Make_applicative_traversals__mapi (fn, normalize t)
      | Make_applicative_traversals__filter_mapi (fn, t) ->
        Make_applicative_traversals__filter_mapi (fn, normalize t)
    ;;

    let or_duplicate_key_exn = function
      | `Ok x -> x
      | `Duplicate_key key -> raise_s [%sexp `Duplicate_key (key : Key.t)]
    ;;

    let or_duplicate_exn = function
      | `Ok x -> x
      | `Duplicate -> raise_s [%sexp `Duplicate]
    ;;

    let or_overlapping_key_ranges_exn = function
      | `Ok x -> x
      | `Overlapping_key_ranges -> raise_s [%sexp `Overlapping_key_ranges]
    ;;

    module Data_option = struct
      type t = Data.t option [@@deriving compare, equal]
    end

    module Traversals = Impl.Make_applicative_traversals (struct
        type 'a t = 'a

        let of_thunk f = f ()

        include Applicative.Make (struct
            type 'a t = 'a

            let return x = x
            let apply f x = f x
            let map = `Define_using_apply
          end)
      end)

    let rec value =
      let open Instance in
      function
      | Empty -> create Impl.empty
      | Singleton (key, data) -> (create Impl.singleton) key data
      | Map_keys (fn, t) ->
        (create Impl.map_keys) (value t) ~f:(Func.apply fn (module Key))
        |> or_duplicate_key_exn
      | Map_keys_exn (fn, t) ->
        (create Impl.map_keys_exn) (value t) ~f:(Func.apply fn (module Key))
      | Of_sorted_array alist ->
        (create Impl.of_sorted_array) (Array.of_list alist) |> Or_error.ok_exn
      | Of_sorted_array_unchecked alist ->
        (create Impl.of_sorted_array_unchecked) (Array.of_list alist)
      | Of_increasing_iterator_unchecked alist ->
        let array = Array.of_list alist in
        (create Impl.of_increasing_iterator_unchecked)
          ~len:(Array.length array)
          ~f:(Array.get array)
      | Of_increasing_sequence alist ->
        (create Impl.of_increasing_sequence) (Sequence.of_list alist) |> Or_error.ok_exn
      | Of_alist alist -> (create Impl.of_alist) alist |> or_duplicate_key_exn
      | Of_alist_or_error alist ->
        (create Impl.of_alist_or_error) alist |> Or_error.ok_exn
      | Of_alist_exn alist -> (create Impl.of_alist_exn) alist
      | Of_alist_fold (init, fn, alist) ->
        (create Impl.of_alist_fold)
          alist
          ~init
          ~f:(Func.apply2 fn (module Data) (module Data))
      | Of_alist_reduce (fn, alist) ->
        (create Impl.of_alist_reduce)
          alist
          ~f:(Func.apply2 fn (module Data) (module Data))
      | Of_sequence alist ->
        (create Impl.of_sequence) (Sequence.of_list alist) |> or_duplicate_key_exn
      | Of_sequence_or_error alist ->
        (create Impl.of_sequence_or_error) (Sequence.of_list alist) |> Or_error.ok_exn
      | Of_sequence_exn alist -> (create Impl.of_sequence_exn) (Sequence.of_list alist)
      | Of_sequence_fold (init, fn, alist) ->
        (create Impl.of_sequence_fold)
          (Sequence.of_list alist)
          ~init
          ~f:(Func.apply2 fn (module Data) (module Data))
      | Of_sequence_reduce (fn, alist) ->
        (create Impl.of_sequence_reduce)
          (Sequence.of_list alist)
          ~f:(Func.apply2 fn (module Data) (module Data))
      | Of_list_with_key (fn, alist) ->
        (create Impl.of_list_with_key) alist ~get_key:(Func.apply fn (module Data))
        |> or_duplicate_key_exn
      | Of_list_with_key_or_error (fn, alist) ->
        (create Impl.of_list_with_key_or_error)
          alist
          ~get_key:(Func.apply fn (module Data))
        |> Or_error.ok_exn
      | Of_list_with_key_exn (fn, alist) ->
        (create Impl.of_list_with_key_exn) alist ~get_key:(Func.apply fn (module Data))
      | Of_list_with_key_fold (init, fn, get_key_fn, alist) ->
        (create Impl.of_list_with_key_fold)
          alist
          ~init
          ~f:(Func.apply2 fn (module Data) (module Data))
          ~get_key:(Func.apply get_key_fn (module Data))
      | Of_list_with_key_reduce (fn, get_key_fn, alist) ->
        (create Impl.of_list_with_key_reduce)
          alist
          ~f:(Func.apply2 fn (module Data) (module Data))
          ~get_key:(Func.apply get_key_fn (module Data))
      | Of_iteri alist ->
        (create Impl.of_iteri) ~iteri:(fun ~f ->
          List.iter alist ~f:(fun (key, data) -> f ~key ~data) [@nontail])
        |> or_duplicate_key_exn
      | Of_iteri_exn alist ->
        (create Impl.of_iteri_exn) ~iteri:(fun ~f ->
          List.iter alist ~f:(fun (key, data) -> f ~key ~data) [@nontail])
      | Add (key, data, t) -> (access Impl.add) (value t) ~key ~data |> or_duplicate_exn
      | Add_exn (key, data, t) -> (access Impl.add_exn) (value t) ~key ~data
      | Set (key, data, t) -> (access Impl.set) (value t) ~key ~data
      | Change (key, fn, t) ->
        (access Impl.change) (value t) key ~f:(Func.apply fn (module Data_option))
      | Update (key, fn, t) ->
        (access Impl.update) (value t) key ~f:(Func.apply fn (module Data_option))
      | Remove (key, t) -> (access Impl.remove) (value t) key
      | Map (fn, t) -> Impl.map (value t) ~f:(Func.apply fn (module Data))
      | Mapi (fn, t) ->
        Impl.mapi (value t) ~f:(fun ~key ~data ->
          Func.apply2 fn (module Key) (module Data) key data)
      | Filter_keys (fn, t) -> Impl.filter_keys (value t) ~f:(Func.apply fn (module Key))
      | Filter (fn, t) -> Impl.filter (value t) ~f:(Func.apply fn (module Data))
      | Filteri (fn, t) ->
        Impl.filteri (value t) ~f:(fun ~key ~data ->
          Func.apply2 fn (module Key) (module Data) key data)
      | Filter_map (fn, t) -> Impl.filter_map (value t) ~f:(Func.apply fn (module Data))
      | Filter_mapi (fn, t) ->
        Impl.filter_mapi (value t) ~f:(fun ~key ~data ->
          Func.apply2 fn (module Key) (module Data) key data)
      | Partition_tf (side, fn, t) ->
        Impl.partition_tf (value t) ~f:(Func.apply fn (module Data)) |> Side.select side
      | Partitioni_tf (side, fn, t) ->
        Impl.partitioni_tf (value t) ~f:(fun ~key ~data ->
          Func.apply2 fn (module Key) (module Data) key data)
        |> Side.select side
      | Partition_map (side, fn, t) ->
        Impl.partition_map (value t) ~f:(Func.apply fn (module Data)) |> Side.select side
      | Partition_mapi (side, fn, t) ->
        Impl.partition_mapi (value t) ~f:(fun ~key ~data ->
          Func.apply2 fn (module Key) (module Data) key data)
        |> Side.select side
      | Merge (left_fn, right_fn, both_fn, a, b) ->
        (access Impl.merge) (value a) (value b) ~f:(fun ~key -> function
          | `Left data -> Func.apply2 left_fn (module Key) (module Data) key data
          | `Right data -> Func.apply2 right_fn (module Key) (module Data) key data
          | `Both (x, y) ->
            Func.apply3 both_fn (module Key) (module Data) (module Data) key x y)
      | Merge_skewed (fn, a, b) ->
        (access Impl.merge_skewed) (value a) (value b) ~combine:(fun ~key x y ->
          Func.apply3 fn (module Key) (module Data) (module Data) key x y)
      | Merge_disjoint_exn (a, b) -> (access Impl.merge_disjoint_exn) (value a) (value b)
      | Split (side, key, t) -> (access Impl.split) (value t) key |> Side.select3 side
      | Split_le_gt (side, key, t) ->
        (access Impl.split_le_gt) (value t) key |> Side.select side
      | Split_lt_ge (side, key, t) ->
        (access Impl.split_lt_ge) (value t) key |> Side.select side
      | Append (a, b) ->
        (access Impl.append) ~lower_part:(value a) ~upper_part:(value b)
        |> or_overlapping_key_ranges_exn
      | Subrange (lower_bound, upper_bound, t) ->
        (access Impl.subrange) ~lower_bound ~upper_bound (value t)
      | Make_applicative_traversals__mapi (fn, t) ->
        Traversals.mapi (value t) ~f:(fun ~key ~data ->
          Func.apply2 fn (module Key) (module Data) key data)
      | Make_applicative_traversals__filter_mapi (fn, t) ->
        Traversals.filter_mapi (value t) ~f:(fun ~key ~data ->
          Func.apply2 fn (module Key) (module Data) key data)
    ;;

    (* Report normalization failure using a small input if possible. *)
    let rec shrink_and_raise ~src ~dst ~exn =
      Shrinker.shrink [%shrinker: (_, _) t] src
      |> Sequence.find_map ~f:(fun src ->
        let dst = normalize src in
        match value dst with
        | _ -> None
        | exception exn -> Some (src, dst, exn))
      |> function
      | Some (src, dst, exn) -> shrink_and_raise ~src ~dst ~exn
      | None ->
        raise_s
          [%message
            "normalization fails"
              (src : (Key.t, Data.t) t)
              (dst : (Key.t, Data.t) t)
              (exn : exn)]
    ;;

    (* We test normalization when we apply it so we can get a good, early error message if
       something goes wrong. *)
    let normalize src =
      let dst = normalize src in
      match value dst with
      | _ -> dst
      | exception exn -> shrink_and_raise ~src ~dst ~exn
    ;;
  end
end

(* Cobble everything together and generate normalized constructor trees. *)
module Make
    (Instance : Instance)
    (Impl : Impl with module Types := Instance.Types)
    (Data : Data.S) =
struct
  open Instance

  type data = Data.t [@@deriving sexp_of]
  type t = (Key.t, Data.t) Constructor.t [@@deriving equal, quickcheck, sexp_of]

  include Constructor.Make (Instance) (Impl) (Data)

  let quickcheck_generator = Generator.map quickcheck_generator ~f:normalize
end

(* Coverage testing so we know we haven't forgotten any constructor functions.

   The number of quickcheck tests on 32-bit architectures is configured to be lower, so we
   skip the coverage checks there. *)
module%test [@tags "64-bits-only"] _ : Impl = struct
  module Types = struct
    type 'key key = 'key
    type 'cmp cmp = 'cmp
    type ('key, 'data, 'cmp) t = ('key, 'data, 'cmp) Map.t
    type ('key, 'data, 'cmp) tree = ('key, 'data, 'cmp) Map.Using_comparator.Tree.t

    type ('key, 'cmp, 'fn) create_options =
      ('key, 'cmp, 'fn) Map.With_first_class_module.t

    type ('key, 'cmp, 'fn) access_options = ('key, 'cmp, 'fn) Map.Without_comparator.t
  end

  open struct
    module Int_data = struct
      include Int

      type t = int [@@deriving quickcheck]

      let combine_non_commutative a b = (10 * a) + b
    end

    module Instance = struct
      module Types = Types
      module Key = Int_data

      type 'a t = (int, 'a, Int.comparator_witness) Map.t

      let compare = Map.compare_direct
      let equal = Map.equal
      let sexp_of_t f t = Map.sexp_of_m__t (module Key) f t
      let create f = f ((module Key) : (_, _) Comparator.Module.t)
      let access f = f
    end

    module Cons = Make (Instance) (Map) (Int_data)

    let sample = Memo.memoize Cons.quickcheck_generator

    let%expect_test "normalization" =
      quickcheck_m
        (module struct
          include Cons

          let sample = sample
        end)
        ~f:(fun t ->
          require_equal (module Cons) t (Cons.normalize t);
          require_does_not_raise (fun () -> ignore (Cons.value t : int Map.M(Int).t)))
    ;;

    let%expect_test "number of constructors" =
      let stats = Stats.create () in
      List.iter (Lazy.force sample) ~f:(fun t ->
        Stats.add stats (Constructor.number_of_constructors t));
      Stats.print stats;
      [%expect
        {|
          % | size | count
        ----+------+------
          0 |    1 | 10000
         50 |    4 |  5747
         75 |   11 |  2687
         90 |   25 |  1057
         95 |   40 |   523
         99 |   77 |   101
        100 |  293 |     1
        |}]
    ;;

    let test predicate =
      let stats = Stats.create () in
      List.iter (force sample) ~f:(fun t ->
        if predicate t
        then (
          let size = Map.length (Cons.value t) in
          Stats.add stats size));
      Stats.print stats
    ;;
  end

  (* Accessors only, not covered. *)

  include (Map : Accessors with module Types := Types)

  (* Complicated types, not covered. *)

  let transpose_keys = Map.transpose_keys
  let of_tree = Map.of_tree
  let combine_errors = Map.combine_errors
  let unzip = Map.unzip
  let of_alist_multi = Map.of_alist_multi
  let of_sequence_multi = Map.of_sequence_multi
  let of_list_with_key_multi = Map.of_list_with_key_multi
  let add_multi = Map.add_multi
  let remove_multi = Map.remove_multi

  (* Tests *)

  let empty = Map.empty

  let%expect_test _ =
    test (function
      | Empty -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    86
      |}]
  ;;

  let singleton = Map.singleton

  let%expect_test _ =
    test (function
      | Singleton _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
      100 |    1 |    79
      |}]
  ;;

  let map_keys = Map.map_keys

  let%expect_test _ =
    test (function
      | Map_keys _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   117
      ----+------+------
        0 |    1 |   170
       50 |    3 |    94
       75 |    8 |    44
       90 |   14 |    21
       95 |   19 |    10
       99 |   27 |     2
      100 |   28 |     1
      |}]
  ;;

  let map_keys_exn = Map.map_keys_exn

  let%expect_test _ =
    test (function
      | Map_keys_exn _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    91
      ----+------+------
        0 |    1 |   192
       50 |    3 |   100
       75 |    8 |    50
       90 |   16 |    20
       95 |   23 |    11
       99 |   28 |     2
      100 |   48 |     1
      |}]
  ;;

  let of_sorted_array = Map.of_sorted_array

  let%expect_test _ =
    test (function
      | Of_sorted_array _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    16
      ----+------+------
        0 |    1 |    56
       50 |   11 |    28
       75 |   21 |    15
       90 |   25 |     8
       95 |   28 |     3
      100 |   31 |     1
      |}]
  ;;

  let of_sorted_array_unchecked = Map.of_sorted_array_unchecked

  let%expect_test _ =
    test (function
      | Of_sorted_array_unchecked _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |     9
      ----+------+------
        0 |    1 |    74
       50 |    8 |    41
       75 |   18 |    20
       90 |   25 |     9
      100 |   28 |     4
      |}]
  ;;

  let of_increasing_iterator_unchecked = Map.of_increasing_iterator_unchecked

  let%expect_test _ =
    test (function
      | Of_increasing_iterator_unchecked _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |     9
      ----+------+------
        0 |    1 |    82
       50 |   10 |    41
       75 |   18 |    21
       90 |   26 |     9
       95 |   27 |     7
      100 |   30 |     2
      |}]
  ;;

  let of_alist = Map.of_alist

  let%expect_test _ =
    test (function
      | Of_alist _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    18
      ----+------+------
        0 |    1 |    64
       50 |   13 |    32
       75 |   22 |    16
       90 |   26 |     8
       95 |   29 |     4
      100 |   31 |     1
      |}]
  ;;

  let of_alist_or_error = Map.of_alist_or_error

  let%expect_test _ =
    test (function
      | Of_alist_or_error _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    18
      ----+------+------
        0 |    1 |    74
       50 |   11 |    37
       75 |   20 |    20
       90 |   26 |     8
       95 |   29 |     4
      100 |   31 |     2
      |}]
  ;;

  let of_alist_exn = Map.of_alist_exn

  let%expect_test _ =
    test (function
      | Of_alist_exn _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |     9
      ----+------+------
        0 |    1 |    59
       50 |    9 |    30
       75 |   16 |    15
       90 |   25 |     6
       95 |   28 |     3
      100 |   30 |     1
      |}]
  ;;

  let of_alist_fold = Map.of_alist_fold

  let%expect_test _ =
    test (function
      | Of_alist_fold _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |     8
      ----+------+------
        0 |    1 |    76
       50 |    3 |    55
       75 |    4 |    32
      100 |    5 |     8
      |}]
  ;;

  let of_alist_reduce = Map.of_alist_reduce

  let%expect_test _ =
    test (function
      | Of_alist_reduce _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    11
      ----+------+------
        0 |    1 |    72
       50 |    3 |    41
       90 |    4 |    29
       95 |    5 |     7
      100 |    6 |     1
      |}]
  ;;

  let of_increasing_sequence = Map.of_increasing_sequence

  let%expect_test _ =
    test (function
      | Of_increasing_sequence _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |     9
      ----+------+------
        0 |    1 |    77
       50 |    9 |    39
       75 |   17 |    20
       90 |   20 |    12
       95 |   24 |     5
      100 |   28 |     1
      |}]
  ;;

  let of_sequence = Map.of_sequence

  let%expect_test _ =
    test (function
      | Of_sequence _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |     9
      ----+------+------
        0 |    1 |    79
       50 |    9 |    44
       75 |   15 |    20
       90 |   22 |     9
       95 |   23 |     7
      100 |   29 |     1
      |}]
  ;;

  let of_sequence_or_error = Map.of_sequence_or_error

  let%expect_test _ =
    test (function
      | Of_sequence_or_error _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    15
      ----+------+------
        0 |    1 |    77
       50 |    9 |    41
       75 |   18 |    22
       90 |   24 |     9
       95 |   26 |     6
      100 |   31 |     1
      |}]
  ;;

  let of_sequence_exn = Map.of_sequence_exn

  let%expect_test _ =
    test (function
      | Of_sequence_exn _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    11
      ----+------+------
        0 |    1 |    71
       50 |   14 |    36
       75 |   21 |    19
       90 |   25 |     8
       95 |   27 |     4
      100 |   28 |     2
      |}]
  ;;

  let of_sequence_fold = Map.of_sequence_fold

  let%expect_test _ =
    test (function
      | Of_sequence_fold _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    13
      ----+------+------
        0 |    1 |    71
       50 |    3 |    48
       90 |    4 |    32
      100 |    5 |     7
      |}]
  ;;

  let of_sequence_reduce = Map.of_sequence_reduce

  let%expect_test _ =
    test (function
      | Of_sequence_reduce _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    11
      ----+------+------
        0 |    1 |    68
       75 |    3 |    45
       95 |    4 |    14
      100 |    5 |     3
      |}]
  ;;

  let of_list_with_key = Map.of_list_with_key

  let%expect_test _ =
    test (function
      | Of_list_with_key _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    10
      ----+------+------
        0 |    1 |    68
       50 |   12 |    35
       75 |   19 |    17
       95 |   25 |     7
      100 |   27 |     1
      |}]
  ;;

  let of_list_with_key_or_error = Map.of_list_with_key_or_error

  let%expect_test _ =
    test (function
      | Of_list_with_key_or_error _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    14
      ----+------+------
        0 |    1 |    86
       50 |   11 |    44
       75 |   18 |    23
       90 |   25 |    10
       95 |   27 |     6
      100 |   30 |     1
      |}]
  ;;

  let of_list_with_key_exn = Map.of_list_with_key_exn

  let%expect_test _ =
    test (function
      | Of_list_with_key_exn _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    10
      ----+------+------
        0 |    1 |    68
       50 |   10 |    34
       75 |   17 |    18
       90 |   24 |     7
       95 |   25 |     6
      100 |   28 |     2
      |}]
  ;;

  let of_list_with_key_fold = Map.of_list_with_key_fold

  let%expect_test _ =
    test (function
      | Of_list_with_key_fold _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    13
      ----+------+------
        0 |    1 |    68
       50 |    2 |    41
      100 |    3 |    17
      |}]
  ;;

  let of_list_with_key_reduce = Map.of_list_with_key_reduce

  let%expect_test _ =
    test (function
      | Of_list_with_key_reduce _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    12
      ----+------+------
        0 |    1 |    69
       50 |    2 |    48
       95 |    3 |    18
      100 |    4 |     1
      |}]
  ;;

  let of_iteri = Map.of_iteri

  let%expect_test _ =
    test (function
      | Of_iteri _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    11
      ----+------+------
        0 |    1 |    78
       50 |   11 |    39
       75 |   20 |    20
       90 |   27 |     8
      100 |   30 |     5
      |}]
  ;;

  let of_iteri_exn = Map.of_iteri_exn

  let%expect_test _ =
    test (function
      | Of_iteri_exn _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    17
      ----+------+------
        0 |    1 |    67
       50 |   10 |    36
       75 |   18 |    17
       90 |   26 |     7
       95 |   29 |     5
      100 |   31 |     1
      |}]
  ;;

  let add = Map.add

  let%expect_test _ =
    test (function
      | Add _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        0 |    1 |   265
       50 |    3 |   143
       75 |    7 |    69
       90 |   17 |    27
       95 |   21 |    16
       99 |   27 |     4
      100 |   38 |     1
      |}]
  ;;

  let add_exn = Map.add_exn

  let%expect_test _ =
    test (function
      | Add_exn _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        0 |    1 |   271
       50 |    2 |   183
       75 |    4 |    85
       90 |   13 |    33
       95 |   21 |    14
       99 |   28 |     4
      100 |   51 |     1
      |}]
  ;;

  let set = Map.set

  let%expect_test _ =
    test (function
      | Set _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        0 |    1 |   263
       50 |    2 |   151
       75 |    5 |    66
       90 |   12 |    29
       95 |   20 |    14
       99 |   26 |     3
      100 |   29 |     1
      |}]
  ;;

  let change = Map.change

  let%expect_test _ =
    test (function
      | Change _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    62
      ----+------+------
        0 |    1 |   202
       50 |    3 |   101
       75 |    8 |    53
       90 |   16 |    22
       95 |   21 |    11
       99 |   28 |     3
      100 |   41 |     1
      |}]
  ;;

  let update = Map.update

  let%expect_test _ =
    test (function
      | Update _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        0 |    1 |   277
       50 |    2 |   178
       75 |    7 |    71
       90 |   18 |    29
       95 |   23 |    18
       99 |   30 |     3
      100 |   48 |     1
      |}]
  ;;

  let remove = Map.remove

  let%expect_test _ =
    test (function
      | Remove _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   100
      ----+------+------
        0 |    1 |   190
       50 |    3 |   113
       75 |   11 |    48
       90 |   20 |    20
       95 |   24 |    10
       99 |   39 |     2
      100 |   42 |     1
      |}]
  ;;

  let map = Map.map

  let%expect_test _ =
    test (function
      | Map _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   110
      ----+------+------
        0 |    1 |   194
       50 |    3 |   106
       75 |    8 |    53
       90 |   18 |    22
       95 |   23 |    10
       99 |   43 |     2
      100 |   49 |     1
      |}]
  ;;

  let mapi = Map.mapi

  let%expect_test _ =
    test (function
      | Mapi _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   100
      ----+------+------
        0 |    1 |   169
       50 |    2 |   114
       75 |    8 |    45
       90 |   18 |    18
       95 |   21 |     9
       99 |   29 |     2
      100 |   30 |     1
      |}]
  ;;

  let filter_keys = Map.filter_keys

  let%expect_test _ =
    test (function
      | Filter_keys _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   168
      ----+------+------
        0 |    1 |   133
       50 |    2 |    76
       75 |    7 |    34
       90 |   16 |    18
       95 |   22 |     7
       99 |   29 |     2
      100 |   38 |     1
      |}]
  ;;

  let filter = Map.filter

  let%expect_test _ =
    test (function
      | Filter _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   163
      ----+------+------
        0 |    1 |   140
       50 |    2 |    97
       75 |    6 |    37
       90 |   12 |    14
       95 |   20 |     7
      100 |   24 |     2
      |}]
  ;;

  let filteri = Map.filteri

  let%expect_test _ =
    test (function
      | Filteri _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   157
      ----+------+------
        0 |    1 |   134
       50 |    2 |    81
       75 |    4 |    39
       90 |   10 |    16
       95 |   14 |     8
       99 |   23 |     2
      100 |   24 |     1
      |}]
  ;;

  let filter_map = Map.filter_map

  let%expect_test _ =
    test (function
      | Filter_map _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   154
      ----+------+------
        0 |    1 |   109
       50 |    3 |    64
       75 |    6 |    30
       90 |   16 |    11
       95 |   20 |     8
       99 |   24 |     2
      100 |   26 |     1
      |}]
  ;;

  let filter_mapi = Map.filter_mapi

  let%expect_test _ =
    test (function
      | Filter_mapi _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   184
      ----+------+------
        0 |    1 |   119
       50 |    2 |    78
       75 |    5 |    32
       90 |   14 |    12
       95 |   19 |     6
       99 |   24 |     3
      100 |   25 |     1
      |}]
  ;;

  let partition_mapi = Map.partition_mapi

  let%expect_test _ =
    test (function
      | Partition_mapi _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   145
      ----+------+------
        0 |    1 |   105
       50 |    3 |    53
       75 |    7 |    27
       90 |   15 |    11
       95 |   22 |     6
       99 |   26 |     3
      100 |   28 |     1
      |}]
  ;;

  let partition_map = Map.partition_map

  let%expect_test _ =
    test (function
      | Partition_map _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   142
      ----+------+------
        0 |    1 |   128
       50 |    3 |    64
       75 |    8 |    32
       90 |   15 |    13
       95 |   18 |     8
      100 |   24 |     2
      |}]
  ;;

  let partitioni_tf = Map.partitioni_tf

  let%expect_test _ =
    test (function
      | Partitioni_tf _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   158
      ----+------+------
        0 |    1 |   132
       50 |    2 |    77
       75 |    5 |    33
       90 |   10 |    15
       95 |   14 |     7
      100 |   27 |     2
      |}]
  ;;

  let partition_tf = Map.partition_tf

  let%expect_test _ =
    test (function
      | Partition_tf _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   142
      ----+------+------
        0 |    1 |   121
       50 |    2 |    75
       75 |    6 |    33
       90 |   12 |    14
       95 |   21 |     7
       99 |   29 |     2
      100 |   41 |     1
      |}]
  ;;

  let merge = Map.merge

  let%expect_test _ =
    test (function
      | Merge _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    83
      ----+------+------
        0 |    1 |   179
       50 |    2 |   120
       75 |    5 |    55
       90 |    9 |    20
       95 |   11 |    10
       99 |   24 |     2
      100 |   53 |     1
      |}]
  ;;

  let merge_disjoint_exn = Map.merge_disjoint_exn

  let%expect_test _ =
    test (function
      | Merge_disjoint_exn _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    33
      ----+------+------
        0 |    1 |   249
       50 |    5 |   126
       75 |   13 |    67
       90 |   23 |    31
       95 |   29 |    13
       99 |   42 |     3
      100 |   69 |     1
      |}]
  ;;

  let merge_skewed = Map.merge_skewed

  let%expect_test _ =
    test (function
      | Merge_skewed _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    37
      ----+------+------
        0 |    1 |   237
       50 |    5 |   122
       75 |   13 |    65
       90 |   22 |    25
       95 |   27 |    15
       99 |   34 |     3
      100 |   40 |     1
      |}]
  ;;

  let split = Map.split

  let%expect_test _ =
    test (function
      | Split _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   176
      ----+------+------
        0 |    1 |   115
       50 |    2 |    72
       75 |    6 |    33
       90 |   10 |    15
       95 |   14 |     8
       99 |   18 |     2
      100 |   27 |     1
      |}]
  ;;

  let split_le_gt = Map.split_le_gt

  let%expect_test _ =
    test (function
      | Split_le_gt _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   164
      ----+------+------
        0 |    1 |   115
       50 |    2 |    67
       75 |    5 |    30
       90 |   12 |    13
       95 |   19 |     7
       99 |   23 |     4
      100 |   25 |     1
      |}]
  ;;

  let split_lt_ge = Map.split_lt_ge

  let%expect_test _ =
    test (function
      | Split_lt_ge _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   132
      ----+------+------
        0 |    1 |   123
       50 |    2 |    69
       75 |    5 |    32
       90 |   13 |    13
       95 |   15 |     7
       99 |   27 |     2
      100 |   63 |     1
      |}]
  ;;

  let append = Map.append

  let%expect_test _ =
    test (function
      | Append _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    23
      ----+------+------
        0 |    1 |   260
       50 |    5 |   140
       75 |   12 |    66
       90 |   22 |    26
       95 |   28 |    13
       99 |   41 |     3
      100 |   43 |     2
      |}]
  ;;

  let subrange = Map.subrange

  let%expect_test _ =
    test (function
      | Subrange _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   191
      ----+------+------
        0 |    1 |    84
       50 |    2 |    51
       75 |    6 |    23
       90 |   16 |     9
       95 |   19 |     5
      100 |   25 |     1
      |}]
  ;;

  module Make_applicative_traversals (A : Applicative.Lazy_applicative) = struct
    module T = Map.Make_applicative_traversals (A)

    let mapi = T.mapi
    let filter_mapi = T.filter_mapi
  end

  let%expect_test _ =
    test (function
      | Make_applicative_traversals__mapi _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    94
      ----+------+------
        0 |    1 |   179
       50 |    3 |    95
       75 |    6 |    53
       90 |   16 |    20
       95 |   21 |     9
       99 |   25 |     2
      100 |   27 |     1
      |}]
  ;;

  let%expect_test _ =
    test (function
      | Make_applicative_traversals__filter_mapi _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   159
      ----+------+------
        0 |    1 |   138
       50 |    3 |    70
       75 |    7 |    35
       90 |   12 |    17
       95 |   16 |     8
       99 |   22 |     2
      100 |   27 |     1
      |}]
  ;;
end
