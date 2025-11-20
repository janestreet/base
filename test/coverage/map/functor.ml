(** Comprehensive testing of [Base.Map].

    This file tests all exports of [Base.Map]. Every time a new export is added, we have
    to add a new definition somewhere here. Every time we add a definition, we should add
    a test unless the definition is untestable (e.g., a module type) or trivial (e.g., a
    module containing only ppx-derived definitions). We should document categories of
    untested definitions, mark them as untested, and keep them separate from definitions
    that need tests. *)

open! Base
open Base_quickcheck
open Expect_test_helpers_base
open Base_test_coverage_helpers
open Overrides
include Functor_intf.Definitions

module Generate
    (Instance : Instance)
    (Impl : Impl with module Types := Instance.Types)
    (Data : Data.S) : Generate with module Instance := Instance and module Data := Data =
struct
  include Constructor.Make (Instance) (Impl) (Data)

  module Value = struct
    type t = Data.t Instance.t [@@deriving compare, equal, sexp_of]
  end
end

module Test_helpers
    (Instance : Instance)
    (Impl : Impl with module Types := Instance.Types) =
struct
  open Instance
  module Generate = Generate (Instance) (Impl)

  module Alist = struct
    type t = (Key.t * int) list [@@deriving compare, equal, quickcheck, sexp_of]

    let sample = Memo.memoize [%generator: t]
  end

  module Alist_merge = struct
    type t = (Key.t * (int, int) Map.Merge_element.t) list [@@deriving equal, sexp_of]
  end

  module Alist_multi = struct
    type t = (Key.t * int list) list [@@deriving equal, quickcheck, sexp_of]
  end

  module Diff = struct
    type t = (Key.t, int) Map.Symmetric_diff_element.t list [@@deriving equal, sexp_of]
  end

  module Inst = struct
    include Generate (Data.Int)

    let sample = Memo.memoize [%generator: t]
  end

  module Inst_and_key = struct
    type t = Inst.t * Key.t [@@deriving quickcheck, sexp_of]

    let sample = Memo.memoize [%generator: t]
  end

  module Inst_and_key_and_data = struct
    type t = Inst.t * Key.t * int [@@deriving quickcheck, sexp_of]

    let sample = Memo.memoize [%generator: t]
  end

  module Inst_and_key_and_maybe_data = struct
    type t = Inst.t * Key.t * int option [@@deriving quickcheck, sexp_of]

    let sample = Memo.memoize [%generator: t]
  end

  module Inst_inst = struct
    include Generate (struct
        type t = Inst.Value.t [@@deriving compare, equal, sexp_of]

        let quickcheck_generator = Generator.map Inst.quickcheck_generator ~f:Inst.value
        let quickcheck_observer = Observer.opaque
        let quickcheck_shrinker = Shrinker.atomic
        let to_int = Impl.length

        let of_int n =
          List.init n ~f:(fun i -> Key.of_int i, i) |> create Impl.of_alist_exn
        ;;

        let combine_non_commutative a b =
          (access Impl.merge_skewed) a b ~combine:(fun ~key:_ a b ->
            Data.Int.combine_non_commutative a b)
        ;;
      end)

    let sample = Memo.memoize [%generator: t]
  end

  module Inst_pair = struct
    include Generate (struct
        type t = int * int [@@deriving compare, equal, quickcheck, sexp_of]

        let to_int (x, y) = Int.max x y
        let of_int n = n, n

        let combine_non_commutative (a, b) (c, d) =
          Data.Int.combine_non_commutative a c, Data.Int.combine_non_commutative b d
        ;;
      end)

    let sample = Memo.memoize [%generator: t]
  end

  module Inst_and_inst = struct
    include Data.Pair (Inst)

    let sample = Memo.memoize [%generator: t]
  end

  module Inst_and_inst_and_key = struct
    module Inst2 = Inst_and_inst

    type t = Inst2.t * Key.t [@@deriving quickcheck, sexp_of]

    let sample = Memo.memoize [%generator: t]
  end

  module Inst_multi = Generate (struct
      type t = int list [@@deriving compare, equal, quickcheck, sexp_of]

      let to_int = List.length
      let of_int n = List.init n ~f:Fn.id
      let combine_non_commutative = ( @ )
    end)

  module Inst_multi_and_key = struct
    type t = Inst_multi.t * Key.t [@@deriving quickcheck, sexp_of]

    let sample = Memo.memoize [%generator: t]
  end

  module Inst_multi_and_key_and_data = struct
    type t = Inst_multi.t * Key.t * int [@@deriving quickcheck, sexp_of]

    let sample = Memo.memoize [%generator: t]
  end

  module Key_and_data = struct
    type t = Key.t * int [@@deriving equal, sexp_of]
  end

  module Key_and_data_inst = struct
    type t = (Key.t * int) Instance.t [@@deriving equal, sexp_of]
  end

  module Key_and_data_inst_multi = struct
    type t = (Key.t * int) list Instance.t [@@deriving equal, sexp_of]
  end

  module Inst_and_bounds = struct
    type t = Inst.t * Key.t Maybe_bound.t * Key.t Maybe_bound.t
    [@@deriving quickcheck, sexp_of]

    let sample = Memo.memoize [%generator: t]
  end

  let ok_or_duplicate_key = function
    | `Ok x -> Ok x
    | `Duplicate_key key -> Or_error.error_s [%sexp (key : Key.t)]
  ;;

  module Physical_equality (T : sig
      type t [@@deriving sexp_of]
    end) =
  struct
    type t = T.t [@@deriving sexp_of]

    let equal a b = phys_equal a b
  end
end

module Test_creators
    (Instance : Instance)
    (Impl : Impl with module Types := Instance.Types) :
  Creators with module Types := Instance.Types = struct
  open Instance
  open Impl
  open Test_helpers (Instance) (Impl)

  let () = print_endline "Functor.Test_creators: running tests."

  (** creators *)

  and empty = empty

  and () = require_equal (module Sexp) [%sexp (create empty : int t)] [%sexp []]
  and singleton = singleton

  and () =
    require_equal
      (module Sexp)
      [%sexp (create singleton (Key.of_int 1) 2 : int t)]
      [%sexp [ [ 1; 2 ] ]]

  and of_alist = of_alist
  and of_alist_or_error = of_alist_or_error
  and of_alist_exn = of_alist_exn

  and () =
    quickcheck_m (module Alist) ~f:(fun alist ->
      let t_or_error = create of_alist_or_error alist in
      let t_exn = Or_error.try_with (fun () -> create of_alist_exn alist) in
      let t_or_duplicate =
        match create of_alist alist with
        | `Ok t -> Ok t
        | `Duplicate_key key -> Or_error.error_s [%sexp (key : Key.t)]
      in
      require_equal
        (module Data.Or_error (Alist))
        (Or_error.map t_or_error ~f:to_alist)
        (let compare a b = Comparable.lift Key.compare ~f:fst a b in
         if List.contains_dup alist ~compare
         then Or_error.error_string "duplicate"
         else Ok (List.sort alist ~compare));
      require_equal (module Data.Or_error (Inst.Value)) t_exn t_or_error;
      require_equal (module Data.Or_error (Inst.Value)) t_or_duplicate t_or_error)

  and of_alist_multi = of_alist_multi
  and of_alist_fold = of_alist_fold
  and of_alist_reduce = of_alist_reduce

  and () =
    quickcheck_m (module Alist) ~f:(fun alist ->
      let t_multi = create of_alist_multi alist in
      let t_fold =
        create of_alist_fold alist ~init:[] ~f:(fun xs x -> x :: xs) |> map ~f:List.rev
      in
      let t_reduce =
        create of_alist_reduce (List.Assoc.map alist ~f:List.return) ~f:(fun x y -> x @ y)
      in
      require_equal
        (module Alist_multi)
        (to_alist t_multi)
        (List.Assoc.sort_and_group alist ~compare:Key.compare);
      require_equal (module Inst_multi.Value) t_fold t_multi;
      require_equal (module Inst_multi.Value) t_reduce t_multi)

  and of_sequence = of_sequence
  and of_sequence_or_error = of_sequence_or_error
  and of_sequence_exn = of_sequence_exn

  and () =
    quickcheck_m (module Alist) ~f:(fun alist ->
      let seq = Sequence.of_list alist in
      let t_or_error = create of_sequence_or_error seq in
      let t_exn = Or_error.try_with (fun () -> create of_sequence_exn seq) in
      let t_or_duplicate =
        match create of_sequence seq with
        | `Ok t -> Ok t
        | `Duplicate_key key -> Or_error.error_s [%sexp (key : Key.t)]
      in
      let expect = create of_alist_or_error alist in
      require_equal (module Data.Or_error (Inst.Value)) t_or_error expect;
      require_equal (module Data.Or_error (Inst.Value)) t_exn expect;
      require_equal (module Data.Or_error (Inst.Value)) t_or_duplicate expect)

  and of_sequence_multi = of_sequence_multi
  and of_sequence_fold = of_sequence_fold
  and of_sequence_reduce = of_sequence_reduce

  and () =
    quickcheck_m (module Alist) ~f:(fun alist ->
      let seq = Sequence.of_list alist in
      let t_multi = create of_sequence_multi seq in
      let t_fold =
        create of_sequence_fold seq ~init:[] ~f:(fun xs x -> x :: xs) |> map ~f:List.rev
      in
      let t_reduce =
        create
          of_sequence_reduce
          (alist |> List.Assoc.map ~f:List.return |> Sequence.of_list)
          ~f:(fun x y -> x @ y)
      in
      let expect = create of_alist_multi alist in
      require_equal (module Inst_multi.Value) t_multi expect;
      require_equal (module Inst_multi.Value) t_fold expect;
      require_equal (module Inst_multi.Value) t_reduce expect)

  and of_list_with_key = of_list_with_key
  and of_list_with_key_or_error = of_list_with_key_or_error
  and of_list_with_key_exn = of_list_with_key_exn
  and of_list_with_key_multi = of_list_with_key_multi
  and of_list_with_key_fold = of_list_with_key_fold
  and of_list_with_key_reduce = of_list_with_key_reduce

  and () =
    quickcheck_m (module Alist) ~f:(fun list ->
      let alist = List.map list ~f:(fun (key, data) -> key, (key, data)) in
      require_equal
        (module Data.Or_error (Key_and_data_inst))
        (create of_list_with_key list ~get_key:fst |> ok_or_duplicate_key)
        (create of_alist alist |> ok_or_duplicate_key);
      require_equal
        (module Data.Or_error (Key_and_data_inst))
        (create of_list_with_key_or_error list ~get_key:fst)
        (create of_alist_or_error alist);
      require_equal
        (module Data.Or_error (Key_and_data_inst))
        (Or_error.try_with (fun () -> create of_list_with_key_exn list ~get_key:fst))
        (Or_error.try_with (fun () -> create of_alist_exn alist));
      require_equal
        (module Key_and_data_inst_multi)
        (create of_list_with_key_multi list ~get_key:fst)
        (create of_alist_multi alist);
      require_equal
        (module Key_and_data_inst_multi)
        (create of_list_with_key_fold list ~get_key:fst ~init:[] ~f:(fun acc x ->
           x :: acc)
         |> map ~f:List.rev)
        (create of_alist_multi alist);
      require_equal
        (module Key_and_data_inst_multi)
        (create
           of_list_with_key_reduce
           (List.map list ~f:List.return)
           ~get_key:(fun x -> x |> List.hd_exn |> fst)
           ~f:(fun x y -> x @ y))
        (create of_alist_multi alist))

  and of_increasing_sequence = of_increasing_sequence

  and () =
    quickcheck_m (module Alist) ~f:(fun alist ->
      let seq = Sequence.of_list alist in
      let actual = create of_increasing_sequence seq in
      let expect =
        if List.is_sorted alist ~compare:(fun a b ->
             Comparable.lift Key.compare ~f:fst a b)
        then create of_alist_or_error alist
        else Or_error.error_string "decreasing keys"
      in
      require_equal (module Data.Or_error (Inst.Value)) actual expect)

  and of_sorted_array = of_sorted_array

  and () =
    quickcheck_m (module Alist) ~f:(fun alist ->
      let actual = create of_sorted_array (Array.of_list alist) in
      let expect =
        let compare a b = Comparable.lift Key.compare ~f:fst a b in
        if List.is_sorted_strictly ~compare alist
           || List.is_sorted_strictly ~compare (List.rev alist)
        then create of_alist_or_error alist
        else Or_error.error_string "unsorted"
      in
      require_equal (module Data.Or_error (Inst.Value)) actual expect)

  and of_sorted_array_unchecked = of_sorted_array_unchecked

  and () =
    quickcheck_m (module Alist) ~f:(fun alist ->
      let alist =
        List.dedup_and_sort alist ~compare:(fun a b ->
          Comparable.lift Key.compare ~f:fst a b)
      in
      let actual_fwd = create of_sorted_array_unchecked (Array.of_list alist) in
      let actual_rev = create of_sorted_array_unchecked (Array.of_list_rev alist) in
      let expect = create of_alist_exn alist in
      require_equal (module Inst.Value) actual_fwd expect;
      require_equal (module Inst.Value) actual_rev expect)

  and of_increasing_iterator_unchecked = of_increasing_iterator_unchecked

  and () =
    quickcheck_m (module Alist) ~f:(fun alist ->
      let alist =
        List.dedup_and_sort alist ~compare:(fun a b ->
          Comparable.lift Key.compare ~f:fst a b)
      in
      let actual =
        let array = Array.of_list alist in
        create
          of_increasing_iterator_unchecked
          ~len:(Array.length array)
          ~f:(Array.get array)
      in
      let expect = create of_alist_exn alist in
      require_equal (module Inst.Value) actual expect)

  and of_iteri = of_iteri
  and of_iteri_exn = of_iteri_exn

  and () =
    quickcheck_m (module Alist) ~f:(fun alist ->
      let iteri ~f = List.iter alist ~f:(fun (key, data) -> f ~key ~data) [@nontail] in
      let actual_or_duplicate =
        match create of_iteri ~iteri with
        | `Ok t -> Ok t
        | `Duplicate_key key -> Or_error.error_s [%sexp (key : Key.t)]
      in
      let actual_exn = Or_error.try_with (fun () -> create of_iteri_exn ~iteri) in
      let expect = create of_alist_or_error alist in
      require_equal (module Data.Or_error (Inst.Value)) actual_or_duplicate expect;
      require_equal (module Data.Or_error (Inst.Value)) actual_exn expect)

  and map_keys = map_keys
  and map_keys_exn = map_keys_exn

  and () =
    quickcheck_m (module Inst_and_key) ~f:(fun (t, k) ->
      let t = Inst.value t in
      let f key = Comparable.min Key.compare k key in
      let actual_or_duplicate =
        match create map_keys t ~f with
        | `Ok t -> Ok t
        | `Duplicate_key key -> Or_error.error_s [%sexp (key : Key.t)]
      in
      let actual_exn = Or_error.try_with (fun () -> create map_keys_exn t ~f) in
      let expect =
        to_alist t
        |> List.map ~f:(fun (key, data) -> f key, data)
        |> create of_alist_or_error
      in
      require_equal (module Data.Or_error (Inst.Value)) actual_or_duplicate expect;
      require_equal (module Data.Or_error (Inst.Value)) actual_exn expect)

  and transpose_keys = transpose_keys

  and () =
    quickcheck_m (module Inst_inst) ~f:(fun t ->
      let t = Inst_inst.value t in
      let transpose_keys = create (access transpose_keys) in
      let transposed = transpose_keys t in
      require (access invariants transposed);
      let round_trip = transpose_keys transposed in
      require_equal (module Inst_inst.Value) (filter t ~f:(Fn.non is_empty)) round_trip)

  and of_tree = of_tree

  and () =
    quickcheck_m (module Inst) ~f:(fun t ->
      let t = Inst.value t in
      let tree = to_tree t in
      let round_trip = create of_tree tree in
      require_equal (module Inst.Value) t round_trip)
  ;;
end

module Test_accessors
    (Instance : Instance)
    (Impl : Impl with module Types := Instance.Types) :
  Accessors with module Types := Instance.Types = struct
  open Instance
  open Impl
  open Test_helpers (Instance) (Impl)

  let () = print_endline "Functor.Test_accessors: running tests."

  (** accessors *)

  and invariants = invariants

  and () =
    quickcheck_m (module Inst) ~f:(fun t ->
      let t = Inst.value t in
      require (access invariants t))

  and is_empty = is_empty
  and length = length

  and () =
    quickcheck_m (module Inst) ~f:(fun t ->
      let t = Inst.value t in
      let len = length t in
      require_equal (module Bool) (is_empty t) (len = 0);
      require_equal (module Int) len (List.length (to_alist t)))

  and mem = mem
  and find = find
  and find_exn = find_exn

  and () =
    quickcheck_m (module Inst_and_key) ~f:(fun (t, key) ->
      let t = Inst.value t in
      let expect = List.Assoc.find (to_alist t) key ~equal:Key.equal in
      require_equal (module Bool) (access mem t key) (Option.is_some expect);
      require_equal (module Data.Option (Int)) (access find t key) expect;
      require_equal
        (module Data.Option (Int))
        (Option.try_with (fun () -> access find_exn t key))
        expect)

  and find_multi = find_multi

  and () =
    quickcheck_m (module Inst_multi_and_key) ~f:(fun (t, key) ->
      let t = Inst_multi.value t in
      require_equal
        (module Data.List (Int))
        (access find_multi t key)
        (access find t key |> Option.value ~default:[]))

  and iter_keys = iter_keys
  and iter = iter
  and iteri = iteri

  and () =
    quickcheck_m (module Inst) ~f:(fun t ->
      let t = Inst.value t in
      let actuali =
        let q = Queue.create () in
        iteri t ~f:(fun ~key ~data -> Queue.enqueue q (key, data));
        Queue.to_list q
      in
      let actual_keys =
        let q = Queue.create () in
        iter_keys t ~f:(Queue.enqueue q);
        Queue.to_list q
      in
      let actual =
        let q = Queue.create () in
        iter t ~f:(Queue.enqueue q);
        Queue.to_list q
      in
      require_equal (module Alist) actuali (to_alist t);
      require_equal (module Data.List (Key)) actual_keys (keys t);
      require_equal (module Data.List (Int)) actual (data t))

  and fold = fold
  and fold_right = fold_right

  and () =
    quickcheck_m (module Inst) ~f:(fun t ->
      let t = Inst.value t in
      require_equal
        (module Alist)
        (fold t ~init:[] ~f:(fun ~key ~data list -> (key, data) :: list))
        (List.rev (to_alist t));
      require_equal
        (module Alist)
        (fold_right t ~init:[] ~f:(fun ~key ~data list -> (key, data) :: list))
        (to_alist t))

  and fold_until = fold_until
  and iteri_until = iteri_until

  and () =
    quickcheck_m (module Inst_and_key) ~f:(fun (t, threshold) ->
      let t = Inst.value t in
      require_equal
        (module struct
          type t = int list * Base.Map.Finished_or_unfinished.t
          [@@deriving equal, sexp_of]
        end)
        (let q = Queue.create () in
         let status =
           iteri_until t ~f:(fun ~key ~data ->
             if Key.( >= ) key threshold
             then Stop
             else (
               Queue.enqueue q data;
               Continue))
         in
         Queue.to_list q, status)
        (let list =
           to_alist t
           |> List.take_while ~f:(fun (key, _) -> Key.( < ) key threshold)
           |> List.map ~f:snd
         in
         list, if List.length list = length t then Finished else Unfinished))

  and equal = equal
  and equal__local = equal__local
  and compare_direct = compare_direct
  and compare_direct__local = compare_direct__local

  and () =
    quickcheck_m (module Inst_and_inst) ~f:(fun (a, b) ->
      let a = Inst.value a in
      let b = Inst.value b in
      require_equal
        (module Ordering)
        (Ordering.of_int (access compare_direct Int.compare a b))
        (Ordering.of_int (Alist.compare (to_alist a) (to_alist b)));
      require_equal
        (module Bool)
        (access compare_direct Int.compare a b = 0)
        (access equal Int.equal a b))

  and keys = keys
  and data = data
  and to_alist = to_alist
  and to_sequence = to_sequence

  and () =
    quickcheck_m (module Inst) ~f:(fun t ->
      let t = Inst.value t in
      let alist = to_alist t in
      require_equal (module Inst.Value) (create of_alist_exn alist) t;
      require_equal (module Data.List (Key)) (keys t) (List.map alist ~f:fst);
      require_equal (module Data.List (Int)) (data t) (List.map alist ~f:snd);
      require_equal (module Alist) (Sequence.to_list ((access to_sequence) t)) alist)

  and () =
    quickcheck_m
      (module struct
        type t = Inst.t * [ `Decreasing | `Increasing ] [@@deriving quickcheck, sexp_of]

        let sample = Memo.memoize [%generator: t]
      end)
      ~f:(fun (t, key_order) ->
        let t = Inst.value t in
        let alist = to_alist t ~key_order in
        require_equal
          (module Data.List (Key_and_data))
          alist
          (match key_order with
           | `Increasing -> to_alist t
           | `Decreasing -> List.rev (to_alist t));
        require_equal
          (module Data.List (Key_and_data))
          alist
          (Sequence.to_list
             ((access to_sequence)
                t
                ~order:
                  (match key_order with
                   | `Decreasing -> `Decreasing_key
                   | `Increasing -> `Increasing_key))))

  and () =
    quickcheck_m
      (module struct
        type t = Inst.t * [ `Decreasing_key | `Increasing_key ] * Key.t * Key.t
        [@@deriving quickcheck, sexp_of]

        let sample = Memo.memoize [%generator: t]
      end)
      ~f:(fun (t, order, keys_greater_or_equal_to, keys_less_or_equal_to) ->
        let t = Inst.value t in
        let alist =
          Sequence.to_list
            ((access to_sequence)
               t
               ~order
               ~keys_greater_or_equal_to
               ~keys_less_or_equal_to)
        in
        require_equal
          (module Data.List (Key_and_data))
          alist
          (List.filter
             (match order with
              | `Decreasing_key -> List.rev (to_alist t)
              | `Increasing_key -> to_alist t)
             ~f:(fun (key, _) ->
               Key.( <= ) keys_greater_or_equal_to key
               && Key.( <= ) key keys_less_or_equal_to)))

  and iter2 = iter2
  and fold2 = fold2

  and () =
    quickcheck_m (module Inst_and_inst_and_key) ~f:(fun ((a, b), k) ->
      let a = Inst.value a in
      let b = Inst.value b in
      let iter2_alist =
        let q = Queue.create () in
        access iter2 a b ~f:(fun ~key ~data:elt ->
          if Key.( > ) key k then Queue.enqueue q (key, elt));
        Queue.to_list q
      in
      let fold2_alist =
        access fold2 a b ~init:[] ~f:(fun ~key ~data:elt acc ->
          if Key.( > ) key k then (key, elt) :: acc else acc)
        |> List.rev
      in
      let expect =
        [ map a ~f:Either.first; map b ~f:Either.second ]
        |> List.concat_map ~f:to_alist
        |> List.Assoc.sort_and_group ~compare:Key.compare
        |> List.filter_map ~f:(fun (key, list) ->
          let elt =
            match (list : _ Either.t list) with
            | [ First x ] -> `Left x
            | [ Second y ] -> `Right y
            | [ First x; Second y ] -> `Both (x, y)
            | _ -> assert false
          in
          Option.some_if (Key.( > ) key k) (key, elt))
      in
      require_equal (module Alist_merge) iter2_alist expect;
      require_equal (module Alist_merge) fold2_alist expect)

  and symmetric_diff = symmetric_diff
  and fold_symmetric_diff = fold_symmetric_diff

  and () =
    quickcheck_m (module Inst_and_inst) ~f:(fun (a, b) ->
      let a = Inst.value a in
      let b = Inst.value b in
      let diff_alist =
        access symmetric_diff a b ~data_equal:Int.equal |> Sequence.to_list
      in
      let fold_alist =
        access
          fold_symmetric_diff
          a
          b
          ~data_equal:(fun x y -> Int.equal x y)
          ~init:[]
          ~f:(fun acc pair -> pair :: acc)
        |> List.rev
      in
      let expect =
        access merge a b ~f:(fun ~key:_ elt ->
          match elt with
          | `Left x -> Some (`Left x)
          | `Right y -> Some (`Right y)
          | `Both (x, y) -> if x = y then None else Some (`Unequal (x, y)))
        |> to_alist
      in
      require_equal (module Diff) diff_alist expect;
      require_equal (module Diff) fold_alist expect)

  and min_elt = min_elt
  and max_elt = max_elt
  and min_elt_exn = min_elt_exn
  and max_elt_exn = max_elt_exn

  and () =
    quickcheck_m (module Inst) ~f:(fun t ->
      let t = Inst.value t in
      require_equal (module Data.Option (Key_and_data)) (min_elt t) (List.hd (to_alist t));
      require_equal
        (module Data.Option (Key_and_data))
        (max_elt t)
        (List.last (to_alist t));
      require_equal
        (module Data.Option (Key_and_data))
        (Option.try_with (fun () -> min_elt_exn t))
        (List.hd (to_alist t));
      require_equal
        (module Data.Option (Key_and_data))
        (Option.try_with (fun () -> max_elt_exn t))
        (List.last (to_alist t)))

  and for_all = for_all
  and for_alli = for_alli
  and exists = exists
  and existsi = existsi
  and count = count
  and counti = counti

  and () =
    quickcheck_m (module Inst_and_key_and_data) ~f:(fun (t, k, d) ->
      let t = Inst.value t in
      let f data = data <= d in
      let fi ~key ~data = Key.( <= ) key k && data <= d in
      let fp (key, data) = fi ~key ~data in
      let data = data t in
      let alist = to_alist t in
      require_equal (module Bool) (for_all t ~f) (List.for_all data ~f);
      require_equal (module Bool) (for_alli t ~f:fi) (List.for_all alist ~f:fp);
      require_equal (module Bool) (exists t ~f) (List.exists data ~f);
      require_equal (module Bool) (existsi t ~f:fi) (List.exists alist ~f:fp);
      require_equal (module Int) (count t ~f) (List.count data ~f);
      require_equal (module Int) (counti t ~f:fi) (List.count alist ~f:fp))

  and sum = sum
  and sumi = sumi

  and () =
    quickcheck_m (module Inst) ~f:(fun t ->
      let t = Inst.value t in
      let f data = data * 2 in
      let fi ~key ~data = (Key.to_int key * 2) + (data * 3) in
      let fp (key, data) = fi ~key ~data in
      let m = (module Int : Container.Summable with type t = int) in
      let data = data t in
      let alist = to_alist t in
      require_equal (module Int) (sum m t ~f) (List.sum m data ~f);
      require_equal (module Int) (sumi m t ~f:fi) (List.sum m alist ~f:fp))

  and fold_range_inclusive = fold_range_inclusive
  and range_to_alist = range_to_alist

  and () =
    quickcheck_m (module Inst_and_bounds) ~f:(fun (t, lower_bound, upper_bound) ->
      let t = Inst.value t in
      let min =
        match lower_bound with
        | Unbounded -> Key.of_int Int.min_value
        | Incl min -> min
        | Excl too_small ->
          (* key generator does not generate [max_value], so this cannot overflow *)
          Key.of_int (Key.to_int too_small + 1)
      in
      let max =
        match upper_bound with
        | Unbounded -> Key.of_int Int.max_value
        | Incl max -> max
        | Excl too_large ->
          (* key generator does not generate [min_value], so this cannot overflow *)
          Key.of_int (Key.to_int too_large - 1)
      in
      let fold_alist =
        access fold_range_inclusive t ~min ~max ~init:[] ~f:(fun ~key ~data acc ->
          (key, data) :: acc)
        |> List.rev
      in
      let range_alist = access range_to_alist t ~min ~max in
      let expect =
        if Maybe_bound.bounds_crossed
             ~lower:lower_bound
             ~upper:upper_bound
             ~compare:Key.compare
        then []
        else
          List.filter (to_alist t) ~f:(fun (key, _) ->
            Maybe_bound.interval_contains_exn
              key
              ~lower:lower_bound
              ~upper:upper_bound
              ~compare:Key.compare)
      in
      require_equal (module Alist) fold_alist expect;
      require_equal (module Alist) range_alist expect)

  and closest_key = closest_key

  and () =
    quickcheck_m (module Inst_and_key) ~f:(fun (t, k) ->
      let t = Inst.value t in
      let alist = to_alist t in
      let rev_alist = List.rev alist in
      require_equal
        (module Data.Option (Key_and_data))
        (access closest_key t `Less_than k)
        (List.find rev_alist ~f:(fun (key, _) -> Key.( < ) key k));
      require_equal
        (module Data.Option (Key_and_data))
        (access closest_key t `Less_or_equal_to k)
        (List.find rev_alist ~f:(fun (key, _) -> Key.( <= ) key k));
      require_equal
        (module Data.Option (Key_and_data))
        (access closest_key t `Greater_or_equal_to k)
        (List.find alist ~f:(fun (key, _) -> Key.( >= ) key k));
      require_equal
        (module Data.Option (Key_and_data))
        (access closest_key t `Greater_than k)
        (List.find alist ~f:(fun (key, _) -> Key.( > ) key k)))

  and nth = nth
  and nth_exn = nth_exn
  and rank = rank

  and () =
    quickcheck_m (module Inst_and_key) ~f:(fun (t, k) ->
      let t = Inst.value t in
      List.iteri (to_alist t) ~f:(fun i (key, data) ->
        require_equal (module Data.Option (Key_and_data)) (nth t i) (Some (key, data));
        require_equal
          (module Data.Option (Key_and_data))
          (Option.try_with (fun () -> nth_exn t i))
          (nth t i);
        require_equal (module Data.Option (Int)) (access rank t key) (Some i));
      require_equal (module Data.Option (Key_and_data)) (nth t (length t)) None;
      require_equal
        (module Data.Option (Int))
        (access rank t k)
        (List.find_mapi (to_alist t) ~f:(fun i (key, _) ->
           Option.some_if (Key.equal key k) i)))

  and count_le = count_le
  and count_gt = count_gt

  and () =
    quickcheck_m (module Inst_and_key) ~f:(fun (t, k) ->
      let t = Inst.value t in
      let le, gt = access split_le_gt t k in
      require_equal (module Int) (length le) (access count_le t k);
      require_equal (module Int) (length gt) (access count_gt t k))

  and count_lt = count_lt
  and count_ge = count_ge

  and () =
    quickcheck_m (module Inst_and_key) ~f:(fun (t, k) ->
      let t = Inst.value t in
      let lt, ge = access split_lt_ge t k in
      require_equal (module Int) (length lt) (access count_lt t k);
      require_equal (module Int) (length ge) (access count_ge t k))

  and binary_search = binary_search

  and () =
    quickcheck_m (module Inst_and_key) ~f:(fun (t, k) ->
      let t = Inst.value t in
      let targets = [%all: Binary_searchable.Which_target_by_key.t] in
      let compare (key, _) k = Key.compare key k in
      List.iter targets ~f:(fun which_target ->
        require_equal
          (module Data.Option (Key_and_data))
          (access
             binary_search
             t
             ~compare:(fun ~key ~data k' ->
               require_equal (module Key) k' k;
               require_equal (module Data.Option (Int)) (access find t key) (Some data);
               compare (key, data) k')
             which_target
             k)
          (let array = Array.of_list (to_alist t) in
           Array.binary_search array ~compare which_target k
           |> [%globalize: int option]
           |> Option.map ~f:(Array.get array))))

  and binary_search_segmented = binary_search_segmented

  and () =
    quickcheck_m (module Inst_and_key) ~f:(fun (t, k) ->
      let t = Inst.value t in
      let targets = [%all: Binary_searchable.Which_target_by_segment.t] in
      let segment_of (key, _) = if Key.( <= ) key k then `Left else `Right in
      List.iter targets ~f:(fun which_target ->
        require_equal
          (module Data.Option (Key_and_data))
          (access
             binary_search_segmented
             t
             ~segment_of:(fun ~key ~data ->
               require_equal (module Data.Option (Int)) (access find t key) (Some data);
               segment_of (key, data))
             which_target)
          (let array = Array.of_list (to_alist t) in
           Array.binary_search_segmented array ~segment_of which_target
           |> [%globalize: int option]
           |> Option.map ~f:(Array.get array))))

  and binary_search_subrange = binary_search_subrange

  and () =
    quickcheck_m
      (module struct
        type t = Inst.t * Key.t Maybe_bound.t * Key.t Maybe_bound.t
        [@@deriving quickcheck, sexp_of]

        let sample = Memo.memoize [%generator: t]
      end)
      ~f:(fun (t, lower_bound, upper_bound) ->
        let t = Inst.value t in
        require_equal
          (module Inst.Value)
          (access
             binary_search_subrange
             t
             ~compare:(fun ~key ~data bound ->
               require_equal (module Data.Option (Int)) (access find t key) (Some data);
               Key.compare key bound)
             ~lower_bound
             ~upper_bound)
          (access subrange t ~lower_bound ~upper_bound))

  and to_tree = to_tree

  and () =
    quickcheck_m (module Inst) ~f:(fun t ->
      let t = Inst.value t in
      let tree = to_tree t in
      let round_trip = create of_tree tree in
      require_equal (module Inst.Value) t round_trip)
  ;;
end

module Test_transformers
    (Instance : Instance)
    (Impl : Impl with module Types := Instance.Types) :
  Transformers with module Types := Instance.Types = struct
  open Instance
  open Impl
  open Test_helpers (Instance) (Impl)

  module type%template Make_applicative_traversals =
      module type of Make_applicative_traversals [@modality p]
  [@@modality p = (nonportable, portable)]

  let%template () = print_endline "Functor.Test_transformers: running tests."

  (** transformers *)

  and set = set

  and () =
    quickcheck_m (module Inst_and_key_and_data) ~f:(fun (t, key, data) ->
      let t = Inst.value t in
      require_equal
        (module Alist)
        (to_alist (access set t ~key ~data))
        (List.sort
           ~compare:(fun a b -> Comparable.lift Key.compare ~f:fst a b)
           ((key, data) :: List.Assoc.remove (to_alist t) key ~equal:Key.equal)))

  and add = add
  and add_exn = add_exn

  and () =
    quickcheck_m (module Inst_and_key_and_data) ~f:(fun (t, key, data) ->
      let t = Inst.value t in
      let t_add =
        match access add t ~key ~data with
        | `Ok t -> Ok t
        | `Duplicate -> Or_error.error_string "duplicate"
      in
      let t_add_exn = Or_error.try_with (fun () -> access add_exn t ~key ~data) in
      let expect =
        if access mem t key
        then Or_error.error_string "duplicate"
        else Ok (access set t ~key ~data)
      in
      require_equal (module Data.Or_error (Inst.Value)) t_add expect;
      require_equal (module Data.Or_error (Inst.Value)) t_add_exn expect)

  and remove = remove

  and () =
    quickcheck_m (module Inst_and_key) ~f:(fun (t, key) ->
      let t = Inst.value t in
      require_equal
        (module Alist)
        (to_alist (access remove t key))
        (List.Assoc.remove (to_alist t) key ~equal:Key.equal))

  and change = change

  and () =
    quickcheck_m (module Inst_and_key_and_maybe_data) ~f:(fun (t, key, maybe_data) ->
      let t = Inst.value t in
      let actual =
        access change t key ~f:(fun previous ->
          require_equal (module Data.Option (Int)) previous (access find t key);
          maybe_data)
      in
      let expect =
        match maybe_data with
        | None -> access remove t key
        | Some data -> access set t ~key ~data
      in
      require_equal (module Inst.Value) actual expect)

  and update = update

  and () =
    quickcheck_m (module Inst_and_key_and_data) ~f:(fun (t, key, data) ->
      let t = Inst.value t in
      let actual =
        access update t key ~f:(fun previous ->
          require_equal (module Data.Option (Int)) previous (access find t key);
          data)
      in
      let expect = access set t ~key ~data in
      require_equal (module Inst.Value) actual expect)

  and update_and_return = update_and_return

  and () =
    quickcheck_m (module Inst_and_key_and_data) ~f:(fun (t, key, data) ->
      let t = Inst.value t in
      let new_data, actual =
        access update_and_return t key ~f:(fun previous ->
          require_equal (module Data.Option (Int)) previous (access find t key);
          data)
      in
      let expect = access set t ~key ~data in
      require_equal (module Inst.Value) actual expect;
      require_equal (module Int) new_data data)

  and add_multi = add_multi
  and remove_multi = remove_multi

  and () =
    quickcheck_m (module Inst_multi_and_key_and_data) ~f:(fun (t, key, data) ->
      let t = Inst_multi.value t in
      require_equal
        (module Inst_multi.Value)
        (access add_multi t ~key ~data)
        (access update t key ~f:(fun option -> data :: Option.value option ~default:[]));
      require_equal
        (module Inst_multi.Value)
        (access remove_multi t key)
        (access change t key ~f:(function
          | None | Some ([] | [ _ ]) -> None
          | Some (_ :: (_ :: _ as rest)) -> Some rest)))

  and map = map
  and mapi = mapi

  and () =
    quickcheck_m (module Inst) ~f:(fun t ->
      let t = Inst.value t in
      require_equal
        (module Inst.Value)
        (map t ~f:Int.succ)
        (t |> to_alist |> List.Assoc.map ~f:Int.succ |> create of_alist_exn);
      require_equal
        (module struct
          type t = (Key.t * int) Instance.t [@@deriving equal, sexp_of]
        end)
        (mapi t ~f:(fun ~key ~data -> key, data))
        (t |> to_alist |> List.map ~f:(fun (k, v) -> k, (k, v)) |> create of_alist_exn))

  and filter_keys = filter_keys
  and filter = filter
  and filteri = filteri

  and () =
    quickcheck_m (module Inst_and_key_and_data) ~f:(fun (t, k, d) ->
      let t = Inst.value t in
      require_equal
        (module Physical_equality (Inst.Value))
        (filter ~f:(fun _ -> true) t)
        t;
      require_equal
        (module Alist)
        (to_alist (filter_keys t ~f:(fun key -> Key.( <= ) key k)))
        (List.filter (to_alist t) ~f:(fun (key, _) -> Key.( <= ) key k));
      require_equal
        (module Alist)
        (to_alist (filter t ~f:(fun data -> data <= d)))
        (List.filter (to_alist t) ~f:(fun (_, data) -> data <= d));
      require_equal
        (module Alist)
        (to_alist (filteri t ~f:(fun ~key ~data -> Key.( <= ) key k && data <= d)))
        (List.filter (to_alist t) ~f:(fun (key, data) -> Key.( <= ) key k && data <= d)))

  and filter_map = filter_map
  and filter_mapi = filter_mapi

  and () =
    quickcheck_m (module Inst_and_key_and_data) ~f:(fun (t, k, d) ->
      let t = Inst.value t in
      require_equal
        (module Alist)
        (to_alist (filter_map t ~f:(fun data -> Option.some_if (data >= d) (data - d))))
        (List.filter_map (to_alist t) ~f:(fun (key, data) ->
           Option.some_if (data >= d) (key, data - d)));
      require_equal
        (module Alist)
        (to_alist
           (filter_mapi t ~f:(fun ~key ~data ->
              Option.some_if (Key.( <= ) key k && data >= d) (data - d))))
        (List.filter_map (to_alist t) ~f:(fun (key, data) ->
           Option.some_if (Key.( <= ) key k && data >= d) (key, data - d))))

  and partition_mapi = partition_mapi
  and partition_map = partition_map
  and partition_result = partition_result
  and partitioni_tf = partitioni_tf
  and partition_tf = partition_tf

  and () =
    quickcheck_m (module Inst_and_key_and_data) ~f:(fun (t, k, d) ->
      let t = Inst.value t in
      require_equal
        (module Physical_equality (Inst.Value))
        (fst (partition_tf ~f:(fun _ -> true) t))
        t;
      require_equal
        (module Data.Pair (Alist))
        (let a, b = partition_tf t ~f:(fun data -> data <= d) in
         to_alist a, to_alist b)
        (List.partition_tf (to_alist t) ~f:(fun (_, data) -> data <= d));
      require_equal
        (module Data.Pair (Alist))
        (let a, b =
           partitioni_tf t ~f:(fun ~key ~data -> Key.( <= ) key k && data <= d)
         in
         to_alist a, to_alist b)
        (List.partition_tf (to_alist t) ~f:(fun (key, data) ->
           Key.( <= ) key k && data <= d));
      require_equal
        (module Data.Pair (Alist))
        (let a, b =
           partition_map t ~f:(fun data ->
             if data >= d then First (data - d) else Second d)
         in
         to_alist a, to_alist b)
        (List.partition_map (to_alist t) ~f:(fun (key, data) ->
           if data >= d then First (key, data - d) else Second (key, d)));
      require_equal
        (module Data.Pair (Alist))
        (let a, b =
           partition_mapi t ~f:(fun ~key ~data ->
             if Key.( <= ) key k && data >= d then First (data - d) else Second d)
         in
         to_alist a, to_alist b)
        (List.partition_map (to_alist t) ~f:(fun (key, data) ->
           if Key.( <= ) key k && data >= d then First (key, data - d) else Second (key, d))))

  and combine_errors = combine_errors

  and () =
    quickcheck_m (module Inst_and_key) ~f:(fun (t, threshold) ->
      let t = Inst.value t in
      let t =
        mapi t ~f:(fun ~key ~data ->
          if Key.( <= ) key threshold then Ok data else Or_error.error_string "too big")
      in
      require_equal
        (module Data.Or_error (Inst.Value))
        (access combine_errors t)
        (to_alist t
         |> List.map ~f:(fun (key, result) ->
           Or_error.map result ~f:(fun data -> key, data))
         |> Or_error.combine_errors
         |> Or_error.map ~f:(create of_alist_exn)))

  and unzip = unzip

  and () =
    quickcheck_m (module Inst_pair) ~f:(fun t ->
      let t = Inst_pair.value t in
      require_equal
        (module Data.Pair (Alist))
        (let a, b = unzip t in
         to_alist a, to_alist b)
        (to_alist t |> List.map ~f:(fun (key, (a, b)) -> (key, a), (key, b)) |> List.unzip))

  and merge = merge

  and () =
    quickcheck_m (module Inst_and_inst_and_key) ~f:(fun ((a, b), k) ->
      let a = Inst.value a in
      let b = Inst.value b in
      let merge_alist =
        access merge a b ~f:(fun ~key elt -> Option.some_if (Key.( > ) key k) (key, elt))
        |> data
      in
      let expect =
        [ map a ~f:Either.first; map b ~f:Either.second ]
        |> List.concat_map ~f:to_alist
        |> List.Assoc.sort_and_group ~compare:Key.compare
        |> List.filter_map ~f:(fun (key, list) ->
          let elt =
            match (list : _ Either.t list) with
            | [ First x ] -> `Left x
            | [ Second y ] -> `Right y
            | [ First x; Second y ] -> `Both (x, y)
            | _ -> assert false
          in
          Option.some_if (Key.( > ) key k) (key, elt))
      in
      require_equal (module Alist_merge) merge_alist expect)

  and merge_by_case = merge_by_case

  and () =
    let module One_side = struct
      type t =
        [ `Drop
        | `Keep
        | `Map
        | `Filter
        | `Filter_map
        ]
      [@@deriving quickcheck, sexp_of]

      let swap = function
        | key, `Left x -> key, `Right x
        | key, `Right y -> key, `Left y
        | key, `Both (x, y) -> key, `Both (y, x)
      ;;

      let map_f ~key ~data = if Key.to_int key <= 1 then data else swap data
      let filter_f ~key ~data:_ = Key.to_int key <= 1

      let filter_map_f ~key ~data =
        match Key.to_int key with
        | 0 -> Some data
        | 1 -> Some (swap data)
        | _ -> None
      ;;

      let apply : t -> _ =
        fun t ~key data ->
        match t with
        | `Drop -> None
        | `Keep -> Some data
        | `Map -> Some (map_f ~key ~data)
        | `Filter -> Option.some_if (filter_f ~key ~data) data
        | `Filter_map -> filter_map_f ~key ~data
      ;;

      let to_when_unmatched : t -> _ Map.When_unmatched.t = function
        | `Drop -> Drop
        | `Keep -> Keep
        | `Map -> Map map_f
        | `Filter -> Filter filter_f
        | `Filter_map -> Filter_map filter_map_f
      ;;

      (* Ensure we haven't missed a case. *)
      let _of_when_unmatched : _ Map.When_unmatched.t -> t = function
        | Drop -> `Drop
        | Keep -> `Keep
        | Map _ -> `Map
        | Filter _ -> `Filter
        | Filter_map _ -> `Filter_map
      ;;
    end
    in
    let module Two_sides = struct
      type t =
        [ `Drop
        | `Keep_first
        | `Keep_second
        | `Map
        | `Filter_first
        | `Filter_second
        | `Filter_map
        ]
      [@@deriving quickcheck, sexp_of]

      let map_f ~key x y = if Key.to_int key <= 1 then x else y
      let filter_f ~key _ _ = Key.to_int key <= 1

      let filter_map_f ~key x y =
        match Key.to_int key with
        | 0 -> Some x
        | 1 -> Some y
        | _ -> None
      ;;

      let apply : t -> _ =
        fun t ~key x y ->
        match t with
        | `Drop -> None
        | `Keep_first -> Some x
        | `Keep_second -> Some y
        | `Map -> Some (map_f ~key x y)
        | `Filter_first -> Option.some_if (filter_f ~key x y) x
        | `Filter_second -> Option.some_if (filter_f ~key x y) y
        | `Filter_map -> filter_map_f ~key x y
      ;;

      let to_when_matched : t -> _ Map.When_matched.t = function
        | `Drop -> Drop
        | `Keep_first -> Keep_first
        | `Keep_second -> Keep_second
        | `Map -> Map map_f
        | `Filter_first -> Filter_first filter_f
        | `Filter_second -> Filter_second filter_f
        | `Filter_map -> Filter_map filter_map_f
      ;;

      (* Ensure we haven't missed a case. *)
      let _of_when_matched : _ Map.When_matched.t -> t = function
        | Drop -> `Drop
        | Keep_first -> `Keep_first
        | Keep_second -> `Keep_second
        | Map _ -> `Map
        | Filter_first _ -> `Filter_first
        | Filter_second _ -> `Filter_second
        | Filter_map _ -> `Filter_map
      ;;
    end
    in
    quickcheck_m
      (module struct
        type t = Inst_and_inst.t * One_side.t * One_side.t * Two_sides.t
        [@@deriving quickcheck, sexp_of]

        let sample = Memo.memoize [%generator: t]
      end)
      ~f:(fun ((a, b), left, right, both) ->
        let a = Inst.value a in
        let b = Inst.value b in
        let actual =
          access
            merge_by_case
            (Impl.mapi a ~f:(fun ~key ~data -> key, `Left data))
            (Impl.mapi b ~f:(fun ~key ~data -> key, `Right data))
            ~first:(One_side.to_when_unmatched left)
            ~second:(One_side.to_when_unmatched right)
            ~both:(Two_sides.to_when_matched both)
          |> data
        in
        let expect =
          access merge a b ~f:(fun ~key elt ->
            match elt with
            | `Left _ -> One_side.apply left ~key (key, elt)
            | `Right _ -> One_side.apply right ~key (key, elt)
            | `Both (x, y) -> Two_sides.apply both ~key (key, `Left x) (key, `Right y))
          |> data
        in
        require_equal (module Alist_merge) actual expect)

  and merge_disjoint_exn = merge_disjoint_exn

  and () =
    quickcheck_m (module Inst_and_inst) ~f:(fun (a, b) ->
      let a = Inst.value a in
      let b = Inst.value b in
      let actual = Option.try_with (fun () -> access merge_disjoint_exn a b) in
      let expect =
        if existsi a ~f:(fun ~key ~data:_ -> access mem b key)
        then None
        else
          Some
            (access merge a b ~f:(fun ~key:_ elt ->
               match elt with
               | `Left x | `Right x -> Some x
               | `Both _ -> assert false))
      in
      require_equal (module Data.Option (Inst.Value)) actual expect)

  and merge_skewed = merge_skewed

  and () =
    quickcheck_m (module Inst_and_inst) ~f:(fun (a, b) ->
      let a = Inst.value a in
      let b = Inst.value b in
      let actual =
        access merge_skewed a b ~combine:(fun ~key a b -> Key.to_int key + a + b)
      in
      let expect =
        access merge a b ~f:(fun ~key elt ->
          match elt with
          | `Left a -> Some a
          | `Right b -> Some b
          | `Both (a, b) -> Some (Key.to_int key + a + b))
      in
      require_equal (module Inst.Value) actual expect)

  and split = split

  and () =
    quickcheck_m (module Inst_and_key) ~f:(fun (t, k) ->
      let t = Inst.value t in
      require_equal
        (module struct
          type t = Inst.Value.t * (Key.t * int) option * Inst.Value.t
          [@@deriving equal, sexp_of]
        end)
        (access split t k)
        (let before, equal, after =
           List.partition3_map (to_alist t) ~f:(fun (key, data) ->
             match Ordering.of_int (Key.compare key k) with
             | Less -> `Fst (key, data)
             | Equal -> `Snd (key, data)
             | Greater -> `Trd (key, data))
         in
         create of_alist_exn before, List.hd equal, create of_alist_exn after))

  and split_le_gt = split_le_gt

  and () =
    quickcheck_m (module Inst_and_key) ~f:(fun (t, k) ->
      let t = Inst.value t in
      require_equal
        (module struct
          type t = Inst.Value.t * Inst.Value.t [@@deriving equal, sexp_of]
        end)
        (access split_le_gt t k)
        (let before, after =
           List.partition_tf (to_alist t) ~f:(fun (key, _) -> Key.( <= ) key k)
         in
         create of_alist_exn before, create of_alist_exn after))

  and split_lt_ge = split_lt_ge

  and () =
    quickcheck_m (module Inst_and_key) ~f:(fun (t, k) ->
      let t = Inst.value t in
      require_equal
        (module struct
          type t = Inst.Value.t * Inst.Value.t [@@deriving equal, sexp_of]
        end)
        (access split_lt_ge t k)
        (let before, after =
           List.partition_tf (to_alist t) ~f:(fun (key, _) -> Key.( < ) key k)
         in
         create of_alist_exn before, create of_alist_exn after))

  and split_n = split_n

  and () =
    quickcheck_m (module Inst) ~f:(fun t ->
      let t = Inst.value t in
      for n = -1 to length t + 1 do
        let l, r = split_n t n in
        require_equal (module Int) (length l) (Int.clamp_exn n ~min:0 ~max:(length t));
        require_equal
          (module Inst.Value)
          (match (access append) ~lower_part:l ~upper_part:r with
           | `Ok t -> t
           | `Overlapping_key_ranges -> assert false)
          t
      done)

  and append = append

  and () =
    quickcheck_m (module Inst_and_inst) ~f:(fun (a, b) ->
      let a = Inst.value a in
      let b = Inst.value b in
      require_equal
        (module Data.Or_error (Inst.Value))
        (match access append ~lower_part:a ~upper_part:b with
         | `Ok t -> Ok t
         | `Overlapping_key_ranges -> Or_error.error_string "overlap")
        (match max_elt a, min_elt b with
         | Some (x, _), Some (y, _) when Key.( >= ) x y -> Or_error.error_string "overlap"
         | _ -> Ok (create of_alist_exn (to_alist a @ to_alist b)));
      let a' =
        (* we rely on the fact that the [Inst] generator uses positive keys *)
        create map_keys_exn a ~f:(fun k -> Key.of_int (-Key.to_int k))
      in
      require_equal
        (module Data.Or_error (Inst.Value))
        (match access append ~lower_part:a' ~upper_part:b with
         | `Ok t -> Ok t
         | `Overlapping_key_ranges -> Or_error.error_string "overlap")
        (Ok (create of_alist_exn (to_alist a' @ to_alist b))))

  and subrange = subrange

  and () =
    quickcheck_m (module Inst_and_bounds) ~f:(fun (t, lower_bound, upper_bound) ->
      let t = Inst.value t in
      let subrange_alist = access subrange t ~lower_bound ~upper_bound |> to_alist in
      let expect =
        if Maybe_bound.bounds_crossed
             ~lower:lower_bound
             ~upper:upper_bound
             ~compare:Key.compare
        then []
        else
          List.filter (to_alist t) ~f:(fun (key, _) ->
            Maybe_bound.interval_contains_exn
              key
              ~lower:lower_bound
              ~upper:upper_bound
              ~compare:Key.compare)
      in
      require_equal (module Alist) subrange_alist expect)

  and m_Make_applicative_traversals : ((module Make_applicative_traversals)[@modality p]) =
    (module functor
              (A : sig
               @@ p
                 include Applicative.Lazy_applicative
               end)
              ->
              struct
                module M = Make_applicative_traversals [@modality p] (A)

                let mapi = M.mapi
                let filter_mapi = M.filter_mapi
              end)
  [@@modality p = (nonportable, portable)]

  and () =
    let module M =
      Make_applicative_traversals (struct
        module M = struct
          type 'a t = 'a

          let return x = x
          let apply f x = f x
          let of_thunk f = f ()
          let map = `Define_using_apply
        end

        include M
        include Applicative.Make (M)
      end)
    in
    quickcheck_m (module Inst) ~f:(fun t ->
      let t = Inst.value t in
      let f1 ~key:_ ~data = (data * 2) + 1 in
      let f2 ~key:_ ~data = if data < 0 then None else Some data in
      require_equal (module Inst.Value) (mapi t ~f:f1) (M.mapi t ~f:f1);
      require_equal (module Inst.Value) (filter_mapi t ~f:f2) (M.filter_mapi t ~f:f2))
  ;;

  module%template.portable [@modality p] Make_applicative_traversals =
    (val (m_Make_applicative_traversals [@modality p]))
end
