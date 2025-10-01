(** Comprehensive testing of [Base.Set].

    This file tests all exports of [Base.Set]. Every time a new export is added, we have
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

module Generate (Instance : Instance) (Impl : Impl with module Types := Instance.Types) :
  Generate with module Instance := Instance = struct
  include Constructor.Make (Instance) (Impl)

  module Value = struct
    type t = Instance.t [@@deriving compare, equal, sexp_of]
  end
end

module Test_helpers
    (Instance : Instance)
    (Impl : Impl with module Types := Instance.Types) =
struct
  open Instance

  module Inst = struct
    include Generate (Instance) (Impl)

    let sample = Memo.memoize [%generator: t]
  end

  module Elts = struct
    type t = Elt.t list [@@deriving compare, equal, quickcheck, sexp_of]

    let sample = Memo.memoize [%generator: t]
  end

  module Elts_result = struct
    type t = (Elt.t list, Elt.t list) Result.t [@@deriving compare, equal, sexp_of]
  end

  module Inst_and_int = struct
    type t = Inst.t * int [@@deriving quickcheck, sexp_of]

    let sample = Memo.memoize [%generator: t]
  end

  module Inst_and_elt = struct
    type t = Inst.t * Elt.t [@@deriving quickcheck, sexp_of]

    let sample = Memo.memoize [%generator: t]
  end

  module Inst_and_inst = struct
    include Data.Pair (Inst)

    let sample = Memo.memoize [%generator: t]

    module Value = struct
      type t = Inst.Value.t * Inst.Value.t [@@deriving equal, sexp_of]
    end
  end

  module Inst_list = struct
    type t = Inst.t list [@@deriving equal, quickcheck, sexp_of]

    let sample = Memo.memoize [%generator: t]

    module Value = struct
      type t = Inst.Value.t list [@@deriving equal, sexp_of]
    end
  end

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

  and () = require_equal (module Sexp) [%sexp (create empty : t)] [%sexp []]
  and singleton = singleton

  and () =
    require_equal
      (module Sexp)
      [%sexp (create singleton (Elt.of_int 1) : t)]
      [%sexp [ 1 ]]

  and of_list = of_list

  and () =
    quickcheck_m (module Elts) ~f:(fun list ->
      let t = create of_list list in
      require_equal
        (module Elts)
        (to_list t)
        (List.dedup_and_sort list ~compare:Elt.compare))

  and of_array = of_array

  and () =
    quickcheck_m (module Elts) ~f:(fun list ->
      let t = create of_array (Array.of_list list) in
      require_equal
        (module Elts)
        (to_list t)
        (List.dedup_and_sort list ~compare:Elt.compare))

  and of_sequence = of_sequence

  and () =
    quickcheck_m (module Elts) ~f:(fun list ->
      let t = create of_sequence (Sequence.of_list list) in
      require_equal
        (module Elts)
        (to_list t)
        (List.dedup_and_sort list ~compare:Elt.compare))

  and of_sorted_array = of_sorted_array

  and () =
    quickcheck_m (module Elts) ~f:(fun elts ->
      let actual = create of_sorted_array (Array.of_list elts) in
      let expect =
        if List.is_sorted_strictly ~compare:Elt.compare elts
           || List.is_sorted_strictly ~compare:Elt.compare (List.rev elts)
        then Ok (create of_list elts)
        else Or_error.error_string "unsorted"
      in
      require_equal (module Data.Or_error (Inst.Value)) actual expect)

  and of_sorted_array_unchecked = of_sorted_array_unchecked

  and () =
    quickcheck_m (module Elts) ~f:(fun elts ->
      let elts = List.dedup_and_sort elts ~compare:Elt.compare in
      let actual_fwd = create of_sorted_array_unchecked (Array.of_list elts) in
      let actual_rev = create of_sorted_array_unchecked (Array.of_list_rev elts) in
      let expect = create of_list elts in
      require_equal (module Inst.Value) actual_fwd expect;
      require_equal (module Inst.Value) actual_rev expect)

  and of_increasing_iterator_unchecked = of_increasing_iterator_unchecked

  and () =
    quickcheck_m (module Elts) ~f:(fun elts ->
      let elts = List.dedup_and_sort elts ~compare:Elt.compare in
      let actual =
        let array = Array.of_list elts in
        create
          of_increasing_iterator_unchecked
          ~len:(Array.length array)
          ~f:(Array.get array)
      in
      let expect = create of_list elts in
      require_equal (module Inst.Value) actual expect)

  and of_tree = of_tree

  and () =
    quickcheck_m (module Inst) ~f:(fun t ->
      let t = Inst.value t in
      let tree = to_tree t in
      let round_trip = create of_tree tree in
      require_equal (module Inst.Value) t round_trip)

  and union_list = union_list

  and () =
    quickcheck_m (module Inst_list) ~f:(fun list ->
      let list = List.map ~f:Inst.value list in
      let t = (create union_list) list in
      require_equal
        (module Elts)
        (to_list t)
        (List.dedup_and_sort ~compare:Elt.compare (List.concat_map ~f:to_list list)))

  and map = map

  and () =
    quickcheck_m (module Inst_and_elt) ~f:(fun (t, e) ->
      let t = Inst.value t in
      let f elt = Comparable.min Elt.compare elt e in
      let actual = (create map) t ~f in
      let expect = to_list t |> List.map ~f |> create of_list in
      require_equal (module Inst.Value) actual expect)

  and filter_map = filter_map

  and () =
    quickcheck_m (module Inst_and_elt) ~f:(fun (t, e) ->
      let t = Inst.value t in
      let f elt =
        match Ordering.of_int (Elt.compare elt e) with
        | Less -> Some elt
        | Equal -> None
        | Greater -> Some e
      in
      let actual = (create filter_map) t ~f in
      let expect = to_list t |> List.filter_map ~f |> create of_list in
      require_equal (module Inst.Value) actual expect)
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
      require_equal (module Int) len (List.length (to_list t)))

  and is_subset = is_subset

  and () =
    quickcheck_m (module Inst_and_inst) ~f:(fun (a, b) ->
      let a = Inst.value a in
      let b = Inst.value b in
      require_equal
        (module Bool)
        (access is_subset a ~of_:b)
        (for_all a ~f:((access mem) b)))

  and are_disjoint = are_disjoint

  and () =
    quickcheck_m (module Inst_and_inst) ~f:(fun (a, b) ->
      let a = Inst.value a in
      let b = Inst.value b in
      require_equal
        (module Bool)
        (access are_disjoint a b)
        (is_empty ((access inter) a b)))

  and mem = mem

  and () =
    quickcheck_m (module Inst_and_elt) ~f:(fun (t, elt) ->
      let t = Inst.value t in
      let expect = List.mem (to_list t) elt ~equal:Elt.equal in
      require_equal (module Bool) (access mem t elt) expect)

  and find = find
  and find_exn = find_exn

  and () =
    quickcheck_m (module Inst_and_elt) ~f:(fun (t, elt) ->
      let t = Inst.value t in
      let expect = List.find (to_list t) ~f:(Elt.equal elt) in
      require_equal (module Data.Option (Elt)) (find t ~f:(Elt.equal elt)) expect;
      require_equal
        (module Data.Option (Elt))
        (Option.try_with (fun () -> find_exn t ~f:(Elt.equal elt)))
        expect)

  and find_map = find_map

  and () =
    quickcheck_m (module Inst_and_elt) ~f:(fun (t, e) ->
      let t = Inst.value t in
      let f elt =
        match Ordering.of_int (Elt.compare elt e) with
        | Less -> Some elt
        | Equal -> None
        | Greater -> Some e
      in
      let actual = find_map t ~f in
      let candidates = List.filter_map (to_list t) ~f in
      match actual with
      | None -> require (List.is_empty candidates)
      | Some elt -> require (List.mem candidates elt ~equal:Elt.equal))

  and iter = iter

  and () =
    quickcheck_m (module Inst) ~f:(fun t ->
      let t = Inst.value t in
      let actual =
        let q = Queue.create () in
        iter t ~f:(Queue.enqueue q);
        Queue.to_list q
      in
      require_equal (module Data.List (Elt)) actual (to_list t))

  and iter_until = iter_until

  and () =
    quickcheck_m (module Inst_and_elt) ~f:(fun (t, threshold) ->
      let t = Inst.value t in
      let res, actual =
        let q = Queue.create () in
        let res =
          iter_until
            t
            ~finish:(fun () -> true)
            ~f:(fun elt ->
              if Elt.( >= ) elt threshold
              then Stop false
              else (
                Queue.enqueue q elt;
                Continue ()))
        in
        res, Queue.to_list q
      in
      let expect = to_list t |> List.take_while ~f:(fun elt -> Elt.( < ) elt threshold) in
      require_equal (module Bool) res (List.length expect = length t);
      require_equal (module Data.List (Elt)) actual expect)

  and fold = fold
  and fold_right = fold_right

  and () =
    quickcheck_m (module Inst) ~f:(fun t ->
      let t = Inst.value t in
      require_equal
        (module Elts)
        (fold t ~init:[] ~f:(fun list elt -> elt :: list))
        (List.rev (to_list t));
      require_equal
        (module Elts)
        (fold_right t ~init:[] ~f:(fun elt list -> elt :: list))
        (to_list t))

  and fold_until = fold_until
  and fold_result = fold_result

  and () =
    quickcheck_m (module Inst_and_elt) ~f:(fun (t, threshold) ->
      let t = Inst.value t in
      let until =
        fold_until
          t
          ~init:[]
          ~finish:(fun list -> Ok list)
          ~f:(fun acc elt ->
            if Elt.( >= ) elt threshold then Stop (Error acc) else Continue (elt :: acc))
      in
      let result =
        fold_result t ~init:[] ~f:(fun acc elt ->
          if Elt.( >= ) elt threshold then Error acc else Ok (elt :: acc))
      in
      let expect =
        let list =
          to_list t |> List.take_while ~f:(fun elt -> Elt.( < ) elt threshold) |> List.rev
        in
        if List.length list = length t then Ok list else Error list
      in
      require_equal (module Elts_result) until expect;
      require_equal (module Elts_result) result expect)

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
        (Ordering.of_int (access compare_direct a b))
        (Ordering.of_int (Elts.compare (to_list a) (to_list b)));
      require_equal (module Bool) (access compare_direct a b = 0) (access equal a b))

  and elements = elements
  and to_list = to_list
  and to_array = to_array
  and to_sequence = to_sequence

  and () =
    quickcheck_m (module Inst) ~f:(fun t ->
      let t = Inst.value t in
      let list = to_list t in
      require_equal (module Inst.Value) (create of_list list) t;
      require_equal (module Data.List (Elt)) (elements t) list;
      require_equal (module Elts) (Array.to_list (to_array t)) list;
      require_equal (module Elts) (Sequence.to_list ((access to_sequence) t)) list)

  and () =
    quickcheck_m
      (module struct
        type t = Inst.t * [ `Decreasing | `Increasing ] * Elt.t * Elt.t
        [@@deriving quickcheck, sexp_of]

        let sample = Memo.memoize [%generator: t]
      end)
      ~f:(fun (t, order, greater_or_equal_to, less_or_equal_to) ->
        let t = Inst.value t in
        let alist =
          Sequence.to_list
            ((access to_sequence) t ~order ~greater_or_equal_to ~less_or_equal_to)
        in
        require_equal
          (module Elts)
          alist
          (List.filter
             (match order with
              | `Decreasing -> List.rev (to_list t)
              | `Increasing -> to_list t)
             ~f:(fun elt ->
               Elt.( <= ) greater_or_equal_to elt && Elt.( <= ) elt less_or_equal_to)))

  and iter2 = iter2

  and () =
    quickcheck_m (module Inst_and_inst) ~f:(fun (a, b) ->
      let a = Inst.value a in
      let b = Inst.value b in
      let iter2_list =
        let q = Queue.create () in
        access iter2 a b ~f:(Queue.enqueue q);
        Queue.to_list q
      in
      let expect =
        [ List.map (to_list a) ~f:Either.first; List.map (to_list b) ~f:Either.second ]
        |> List.concat
        |> List.sort_and_group ~compare:(Comparable.lift ~f:Either.value Elt.compare)
        |> List.map ~f:(fun list ->
          match (list : _ Either.t list) with
          | [ First x ] -> `Left x
          | [ Second y ] -> `Right y
          | [ First x; Second y ] -> `Both (x, y)
          | _ -> assert false)
      in
      require_equal
        (module struct
          type t = [ `Left of Elt.t | `Right of Elt.t | `Both of Elt.t * Elt.t ] list
          [@@deriving equal, sexp_of]
        end)
        iter2_list
        expect)

  and symmetric_diff = symmetric_diff

  and () =
    quickcheck_m (module Inst_and_inst) ~f:(fun (a, b) ->
      let a = Inst.value a in
      let b = Inst.value b in
      let actual = (access symmetric_diff) a b |> Sequence.to_list in
      let expect =
        [ (access diff) a b |> to_list |> List.map ~f:Either.first
        ; (access diff) b a |> to_list |> List.map ~f:Either.second
        ]
        |> List.concat
        |> List.sort ~compare:(Comparable.lift Elt.compare ~f:Either.value)
      in
      require_equal
        (module struct
          type t = (Elt.t, Elt.t) Either.t list [@@deriving equal, sexp_of]
        end)
        actual
        expect)

  and merge_to_sequence = merge_to_sequence

  and () =
    quickcheck_m
      (module struct
        type t =
          Inst.t
          * Inst.t
          * [ `Decreasing | `Increasing ] option
          * Elt.t option
          * Elt.t option
        [@@deriving quickcheck, sexp_of]

        let sample = Memo.memoize [%generator: t]
      end)
      ~f:(fun (a, b, order, greater_or_equal_to, less_or_equal_to) ->
        let a = Inst.value a in
        let b = Inst.value b in
        let actual =
          (access merge_to_sequence) a b ?order ?greater_or_equal_to ?less_or_equal_to
          |> Sequence.to_list
        in
        let expect =
          let order =
            match order with
            | Some `Increasing | None -> Fn.id
            | Some `Decreasing -> List.rev
          in
          let greater_or_equal_to =
            match greater_or_equal_to with
            | None -> Fn.id
            | Some elt -> List.filter ~f:(fun e -> Elt.compare e elt >= 0)
          in
          let less_or_equal_to =
            match less_or_equal_to with
            | None -> Fn.id
            | Some elt -> List.filter ~f:(fun e -> Elt.compare e elt <= 0)
          in
          (access union) a b
          |> to_list
          |> order
          |> greater_or_equal_to
          |> less_or_equal_to
          |> List.map ~f:(fun elt : _ Sequence.Merge_with_duplicates_element.t ->
            if not ((access mem) a elt)
            then Right elt
            else if not ((access mem) b elt)
            then Left elt
            else Both (elt, elt))
        in
        require_equal
          (module struct
            type t = (Elt.t, Elt.t) Sequence.Merge_with_duplicates_element.t list
            [@@deriving equal, sexp_of]
          end)
          actual
          expect)

  and min_elt = min_elt
  and max_elt = max_elt
  and min_elt_exn = min_elt_exn
  and max_elt_exn = max_elt_exn

  and () =
    quickcheck_m (module Inst) ~f:(fun t ->
      let t = Inst.value t in
      require_equal (module Data.Option (Elt)) (min_elt t) (List.hd (to_list t));
      require_equal (module Data.Option (Elt)) (max_elt t) (List.last (to_list t));
      require_equal
        (module Data.Option (Elt))
        (Option.try_with (fun () -> min_elt_exn t))
        (List.hd (to_list t));
      require_equal
        (module Data.Option (Elt))
        (Option.try_with (fun () -> max_elt_exn t))
        (List.last (to_list t)))

  and choose = choose
  and choose_exn = choose_exn

  and () =
    quickcheck_m (module Inst) ~f:(fun t ->
      let t = Inst.value t in
      let choose = choose t in
      let choose_exn = Option.try_with (fun () -> choose_exn t) in
      require_equal (module Data.Option (Elt)) choose choose_exn;
      match choose with
      | None -> require (is_empty t)
      | Some elt -> require ((access mem) t elt))

  and for_all = for_all
  and exists = exists
  and count = count

  and () =
    quickcheck_m (module Inst_and_elt) ~f:(fun (t, e) ->
      let t = Inst.value t in
      let f elt = Elt.compare elt e <= 0 in
      let list = to_list t in
      require_equal (module Bool) (for_all t ~f) (List.for_all list ~f);
      require_equal (module Bool) (exists t ~f) (List.exists list ~f);
      require_equal (module Int) (count t ~f) (List.count list ~f))

  and sum = sum

  and () =
    quickcheck_m (module Inst) ~f:(fun t ->
      let t = Inst.value t in
      let f = Elt.to_int in
      let m = (module Int : Container.Summable with type t = int) in
      let list = to_list t in
      require_equal (module Int) (sum m t ~f) (List.sum m list ~f))

  and nth = nth

  and () =
    quickcheck_m (module Inst) ~f:(fun t ->
      let t = Inst.value t in
      List.iteri (to_list t) ~f:(fun i elt ->
        require_equal (module Data.Option (Elt)) (nth t i) (Some elt));
      require_equal (module Data.Option (Elt)) (nth t (length t)) None)

  and rank = rank

  and () =
    quickcheck_m (module Inst_and_elt) ~f:(fun (t, e) ->
      let t = Inst.value t in
      List.iteri (to_list t) ~f:(fun i elt ->
        require_equal (module Data.Option (Int)) (access rank t elt) (Some i));
      if not (access mem t e) then require_none [%sexp_of: int] (access rank t e))

  and binary_search = binary_search

  and () =
    quickcheck_m (module Inst_and_elt) ~f:(fun (t, e) ->
      let t = Inst.value t in
      let targets = [%all: Binary_searchable.Which_target_by_key.t] in
      let compare elt e = Elt.compare elt e in
      List.iter targets ~f:(fun which_target ->
        require_equal
          (module Data.Option (Elt))
          (access
             binary_search
             t
             ~compare:(fun elt e' ->
               require_equal (module Elt) e' e;
               require (access mem t elt);
               compare elt e')
             which_target
             e)
          (let array = Array.of_list (to_list t) in
           Array.binary_search array ~compare which_target e
           |> [%globalize: int option]
           |> Option.map ~f:(Array.get array))))

  and binary_search_segmented = binary_search_segmented

  and () =
    quickcheck_m (module Inst_and_elt) ~f:(fun (t, e) ->
      let t = Inst.value t in
      let targets = [%all: Binary_searchable.Which_target_by_segment.t] in
      let segment_of elt = if Elt.( <= ) elt e then `Left else `Right in
      List.iter targets ~f:(fun which_target ->
        require_equal
          (module Data.Option (Elt))
          (access
             binary_search_segmented
             t
             ~segment_of:(fun elt ->
               require (access mem t elt);
               segment_of elt)
             which_target)
          (let array = Array.of_list (to_list t) in
           Array.binary_search_segmented array ~segment_of which_target
           |> [%globalize: int option]
           |> Option.map ~f:(Array.get array))))

  and to_tree = to_tree

  and () =
    quickcheck_m (module Inst) ~f:(fun t ->
      let t = Inst.value t in
      let tree = to_tree t in
      let round_trip = create of_tree tree in
      require_equal (module Inst.Value) t round_trip)
  ;;

  module Named = struct
    let () = ()
    and is_subset = Named.is_subset

    and () =
      quickcheck_m (module Inst_and_inst) ~f:(fun (a, b) ->
        let a = Inst.value a in
        let b = Inst.value b in
        require_equal
          (module Data.Or_error (Unit))
          ((access Named.is_subset) { name = "a"; set = a } ~of_:{ name = "b"; set = b })
          (if (access is_subset) a ~of_:b then Ok () else Or_error.error_string "oops"))

    and equal = Named.equal

    and () =
      quickcheck_m (module Inst_and_inst) ~f:(fun (a, b) ->
        let a = Inst.value a in
        let b = Inst.value b in
        require_equal
          (module Data.Or_error (Unit))
          ((access Named.equal) { name = "a"; set = a } { name = "b"; set = b })
          (if (access equal) a b then Ok () else Or_error.error_string "oops"))
    ;;
  end
end

module Test_transformers
    (Instance : Instance)
    (Impl : Impl with module Types := Instance.Types) :
  Transformers with module Types := Instance.Types = struct
  open Instance
  open Impl
  open Test_helpers (Instance) (Impl)

  let () = print_endline "Functor.Test_transformers: running tests."

  (** transformers *)

  and add = add

  and () =
    quickcheck_m (module Inst_and_elt) ~f:(fun (t, elt) ->
      let t = Inst.value t in
      require_equal
        (module Elts)
        (to_list (access add t elt))
        (List.dedup_and_sort ~compare:Elt.compare (elt :: to_list t)))

  and remove = remove

  and () =
    quickcheck_m (module Inst_and_elt) ~f:(fun (t, elt) ->
      let t = Inst.value t in
      require_equal
        (module Elts)
        (to_list (access remove t elt))
        (List.filter (to_list t) ~f:(fun e -> not (Elt.equal e elt))))

  and remove_index = remove_index

  and () =
    quickcheck_m (module Inst_and_int) ~f:(fun (t, i) ->
      let t = Inst.value t in
      let actual = (access remove_index) t i in
      let expect = List.filteri (to_list t) ~f:(fun j _ -> i <> j) |> create of_list in
      require_equal (module Inst.Value) actual expect)

  and filter = filter
  and partition_tf = partition_tf

  and () =
    quickcheck_m (module Inst_and_elt) ~f:(fun (t, e) ->
      let t = Inst.value t in
      require_equal
        (module Physical_equality (Inst.Value))
        (fst (partition_tf ~f:(fun _ -> true) t))
        t;
      let f elt = Elt.compare elt e <= 0 in
      let by_partition_tf = partition_tf t ~f in
      let by_filter = filter t ~f, filter t ~f:(Fn.non f) in
      let expect =
        let ts, fs =
          List.partition_tf (to_list t) ~f:(fun elt -> Elt.compare elt e <= 0)
        in
        (create of_list) ts, create of_list fs
      in
      require_equal (module Inst_and_inst.Value) by_partition_tf expect;
      require_equal (module Inst_and_inst.Value) by_filter expect)

  and group_by = (group_by [@alert "-deprecated"])

  and () =
    quickcheck_m (module Inst_and_int) ~f:(fun (t, i) ->
      let t = Inst.value t in
      let f e = Elt.to_int e % i in
      let actual =
        (group_by [@alert "-deprecated"]) t ~equiv:(Comparable.lift Int.equal ~f)
      in
      let expect =
        List.sort_and_group ~compare:(Comparable.lift Int.compare ~f) (to_list t)
        |> List.map ~f:(create of_list)
      in
      require_equal
        (module struct
          type t = Inst_list.Value.t [@@deriving sexp_of]

          let equal =
            Comparable.lift
              Inst_list.Value.equal
              ~f:(List.sort ~compare:Inst.Value.compare)
          ;;
        end)
        actual
        expect)

  and diff = diff

  and () =
    quickcheck_m (module Inst_and_inst) ~f:(fun (a, b) ->
      let a = Inst.value a in
      let b = Inst.value b in
      let actual = (access diff) a b in
      let expect =
        (create of_list) (List.filter (to_list a) ~f:(fun e -> not ((access mem) b e)))
      in
      require_equal (module Inst.Value) actual expect)

  and inter = inter

  and () =
    quickcheck_m (module Inst_and_inst) ~f:(fun (a, b) ->
      let a = Inst.value a in
      let b = Inst.value b in
      let actual = (access inter) a b in
      let expect = (create of_list) (List.filter (to_list a) ~f:((access mem) b)) in
      require_equal (module Inst.Value) actual expect)

  and union = union

  and () =
    quickcheck_m (module Inst_and_inst) ~f:(fun (a, b) ->
      let a = Inst.value a in
      let b = Inst.value b in
      let actual = (access union) a b in
      let expect = (create of_list) (to_list a @ to_list b) in
      require_equal (module Inst.Value) actual expect)

  and split = split

  and () =
    quickcheck_m (module Inst_and_elt) ~f:(fun (t, e) ->
      let t = Inst.value t in
      require_equal
        (module struct
          type t = Inst.Value.t * Elt.t option * Inst.Value.t [@@deriving equal, sexp_of]
        end)
        (access split t e)
        (let before, equal, after =
           List.partition3_map (to_list t) ~f:(fun elt ->
             match Ordering.of_int (Elt.compare elt e) with
             | Less -> `Fst elt
             | Equal -> `Snd elt
             | Greater -> `Trd elt)
         in
         create of_list before, List.hd equal, create of_list after))

  and split_le_gt = split_le_gt

  and () =
    quickcheck_m (module Inst_and_elt) ~f:(fun (t, e) ->
      let t = Inst.value t in
      require_equal
        (module struct
          type t = Inst.Value.t * Inst.Value.t [@@deriving equal, sexp_of]
        end)
        (access split_le_gt t e)
        (let before, after =
           List.partition_tf (to_list t) ~f:(fun elt -> Elt.( <= ) elt e)
         in
         create of_list before, create of_list after))

  and split_lt_ge = split_lt_ge

  and () =
    quickcheck_m (module Inst_and_elt) ~f:(fun (t, e) ->
      let t = Inst.value t in
      require_equal
        (module struct
          type t = Inst.Value.t * Inst.Value.t [@@deriving equal, sexp_of]
        end)
        (access split_lt_ge t e)
        (let before, after =
           List.partition_tf (to_list t) ~f:(fun elt -> Elt.( < ) elt e)
         in
         create of_list before, create of_list after))
  ;;
end
