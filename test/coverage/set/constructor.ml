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

(* Set operations allow overlap, since unlike maps two identical keys do not potentially
   carry different data. But we want to sometimes force no overlap to consider larger
   sets. So for operations that allow overlap, we sometimes randomly disallow it. This is
   enforced in the [normalize] operation, below. *)
module Overlap = struct
  type t =
    | Allow_overlap
    | Disallow_overlap
  [@@deriving equal, quickcheck, sexp_of]
end

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

  type 'elt t =
    | Empty
    | Singleton of 'elt
    | Of_sorted_array of 'elt list
    | Of_sorted_array_unchecked of 'elt list
    | Of_increasing_iterator_unchecked of 'elt list
    | Of_list of Overlap.t * 'elt list
    | Of_array of Overlap.t * 'elt list
    | Of_sequence of Overlap.t * 'elt list
    | Add of Overlap.t * 'elt * 'elt t [@weight rec_weight]
    | Remove of 'elt * 'elt t [@weight rec_weight]
    | Map of Overlap.t * ('elt, 'elt) Func.t * 'elt t [@weight rec_weight]
    | Filter of ('elt, bool) Func.t * 'elt t [@weight rec_weight]
    | Filter_map of Overlap.t * ('elt, 'elt option) Func.t * 'elt t [@weight rec_weight]
    | Partition_tf of Side.t * ('elt, bool) Func.t * 'elt t [@weight rec_weight]
    | Diff of 'elt t * 'elt t [@weight rec_weight]
    | Inter of 'elt t * 'elt t [@weight rec_weight]
    | Union of Overlap.t * 'elt t * 'elt t [@weight rec_weight]
    | Union_list of Overlap.t * 'elt t list [@weight rec_weight]
    | Split of Side.t * 'elt * 'elt t [@weight rec_weight]
    | Split_le_gt of Side.t * 'elt * 'elt t [@weight rec_weight]
    | Split_lt_ge of Side.t * 'elt * 'elt t [@weight rec_weight]
    | Group_by of ('elt, int) Func.t * int * 'elt t [@weight rec_weight]
    | Remove_index of int * 'elt t [@weight rec_weight]
  [@@deriving equal, quickcheck ~generator ~observer, sexp_of]

  let nested = function
    | Empty -> []
    | Singleton _ -> []
    | Of_sorted_array _ -> []
    | Of_sorted_array_unchecked _ -> []
    | Of_increasing_iterator_unchecked _ -> []
    | Of_list _ -> []
    | Of_array _ -> []
    | Of_sequence _ -> []
    | Add (_, _, t) -> [ t ]
    | Remove (_, t) -> [ t ]
    | Map (_, _, t) -> [ t ]
    | Filter (_, t) -> [ t ]
    | Filter_map (_, _, t) -> [ t ]
    | Partition_tf (_, _, t) -> [ t ]
    | Diff (a, b) -> [ a; b ]
    | Inter (a, b) -> [ a; b ]
    | Union (_, a, b) -> [ a; b ]
    | Union_list (_, list) -> list
    | Split (_, _, t) -> [ t ]
    | Split_le_gt (_, _, t) -> [ t ]
    | Split_lt_ge (_, _, t) -> [ t ]
    | Group_by (_, _, t) -> [ t ]
    | Remove_index (_, t) -> [ t ]
  ;;

  let rec number_of_constructors t =
    1 + List.sum (module Int) (nested t) ~f:number_of_constructors
  ;;

  (* Simple shrinker to whittle away unnecessary enclosing constructors. *)
  let quickcheck_shrinker (type elt) (_ : elt Shrinker.t) : elt t Shrinker.t =
    Shrinker.create (fun t -> Sequence.of_list (nested t))
  ;;

  let elts t ~compare =
    let rec elts = function
      | Empty -> []
      | Singleton elt -> [ elt ]
      | Of_sorted_array list -> list
      | Of_sorted_array_unchecked list -> list
      | Of_increasing_iterator_unchecked list -> list
      | Of_list (_, list) -> list
      | Of_array (_, list) -> list
      | Of_sequence (_, list) -> list
      | Add (_, elt, t) -> elt :: elts t
      | Remove (elt, t) -> elt :: elts t
      | Map (_, fn, t) -> Func.inputs fn @ Func.outputs fn @ elts t
      | Filter (fn, t) -> Func.inputs fn @ elts t
      | Filter_map (_, fn, t) ->
        Func.inputs fn @ List.filter_opt (Func.outputs fn) @ elts t
      | Partition_tf (_, fn, t) -> Func.inputs fn @ elts t
      | Diff (a, b) -> elts a @ elts b
      | Inter (a, b) -> elts a @ elts b
      | Union (_, a, b) -> elts a @ elts b
      | Union_list (_, list) -> List.concat_map ~f:elts list
      | Split (_, elt, t) -> elt :: elts t
      | Split_le_gt (_, elt, t) -> elt :: elts t
      | Split_lt_ge (_, elt, t) -> elt :: elts t
      | Group_by (fn, _, t) -> Func.inputs fn @ elts t
      | Remove_index (_, t) -> elts t
    in
    List.dedup_and_sort ~compare (elts t)
  ;;

  let rec map : type elt1 elt2. elt1 t -> f:(elt1 -> elt2) -> elt2 t =
    fun t ~f ->
    match t with
    | Empty -> Empty
    | Singleton elt -> Singleton (f elt)
    | Of_sorted_array list -> Of_sorted_array (List.map ~f:(fun elt -> f elt) list)
    | Of_sorted_array_unchecked list ->
      Of_sorted_array_unchecked (List.map ~f:(fun elt -> f elt) list)
    | Of_increasing_iterator_unchecked list ->
      Of_increasing_iterator_unchecked (List.map ~f:(fun elt -> f elt) list)
    | Of_list (overlap, list) -> Of_list (overlap, List.map ~f:(fun elt -> f elt) list)
    | Of_array (overlap, list) -> Of_array (overlap, List.map ~f:(fun elt -> f elt) list)
    | Of_sequence (overlap, list) ->
      Of_sequence (overlap, List.map ~f:(fun elt -> f elt) list)
    | Add (overlap, elt, t) -> Add (overlap, f elt, map ~f t)
    | Remove (elt, t) -> Remove (f elt, map ~f t)
    | Map (overlap, fn, t) -> Map (overlap, Func.map ~i:f ~o:f fn, map ~f t)
    | Filter (fn, t) -> Filter (Func.map ~i:f ~o:Fn.id fn, map ~f t)
    | Filter_map (overlap, fn, t) ->
      Filter_map (overlap, Func.map ~i:f ~o:(Option.map ~f) fn, map ~f t)
    | Partition_tf (side, fn, t) ->
      Partition_tf (side, Func.map ~i:f ~o:Fn.id fn, map ~f t)
    | Diff (a, b) -> Diff (map ~f a, map ~f b)
    | Inter (a, b) -> Inter (map ~f a, map ~f b)
    | Union (overlap, a, b) -> Union (overlap, map ~f a, map ~f b)
    | Union_list (overlap, list) -> Union_list (overlap, List.map ~f:(map ~f) list)
    | Split (side, elt, t) -> Split (side, f elt, map ~f t)
    | Split_le_gt (side, elt, t) -> Split_le_gt (side, f elt, map ~f t)
    | Split_lt_ge (side, elt, t) -> Split_lt_ge (side, f elt, map ~f t)
    | Group_by (fn, i, t) -> Group_by (Func.map ~i:f ~o:Fn.id fn, i, map ~f t)
    | Remove_index (i, t) -> Remove_index (i, map ~f t)
  ;;

  let _ = map

  module Make (Instance : Instance) (Impl : Impl with module Types := Instance.Types) =
  struct
    module Elt = struct
      include Instance.Elt

      let get = to_int
      let set _ n = of_int n
    end

    let unique_elt = Adjustable.unique (module Elt)
    let non_overlapping_elts = Adjustable.non_overlapping (module Elt)
    let elts t = elts t ~compare:Elt.compare

    (* [value (normalize t)] is guaranteed not to raise, at least for [t] from
       [quickcheck_generator]. We rely on elt values being small so we don't have to deal
       with integer overflow. *)
    let rec normalize = function
      | Empty -> Empty
      | Singleton elt -> Singleton elt
      | Of_sorted_array list ->
        let list =
          non_overlapping_elts list |> sort_either_direction ~compare:Elt.compare
        in
        Of_sorted_array list
      | Of_sorted_array_unchecked list ->
        let list =
          non_overlapping_elts list |> sort_either_direction ~compare:Elt.compare
        in
        Of_sorted_array_unchecked list
      | Of_increasing_iterator_unchecked list ->
        let list = non_overlapping_elts list |> List.sort ~compare:Elt.compare in
        Of_increasing_iterator_unchecked list
      | Of_list (overlap, list) ->
        let list =
          match overlap with
          | Allow_overlap -> list
          | Disallow_overlap -> non_overlapping_elts list
        in
        Of_list (overlap, list)
      | Of_array (overlap, list) ->
        let list =
          match overlap with
          | Allow_overlap -> list
          | Disallow_overlap -> non_overlapping_elts list
        in
        Of_array (overlap, list)
      | Of_sequence (overlap, list) ->
        let list =
          match overlap with
          | Allow_overlap -> list
          | Disallow_overlap -> non_overlapping_elts list
        in
        Of_sequence (overlap, list)
      | Add (overlap, elt, t) ->
        let t = normalize t in
        let elt =
          match overlap with
          | Allow_overlap -> elt
          | Disallow_overlap -> unique_elt elt (elts t)
        in
        Add (overlap, elt, t)
      | Remove (elt, t) -> Remove (elt, normalize t)
      | Map (overlap, fn, t) ->
        let t = normalize t in
        let fn =
          match overlap with
          | Allow_overlap -> fn
          | Disallow_overlap -> Func.injective fn (module Elt) (module Elt) (elts t)
        in
        Map (overlap, fn, t)
      | Filter (fn, t) -> Filter (fn, normalize t)
      | Filter_map (overlap, fn, t) ->
        let t = normalize t in
        let fn =
          match overlap with
          | Allow_overlap -> fn
          | Disallow_overlap ->
            Func.injective
              fn
              (module Elt)
              (module struct
                type t = Elt.t option

                let get = function
                  | None -> 0
                  | Some e -> 1 + Elt.get e
                ;;

                let set _ n = if n <= 0 then None else Some (Elt.of_int (n - 1))
              end)
              (elts t)
        in
        Filter_map (overlap, fn, t)
      | Partition_tf (side, fn, t) -> Partition_tf (side, fn, normalize t)
      | Diff (a, b) -> Diff (normalize a, normalize b)
      | Inter (a, b) -> Inter (normalize a, normalize b)
      | Union (overlap, a, b) ->
        let a = normalize a
        and b = normalize b in
        let rename_a, rename_b =
          List.concat
            [ List.map (elts a) ~f:(fun elt -> Side.Left, (elt, elt))
            ; List.map (elts b) ~f:(fun elt -> Side.Right, (elt, elt))
            ]
          |> Adjustable.non_overlapping
               (module struct
                 type t = Side.t * (Elt.t * Elt.t)

                 let get (_, (_, elt)) = Elt.to_int elt
                 let set (side, (elt, _)) int = side, (elt, Elt.of_int int)
               end)
          |> List.partition_map ~f:(function
            | Left, pair -> First pair
            | Right, pair -> Second pair)
        in
        let a = map a ~f:(fun e -> List.Assoc.find_exn rename_a e ~equal:Elt.equal)
        and b = map b ~f:(fun e -> List.Assoc.find_exn rename_b e ~equal:Elt.equal) in
        Union (overlap, a, b)
      | Union_list (overlap, list) ->
        let list = List.map ~f:normalize list in
        let list =
          match overlap with
          | Allow_overlap -> list
          | Disallow_overlap ->
            let renamings =
              let alist =
                List.concat_mapi list ~f:(fun i t -> List.map (elts t) ~f:(fun e -> i, e))
                |> Adjustable.non_overlapping
                     (module struct
                       type t = int * Elt.t

                       let get (_, elt) = Elt.to_int elt
                       let set (i, _) n = i, Elt.of_int n
                     end)
              in
              List.mapi list ~f:(fun i t ->
                List.zip_exn
                  (elts t)
                  (List.filter_map alist ~f:(fun (j, e) -> Option.some_if (i = j) e)))
            in
            List.map2_exn renamings list ~f:(fun renaming t ->
              map t ~f:(fun e -> List.Assoc.find_exn renaming e ~equal:Elt.equal))
        in
        Union_list (overlap, list)
      | Split (side, elt, t) -> Split (side, elt, normalize t)
      | Split_le_gt (side, elt, t) -> Split_le_gt (side, elt, normalize t)
      | Split_lt_ge (side, elt, t) -> Split_lt_ge (side, elt, normalize t)
      | Group_by (fn, n, t) -> Group_by (fn, n, normalize t)
      | Remove_index (i, t) -> Remove_index (i, normalize t)
    ;;

    let rec value =
      let open Instance in
      function
      | Empty -> create Impl.empty
      | Singleton elt -> (create Impl.singleton) elt
      | Of_sorted_array list ->
        (create Impl.of_sorted_array) (Array.of_list list) |> Or_error.ok_exn
      | Of_sorted_array_unchecked list ->
        (create Impl.of_sorted_array_unchecked) (Array.of_list list)
      | Of_increasing_iterator_unchecked list ->
        let array = Array.of_list list in
        (create Impl.of_increasing_iterator_unchecked)
          ~len:(Array.length array)
          ~f:(Array.get array)
      | Of_list (_, list) -> (create Impl.of_list) list
      | Of_array (_, list) -> (create Impl.of_array) (Array.of_list list)
      | Of_sequence (_, list) -> (create Impl.of_sequence) (Sequence.of_list list)
      | Add (_, elt, t) -> (access Impl.add) (value t) elt
      | Remove (elt, t) -> (access Impl.remove) (value t) elt
      | Map (_, fn, t) ->
        (create Impl.map) (value t) ~f:(fun elt -> Func.apply fn (module Elt) elt)
      | Filter (fn, t) -> Impl.filter (value t) ~f:(Func.apply fn (module Elt))
      | Filter_map (_, fn, t) ->
        (create Impl.filter_map) (value t) ~f:(fun elt -> Func.apply fn (module Elt) elt)
      | Partition_tf (side, fn, t) ->
        Impl.partition_tf (value t) ~f:(Func.apply fn (module Elt)) |> Side.select side
      | Diff (a, b) -> (access Impl.diff) (value a) (value b)
      | Inter (a, b) -> (access Impl.inter) (value a) (value b)
      | Union (_, a, b) -> (access Impl.union) (value a) (value b)
      | Union_list (_, list) -> (create Impl.union_list) (List.map ~f:value list)
      | Split (side, elt, t) -> (access Impl.split) (value t) elt |> Side.select3 side
      | Split_le_gt (side, elt, t) ->
        (access Impl.split_le_gt) (value t) elt |> Side.select side
      | Split_lt_ge (side, elt, t) ->
        (access Impl.split_lt_ge) (value t) elt |> Side.select side
      | Group_by (fn, n, t) ->
        let list =
          (Impl.group_by [@alert "-deprecated"])
            (value t)
            ~equiv:(Comparable.lift Int.equal ~f:(Func.apply fn (module Elt)))
        in
        List.nth list n |> Option.value ~default:(create Impl.empty)
      | Remove_index (n, t) -> (access Impl.remove_index) (value t) n
    ;;

    (* Report normalization failure using a small input if possible. *)
    let rec shrink_and_raise ~src ~dst ~exn =
      Shrinker.shrink [%shrinker: _ t] src
      |> Sequence.find_map ~f:(fun src ->
        let dst = normalize src in
        match value dst with
        | _ -> None
        | exception exn -> Some (src, dst, exn))
      |> function
      | Some (src, dst, exn) -> shrink_and_raise ~src ~dst ~exn
      | None ->
        raise_s
          [%message "normalization fails" (src : Elt.t t) (dst : Elt.t t) (exn : exn)]
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
module Make (Instance : Instance) (Impl : Impl with module Types := Instance.Types) =
struct
  open Instance

  type t = Elt.t Constructor.t [@@deriving equal, quickcheck]

  include Constructor.Make (Instance) (Impl)

  let sexp_of_t cons =
    let value = value cons in
    [%sexp { value : Instance.t; cons : Elt.t Constructor.t }]
  ;;

  let quickcheck_generator = Generator.map quickcheck_generator ~f:normalize
end

(* Coverage testing so we know we haven't forgotten any constructor functions.

   The number of quickcheck tests on 32-bit architectures is configured to be lower, so we
   skip the coverage checks there. *)
module%test [@tags "64-bits-only"] _ : Impl = struct
  module Types = struct
    type 'elt elt = 'elt
    type 'cmp cmp = 'cmp
    type ('elt, 'cmp) set = ('elt, 'cmp) Set.t
    type ('elt, 'cmp) t = ('elt, 'cmp) Set.t
    type ('elt, 'cmp) tree = ('elt, 'cmp) Set.Using_comparator.Tree.t

    type ('elt, 'cmp, 'fn) create_options =
      ('elt, 'cmp, 'fn) Set.With_first_class_module.t

    type ('elt, 'cmp, 'fn) access_options = ('elt, 'cmp, 'fn) Set.Without_comparator.t
  end

  open struct
    module Elt = struct
      include Int

      type t = int [@@deriving quickcheck]
    end

    module Instance = struct
      module Types = Types
      module Elt = Elt

      type t = (int, Int.comparator_witness) Set.t

      let compare = Set.compare_direct
      let equal = Set.equal
      let sexp_of_t t = Set.sexp_of_m__t (module Elt) t
      let create f = f ((module Elt) : (_, _) Comparator.Module.t)
      let access f = f
    end

    module Cons = Make (Instance) (Set)

    let sample = Memo.memoize Cons.quickcheck_generator

    let%expect_test "normalization" =
      quickcheck_m
        (module struct
          include Cons

          let sample = sample
        end)
        ~f:(fun t ->
          require_equal (module Cons) t (Cons.normalize t);
          require_does_not_raise (fun () -> ignore (Cons.value t : Set.M(Int).t)))
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
         50 |    9 |  5119
         75 |   38 |  2510
         90 |   80 |  1004
         95 |  129 |   505
         99 |  274 |   100
        100 |  718 |     1
        |}]
    ;;

    let test predicate =
      let stats = Stats.create () in
      List.iter (force sample) ~f:(fun t ->
        if predicate t
        then (
          let size = Set.length (Cons.value t) in
          Stats.add stats size));
      Stats.print stats
    ;;
  end

  (* Accessors only, not covered. *)

  include (Set : Accessors with module Types := Types)

  (* Complicated types, not covered. *)

  let of_tree = Set.of_tree

  (* Tests *)

  let empty = Set.empty

  let%expect_test _ =
    test (function
      | Empty -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   198
      |}]
  ;;

  let singleton = Set.singleton

  let%expect_test _ =
    test (function
      | Singleton _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
      100 |    1 |   181
      |}]
  ;;

  let of_sorted_array = Set.of_sorted_array

  let%expect_test _ =
    test (function
      | Of_sorted_array _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    25
      ----+------+------
        0 |    1 |   152
       50 |    8 |    78
       75 |   16 |    38
       90 |   23 |    16
       95 |   28 |     8
       99 |   30 |     2
      100 |   31 |     1
      |}]
  ;;

  let of_sorted_array_unchecked = Set.of_sorted_array_unchecked

  let%expect_test _ =
    test (function
      | Of_sorted_array_unchecked _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    29
      ----+------+------
        0 |    1 |   172
       50 |   10 |    86
       75 |   19 |    47
       90 |   25 |    20
       95 |   29 |     9
       99 |   30 |     7
      100 |   31 |     1
      |}]
  ;;

  let of_increasing_iterator_unchecked = Set.of_increasing_iterator_unchecked

  let%expect_test _ =
    test (function
      | Of_increasing_iterator_unchecked _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    40
      ----+------+------
        0 |    1 |   143
       50 |    9 |    76
       75 |   19 |    36
       90 |   25 |    18
       95 |   27 |    10
       99 |   29 |     4
      100 |   31 |     1
      |}]
  ;;

  let of_list = Set.of_list

  let%expect_test _ =
    test (function
      | Of_list _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    32
      ----+------+------
        0 |    1 |   159
       50 |    4 |    85
       75 |    6 |    41
       90 |   16 |    16
       95 |   24 |    11
       99 |   29 |     2
      100 |   31 |     1
      |}]
  ;;

  let of_array = Set.of_array

  let%expect_test _ =
    test (function
      | Of_array _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    27
      ----+------+------
        0 |    1 |   165
       50 |    4 |    86
       75 |   12 |    42
       90 |   21 |    17
       95 |   24 |     9
      100 |   30 |     2
      |}]
  ;;

  let of_sequence = Set.of_sequence

  let%expect_test _ =
    test (function
      | Of_sequence _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    34
      ----+------+------
        0 |    1 |   154
       50 |    4 |    82
       75 |    8 |    40
       90 |   20 |    16
       95 |   26 |     8
       99 |   30 |     5
      100 |   31 |     1
      |}]
  ;;

  let add = Set.add

  let%expect_test _ =
    test (function
      | Add _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
       50 |    1 |   574
       75 |    3 |   155
       90 |    8 |    59
       95 |   12 |    29
       99 |   25 |     6
      100 |   29 |     1
      |}]
  ;;

  let remove = Set.remove

  let%expect_test _ =
    test (function
      | Remove _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   325
      ----+------+------
        0 |    1 |   238
       50 |    2 |   146
       75 |    8 |    62
       90 |   17 |    25
       95 |   21 |    12
       99 |   27 |     3
      100 |   28 |     1
      |}]
  ;;

  let map = Set.map

  let%expect_test _ =
    test (function
      | Map _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   295
      ----+------+------
        0 |    1 |   249
       50 |    2 |   130
       75 |    3 |    79
       90 |    8 |    26
       95 |   12 |    13
       99 |   24 |     4
      100 |   29 |     1
      |}]
  ;;

  let filter = Set.filter

  let%expect_test _ =
    test (function
      | Filter _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   404
      ----+------+------
        0 |    1 |   145
       50 |    2 |    82
       75 |    5 |    38
       90 |   14 |    15
       95 |   19 |     8
       99 |   23 |     3
      100 |   25 |     1
      |}]
  ;;

  let filter_map = Set.filter_map

  let%expect_test _ =
    test (function
      | Filter_map _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   350
      ----+------+------
        0 |    1 |   207
       50 |    2 |   107
       75 |    3 |    63
       90 |   12 |    21
       95 |   14 |    14
       99 |   24 |     4
      100 |   27 |     1
      |}]
  ;;

  let partition_tf = Set.partition_tf

  let%expect_test _ =
    test (function
      | Partition_tf _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   411
      ----+------+------
        0 |    1 |   155
       50 |    2 |    86
       75 |    4 |    43
       90 |   13 |    17
       95 |   17 |     8
       99 |   25 |     3
      100 |   28 |     1
      |}]
  ;;

  let diff = Set.diff

  let%expect_test _ =
    test (function
      | Diff _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   348
      ----+------+------
        0 |    1 |   218
       50 |    2 |   135
       75 |    7 |    55
       90 |   14 |    23
       95 |   18 |    13
      100 |   29 |     3
      |}]
  ;;

  let inter = Set.inter

  let%expect_test _ =
    test (function
      | Inter _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   519
      ----+------+------
       50 |    1 |    63
       75 |    2 |    30
       90 |    6 |     7
       95 |    8 |     4
      100 |   25 |     1
      |}]
  ;;

  let union = Set.union

  let%expect_test _ =
    test (function
      | Union _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   173
      ----+------+------
        0 |    1 |   395
       50 |    3 |   229
       75 |    9 |   104
       90 |   17 |    45
       95 |   25 |    22
       99 |   37 |     4
      100 |   48 |     2
      |}]
  ;;

  let union_list = Set.union_list

  let%expect_test _ =
    test (function
      | Union_list _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |    88
      ----+------+------
        0 |    1 |   448
       50 |    3 |   251
       75 |    6 |   120
       90 |   10 |    53
       95 |   13 |    30
       99 |   19 |     5
      100 |   24 |     1
      |}]
  ;;

  let split = Set.split

  let%expect_test _ =
    test (function
      | Split _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   413
      ----+------+------
        0 |    1 |   155
       50 |    2 |    88
       75 |    5 |    39
       90 |   13 |    16
       95 |   16 |    10
       99 |   26 |     2
      100 |   27 |     1
      |}]
  ;;

  let split_le_gt = Set.split_le_gt

  let%expect_test _ =
    test (function
      | Split_le_gt _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   401
      ----+------+------
        0 |    1 |   160
       50 |    2 |    91
       75 |    5 |    44
       90 |   11 |    16
       95 |   14 |     9
       99 |   17 |     4
      100 |   24 |     1
      |}]
  ;;

  let split_lt_ge = Set.split_lt_ge

  let%expect_test _ =
    test (function
      | Split_lt_ge _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   427
      ----+------+------
        0 |    1 |   172
       50 |    2 |    88
       75 |    4 |    54
       90 |   10 |    19
       95 |   13 |    10
       99 |   26 |     2
      100 |   36 |     1
      |}]
  ;;

  let group_by = (Set.group_by [@alert "-deprecated"])

  let%expect_test _ =
    test (function
      | Group_by _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   559
      ----+------+------
        0 |    1 |     9
       50 |    2 |     6
       75 |    4 |     3
      100 |   14 |     1
      |}]
  ;;

  let remove_index = Set.remove_index

  let%expect_test _ =
    test (function
      | Remove_index _ -> true
      | _ -> false);
    [%expect
      {|
        % | size | count
      ----+------+------
        - |    0 |   304
      ----+------+------
        0 |    1 |   284
       50 |    2 |   157
       75 |    6 |    75
       90 |   13 |    32
       95 |   19 |    15
       99 |   26 |     4
      100 |   27 |     2
      |}]
  ;;
end
