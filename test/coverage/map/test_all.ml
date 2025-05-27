open! Base
open Base_quickcheck
open Expect_test_helpers_base
open Base_test_coverage_helpers
open Functor
open Map
open Overrides

open struct
  (** Instantiating key and data both as [int]. *)
  module Instance_int = struct
    include Generate (Instances.Toplevel) (Base.Map) (Data.Int)

    let sample = Memo.memoize [%generator: t]
  end

  module Instance_int_pair = struct
    include Data.Pair (Instance_int)

    let sample = Memo.memoize [%generator: t]
  end

  module Tree_int = struct
    include Generate (Instances.Tree) (Base.Map.Using_comparator.Tree) (Data.Int)

    let sample = Memo.memoize [%generator: t]
  end
end

(** module types *)

module type Accessors_generic = Accessors_generic
module type Transformers_generic = Transformers_generic

module type Creators_and_accessors_and_transformers_generic =
  Creators_and_accessors_and_transformers_generic

module type Creators_generic = Creators_generic
module type For_deriving = For_deriving
module type S_poly = S_poly

(** type-only modules for module type instantiation - untested *)

module With_comparator = With_comparator
module With_first_class_module = With_first_class_module
module Without_comparator = Without_comparator

(** supporting datatypes - untested *)

module Continue_or_stop = Continue_or_stop
module Finished_or_unfinished = Finished_or_unfinished
module Merge_element = Merge_element
module Or_duplicate = Or_duplicate
module Symmetric_diff_element = Symmetric_diff_element

(** private exports - untested *)

module Private = Private [@@alert "-map_private"]

(** types *)

type nonrec ('k, 'v, 'c) t = ('k, 'v, 'c) t

(** module types for ppx deriving *)

module type Compare_m = Compare_m
module type Equal_m = Equal_m
module type Hash_fold_m = Hash_fold_m
module type Globalize_m = Globalize_m
module type M_sexp_grammar = M_sexp_grammar
module type M_of_sexp = M_of_sexp
module type Sexp_of_m = Sexp_of_m

(** functor for ppx deriving - tested below *)

module M = M

(** Portable empty *)

let%template empty = (empty [@mode portable]) [@@mode portable]

let%expect_test "" =
  require_equal
    (module Sexp)
    [%sexp ((empty [@mode portable]) (module Sexp) : _ Map.M(Sexp).t)]
    [%sexp []]
;;

(** globalizing *)

let globalize = globalize
let globalize0 = globalize0
let globalize_m__t = globalize_m__t

let%expect_test _ =
  quickcheck_m
    (module Instance_int)
    ~f:(fun t ->
      let t = Instance_int.value t in
      let round_trip = globalize0 t in
      require_equal (module Instance_int.Value) round_trip t);
  [%expect {| |}]
;;

(** sexp conversions and grammar *)

let sexp_of_m__t = sexp_of_m__t
let m__t_of_sexp = m__t_of_sexp

let%expect_test _ =
  quickcheck_m
    (module Instance_int)
    ~f:(fun t ->
      let t = Instance_int.value t in
      let sexp = [%sexp_of: int M(Int).t] t in
      require_equal (module Sexp) sexp [%sexp (to_alist t : (int * int) list)];
      let round_trip = [%of_sexp: int M(Int).t] sexp in
      require_equal (module Instance_int.Value) round_trip t);
  [%expect {| |}]
;;

let m__t_sexp_grammar = m__t_sexp_grammar

let%expect_test _ =
  print_s [%sexp ([%sexp_grammar: int M(Int).t] : _ Sexp_grammar.t)];
  [%expect
    {|
    (Tagged (
      (key sexp_grammar.assoc)
      (value ())
      (grammar (
        List (
          Many (
            List (
              Cons
              (Tagged ((key sexp_grammar.assoc.key) (value ()) (grammar Integer)))
              (Cons
                (Tagged (
                  (key sexp_grammar.assoc.value) (value ()) (grammar Integer)))
                Empty))))))))
    |}]
;;

(** comparisons *)

let compare_m__t = compare_m__t
let equal_m__t = equal_m__t
let compare__local_m__t = compare__local_m__t
let equal__local_m__t = equal__local_m__t

let%expect_test _ =
  quickcheck_m
    (module Instance_int_pair)
    ~f:(fun (a, b) ->
      let a = Instance_int.value a in
      let b = Instance_int.value b in
      require_equal
        (module Ordering)
        (Ordering.of_int ([%compare: int M(Int).t] a b))
        (Ordering.of_int ([%compare: (int * int) list] (to_alist a) (to_alist b)));
      require_equal
        (module Bool)
        ([%equal: int M(Int).t] a b)
        ([%equal: (int * int) list] (to_alist a) (to_alist b)));
  [%expect {| |}]
;;

(** hash functions *)

let hash_fold_m__t = hash_fold_m__t
let hash_fold_direct = hash_fold_direct

let%expect_test _ =
  quickcheck_m
    (module Instance_int)
    ~f:(fun t ->
      let t = Instance_int.value t in
      let actual_m = Hash.run [%hash_fold: int M(Int).t] t in
      let actual_direct = Hash.run (hash_fold_direct Int.hash_fold_t Int.hash_fold_t) t in
      let expect = Hash.run [%hash_fold: (int * int) list] (to_alist t) in
      require_equal (module Int) actual_m expect;
      require_equal (module Int) actual_direct expect);
  [%expect {| |}]
;;

(** comparator accessors - untested *)

let comparator_s = comparator_s
let comparator = comparator

(** creators and accessors and transformers *)

include (
  Test_toplevel_accessors :
    Functor.Accessors with module Types := Instances.Types.Toplevel)

include (
  Test_toplevel_creators : Functor.Creators with module Types := Instances.Types.Toplevel)

include (
  Test_toplevel_transformers :
    Functor.Transformers with module Types := Instances.Types.Toplevel)

(** polymorphic comparison interface *)
module Poly = struct
  open Poly

  type nonrec ('k, 'v) t = ('k, 'v) t
  type nonrec ('k, 'v) tree = ('k, 'v) tree
  type nonrec comparator_witness = comparator_witness

  include (
    Test_poly_accessors : Functor.Accessors with module Types := Instances.Types.Poly)

  include (
    Test_poly_creators : Functor.Creators with module Types := Instances.Types.Poly)

  include (
    Test_poly_transformers :
      Functor.Transformers with module Types := Instances.Types.Poly)
end

(** comparator interface *)

module Using_comparator = struct
  open Using_comparator

  (** type *)

  type nonrec ('k, 'v, 'c) t = ('k, 'v, 'c) t

  (** comparator accessor - untested *)

  let comparator = comparator

  (** sexp conversions *)

  let sexp_of_t = sexp_of_t
  let t_of_sexp_direct = t_of_sexp_direct

  let%expect_test _ =
    quickcheck_m
      (module Instance_int)
      ~f:(fun t ->
        let t = Instance_int.value t in
        let sexp = sexp_of_t Int.sexp_of_t Int.sexp_of_t [%sexp_of: _] t in
        require_equal (module Sexp) sexp ([%sexp_of: int Map.M(Int).t] t);
        let round_trip =
          t_of_sexp_direct ~comparator:Int.comparator Int.t_of_sexp Int.t_of_sexp sexp
        in
        require_equal (module Instance_int.Value) round_trip t);
    [%expect {| |}]
  ;;

  (** hash function *)

  let hash_fold_direct = hash_fold_direct

  let%expect_test _ =
    quickcheck_m
      (module Instance_int)
      ~f:(fun t ->
        let t = Instance_int.value t in
        require_equal
          (module Int)
          (Hash.run (hash_fold_direct Int.hash_fold_t Int.hash_fold_t) t)
          (Hash.run [%hash_fold: int Map.M(Int).t] t));
    [%expect {| |}]
  ;;

  (** functor for polymorphic definition - untested *)

  module%template.portable
    [@modality p] Empty_without_value_restriction
      (Cmp : Comparator.S1) =
  struct
    open Empty_without_value_restriction [@modality p] (Cmp)

    let empty = empty
  end

  (** creators and accessors and transformers *)

  include (
    Test_using_comparator_accessors :
      Functor.Accessors with module Types := Instances.Types.Using_comparator)

  include (
    Test_using_comparator_creators :
      Functor.Creators with module Types := Instances.Types.Using_comparator)

  include (
    Test_using_comparator_transformers :
      Functor.Transformers with module Types := Instances.Types.Using_comparator)

  (** tree interface *)

  module Tree = struct
    open Tree

    (** type *)

    type nonrec ('k, 'v, 'c) t = ('k, 'v, 'c) t

    (** globalizing *)

    let globalize = globalize

    let%expect_test _ =
      quickcheck_m
        (module Tree_int)
        ~f:(fun tree ->
          let tree = Tree_int.value tree in
          let round_trip = globalize () () () tree in
          require_equal (module Tree_int.Value) round_trip tree);
      [%expect {| |}]
    ;;

    (** sexp conversions *)

    let sexp_of_t = sexp_of_t
    let t_of_sexp_direct = t_of_sexp_direct

    let%expect_test _ =
      quickcheck_m
        (module Tree_int)
        ~f:(fun tree ->
          let tree = Tree_int.value tree in
          let sexp = sexp_of_t Int.sexp_of_t Int.sexp_of_t [%sexp_of: _] tree in
          require_equal
            (module Sexp)
            sexp
            ([%sexp_of: int Map.M(Int).t]
               (Using_comparator.of_tree tree ~comparator:Int.comparator));
          let round_trip =
            t_of_sexp_direct ~comparator:Int.comparator Int.t_of_sexp Int.t_of_sexp sexp
          in
          require_equal (module Tree_int.Value) round_trip tree);
      [%expect {| |}]
    ;;

    (** polymorphic constructor - untested *)

    let empty_without_value_restriction = empty_without_value_restriction

    (** builders *)

    module Build_increasing = struct
      open Build_increasing

      type nonrec ('k, 'v, 'c) t = ('k, 'v, 'c) t

      (** tree builder functions *)

      let empty = empty
      let add_exn = add_exn
      let to_tree = to_tree

      let%expect_test _ =
        quickcheck_m
          (module struct
            type t =
              ((int[@generator Base_quickcheck.Generator.small_strictly_positive_int])
              * int)
                list
            [@@deriving quickcheck, sexp_of]

            let sample = Memo.memoize [%generator: t]
          end)
          ~f:(fun alist ->
            let actual =
              List.fold_result alist ~init:empty ~f:(fun builder (key, data) ->
                Or_error.try_with (fun () ->
                  add_exn builder ~comparator:Int.comparator ~key ~data))
              |> Or_error.map ~f:to_tree
            in
            Or_error.iter actual ~f:(fun map ->
              require (Tree.invariants map ~comparator:Int.comparator));
            let expect =
              match List.is_sorted_strictly alist ~compare:[%compare: int * _] with
              | false -> Error (Error.of_string "not sorted")
              | true ->
                Ok
                  (Map.Using_comparator.Tree.of_sequence_exn
                     ~comparator:Int.comparator
                     (Sequence.of_list alist))
            in
            require_equal (module Data.Or_error (Tree_int.Value)) actual expect);
        [%expect {| |}]
      ;;
    end

    (** creators and accessors and transformers *)

    include (
      Test_tree_accessors : Functor.Accessors with module Types := Instances.Types.Tree)

    include (
      Test_tree_creators : Functor.Creators with module Types := Instances.Types.Tree)

    include (
      Test_tree_transformers :
        Functor.Transformers with module Types := Instances.Types.Tree)
  end
end
