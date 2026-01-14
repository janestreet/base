open! Base
open Expect_test_helpers_base
open Base_test_coverage_helpers
open Functor
open Set
open Overrides

open struct
  (** Instantiating key and data both as [int]. *)
  module Instance = struct
    include Generate (Instances.Toplevel) (Base.Set)

    let sample = Memo.memoize [%generator: t]
  end

  module Instance_pair = struct
    include Data.Pair (Instance)

    let sample = Memo.memoize [%generator: t]
  end

  module Tree_int = struct
    include Generate (Instances.Tree) (Base.Set.Using_comparator.Tree)

    let sample = Memo.memoize [%generator: t]
  end
end

(** module types *)

module type Accessors_generic = Accessors_generic
module type Transformers_generic = Transformers_generic

module type Creators_and_accessors_and_transformers_generic =
  Creators_and_accessors_and_transformers_generic

module type Creators_generic = Creators_generic
module type%template Elt_plain = Set.Elt_plain [@mode m] [@@mode m = (local, global)]
module type For_deriving = For_deriving
module type S_poly = S_poly

(** type-only modules for module type instantiation - untested *)

module With_comparator = With_comparator
module With_first_class_module = With_first_class_module
module Without_comparator = Without_comparator

(** supporting datatypes - untested *)

module Merge_to_sequence_element = Merge_to_sequence_element

(** types *)

type nonrec ('e, 'c) t = ('e, 'c) t

(** module types for ppx deriving *)

module type Compare_m = Compare_m
module type Equal_m = Equal_m
module type Hash_fold_m = Hash_fold_m
module type Globalize_m = Globalize_m
module type M_sexp_grammar = M_sexp_grammar
module type M_of_sexp = M_of_sexp
module type%template [@alloc a = (heap, stack)] Sexp_of_m = Sexp_of_m [@alloc a]

(** functor for ppx deriving - tested below *)

module M = M

(** globalizing *)

let globalize = globalize
let globalize0 = globalize0
let globalize_m__t = globalize_m__t

let%expect_test _ =
  quickcheck_m (module Instance) ~f:(fun t ->
    let t = Instance.value t in
    let round_trip = globalize0 t in
    require_equal (module Instance.Value) round_trip t);
  [%expect {| |}]
;;

(** sexp conversions and grammar *)

let%template[@alloc a = (heap, stack)] sexp_of_m__t = (sexp_of_m__t [@alloc a])
let m__t_of_sexp = m__t_of_sexp

let%expect_test _ =
  quickcheck_m (module Instance) ~f:(fun t ->
    let t = Instance.value t in
    let sexp = [%sexp_of: M(Int).t] t in
    require_equal (module Sexp) sexp [%sexp (to_list t : int list)];
    let round_trip = [%of_sexp: M(Int).t] sexp in
    require_equal (module Instance.Value) round_trip t);
  [%expect {| |}]
;;

let m__t_sexp_grammar = m__t_sexp_grammar

let%expect_test _ =
  print_s [%sexp ([%sexp_grammar: M(Int).t] : _ Sexp_grammar.t)];
  [%expect {| (List (Many Integer)) |}]
;;

(** comparisons *)

let compare = compare
let compare__local = compare__local
let compare_m__t = compare_m__t
let compare_m__t__local = compare_m__t__local
let equal_m__t = equal_m__t
let equal_m__t__local = equal_m__t__local

let%expect_test _ =
  quickcheck_m (module Instance_pair) ~f:(fun (a, b) ->
    let a = Instance.value a in
    let b = Instance.value b in
    require_equal
      (module Ordering)
      (Ordering.of_int ([%compare: M(Int).t] a b))
      (Ordering.of_int ([%compare: int list] (to_list a) (to_list b)));
    require_equal
      (module Ordering)
      (Ordering.of_int ([%compare: M(Int).t] a b))
      (Ordering.of_int (compare Int.compare [%compare: _] a b));
    require_equal
      (module Bool)
      ([%equal: M(Int).t] a b)
      ([%equal: int list] (to_list a) (to_list b)));
  [%expect {| |}]
;;

(** hash functions *)

let hash_m__t = hash_m__t
let hash_fold_m__t = hash_fold_m__t
let hash_fold_direct = hash_fold_direct

let%expect_test _ =
  quickcheck_m (module Instance) ~f:(fun t ->
    let t = Instance.value t in
    let actual_m = [%hash: M(Int).t] t in
    let actual_fold_m = Hash.run [%hash_fold: M(Int).t] t in
    let actual_fold_direct = Hash.run (hash_fold_direct Int.hash_fold_t) t in
    let expect = Hash.run [%hash_fold: int list] (to_list t) in
    require_equal (module Int) actual_m expect;
    require_equal (module Int) actual_fold_m expect;
    require_equal (module Int) actual_fold_direct expect);
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

module Named = struct
  type 'a t = 'a Set.Named.t =
    { set : 'a
    ; name : string
    }

  include Named
end

(** polymorphic comparison interface *)
module Poly = struct
  open Poly

  type nonrec 'e t = 'e t

  include (
    Test_poly_accessors : Functor.Accessors with module Types := Instances.Types.Poly)

  include (
    Test_poly_creators : Functor.Creators with module Types := Instances.Types.Poly)

  include (
    Test_poly_transformers :
      Functor.Transformers with module Types := Instances.Types.Poly)
end

(** Portable empty *)

let%template empty = (empty [@mode portable]) [@@mode portable]

let%expect_test "" =
  require_equal
    (module Sexp)
    [%sexp ((empty [@mode portable]) (module Sexp) : Set.M(Sexp).t)]
    [%sexp []]
;;

(** tree interface *)

module Tree = struct
  open Tree

  (** type *)

  type nonrec weight = weight

  type nonrec ('e, 'c) t = ('e, 'c) t = private
    | Empty
    | Leaf of { global_ elt : 'e }
    | Node of
        { global_ left : ('e, 'c) t
        ; global_ elt : 'e
        ; global_ right : ('e, 'c) t
        ; weight : weight
        }

  (** globalizing *)

  let globalize = globalize
  let globalize0 = globalize0

  let%expect_test _ =
    quickcheck_m (module Tree_int) ~f:(fun tree ->
      let tree = Tree_int.value tree in
      let round_trip = globalize0 tree in
      require_equal (module Tree_int.Value) round_trip tree)
  ;;

  (** sexp conversions *)

  let sexp_of_t = sexp_of_t
  let t_of_sexp_direct = t_of_sexp_direct

  let%expect_test _ =
    quickcheck_m (module Tree_int) ~f:(fun tree ->
      let tree = Tree_int.value tree in
      let sexp = sexp_of_t Int.sexp_of_t [%sexp_of: _] tree in
      require_equal
        (module Sexp)
        sexp
        ([%sexp_of: Set.M(Int).t]
           (Using_comparator.of_tree tree ~comparator:Int.comparator));
      let round_trip = t_of_sexp_direct ~comparator:Int.comparator Int.t_of_sexp sexp in
      require_equal (module Tree_int.Value) round_trip tree)
  ;;

  (** polymorphic constructor - untested *)

  let empty_without_value_restriction = empty_without_value_restriction

  (** expert interface - untested *)

  module Expert = Set.Tree.Expert

  (** creators and accessors and transformers *)

  include (
    Test_tree_accessors : Functor.Accessors with module Types := Instances.Types.Tree)

  include (
    Test_tree_creators : Functor.Creators with module Types := Instances.Types.Tree)

  include (
    Test_tree_transformers :
      Functor.Transformers with module Types := Instances.Types.Tree)
end

(** comparator interface *)

module Using_comparator = struct
  open Using_comparator

  (** type *)

  type nonrec ('e, 'c) t = ('e, 'c) t

  (** comparator accessor - untested *)

  let comparator = comparator
  let comparator_s = comparator_s

  (** sexp conversions *)

  let sexp_of_t = sexp_of_t
  let t_of_sexp_direct = t_of_sexp_direct

  let%expect_test _ =
    quickcheck_m (module Instance) ~f:(fun t ->
      let t = Instance.value t in
      let sexp = sexp_of_t Int.sexp_of_t [%sexp_of: _] t in
      require_equal (module Sexp) sexp ([%sexp_of: Set.M(Int).t] t);
      let round_trip = t_of_sexp_direct ~comparator:Int.comparator Int.t_of_sexp sexp in
      require_equal (module Instance.Value) round_trip t);
    [%expect {| |}]
  ;;

  (** hash function *)

  let hash_fold_direct = hash_fold_direct

  let%expect_test _ =
    quickcheck_m (module Instance) ~f:(fun t ->
      let t = Instance.value t in
      require_equal
        (module Int)
        (Hash.run (hash_fold_direct Int.hash_fold_t) t)
        (Hash.run [%hash_fold: Set.M(Int).t] t));
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

  module Tree = Tree
end

(** Enum module *)

module Private = struct
  module Enum = Test_enum
end
