open! Import
open! T

module type Elt_plain = sig
  type t [@@deriving_inline compare, sexp_of]

  include Ppx_compare_lib.Comparable.S with type t := t

  val sexp_of_t : t -> Sexplib0.Sexp.t

  [@@@end]
end

module Without_comparator = Map_intf.Without_comparator
module With_comparator = Map_intf.With_comparator
module With_first_class_module = Map_intf.With_first_class_module
module Merge_to_sequence_element = Sequence.Merge_with_duplicates_element

module Named = struct
  type 'a t =
    { set : 'a
    ; name : string
    }
end

module type Accessors_generic = sig
  type ('a, 'cmp) t

  include Container.Generic with type ('a, 'cmp, _) t := ('a, 'cmp) t

  type ('a, 'cmp) tree

  (** The [access_options] type is used to make [Accessors_generic] flexible as to whether
      a comparator is required to be passed to certain functions. *)
  type ('a, 'cmp, 'z) access_options

  type 'cmp cmp

  val invariants : ('a, 'cmp, ('a, 'cmp) t -> bool) access_options

  (** override [Container]'s [mem] *)
  val mem : ('a, 'cmp, ('a, 'cmp) t -> 'a elt -> bool) access_options

  val add : ('a, 'cmp, ('a, 'cmp) t -> 'a elt -> ('a, 'cmp) t) access_options
  val remove : ('a, 'cmp, ('a, 'cmp) t -> 'a elt -> ('a, 'cmp) t) access_options
  val union : ('a, 'cmp, ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) t) access_options
  val inter : ('a, 'cmp, ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) t) access_options
  val diff : ('a, 'cmp, ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) t) access_options

  val symmetric_diff
    : ( 'a
      , 'cmp
      , ('a, 'cmp) t -> ('a, 'cmp) t -> ('a elt, 'a elt) Either.t Sequence.t )
      access_options

  val compare_direct : ('a, 'cmp, ('a, 'cmp) t -> ('a, 'cmp) t -> int) access_options
  val equal : ('a, 'cmp, ('a, 'cmp) t -> ('a, 'cmp) t -> bool) access_options
  val is_subset : ('a, 'cmp, ('a, 'cmp) t -> of_:('a, 'cmp) t -> bool) access_options
  val are_disjoint : ('a, 'cmp, ('a, 'cmp) t -> ('a, 'cmp) t -> bool) access_options

  module Named : sig
    val is_subset
      : ( 'a
        , 'cmp
        , ('a, 'cmp) t Named.t -> of_:('a, 'cmp) t Named.t -> unit Or_error.t )
        access_options

    val equal
      : ( 'a
        , 'cmp
        , ('a, 'cmp) t Named.t -> ('a, 'cmp) t Named.t -> unit Or_error.t )
        access_options
  end

  val fold_until
    :  ('a, _) t
    -> init:'acc
    -> f:('acc -> 'a elt -> ('acc, 'final) Container.Continue_or_stop.t)
    -> finish:('acc -> 'final)
    -> 'final

  val fold_right : ('a, _) t -> init:'acc -> f:('a elt -> 'acc -> 'acc) -> 'acc

  val iter2
    : ( 'a
      , 'cmp
      , ('a, 'cmp) t
        -> ('a, 'cmp) t
        -> f:([ `Left of 'a elt | `Right of 'a elt | `Both of 'a elt * 'a elt ] -> unit)
        -> unit )
      access_options

  val filter : ('a, 'cmp) t -> f:('a elt -> bool) -> ('a, 'cmp) t
  val partition_tf : ('a, 'cmp) t -> f:('a elt -> bool) -> ('a, 'cmp) t * ('a, 'cmp) t
  val elements : ('a, _) t -> 'a elt list
  val min_elt : ('a, _) t -> 'a elt option
  val min_elt_exn : ('a, _) t -> 'a elt
  val max_elt : ('a, _) t -> 'a elt option
  val max_elt_exn : ('a, _) t -> 'a elt
  val choose : ('a, _) t -> 'a elt option
  val choose_exn : ('a, _) t -> 'a elt

  val split
    : ( 'a
      , 'cmp
      , ('a, 'cmp) t -> 'a elt -> ('a, 'cmp) t * 'a elt option * ('a, 'cmp) t )
      access_options

  val split_le_gt
    : ('a, 'cmp, ('a, 'cmp) t -> 'a elt -> ('a, 'cmp) t * ('a, 'cmp) t) access_options

  val split_lt_ge
    : ('a, 'cmp, ('a, 'cmp) t -> 'a elt -> ('a, 'cmp) t * ('a, 'cmp) t) access_options

  val group_by : ('a, 'cmp) t -> equiv:('a elt -> 'a elt -> bool) -> ('a, 'cmp) t list
  val find_exn : ('a, _) t -> f:('a elt -> bool) -> 'a elt
  val nth : ('a, _) t -> int -> 'a elt option
  val remove_index : ('a, 'cmp, ('a, 'cmp) t -> int -> ('a, 'cmp) t) access_options
  val to_tree : ('a, 'cmp) t -> ('a, 'cmp) tree

  val to_sequence
    : ( 'a
      , 'cmp
      , ?order:[ `Increasing | `Decreasing ]
        -> ?greater_or_equal_to:'a elt
        -> ?less_or_equal_to:'a elt
        -> ('a, 'cmp) t
        -> 'a elt Sequence.t )
      access_options

  val binary_search
    : ( 'a
      , 'cmp
      , ('a, 'cmp) t
        -> compare:('a elt -> 'key -> int)
        -> Binary_searchable.Which_target_by_key.t
        -> 'key
        -> 'a elt option )
      access_options

  val binary_search_segmented
    : ( 'a
      , 'cmp
      , ('a, 'cmp) t
        -> segment_of:('a elt -> [ `Left | `Right ])
        -> Binary_searchable.Which_target_by_segment.t
        -> 'a elt option )
      access_options

  val merge_to_sequence
    : ( 'a
      , 'cmp
      , ?order:[ `Increasing | `Decreasing ]
        -> ?greater_or_equal_to:'a elt
        -> ?less_or_equal_to:'a elt
        -> ('a, 'cmp) t
        -> ('a, 'cmp) t
        -> ('a elt, 'a elt) Merge_to_sequence_element.t Sequence.t )
      access_options
end

module type Creators_generic = sig
  type ('a, 'cmp) t
  type ('a, 'cmp) set
  type ('a, 'cmp) tree
  type 'a elt
  type ('a, 'cmp, 'z) create_options
  type 'cmp cmp

  val empty : ('a, 'cmp, ('a, 'cmp) t) create_options
  val singleton : ('a, 'cmp, 'a elt -> ('a, 'cmp) t) create_options
  val union_list : ('a, 'cmp, ('a, 'cmp) t list -> ('a, 'cmp) t) create_options
  val of_list : ('a, 'cmp, 'a elt list -> ('a, 'cmp) t) create_options
  val of_sequence : ('a, 'cmp, 'a elt Sequence.t -> ('a, 'cmp) t) create_options
  val of_array : ('a, 'cmp, 'a elt array -> ('a, 'cmp) t) create_options
  val of_sorted_array : ('a, 'cmp, 'a elt array -> ('a, 'cmp) t Or_error.t) create_options
  val of_sorted_array_unchecked : ('a, 'cmp, 'a elt array -> ('a, 'cmp) t) create_options

  val of_increasing_iterator_unchecked
    : ('a, 'cmp, len:int -> f:(int -> 'a elt) -> ('a, 'cmp) t) create_options

  val stable_dedup_list : ('a, _, 'a elt list -> 'a elt list) create_options
    [@@deprecated "[since 2023-04] Use [List.stable_dedup] instead."]

  (** The types of [map] and [filter_map] are subtle.  The input set, [('a, _) set],
      reflects the fact that these functions take a set of *any* type, with any
      comparator, while the output set, [('b, 'cmp) t], reflects that the output set has
      the particular ['cmp] of the creation function.  The comparator can come in one of
      three ways, depending on which set module is used

      - [Set.map] -- comparator comes as an argument
      - [Set.Poly.map] -- comparator is polymorphic comparison
      - [Foo.Set.map] -- comparator is [Foo.comparator] *)
  val map : ('b, 'cmp, ('a, _) set -> f:('a -> 'b elt) -> ('b, 'cmp) t) create_options

  val filter_map
    : ('b, 'cmp, ('a, _) set -> f:('a -> 'b elt option) -> ('b, 'cmp) t) create_options

  val of_tree : ('a, 'cmp, ('a, 'cmp) tree -> ('a, 'cmp) t) create_options
end

module type Creators_and_accessors_generic = sig
  type ('elt, 'cmp) set
  type ('elt, 'cmp) t
  type ('elt, 'cmp) tree
  type 'elt elt
  type 'cmp cmp

  include
    Accessors_generic
      with type ('a, 'b) t := ('a, 'b) t
      with type ('a, 'b) tree := ('a, 'b) tree
      with type 'a elt := 'a elt
      with type 'cmp cmp := 'cmp cmp

  include
    Creators_generic
      with type ('a, 'b) set := ('a, 'b) set
      with type ('a, 'b) t := ('a, 'b) t
      with type ('a, 'b) tree := ('a, 'b) tree
      with type 'a elt := 'a elt
      with type 'cmp cmp := 'cmp cmp
end

module type S_poly = sig
  type ('elt, 'cmp) set
  type 'elt t
  type 'elt tree
  type comparator_witness

  include
    Creators_and_accessors_generic
      with type ('elt, 'cmp) set := ('elt, 'cmp) set
      with type ('elt, 'cmp) t := 'elt t
      with type ('elt, 'cmp) tree := 'elt tree
      with type 'a elt := 'a
      with type 'c cmp := comparator_witness
      with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) Without_comparator.t
      with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) Without_comparator.t
end

module type For_deriving = sig
  type ('a, 'b) t

  module type Sexp_of_m = sig
    type t [@@deriving_inline sexp_of]

    val sexp_of_t : t -> Sexplib0.Sexp.t

    [@@@end]
  end

  module type M_of_sexp = sig
    type t [@@deriving_inline of_sexp]

    val t_of_sexp : Sexplib0.Sexp.t -> t

    [@@@end]

    include Comparator.S with type t := t
  end

  module type M_sexp_grammar = sig
    type t [@@deriving_inline sexp_grammar]

    val t_sexp_grammar : t Sexplib0.Sexp_grammar.t

    [@@@end]
  end

  module type Compare_m = sig end
  module type Equal_m = sig end
  module type Hash_fold_m = Hasher.S

  val sexp_of_m__t : (module Sexp_of_m with type t = 'elt) -> ('elt, 'cmp) t -> Sexp.t

  val m__t_of_sexp
    :  (module M_of_sexp with type t = 'elt and type comparator_witness = 'cmp)
    -> Sexp.t
    -> ('elt, 'cmp) t

  val m__t_sexp_grammar
    :  (module M_sexp_grammar with type t = 'elt)
    -> ('elt, 'cmp) t Sexplib0.Sexp_grammar.t

  val compare_m__t : (module Compare_m) -> ('elt, 'cmp) t -> ('elt, 'cmp) t -> int
  val equal_m__t : (module Equal_m) -> ('elt, 'cmp) t -> ('elt, 'cmp) t -> bool

  val hash_fold_m__t
    :  (module Hash_fold_m with type t = 'elt)
    -> Hash.state
    -> ('elt, _) t
    -> Hash.state

  val hash_m__t : (module Hash_fold_m with type t = 'elt) -> ('elt, _) t -> int
end

module type Set = sig
  (** Sets based on {!Comparator.S}.

      Creators require a comparator argument to be passed in, whereas accessors use the
      comparator provided by the input set. *)

  (** The type of a set.  The first type parameter identifies the type of the element, and
      the second identifies the comparator, which determines the comparison function that
      is used for ordering elements in this set.  Many operations (e.g., {!union}),
      require that they be passed sets with the same element type and the same comparator
      type. *)
  type (!'elt, !'cmp) t [@@deriving_inline compare]

  include Ppx_compare_lib.Comparable.S2 with type (!'elt, !'cmp) t := ('elt, 'cmp) t

  [@@@end]

  (** Tests internal invariants of the set data structure.  Returns true on success. *)
  val invariants : (_, _) t -> bool

  (** Returns a first-class module that can be used to build other map/set/etc
      with the same notion of comparison. *)
  val comparator_s : ('a, 'cmp) t -> ('a, 'cmp) Comparator.Module.t

  val comparator : ('a, 'cmp) t -> ('a, 'cmp) Comparator.t

  (** Creates an empty set based on the provided comparator. *)
  val empty : ('a, 'cmp) Comparator.Module.t -> ('a, 'cmp) t

  (** Creates a set based on the provided comparator that contains only the provided
      element. *)
  val singleton : ('a, 'cmp) Comparator.Module.t -> 'a -> ('a, 'cmp) t

  (** Returns the cardinality of the set. [O(1)]. *)
  val length : (_, _) t -> int

  (** [is_empty t] is [true] iff [t] is empty.  [O(1)]. *)
  val is_empty : (_, _) t -> bool

  (** [mem t a] returns [true] iff [a] is in [t].  [O(log n)]. *)
  val mem : ('a, _) t -> 'a -> bool

  (** [add t a] returns a new set with [a] added to [t], or returns [t] if [mem t a].
      [O(log n)]. *)
  val add : ('a, 'cmp) t -> 'a -> ('a, 'cmp) t

  (** [remove t a] returns a new set with [a] removed from [t] if [mem t a], or returns [t]
      otherwise.  [O(log n)]. *)
  val remove : ('a, 'cmp) t -> 'a -> ('a, 'cmp) t

  (** [union t1 t2] returns the union of the two sets.  [O(length t1 + length t2)]. *)
  val union : ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) t

  (** [union_list c list] returns the union of all the sets in [list].  The
      [comparator] argument is required for the case where [list] is empty.
      [O(max(List.length list, n log n))], where [n] is the sum of sizes of the input sets. *)
  val union_list : ('a, 'cmp) Comparator.Module.t -> ('a, 'cmp) t list -> ('a, 'cmp) t

  (** [inter t1 t2] computes the intersection of sets [t1] and [t2].  [O(length t1 +
      length t2)]. *)
  val inter : ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) t

  (** [diff t1 t2] computes the set difference [t1 - t2], i.e., the set containing all
      elements in [t1] that are not in [t2].  [O(length t1 + length t2)]. *)
  val diff : ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) t

  (** [symmetric_diff t1 t2] returns a sequence of changes between [t1] and [t2]. It is
      intended to be efficient in the case where [t1] and [t2] share a large amount of
      structure. *)
  val symmetric_diff : ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'a) Either.t Sequence.t

  (** [compare_direct t1 t2] compares the sets [t1] and [t2].  It returns the same result
      as [compare], but unlike compare, doesn't require arguments to be passed in for the
      type parameters of the set.  [O(length t1 + length t2)]. *)
  val compare_direct : ('a, 'cmp) t -> ('a, 'cmp) t -> int

  (** Hash function: a building block to use when hashing data structures containing sets in
      them. [hash_fold_direct hash_fold_key] is compatible with [compare_direct] iff
      [hash_fold_key] is compatible with [(comparator s).compare] of the set [s] being
      hashed. *)
  val hash_fold_direct : 'a Hash.folder -> ('a, 'cmp) t Hash.folder

  (** [equal t1 t2] returns [true] iff the two sets have the same elements.  [O(length t1 +
      length t2)] *)
  val equal : ('a, 'cmp) t -> ('a, 'cmp) t -> bool

  (** [exists t ~f] returns [true] iff there exists an [a] in [t] for which [f a].  [O(n)],
      but returns as soon as it finds an [a] for which [f a]. *)
  val exists : ('a, _) t -> f:('a -> bool) -> bool

  (** [for_all t ~f] returns [true] iff for all [a] in [t], [f a].  [O(n)], but returns as
      soon as it finds an [a] for which [not (f a)]. *)
  val for_all : ('a, _) t -> f:('a -> bool) -> bool

  (** [count t] returns the number of elements of [t] for which [f] returns [true].
      [O(n)]. *)
  val count : ('a, _) t -> f:('a -> bool) -> int

  (** [sum t] returns the sum of [f t] for each [t] in the set.
      [O(n)]. *)
  val sum
    :  (module Container.Summable with type t = 'sum)
    -> ('a, _) t
    -> f:('a -> 'sum)
    -> 'sum

  (** [find t f] returns an element of [t] for which [f] returns true, with no guarantee as
      to which element is returned.  [O(n)], but returns as soon as a suitable element is
      found. *)
  val find : ('a, _) t -> f:('a -> bool) -> 'a option

  (** [find_map t f] returns [b] for some [a] in [t] for which [f a = Some b].  If no such
      [a] exists, then [find] returns [None].  [O(n)], but returns as soon as a suitable
      element is found. *)
  val find_map : ('a, _) t -> f:('a -> 'b option) -> 'b option

  (** Like [find], but throws an exception on failure. *)
  val find_exn : ('a, _) t -> f:('a -> bool) -> 'a

  (** [nth t i] returns the [i]th smallest element of [t], in [O(log n)] time.  The
      smallest element has [i = 0].  Returns [None] if [i < 0] or [i >= length t]. *)
  val nth : ('a, _) t -> int -> 'a option

  (** [remove_index t i] returns a version of [t] with the [i]th smallest element removed,
      in [O(log n)] time.  The smallest element has [i = 0].  Returns [t] if [i < 0] or
      [i >= length t]. *)
  val remove_index : ('a, 'cmp) t -> int -> ('a, 'cmp) t

  (** [is_subset t1 ~of_:t2] returns true iff [t1] is a subset of [t2]. *)
  val is_subset : ('a, 'cmp) t -> of_:('a, 'cmp) t -> bool

  (** [are_disjoint t1 t2] returns [true] iff [is_empty (inter t1 t2)], but is more
      efficient. *)
  val are_disjoint : ('a, 'cmp) t -> ('a, 'cmp) t -> bool

  (** [Named] allows the validation of subset and equality relationships between sets.  A
      [Named.t] is a record of a set and a name, where the name is used in error messages,
      and [Named.is_subset] and [Named.equal] validate subset and equality relationships
      respectively.

      The error message for, e.g.,
      {[
        Named.is_subset { set = set1; name = "set1" } ~of_:{set = set2; name = "set2" }
      ]}

      looks like
      {v
        ("set1 is not a subset of set2" (invalid_elements (...elements of set1 - set2...)))
       v}

      so [name] should be a noun phrase that doesn't sound awkward in the above error
      message.  Even though it adds verbosity, choosing [name]s that start with the phrase
      "the set of" often makes the error message sound more natural.
  *)
  module Named : sig
    type ('a, 'cmp) set := ('a, 'cmp) t

    type 'a t = 'a Named.t =
      { set : 'a
      ; name : string
      }

    (** [is_subset t1 ~of_:t2] returns [Ok ()] if [t1] is a subset of [t2] and a
        human-readable error otherwise.  *)
    val is_subset : ('a, 'cmp) set t -> of_:('a, 'cmp) set t -> unit Or_error.t

    (** [equal t1 t2] returns [Ok ()] if [t1] is equal to [t2] and a human-readable
        error otherwise.  *)
    val equal : ('a, 'cmp) set t -> ('a, 'cmp) set t -> unit Or_error.t
  end

  (** The list or array given to [of_list] and [of_array] need not be sorted. *)
  val of_list : ('a, 'cmp) Comparator.Module.t -> 'a list -> ('a, 'cmp) t

  val of_sequence : ('a, 'cmp) Comparator.Module.t -> 'a Sequence.t -> ('a, 'cmp) t
  val of_array : ('a, 'cmp) Comparator.Module.t -> 'a array -> ('a, 'cmp) t

  (** [to_list] and [to_array] produce sequences sorted in ascending order according to the
      comparator. *)
  val to_list : ('a, _) t -> 'a list

  val to_array : ('a, _) t -> 'a array

  (** Create set from sorted array.  The input must be sorted (either in ascending or
      descending order as given by the comparator) and contain no duplicates, otherwise the
      result is an error.  The complexity of this function is [O(n)]. *)
  val of_sorted_array
    :  ('a, 'cmp) Comparator.Module.t
    -> 'a array
    -> ('a, 'cmp) t Or_error.t

  (** Similar to [of_sorted_array], but without checking the input array. *)
  val of_sorted_array_unchecked
    :  ('a, 'cmp) Comparator.Module.t
    -> 'a array
    -> ('a, 'cmp) t

  (** [of_increasing_iterator_unchecked c ~len ~f] behaves like [of_sorted_array_unchecked c
      (Array.init len ~f)], with the additional restriction that a decreasing order is not
      supported.  The advantage is not requiring you to allocate an intermediate array.  [f]
      will be called with 0, 1, ... [len - 1], in order. *)
  val of_increasing_iterator_unchecked
    :  ('a, 'cmp) Comparator.Module.t
    -> len:int
    -> f:(int -> 'a)
    -> ('a, 'cmp) t

  (** [stable_dedup_list] is here rather than in the [List] module because the
      implementation relies crucially on sets, and because doing so allows one to avoid uses
      of polymorphic comparison by instantiating the functor at a different implementation
      of [Comparator] and using the resulting [stable_dedup_list]. *)
  val stable_dedup_list : ('a, _) Comparator.Module.t -> 'a list -> 'a list
    [@@deprecated "[since 2023-04] Use [List.stable_dedup] instead."]

  (** [map c t ~f] returns a new set created by applying [f] to every element in
      [t].  The returned set is based on the provided [comparator].  [O(n log n)]. *)
  val map : ('b, 'cmp) Comparator.Module.t -> ('a, _) t -> f:('a -> 'b) -> ('b, 'cmp) t

  (** Like {!map}, except elements for which [f] returns [None] will be dropped.  *)
  val filter_map
    :  ('b, 'cmp) Comparator.Module.t
    -> ('a, _) t
    -> f:('a -> 'b option)
    -> ('b, 'cmp) t

  (** [filter t ~f] returns the subset of [t] for which [f] evaluates to true.  [O(n log
      n)]. *)
  val filter : ('a, 'cmp) t -> f:('a -> bool) -> ('a, 'cmp) t

  (** [fold t ~init ~f] folds over the elements of the set from smallest to largest. *)
  val fold : ('a, _) t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc

  (** [fold_result ~init ~f] folds over the elements of the set from smallest to
      largest, short circuiting the fold if [f accum x] is an [Error _] *)
  val fold_result
    :  ('a, _) t
    -> init:'acc
    -> f:('acc -> 'a -> ('acc, 'e) Result.t)
    -> ('acc, 'e) Result.t

  (** [fold_until t ~init ~f] is a short-circuiting version of [fold]. If [f]
      returns [Stop _] the computation ceases and results in that value. If [f] returns
      [Continue _], the fold will proceed. *)
  val fold_until
    :  ('a, _) t
    -> init:'acc
    -> f:('acc -> 'a -> ('acc, 'final) Container.Continue_or_stop.t)
    -> finish:('acc -> 'final)
    -> 'final

  (** Like {!fold}, except that it goes from the largest to the smallest element. *)
  val fold_right : ('a, _) t -> init:'acc -> f:('a -> 'acc -> 'acc) -> 'acc

  (** [iter t ~f] calls [f] on every element of [t], going in order from the smallest to
      largest.  *)
  val iter : ('a, _) t -> f:('a -> unit) -> unit

  (** Iterate two sets side by side.  Complexity is [O(m+n)] where [m] and [n] are the sizes
      of the two input sets.  As an example, with the inputs [0; 1] and [1; 2], [f] will be
      called with [`Left 0]; [`Both (1, 1)]; and [`Right 2]. *)
  val iter2
    :  ('a, 'cmp) t
    -> ('a, 'cmp) t
    -> f:([ `Left of 'a | `Right of 'a | `Both of 'a * 'a ] -> unit)
    -> unit

  (** if [a, b = partition_tf set ~f] then [a] is the elements on which [f] produced [true],
      and [b] is the elements on which [f] produces [false]. *)
  val partition_tf : ('a, 'cmp) t -> f:('a -> bool) -> ('a, 'cmp) t * ('a, 'cmp) t

  (** Same as {!to_list}. *)
  val elements : ('a, _) t -> 'a list

  (** Returns the smallest element of the set.  [O(log n)]. *)
  val min_elt : ('a, _) t -> 'a option

  (** Like {!min_elt}, but throws an exception when given an empty set. *)
  val min_elt_exn : ('a, _) t -> 'a

  (** Returns the largest element of the set.  [O(log n)].  *)
  val max_elt : ('a, _) t -> 'a option

  (** Like {!max_elt}, but throws an exception when given an empty set. *)
  val max_elt_exn : ('a, _) t -> 'a

  (** returns an arbitrary element, or [None] if the set is empty. *)
  val choose : ('a, _) t -> 'a option

  (** Like {!choose}, but throws an exception on an empty set. *)
  val choose_exn : ('a, _) t -> 'a

  (** [split t x] produces a triple [(t1, maybe_x, t2)].

      [t1] is the set of elements strictly less than [x],
      [maybe_x] is the member (if any) of [t] which compares equal to [x],
      [t2] is the set of elements strictly larger than [x]. *)
  val split : ('a, 'cmp) t -> 'a -> ('a, 'cmp) t * 'a option * ('a, 'cmp) t

  (** [split_le_gt t x] produces a pair [(t1, t2)].

      [t1] is the set of elements less than or equal to [x],
      [t2] is the set of elements strictly greater than [x]. *)
  val split_le_gt : ('a, 'cmp) t -> 'a -> ('a, 'cmp) t * ('a, 'cmp) t

  (** [split_lt_ge t x] produces a pair [(t1, t2)].

      [t1] is the set of elements strictly less than [x],
      [t2] is the set of elements greater than or equal to [x]. *)
  val split_lt_ge : ('a, 'cmp) t -> 'a -> ('a, 'cmp) t * ('a, 'cmp) t

  (** if [equiv] is an equivalence predicate, then [group_by set ~equiv] produces a list
      of equivalence classes (i.e., a set-theoretic quotient).  E.g.,

      {[
        let chars = Set.of_list ['A'; 'a'; 'b'; 'c'] in
        let equiv c c' = Char.equal (Char.uppercase c) (Char.uppercase c') in
        group_by chars ~equiv
      ]}

      produces:

      {[
        [Set.of_list ['A';'a']; Set.singleton 'b'; Set.singleton 'c']
      ]}

      [group_by] runs in O(n^2) time, so if you have a comparison function, it's usually
      much faster to use [Set.of_list]. *)
  val group_by : ('a, 'cmp) t -> equiv:('a -> 'a -> bool) -> ('a, 'cmp) t list

  (** [to_sequence t] converts the set [t] to a sequence of the elements between
      [greater_or_equal_to] and [less_or_equal_to] inclusive in the order indicated by
      [order].  If [greater_or_equal_to > less_or_equal_to] the sequence is empty.  Cost is
      O(log n) up front and amortized O(1) for each element produced. *)
  val to_sequence
    :  ?order:[ `Increasing (** default *) | `Decreasing ]
    -> ?greater_or_equal_to:'a
    -> ?less_or_equal_to:'a
    -> ('a, 'cmp) t
    -> 'a Sequence.t

  (** [binary_search t ~compare which elt] returns the element in [t] specified by
      [compare] and [which], if one exists.

      [t] must be sorted in increasing order according to [compare], where [compare] and
      [elt] divide [t] into three (possibly empty) segments:

      {v
        |  < elt  |  = elt  |  > elt  |
      v}

      [binary_search] returns an element on the boundary of segments as specified by
      [which].  See the diagram below next to the [which] variants.

      [binary_search] does not check that [compare] orders [t], and behavior is
      unspecified if [compare] doesn't order [t].  Behavior is also unspecified if
      [compare] mutates [t]. *)
  val binary_search
    :  ('a, 'cmp) t
    -> compare:('a -> 'key -> int)
    -> [ `Last_strictly_less_than (**        {v | < elt X |                       v} *)
       | `Last_less_than_or_equal_to (**     {v |      <= elt       X |           v} *)
       | `Last_equal_to (**                  {v           |   = elt X |           v} *)
       | `First_equal_to (**                 {v           | X = elt   |           v} *)
       | `First_greater_than_or_equal_to (** {v           | X       >= elt      | v} *)
       | `First_strictly_greater_than (**    {v                       | X > elt | v} *)
       ]
    -> 'key
    -> 'a option

  (** [binary_search_segmented t ~segment_of which] takes a [segment_of] function that
      divides [t] into two (possibly empty) segments:

      {v
        | segment_of elt = `Left | segment_of elt = `Right |
      v}

      [binary_search_segmented] returns the element on the boundary of the segments as
      specified by [which]: [`Last_on_left] yields the last element of the left segment,
      while [`First_on_right] yields the first element of the right segment.  It returns
      [None] if the segment is empty.

      [binary_search_segmented] does not check that [segment_of] segments [t] as in the
      diagram, and behavior is unspecified if [segment_of] doesn't segment [t].  Behavior
      is also unspecified if [segment_of] mutates [t]. *)
  val binary_search_segmented
    :  ('a, 'cmp) t
    -> segment_of:('a -> [ `Left | `Right ])
    -> [ `Last_on_left | `First_on_right ]
    -> 'a option

  (** Produces the elements of the two sets between [greater_or_equal_to] and
      [less_or_equal_to] in [order], noting whether each element appears in the left set,
      the right set, or both.  In the both case, both elements are returned, in case the
      caller can distinguish between elements that are equal to the sets' comparator.  Runs
      in O(length t + length t'). *)
  module Merge_to_sequence_element : sig
    type ('a, 'b) t = ('a, 'b) Sequence.Merge_with_duplicates_element.t =
      | Left of 'a
      | Right of 'b
      | Both of 'a * 'b
    [@@deriving_inline compare, sexp]

    include Ppx_compare_lib.Comparable.S2 with type ('a, 'b) t := ('a, 'b) t
    include Sexplib0.Sexpable.S2 with type ('a, 'b) t := ('a, 'b) t

    [@@@end]
  end

  val merge_to_sequence
    :  ?order:[ `Increasing (** default *) | `Decreasing ]
    -> ?greater_or_equal_to:'a
    -> ?less_or_equal_to:'a
    -> ('a, 'cmp) t
    -> ('a, 'cmp) t
    -> ('a, 'a) Merge_to_sequence_element.t Sequence.t

  (** [M] is meant to be used in combination with OCaml applicative functor types:

      {[
        type string_set = Set.M(String).t
      ]}

      which stands for:

      {[
        type string_set = (String.t, String.comparator_witness) Set.t
      ]}

      The point is that [Set.M(String).t] supports deriving, whereas the second syntax
      doesn't (because there is no such thing as, say, String.sexp_of_comparator_witness,
      instead you would want to pass the comparator directly). *)
  module M (Elt : sig
    type t
    type comparator_witness
  end) : sig
    type nonrec t = (Elt.t, Elt.comparator_witness) t
  end

  include For_deriving with type ('a, 'b) t := ('a, 'b) t

  (** Using comparator is a similar interface as the toplevel of [Set], except the functions
      take a [~comparator:('elt, 'cmp) Comparator.t] where the functions at the toplevel of
      [Set] takes a [('elt, 'cmp) comparator]. *)
  module Using_comparator : sig
    type nonrec ('elt, 'cmp) t = ('elt, 'cmp) t [@@deriving_inline sexp_of]

    val sexp_of_t
      :  ('elt -> Sexplib0.Sexp.t)
      -> ('cmp -> Sexplib0.Sexp.t)
      -> ('elt, 'cmp) t
      -> Sexplib0.Sexp.t

    [@@@end]

    val t_of_sexp_direct
      :  comparator:('elt, 'cmp) Comparator.t
      -> (Sexp.t -> 'elt)
      -> Sexp.t
      -> ('elt, 'cmp) t

    module Tree : sig
      (** A [Tree.t] contains just the tree data structure that a set is based on, without
          including the comparator.  Accordingly, any operation on a [Tree.t] must also take
          as an argument the corresponding comparator. *)
      type ('a, 'cmp) t [@@deriving_inline sexp_of]

      val sexp_of_t
        :  ('a -> Sexplib0.Sexp.t)
        -> ('cmp -> Sexplib0.Sexp.t)
        -> ('a, 'cmp) t
        -> Sexplib0.Sexp.t

      [@@@end]

      val t_of_sexp_direct
        :  comparator:('elt, 'cmp) Comparator.t
        -> (Sexp.t -> 'elt)
        -> Sexp.t
        -> ('elt, 'cmp) t

      include
        Creators_and_accessors_generic
          with type ('a, 'b) set := ('a, 'b) t
          with type ('a, 'b) t := ('a, 'b) t
          with type ('a, 'b) tree := ('a, 'b) t
          with type 'a elt := 'a
          with type 'c cmp := 'c
          with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) With_comparator.t
          with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) With_comparator.t

      val empty_without_value_restriction : (_, _) t
    end

    include
      Creators_and_accessors_generic
        with type ('a, 'b) t := ('a, 'b) t
        with type ('a, 'b) tree := ('a, 'b) Tree.t
        with type ('a, 'b) set := ('a, 'b) t
        with type 'a elt := 'a
        with type 'c cmp := 'c
        with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) Without_comparator.t
        with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) With_comparator.t

    val comparator_s : ('a, 'cmp) t -> ('a, 'cmp) Comparator.Module.t
    val comparator : ('a, 'cmp) t -> ('a, 'cmp) Comparator.t
    val hash_fold_direct : 'elt Hash.folder -> ('elt, 'cmp) t Hash.folder

    module Empty_without_value_restriction (Elt : Comparator.S1) : sig
      val empty : ('a Elt.t, Elt.comparator_witness) t
    end
  end

  val to_tree : ('a, 'cmp) t -> ('a, 'cmp) Using_comparator.Tree.t

  val of_tree
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a, 'cmp) Using_comparator.Tree.t
    -> ('a, 'cmp) t

  (** A polymorphic Set. *)
  module Poly :
    S_poly
      with type 'elt t = ('elt, Comparator.Poly.comparator_witness) t
      with type comparator_witness := Comparator.Poly.comparator_witness
      with type 'elt tree :=
        ('elt, Comparator.Poly.comparator_witness) Using_comparator.Tree.t
      with type ('elt, 'cmp) set := ('elt, 'cmp) t

  (** {2 Modules and module types for extending [Set]}

      For use in extensions of Base, like [Core]. *)

  module With_comparator = With_comparator
  module With_first_class_module = With_first_class_module
  module Without_comparator = Without_comparator

  module type For_deriving = For_deriving
  module type S_poly = S_poly
  module type Accessors_generic = Accessors_generic
  module type Creators_generic = Creators_generic
  module type Creators_and_accessors_generic = Creators_and_accessors_generic
  module type Elt_plain = Elt_plain
end
