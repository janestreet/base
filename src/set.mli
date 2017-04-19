open! Import
open! Set_intf

(** The type of a set.  The first type parameter identifies the type of the element, and
    the second identifies the comparator, which determines the comparison function that is
    used for ordering elements in this set.  Many operations (e.g., {!union}), require
    that they be passed sets with the same element type and the same comparator type. *)
type ('elt, 'cmp) t [@@deriving_inline compare]
include
sig
  [@@@ocaml.warning "-32"]
  val compare :
    ('elt -> 'elt -> int) ->
    ('cmp -> 'cmp -> int) -> ('elt,'cmp) t -> ('elt,'cmp) t -> int
end
[@@@end]

type ('k, 'cmp) comparator =
  (module Comparator.S with type t = 'k and type comparator_witness = 'cmp)

(** Tests internal invariants of the set data structure.  Returns true on success. *)
val invariants : (_, _) t -> bool

val comparator : ('a, 'cmp) t -> ('a, 'cmp) Comparator.t

(** Creates an empty set based on the provided comparator. *)
val empty : ('a, 'cmp) comparator -> ('a, 'cmp) t

(** Creates a set based on the provided comparator that contains only the provided
    element. *)
val singleton : ('a, 'cmp) comparator -> 'a -> ('a, 'cmp) t

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

(** [union c list] returns the union of all the sets in [list].  The
    [comparator] argument is required for the case where [list] is empty.
    [O(max(List.length list, n log n))], where [n] is the sum of sizes of the input sets. *)
val union_list : ('a, 'cmp) comparator -> ('a, 'cmp) t list -> ('a, 'cmp) t

(** [inter t1 t2] computes the intersection of sets [t1] and [t2].  [O(length t1 +
    length t2)]. *)
val inter : ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) t

(** [diff t1 t2] computes the set difference [t1 - t2], i.e., the set containing all
    elements in [t1] that are not in [t2].  [O(length t1 + length t2)]. *)
val diff : ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) t

(** [symmetric_diff t1 t2] returns a sequence of changes between [t1] and [t2]. It is
    intended to be efficient in the case where [t1] and [t2] share a large amount of
    structure. *)
val symmetric_diff
  :  ('a, 'cmp) t
  -> ('a, 'cmp) t
  -> ('a, 'a) Either.t Sequence.t

(** [compare_direct t1 t2] compares the sets [t1] and [t2].  It returns the same result
    as [compare], but unlike compare, doesn't require arguments to be passed in for the
    type parameters of the set.  [O(length t1 + length t2)]. *)
val compare_direct : ('a, 'cmp) t -> ('a, 'cmp) t -> int

(** Hash function: a building block to use when hashing data structures containing sets in
    them. [hash_fold_direct hash_fold_key] is compatible with [compare_direct] iff
    [hash_fold_key] is compatible with [(comparator s).compare] of the set [s] being
    hashed. *)
val hash_fold_direct
  :  'a Hash.folder
  -> ('a, 'cmp) t Hash.folder

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
  : (module Commutative_group.S with type t = 'sum)
  -> ('a, _) t -> f:('a -> 'sum) -> 'sum

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
val nth        : ('a, _) t -> int -> 'a option
val find_index : ('a, _) t -> int -> 'a option
[@@deprecated "[since 2016-10] Use [nth]"]

(** [remove_index t i] returns a version of [t] with the [i]th smallest element removed,
    in [O(log n)] time.  The smallest element has [i = 0].  Returns [t] if [i < 0] or
    [i >= length t]. *)
val remove_index : ('a, 'cmp) t -> int -> ('a, 'cmp) t

(** [is_subset t1 ~of_:t2] returns true iff [t1] is a subset of [t2]. *)
val is_subset : ('a, 'cmp) t -> of_:('a, 'cmp) t -> bool

(** [subset] is a synonym for [is_subset]. *)
val subset : ('a, 'cmp) t -> ('a, 'cmp) t -> bool
[@@deprecated "[since 2016-09] Replace [Set.subset t1 t2] with [Set.is_subset t1 ~of_:t2]"]

(** The list or array given to [of_list] and [of_array] need not be sorted. *)
val of_list  : ('a, 'cmp) comparator -> 'a list  -> ('a, 'cmp) t
val of_array : ('a, 'cmp) comparator -> 'a array -> ('a, 'cmp) t

(** [to_list] and [to_array] produce sequences sorted in ascending order according to the
    comparator. *)
val to_list  : ('a, _) t -> 'a list
val to_array : ('a, _) t -> 'a array

(** Create set from sorted array.  The input must be sorted (either in ascending or
    descending order as given by the comparator) and contain no duplicates, otherwise the
    result is an error.  The complexity of this function is [O(n)]. *)
val of_sorted_array
  :  ('a, 'cmp) comparator
  -> 'a array
  -> ('a, 'cmp) t Or_error.t

(** Similar to [of_sorted_array], but without checking the input array. *)
val of_sorted_array_unchecked
  :  ('a, 'cmp) comparator
  -> 'a array
  -> ('a, 'cmp) t

(** [of_increasing_iterator_unchecked c ~len ~f] behaves like [of_sorted_array_unchecked c
    (Array.init len ~f)], with the additional restriction that a decreasing order is not
    supported.  The advantage is not requiring you to allocate an intermediate array.  [f]
    will be called with 0, 1, ... [len - 1], in order. *)
val of_increasing_iterator_unchecked
  :  ('a, 'cmp) comparator
  -> len:int
  -> f:(int -> 'a)
  -> ('a, 'cmp) t

(** [stable_dedup_list] is here rather than in the [List] module because the
    implementation relies crucially on sets, and because doing so allows one to avoid uses
    of polymorphic comparison by instantiating the functor at a different implementation
    of [Comparator] and using the resulting [stable_dedup_list]. *)
val stable_dedup_list : ('a, _) comparator -> 'a list -> 'a list

(** [map c t ~f] returns a new set created by applying [f] to every element in
    [t].  The returned set is based on the provided [comparator].  [O(n log n)]. *)
val map
  :  ('b, 'cmp) comparator
  -> ('a, _) t
  -> f:('a -> 'b)
  -> ('b, 'cmp) t

(** Like {!map}, except elements for which [f] returns [None] will be dropped.  *)
val filter_map
  :  ('b, 'cmp) comparator
  -> ('a, _) t
  -> f:('a -> 'b option)
  -> ('b, 'cmp) t

(** [filter t ~f] returns the subset of [t] for which [f] evaluates to true.  [O(n log
    n)]. *)
val filter :  ('a, 'cmp) t -> f:('a -> bool) -> ('a, 'cmp) t

(** [fold t ~init ~f] folds over the elements of the set from smallest to largest. *)
val fold
  :  ('a, _) t
  -> init:'accum
  -> f:('accum -> 'a -> 'accum)
  -> 'accum

(** [fold_result ~init ~f] folds over the elements of the set from smallest to
    largest, short circuiting the fold if [f accum x] is an [Error _] *)
val fold_result
  :  ('a, _) t
  -> init:'accum
  -> f:('accum -> 'a -> ('accum, 'e) Result.t)
  -> ('accum, 'e) Result.t

(** [fold_until t ~init ~f] is a short-circuiting version of [fold]. If [f]
    returns [Stop _] the computation ceases and results in that value. If [f] returns
    [Continue _], the fold will proceed. *)
val fold_until
  :  ('a, _) t
  -> init:'accum
  -> f:('accum -> 'a -> ('accum, 'stop) Continue_or_stop.t)
  -> ('accum, 'stop) Finished_or_stopped_early.t

(** Like {!fold}, except that it goes from the largest to the smallest element. *)
val fold_right
  :  ('a, _) t
  -> init:'accum
  -> f:('a -> 'accum -> 'accum)
  -> 'accum

(** [iter t ~f] calls [f] on every element of [t], going in order from the smallest to
    largest.  *)
val iter : ('a, _) t -> f:('a -> unit) -> unit

(** Iterate two sets side by side.  Complexity is [O(m+n)] where [m] and [n] are the sizes
    of the two input sets.  As an example, with the inputs [0; 1] and [1; 2], [f] will be
    called with [`Left 0]; [`Both (1, 1)]; and [`Right 2]. *)
val iter2
  :  ('a, 'cmp) t
  -> ('a, 'cmp) t
  -> f:([`Left of 'a | `Right of 'a | `Both of 'a * 'a] -> unit)
  -> unit

(** if [a, b = partition_tf set ~f] then [a] is the elements on which [f] produced [true],
    and [b] is the elements on which [f] produces [false]. *)
val partition_tf
  :  ('a, 'cmp) t
  -> f:('a -> bool)
  -> ('a, 'cmp) t * ('a, 'cmp) t

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

(** [split t x] produces a triple [(t1, maybe_x, t2)] where [t1] is the set of elements
    strictly less than [x], [maybe_x] is the member (if any) of [t] which compares equal
    to [x], and [t2] is the set of elements strictly larger than [x]. *)
val split : ('a, 'cmp) t -> 'a -> ('a, 'cmp) t * 'a option * ('a, 'cmp) t

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
val group_by :  ('a, 'cmp) t -> equiv:('a -> 'a -> bool) -> ('a, 'cmp) t list

(** [to_sequence t] converts the set [t] to a sequence of the elements between
    [greater_or_equal_to] and [less_or_equal_to] inclusive in the order indicated by
    [order].  If [greater_or_equal_to > less_or_equal_to] the sequence is empty.  Cost is
    O(log n) up front and amortized O(1) for each element produced. *)
val to_sequence
  :  ?order               : [ `Increasing (** default *) | `Decreasing ]
  -> ?greater_or_equal_to : 'a
  -> ?less_or_equal_to    : 'a
  -> ('a, 'cmp) t
  -> 'a Sequence.t

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
  include
  sig
    [@@@ocaml.warning "-32"]
    val compare :
      ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a,'b) t -> ('a,'b) t -> int
    val t_of_sexp :
      (Sexplib.Sexp.t -> 'a) ->
      (Sexplib.Sexp.t -> 'b) -> Sexplib.Sexp.t -> ('a,'b) t
    val sexp_of_t :
      ('a -> Sexplib.Sexp.t) ->
      ('b -> Sexplib.Sexp.t) -> ('a,'b) t -> Sexplib.Sexp.t
  end
  [@@@end]
end

val merge_to_sequence
  :  ?order               : [ `Increasing (** default *) | `Decreasing ]
  -> ?greater_or_equal_to : 'a
  -> ?less_or_equal_to    : 'a
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
module M (Elt : sig type t type comparator_witness end) : sig
  type nonrec t = (Elt.t, Elt.comparator_witness) t
end

module type Sexp_of_m = sig type t [@@deriving_inline sexp_of]
  include sig [@@@ocaml.warning "-32"] val sexp_of_t : t -> Sexplib.Sexp.t end
  [@@@end] end
module type M_of_sexp = sig
  type t [@@deriving_inline of_sexp]
  include sig [@@@ocaml.warning "-32"] val t_of_sexp : Sexplib.Sexp.t -> t end
  [@@@end] include Comparator.S with type t := t
end
module type Compare_m = sig end
module type Hash_fold_m = Hasher.S

val sexp_of_m__t : (module Sexp_of_m with type t = 'elt) -> ('elt, 'cmp) t -> Sexp.t

val m__t_of_sexp
  :  (module M_of_sexp with type t = 'elt and type comparator_witness = 'cmp)
  -> Sexp.t
  -> ('elt, 'cmp) t

val compare_m__t :  (module Compare_m) -> ('elt, 'cmp) t -> ('elt, 'cmp) t -> int

val hash_fold_m__t
  :  (module Hash_fold_m with type t = 'elt) -> (Hash.state -> ('elt, _) t -> Hash.state)

(** Using comparator is a similar interface as the toplevel of [Set], except the functions
    take a [~comparator:('elt, 'cmp) Comparator.t] where the functions at the toplevel of
    [Set] takes a [('elt, 'cmp) comparator]. *)
module Using_comparator : sig
  type nonrec ('elt, 'cmp) t = ('elt, 'cmp) t [@@deriving_inline sexp_of]
  include
  sig
    [@@@ocaml.warning "-32"]
    val sexp_of_t :
      ('elt -> Sexplib.Sexp.t) ->
      ('cmp -> Sexplib.Sexp.t) -> ('elt,'cmp) t -> Sexplib.Sexp.t
  end
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
    include
    sig
      [@@@ocaml.warning "-32"]
      val sexp_of_t :
        ('a -> Sexplib.Sexp.t) ->
        ('cmp -> Sexplib.Sexp.t) -> ('a,'cmp) t -> Sexplib.Sexp.t
    end
    [@@@end]

    val t_of_sexp_direct
      :  comparator:('elt, 'cmp) Comparator.t
      -> (Sexp.t -> 'elt)
      -> Sexp.t
      -> ('elt, 'cmp) t

    include Creators_and_accessors2_with_comparator
      with type ('a, 'b) set  := ('a, 'b) t
      with type ('a, 'b) t    := ('a, 'b) t
      with type ('a, 'b) tree := ('a, 'b) t

    val empty_without_value_restriction : (_, _) t
  end

  include Accessors2
    with type ('a, 'b) t    := ('a, 'b) t
    with type ('a, 'b) tree := ('a, 'b) Tree.t
  include Creators2_with_comparator
    with type ('a, 'b) t    := ('a, 'b) t
    with type ('a, 'b) tree := ('a, 'b) Tree.t
    with type ('a, 'b) set  := ('a, 'b) t

  val comparator : ('a, 'cmp) t -> ('a, 'cmp) Comparator.t

  val hash_fold_direct
    :  'elt Hash.folder
    -> ('elt, 'cmp) t Hash.folder

  module Empty_without_value_restriction (Elt : Comparator.S1) : sig
    val empty : ('a Elt.t, Elt.comparator_witness) t
  end
end
