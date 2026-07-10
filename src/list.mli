@@ portable

(** Immutable, singly-linked lists, giving fast access to the front of the list, and slow
    (i.e., O(n)) access to the back of the list. The comparison functions on lists are
    lexicographic. *)

open! Import
module Invariant := Invariant_intf.Definitions
module Constructors : module type of List0.Constructors

[%%template:
[@@@kind_set.define all_ks_non_value = base_non_value]
[@@@kind_set.define all_ks = (all_ks_non_value, value_or_null)]

type%template ('a : k) t = (('a : k) Constructors.t[@kind k])
[@@kind k = all_ks_non_value]
[@@deriving compare ~localize, equal ~localize, sexp_of ~stackify]

type ('a : value_or_null) t = 'a list
[@@deriving
  compare ~localize, equal ~localize, globalize, hash, sexp ~stackify, sexp_grammar]

include%template
  Indexed_container.S1_with_creators
  [@kind_set.explicit value_or_null] [@alloc stack]
  with type ('a : value_or_null) t := 'a t

include Invariant.S1 with type 'a t := 'a t

(** Implements cartesian-product behavior for [map] and [bind]. *)
module Cartesian_product : sig
  include%template
    Applicative.S
    [@kind value_or_null mod maybe_null] [@mode local]
    with type ('a : value_or_null) t := 'a t

  include%template
    Monad.S
    [@kind value_or_null mod maybe_null] [@mode local]
    with type ('a : value_or_null) t := 'a t
end

(** The monad portion of [Cartesian_product] is re-exported at top level. *)
include%template
  Monad.S
  [@kind value_or_null mod maybe_null] [@mode local]
  with type ('a : value_or_null) t := 'a t

(** [Or_unequal_lengths] is used for functions that take multiple lists and that only make
    sense if all the lists have the same length, e.g., [iter2], [map3]. Such functions
    check the list lengths prior to doing anything else, and return [Unequal_lengths] if
    not all the lists have the same length. *)
module Or_unequal_lengths : sig
  type ('a : value_or_null) t =
    | Ok of 'a
    | Unequal_lengths
  [@@deriving compare ~localize, sexp_of ~stackify]
end

(** [create ~len x] returns a list of length [len] populated [x]. *)
val create : ('a : value_or_null). len:int -> 'a -> 'a list

(** [singleton x] returns a list with a single element [x]. *)
val singleton : ('a : value_or_null). 'a -> 'a t

[%%template:
(*_ Unmangled [t] gets shadowed below, so we alias a mangled [t] to it first. *)
type ('a : value_or_null) t := 'a t [@@kind.explicit value_or_null]

[@@@kind k = all_ks]

type ('a : k) t := ('a t[@kind.explicit k])

[@@@kind.default k]

val iter : ('a : k). 'a t @ l -> f:('a @ l -> unit) @ local -> unit
[@@mode l = (local, global)]

val iteri : ('a : k). 'a t @ l -> f:(int -> 'a @ l -> unit) @ local -> unit
[@@mode l = (local, global)]

val length : ('a : k). 'a t @ immutable local -> int

(** Return the [n]-th element of the given list. The first element (head of the list) is
    at position 0. Raise if the list is too short or [n] is negative. *)
val nth_exn : ('a : k). 'a t @ l -> int -> 'a @ l
[@@mode l = (local, global)]

val nth : ('a : k). 'a t @ l -> int -> ('a Option0.t[@kind k]) @ l
[@@mode l = (local, global)]

val mem : 'a t @ l -> 'a @ l -> equal:('a @ l -> 'a @ l -> bool) @ local -> bool
[@@mode l = (local, global)]

[@@@alloc.default a @ l = (stack_local, heap_global)]

(** [init n ~f] creates a list of length [n] where the element at index [i] is the value
    of [f i]. Raises if [n < 0]. [f] is called on indices from the highest to lowest, so
    the order of side effects is reversed: [List.init 3 ~f:print_int] prints [210]. *)
val init : ('a : k). int -> f:(int -> 'a @ l) @ local -> 'a t @ l

(** *)
val init_i64 : ('a : k). int64# -> f:(int64# -> 'a @ l) @ local -> 'a t @ l

val append : ('a : k). 'a t @ l -> 'a t @ l -> 'a t @ l
val filteri : ('a : k). 'a t @ l -> f:(int -> 'a @ l -> bool) @ local -> 'a t @ l
val filter : ('a : k). 'a t @ l -> f:('a @ l -> bool) @ local -> 'a t @ l

(** [rev_append l1 l2] reverses [l1] and concatenates it to [l2]. This is equivalent to
    [(]{!List.rev}[ l1) @ l2], but [rev_append] is more efficient. *)
val rev_append : ('a : k). 'a t @ l -> 'a t @ l -> 'a t @ l

(** List reversal. *)
val rev : ('a : k). 'a t @ l u -> 'a t @ l u
[@@mode u = (aliased, unique)]]

[%%template:
[@@@mode.default l = (local, global)]

val find_or_null : 'a t @ l -> f:('a @ l -> bool) @ local -> 'a or_null @ l
val nth_or_null : 'a t @ l -> int -> 'a or_null @ l [@@zero_alloc]]

[%%template:
[@@@kind.default ka = all_ks, kb = all_ks]
[@@@mode.default la = (local, global)]
[@@@alloc.default __ @ lb = (heap_global, stack_local)]

val map
  : ('a : ka) ('b : kb).
  ('a t[@kind ka]) @ la -> f:('a @ la -> 'b @ lb) @ local -> ('b t[@kind kb]) @ lb

val mapi
  : ('a : ka) ('b : kb).
  ('a t[@kind ka]) @ la -> f:(int -> 'a @ la -> 'b @ lb) @ local -> ('b t[@kind kb]) @ lb

(** [rev_map l ~f] gives the same result as {!List.rev}[ (]{!List.map}[ l ~f)], but is
    more efficient. *)
val rev_map
  : ('a : ka) ('b : kb).
  ('a t[@kind ka]) @ la -> f:('a @ la -> 'b @ lb) @ local -> ('b t[@kind kb]) @ lb

val rev_mapi
  : ('a : ka) ('b : kb).
  ('a t[@kind ka]) @ la -> f:(int -> 'a @ la -> 'b @ lb) @ local -> ('b t[@kind kb]) @ lb

val filter_map
  : ('a : ka) ('b : kb).
  ('a t[@kind ka]) @ la
  -> f:('a @ la -> ('b Option0.t[@kind kb]) @ lb) @ local
  -> ('b t[@kind kb]) @ lb

val filter_mapi
  : ('a : ka) ('b : kb).
  ('a t[@kind ka]) @ la
  -> f:(int -> 'a @ la -> ('b Option0.t[@kind kb]) @ lb) @ local
  -> ('b t[@kind kb]) @ lb

val concat_map
  : ('a : ka) ('b : kb).
  ('a t[@kind ka]) @ la
  -> f:('a @ la -> ('b t[@kind kb]) @ lb) @ local
  -> ('b t[@kind kb]) @ lb

val concat_mapi
  : ('a : ka) ('b : kb).
  ('a t[@kind ka]) @ la
  -> f:(int -> 'a @ la -> ('b t[@kind kb]) @ lb) @ local
  -> ('b t[@kind kb]) @ lb]

[%%template:
[@@@kind.default ka = all_ks]
[@@@mode.default li = (global, local)]
[@@@alloc.default __ @ lo = (heap_global, stack_local)]

(** Like [filter_map] but with a function returning [or_null] instead of [option]. *)
val filter_map_or_null
  : ('a : ka) 'b.
  ('a t[@kind ka]) @ li -> f:('a @ li -> 'b or_null @ lo) @ local -> 'b t @ lo

(** Like [filter_mapi] but with a function returning [or_null] instead of [option]. *)
val filter_mapi_or_null
  : ('a : ka) 'b.
  ('a t[@kind ka]) @ li -> f:(int -> 'a @ li -> 'b or_null @ lo) @ local -> 'b t @ lo]

[%%template:
[@@@kind.default ka = all_ks, kb = all_ks]
[@@@mode.default la = (local, global), lb = (local, global)]

val fold
  : ('a : ka) ('b : kb).
  ('a t[@kind ka]) @ la
  -> init:'b @ lb
  -> f:('b @ lb -> 'a @ la -> 'b @ lb) @ local
  -> 'b @ lb

val foldi
  : ('a : ka) ('b : kb).
  ('a t[@kind ka]) @ la
  -> init:'b @ lb
  -> f:(int -> 'b @ lb -> 'a @ la -> 'b @ lb) @ local
  -> 'b @ lb]

(** [unordered_append l1 l2] has the same elements as [l1 @ l2], but in some unspecified
    order. Generally takes time proportional to length of first list, but is O(1) if
    either list is empty. *)
val unordered_append : ('a : value_or_null). 'a t -> 'a t -> 'a t

[%%template:
[@@@mode.default l = (global, local)]

(** [iter2 [a1; ...; an] [b1; ...; bn] ~f] calls in turn [f a1 b1; ...; f an bn]. The exn
    version will raise if the two lists have different lengths. *)
val iter2_exn
  : ('a : value_or_null) ('b : value_or_null).
  'a t @ l -> 'b t @ l -> f:local_ ('a @ l -> 'b @ l -> unit) -> unit

val iter2
  : ('a : value_or_null) ('b : value_or_null).
  'a t @ l
  -> 'b t @ l
  -> f:local_ ('a @ l -> 'b @ l -> unit)
  -> unit Or_unequal_lengths.t @ l]

(** [rev_map2_exn l1 l2 ~f] gives the same result as [List.rev (List.map2_exn l1 l2 ~f)],
    but is more efficient. *)
val rev_map2_exn
  : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
  'a t -> 'b t -> f:local_ ('a -> 'b -> 'c) -> 'c t

val rev_map2
  : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
  'a t -> 'b t -> f:local_ ('a -> 'b -> 'c) -> 'c t Or_unequal_lengths.t

(** [fold2 ~f ~init:a [b1; ...; bn] [c1; ...; cn]] is
    [f (... (f (f a b1 c1) b2 c2) ...) bn cn]. The exn version will raise if the two lists
    have different lengths. *)
val fold2_exn
  : ('a : value_or_null) ('b : value_or_null) ('acc : value_or_null).
  'a t -> 'b t -> init:'acc -> f:local_ ('acc -> 'a -> 'b -> 'acc) -> 'acc

val fold2
  : ('a : value_or_null) ('b : value_or_null) ('acc : value_or_null).
  'a t
  -> 'b t
  -> init:'acc
  -> f:local_ ('acc -> 'a -> 'b -> 'acc)
  -> 'acc Or_unequal_lengths.t

(** [fold_right2 ~f [a1; ...; an] [b1; ...; bn] ~init:c] is
    [f a1 b1 (f a2 b2 (... (f an bn c) ...))]. The exn version will raise if the two lists
    have different lengths. *)
val fold_right2_exn
  : ('a : value_or_null) ('b : value_or_null) ('acc : value_or_null).
  'a t -> 'b t -> f:local_ ('a -> 'b -> 'acc -> 'acc) -> init:'acc -> 'acc

val fold_right2
  : ('a : value_or_null) ('b : value_or_null) ('acc : value_or_null).
  'a t
  -> 'b t
  -> f:local_ ('a -> 'b -> 'acc -> 'acc)
  -> init:'acc
  -> 'acc Or_unequal_lengths.t

(** Like {!List.for_all}, but for a two-argument predicate. The exn version will raise if
    the two lists have different lengths. *)
val for_all2_exn
  : ('a : value_or_null) ('b : value_or_null).
  'a t -> 'b t -> f:local_ ('a -> 'b -> bool) -> bool

val for_all2
  : ('a : value_or_null) ('b : value_or_null).
  'a t -> 'b t -> f:local_ ('a -> 'b -> bool) -> bool Or_unequal_lengths.t

(** Like {!List.exists}, but for a two-argument predicate. The exn version will raise if
    the two lists have different lengths. *)
val exists2_exn
  : ('a : value_or_null) ('b : value_or_null).
  'a t -> 'b t -> f:local_ ('a -> 'b -> bool) -> bool

val exists2
  : ('a : value_or_null) ('b : value_or_null).
  'a t -> 'b t -> f:local_ ('a -> 'b -> bool) -> bool Or_unequal_lengths.t

(** Like [filter], but reverses the order of the input list. *)
val rev_filter : ('a : value_or_null). 'a t -> f:local_ ('a -> bool) -> 'a t

val%template partition_map
  : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
  'a t @ li -> f:('a @ li -> ('b, 'c) Either0.t @ lo) @ local -> 'b t * 'c t @ lo
[@@mode li = (global, local)] [@@alloc __ @ lo = (heap_global, stack_local)]

val partition3_map
  : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null) ('d : value_or_null).
  'a t -> f:local_ ('a -> [ `Fst of 'b | `Snd of 'c | `Trd of 'd ]) -> 'b t * 'c t * 'd t

[%%template:
[@@@alloc.default a @ l = (heap_global, stack_local)]

(** [partition_result l] returns a pair of lists [(l1, l2)], where [l1] is the list of all
    [Ok] elements in [l] and [l2] is the list of all [Error] elements. The order of
    elements in the input list is preserved. *)
val partition_result
  : ('ok : value_or_null) ('error : value_or_null).
  ('ok, 'error) Result.t t @ l -> 'ok t * 'error t @ l

(** [split_n [e1; ...; em] n] is [([e1; ...; en], [en+1; ...; em])].

    - If [n >= m], [([e1; ...; em], [])] is returned.
    - If [n <= 0], [([], [e1; ...; em])] is returned.

    In either of these cases, the input list is returned as one side of the pair, rather
    than being copied. *)
val split_n : ('a : value_or_null). 'a t @ l -> int -> #('a t * 'a t) @ l
[@@zero_alloc_if_stack a]

(** Sort a list in increasing order according to a comparison function. The comparison
    function must return 0 if its arguments compare as equal, a positive integer if the
    first is greater, and a negative integer if the first is smaller (see [Array.sort] for
    a complete specification). For example, {!Poly.compare} is a suitable comparison
    function.

    The current implementation uses Merge Sort. It runs in linear heap space and
    logarithmic stack space. The stack-allocating versions of sorting functions
    additionally allocate O(n log n) space in the caller's region.

    Presently, the sort is stable, meaning that two equal elements in the input will be in
    the same order in the output. *)
val sort
  : ('a : value_or_null).
  'a t @ l -> compare:local_ ('a @ l -> 'a @ l -> int) -> 'a t @ l

(** Like [sort], but guaranteed to be stable. *)
val stable_sort
  : ('a : value_or_null).
  'a t @ l -> compare:local_ ('a @ l -> 'a @ l -> int) -> 'a t @ l]

(** Merges two lists: assuming that [l1] and [l2] are sorted according to the comparison
    function [compare], [merge compare l1 l2] will return a sorted list containing all the
    elements of [l1] and [l2]. If several elements compare equal, the elements of [l1]
    will be before the elements of [l2]. *)
val merge : ('a : value_or_null). 'a t -> 'a t -> compare:local_ ('a -> 'a -> int) -> 'a t

(** Merges a list of [k] sorted lists into one sorted list, in O(n log k) time where [n]
    is the total number of elements across all input lists. Elements that compare equal
    appear in the same order as the input lists (i.e., elements of an earlier list come
    before equal elements of a later list).

    Implemented by pairwise merging, so the per-comparison cost is small but it allocates
    O(n log k) intermediate cons cells. *)
val merge_all : ('a : value_or_null). 'a t t -> compare:local_ ('a -> 'a -> int) -> 'a t

[%%template:
[@@@mode.default l = (global, local)]

val hd : ('a : value_or_null). 'a t @ l -> 'a option @ l
val hd_or_null : 'a t @ l -> 'a or_null @ l [@@zero_alloc]
val tl : ('a : value_or_null). 'a t @ l -> 'a t option @ l
val tl_or_null : 'a t @ l -> 'a t or_null @ l [@@zero_alloc]

(** Returns the first element of the given list. Raises if the list is empty. *)
val hd_exn : ('a : value_or_null). 'a t @ l -> 'a @ l

(** Returns the given list without its first element. Raises if the list is empty. *)
val tl_exn : ('a : value_or_null). 'a t @ l -> 'a t @ l

(** Like [find_exn], but passes the index as an argument. *)
val findi_exn
  : ('a : value_or_null).
  'a t @ l -> f:local_ (int -> 'a @ l -> bool) -> int * 'a @ l

(** [find_exn t ~f] returns the first element of [t] that satisfies [f]. It raises
    [Stdlib.Not_found] or [Not_found_s] if there is no such element. *)
val find_exn : ('a : value_or_null). 'a t @ l -> f:local_ ('a @ l -> bool) -> 'a @ l]

(** Like [find_map] but with a function returning [or_null] instead of [option]. *)
val%template find_map_or_null
  :  'a t @ li
  -> f:('a @ li -> 'b or_null @ lo) @ local
  -> 'b or_null @ lo
[@@mode li = (local, global), lo = (local, global)]

(** Like [find_map_or_null], but raises [Stdlib.Not_found] or [Not_found_s] if [f] always
    returns [Null]. *)
val find_map_or_null_exn : 'a t -> f:('a -> 'b or_null) @ local -> 'b

(** Returns the first evaluation of [f] that returns [Some]. Raises [Stdlib.Not_found] or
    [Not_found_s] if [f] always returns [None]. *)
val find_map_exn
  : ('a : value_or_null) ('b : value_or_null).
  'a t -> f:local_ ('a -> 'b option) -> 'b

(** Like [find_map_exn], but passes the index as an argument. *)
val find_mapi_exn
  : ('a : value_or_null) ('b : value_or_null).
  'a t -> f:local_ (int -> 'a -> 'b option) -> 'b

(** Like [find_mapi] but with a function returning [or_null] instead of [option]. *)
val%template find_mapi_or_null
  :  'a t @ li
  -> f:(int -> 'a @ li -> 'b or_null @ lo) @ local
  -> 'b or_null @ lo
[@@mode li = (local, global), lo = (local, global)]

(** Like [find_mapi_or_null], but raises [Stdlib.Not_found] or [Not_found_s] if [f] always
    returns [Null]. *)
val find_mapi_or_null_exn : 'a t -> f:(int -> 'a -> 'b or_null) @ local -> 'b

(** [folding_map] is a version of [map] that threads an accumulator through calls to [f].
    The accumulator is threaded left-to-right through the list, but unlike [fold_map] only
    the mapped result list is returned; the final accumulator value is discarded. *)
val folding_map
  : ('a : value_or_null) ('b : value_or_null) ('acc : value_or_null).
  'a t -> init:'acc -> f:local_ ('acc -> 'a -> 'acc * 'b) -> 'b t

val folding_mapi
  : ('a : value_or_null) ('b : value_or_null) ('acc : value_or_null).
  'a t -> init:'acc -> f:local_ (int -> 'acc -> 'a -> 'acc * 'b) -> 'b t

[%%template:
[@@@mode.default li = (global, local)]
[@@@alloc.default a @ lo = (heap_global, stack_local)]

(** [fold_map] is a combination of [fold] and [map] that threads an accumulator through
    calls to [f]. *)

val fold_map
  : ('a : value_or_null) ('b : value_or_null) ('acc : value_or_null).
  'a t @ li
  -> init:'acc @ lo
  -> f:local_ ('acc @ lo -> 'a @ li -> 'acc * 'b @ lo)
  -> 'acc * 'b t @ lo

val fold_mapi
  : ('a : value_or_null) ('b : value_or_null) ('acc : value_or_null).
  'a t @ li
  -> init:'acc @ lo
  -> f:local_ (int -> 'acc @ lo -> 'a @ li -> 'acc * 'b @ lo)
  -> 'acc * 'b t @ lo

(** [map2 [a1; ...; an] [b1; ...; bn] ~f] is [[f a1 b1; ...; f an bn]]. The exn version
    will raise if the two lists have different lengths. *)

val map2_exn
  : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
  'a t @ li -> 'b t @ li -> f:local_ ('a @ li -> 'b @ li -> 'c @ lo) -> 'c t @ lo

val map2
  : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
  'a t @ li
  -> 'b t @ li
  -> f:local_ ('a @ li -> 'b @ li -> 'c @ lo)
  -> 'c t Or_unequal_lengths.t @ lo]

(** Analogous to [rev_map2]. *)

val rev_map3_exn
  : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null) ('d : value_or_null).
  'a t -> 'b t -> 'c t -> f:local_ ('a -> 'b -> 'c -> 'd) -> 'd t

val rev_map3
  : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null) ('d : value_or_null).
  'a t -> 'b t -> 'c t -> f:local_ ('a -> 'b -> 'c -> 'd) -> 'd t Or_unequal_lengths.t

(** Analogous to [map2]. *)

val map3_exn
  : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null) ('d : value_or_null).
  'a t -> 'b t -> 'c t -> f:local_ ('a -> 'b -> 'c -> 'd) -> 'd t

val map3
  : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null) ('d : value_or_null).
  'a t -> 'b t -> 'c t -> f:local_ ('a -> 'b -> 'c -> 'd) -> 'd t Or_unequal_lengths.t

(** [rev_map_append l1 l2 ~f] reverses [l1] mapping [f] over each element, and appends the
    result to the front of [l2]. *)
val rev_map_append
  : ('a : value_or_null) ('b : value_or_null).
  'a t -> 'b t -> f:local_ ('a -> 'b) -> 'b t

(** [fold_right [a1; ...; an] ~f ~init:b] is [f a1 (f a2 (... (f an b) ...))]. *)
val%template fold_right
  : ('a : value_or_null) ('acc : value_or_null).
  'a t @ li
  -> f:('a @ li -> 'acc @ lo -> 'acc @ lo) @ local
  -> init:'acc @ lo
  -> 'acc @ lo
[@@mode li = (local, global), lo = (local, global)]

(** [fold_left] is the same as {!Container.S1.fold}, and one should always use [fold]
    rather than [fold_left], except in functors that are parameterized over a more general
    signature where this equivalence does not hold. *)
val fold_left
  : ('a : value_or_null) ('acc : value_or_null).
  'a t -> init:'acc -> f:local_ ('acc -> 'a -> 'acc) -> 'acc

(** Transform a list of pairs into a pair of lists: [unzip [(a1,b1); ...; (an,bn)]] is
    [([a1; ...; an], [b1; ...; bn])]. *)

val%template unzip
  : ('a : value_or_null) ('b : value_or_null).
  ('a * 'b) t @ l -> 'a t * 'b t @ l
[@@alloc __ @ l = (heap_global, stack_local)]

val unzip3
  : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
  ('a * 'b * 'c) t -> 'a t * 'b t * 'c t

(** Transform a pair of lists into an (optional) list of pairs:
    [zip [a1; ...; an] [b1; ...; bn]] is [[(a1,b1); ...; (an,bn)]]. Returns
    [Unequal_lengths] if the two lists have different lengths. *)

[%%template:
[@@@alloc.default __ @ l = (heap_global, stack_local)]

val zip
  : ('a : value_or_null) ('b : value_or_null).
  'a t @ l -> 'b t @ l -> ('a * 'b) t Or_unequal_lengths.t @ l

val zip_exn
  : ('a : value_or_null) ('b : value_or_null).
  'a t @ l -> 'b t @ l -> ('a * 'b) t @ l]

(** Analogous to [zip], but with 3 lists rather than 2. *)

val zip3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t Or_unequal_lengths.t
val zip3_exn : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

[%%template:
[@@@mode.default l = (global, local)]

(** [reduce_exn [a1; ...; an] ~f] is [f (... (f (f a1 a2) a3) ...) an]. It fails on the
    empty list. Tail recursive. *)
val reduce
  : ('a : value_or_null).
  'a t @ l -> f:local_ ('a @ l -> 'a @ l -> 'a @ l) -> 'a option @ l

val reduce_exn
  : ('a : value_or_null).
  'a t @ l -> f:local_ ('a @ l -> 'a @ l -> 'a @ l) -> 'a @ l]

(** [reduce_balanced] returns the same value as [reduce] when [f] is associative, but
    differs in that the tree of nested applications of [f] has logarithmic depth.

    This is useful when your ['a] grows in size as you reduce it and [f] becomes more
    expensive with bigger inputs. For example, [reduce_balanced ~f:(^)] takes [n*log(n)]
    time, while [reduce ~f:(^)] takes quadratic time. *)
val reduce_balanced : ('a : value_or_null). 'a t -> f:local_ ('a -> 'a -> 'a) -> 'a option

val reduce_balanced_exn : ('a : value_or_null). 'a t -> f:local_ ('a -> 'a -> 'a) -> 'a

[%%template:
[@@@alloc.default __ @ l = (heap_global, stack_local)]

(** [group l ~break] returns a list of lists (i.e., groups) whose concatenation is equal
    to the original list. Each group is broken where [break] returns true on a pair of
    successive elements.

    Example:

    {[
      group ~break:(<>) ['M';'i';'s';'s';'i';'s';'s';'i';'p';'p';'i'] ->

      [['M'];['i'];['s';'s'];['i'];['s';'s'];['i'];['p';'p'];['i']]
    ]} *)
val group
  : ('a : value_or_null).
  'a t @ l -> break:local_ ('a @ l -> 'a @ l -> bool) -> 'a t t @ l

(** This is just like [group], except that you get the index in the original list of the
    current element along with the two elements.

    Example, group the chars of ["Mississippi"] into triples:

    {[
      groupi ~break:(fun i _ _ -> i mod 3 = 0)
        ['M';'i';'s';'s';'i';'s';'s';'i';'p';'p';'i'] ->

      [['M'; 'i'; 's']; ['s'; 'i'; 's']; ['s'; 'i'; 'p']; ['p'; 'i']]
    ]} *)
val groupi
  : ('a : value_or_null).
  'a t @ l -> break:local_ (int -> 'a @ l -> 'a @ l -> bool) -> 'a t t @ l]

(** Group equal elements into the same buckets. Sorting is stable. *)
val sort_and_group
  : ('a : value_or_null).
  'a t -> compare:local_ ('a -> 'a -> int) -> 'a t t

(** [chunks_of l ~length] returns a list of lists whose concatenation is equal to the
    original list. Every list has [length] elements, except for possibly the last list,
    which may have fewer. [chunks_of] raises if [length <= 0]. *)
val%template chunks_of : ('a : value_or_null). 'a t @ l -> length:int -> 'a t t @ l
[@@zero_alloc_if_stack a] [@@alloc a @ l = (heap_global, stack_local)]

(** [chunk_evenly l ~into] returns a list of exactly [into] lists whose concatenation is
    equal to the original list. The number of items in each list are split as evenly as
    possible (i.e. lengths differ by at most 1).

    Specifically (with [n = length l]), the output consists of [n % into] lists of length
    [(n / into) + 1] followed by [into - n % into] of length [n / into]. Raises if
    [into <= 0]. *)
val chunk_evenly : 'a t -> into:int -> 'a t t

(** The final element of a list. The [_exn] version raises on the empty list. *)
val last : ('a : value_or_null). 'a t -> 'a option

val last_exn : ('a : value_or_null). 'a t -> 'a

(** [is_prefix xs ~prefix] returns [true] if [xs] starts with [prefix]. *)
val is_prefix
  : ('a : value_or_null).
  'a t -> prefix:'a t -> equal:local_ ('a -> 'a -> bool) -> bool

(** [is_suffix xs ~suffix] returns [true] if [xs] ends with [suffix]. *)
val is_suffix
  : ('a : value_or_null).
  'a t -> suffix:'a t -> equal:local_ ('a -> 'a -> bool) -> bool

(** [find_consecutive_duplicate t ~equal] returns the first pair of consecutive elements
    [(a1, a2)] in [t] such that [equal a1 a2]. They are returned in the same order as they
    appear in [t]. [equal] need not be an equivalence relation; it is simply used as a
    predicate on consecutive elements. *)
val find_consecutive_duplicate
  : ('a : value_or_null).
  'a t -> equal:local_ ('a -> 'a -> bool) -> ('a * 'a) option

(** Returns the given list with consecutive duplicates removed. The relative order of the
    other elements is unaffected. The element kept from a run of duplicates is determined
    by [which_to_keep]. *)
val remove_consecutive_duplicates
  : ('a : value_or_null).
  ?which_to_keep:[ `First | `Last ] (** default = `Last *)
  -> 'a t
  -> equal:local_ ('a -> 'a -> bool)
  -> 'a t

(** Returns the given list with duplicates removed and in sorted order. Of duplicates in
    the original list, the element occurring last in the original list is kept. *)
val%template dedup_and_sort
  : ('a : value_or_null).
  'a t @ l -> compare:local_ ('a @ l -> 'a @ l -> int) -> 'a t @ l
[@@alloc __ @ l = (heap_global, stack_local)]

(** Returns the original list, dropping all occurrences of duplicates after the first. *)
val stable_dedup : ('a : value_or_null). 'a t -> compare:local_ ('a -> 'a -> int) -> 'a t

(** [find_a_dup] returns a duplicate from the list (with no guarantees about which
    duplicate you get), or [None] if there are no dups. *)
val find_a_dup
  : ('a : value_or_null).
  'a t -> compare:local_ ('a -> 'a -> int) -> 'a option

(** Returns true if there are any two elements in the list which are the same. O(n log n)
    time complexity. *)
val contains_dup : ('a : value_or_null). 'a t -> compare:local_ ('a -> 'a -> int) -> bool

(** [find_all_dups] returns a list of all elements that occur more than once, with no
    guarantees about order. O(n log n) time complexity. *)
val find_all_dups
  : ('a : value_or_null).
  'a t -> compare:local_ ('a -> 'a -> int) -> 'a list

[%%template:
[@@@mode.default l = (global, local)]

(** [all_equal] returns a single element of the list that is equal to all other elements,
    or [None] if no such element exists. *)
val all_equal
  : ('a : value_or_null).
  'a t @ l -> equal:('a @ l -> 'a @ l -> bool) @ local -> 'a option @ l]

(** [range ?stride ?start ?stop start_i stop_i] is the list of integers from [start_i] to
    [stop_i], stepping by [stride]. If [stride] < 0 then we need [start_i] > [stop_i] for
    the result to be nonempty (or [start_i] = [stop_i] in the case where both bounds are
    inclusive). *)
val range
  :  ?stride:int (** default = 1 *)
  -> ?start:[ `inclusive | `exclusive ] (** default = `inclusive *)
  -> ?stop:[ `inclusive | `exclusive ] (** default = `exclusive *)
  -> int
  -> int
  -> int t

(** [range'] is analogous to [range] for general start/stop/stride types. [range'] raises
    if [stride x] returns [x] or if the direction that [stride x] moves [x] changes from
    one call to the next. *)
val range'
  : ('a : value_or_null).
  compare:local_ ('a -> 'a -> int)
  -> stride:local_ ('a -> 'a)
  -> ?start:[ `inclusive | `exclusive ] (** default = `inclusive *)
  -> ?stop:[ `inclusive | `exclusive ] (** default = `exclusive *)
  -> 'a
  -> 'a
  -> 'a t

[%%template:
[@@@alloc.default a @ l = (heap_global, stack_local)]

(** [filter_opt l] is the sublist of [l] containing only elements which are [Some e]. In
    other words, [filter_opt l] = [filter_map ~f:Fn.id l]. *)
val filter_opt : ('a : value_or_null). 'a option t @ l -> 'a t @ l

(** Like [filter_opt] but for [or_null] instead of [option]. In other words,
    [filter_or_null l] = [filter_map_or_null ~f:Fn.id l]. *)
val filter_or_null : 'a or_null t @ l -> 'a t @ l

[@@@mode lo = l]
[@@@mode.default li = (global, local)]

(** [rev_filter_map l ~f] is the reversed sublist of [l] containing only elements for
    which [f] returns [Some e]. *)
val rev_filter_map
  : ('a : value_or_null) ('b : value_or_null).
  'a t @ li -> f:('a @ li -> 'b option @ lo) @ local -> 'b t @ lo

(** rev_filter_mapi is just like [rev_filter_map], but it also passes in the index of each
    element as the first argument to the mapped function. Tail-recursive. *)
val rev_filter_mapi
  : ('a : value_or_null) ('b : value_or_null).
  'a t @ li -> f:(int -> 'a @ li -> 'b option @ lo) @ local -> 'b t @ lo

(** Like [rev_filter_map] but with a function returning [or_null] instead of [option]. *)
val rev_filter_map_or_null
  :  'a t @ li
  -> f:('a @ li -> 'b or_null @ lo) @ local
  -> 'b t @ lo

(** Like [rev_filter_mapi] but with a function returning [or_null] instead of [option]. *)
val rev_filter_mapi_or_null
  :  'a t @ li
  -> f:(int -> 'a @ li -> 'b or_null @ lo) @ local
  -> 'b t @ lo]

(** Create a list of every value [iter] passes to [f], in chronological order. *)
val of_iter : ('a : value_or_null). iter:local_ (f:local_ ('a -> unit) -> unit) -> 'a t

(** Interpret a list of (key, value) pairs as a map in which only the first occurrence of
    a key affects the semantics, i.e.:

    {[
      List.Assoc.xxx alist ...args...
    ]}

    is always the same as (or at least sort of isomorphic to):

    {[
      Map.xxx (alist |> Map.of_alist_multi |> Map.map ~f:List.hd) ...args...
    ]} *)
module Assoc : sig
  type ('a : value_or_null, 'b : value_or_null) t = ('a * 'b) list
  [@@deriving sexp ~stackify, sexp_grammar]

  (** Removes all existing entries with the same key before adding. *)
  val add
    : ('a : value_or_null) ('b : value_or_null).
    ('a, 'b) t -> equal:local_ ('a -> 'a -> bool) -> 'a -> 'b -> ('a, 'b) t

  val%template find
    : ('a : value_or_null) ('b : value_or_null).
    ('a, 'b) t @ l -> equal:local_ ('a @ l -> 'a @ l -> bool) -> 'a @ l -> 'b option @ l
  [@@mode l = (local, global)]

  val%template find_or_null
    : ('a : value) ('b : value).
    ('a, 'b) t @ l -> equal:local_ ('a @ l -> 'a @ l -> bool) -> 'a @ l -> 'b or_null @ l
  [@@mode l = (local, global)]

  val%template find_exn
    : ('a : value_or_null) ('b : value_or_null).
    ('a, 'b) t @ l -> equal:local_ ('a @ l -> 'a @ l -> bool) -> 'a @ l -> 'b @ l
  [@@mode l = (local, global)]

  val mem
    : ('a : value_or_null) ('b : value_or_null).
    ('a, 'b) t -> equal:local_ ('a -> 'a -> bool) -> 'a -> bool

  val remove
    : ('a : value_or_null) ('b : value_or_null).
    ('a, 'b) t -> equal:local_ ('a -> 'a -> bool) -> 'a -> ('a, 'b) t

  val map
    : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
    ('a, 'b) t -> f:local_ ('b -> 'c) -> ('a, 'c) t

  (** Bijectivity is not guaranteed because we allow a key to appear more than once. *)
  val inverse : ('a : value_or_null) ('b : value_or_null). ('a, 'b) t -> ('b, 'a) t

  (** Converts an association list with potential consecutive duplicate keys into an
      association list of (non-empty) lists with no (consecutive) duplicate keys. Any
      non-consecutive duplicate keys in the input will remain in the output. *)
  val group
    : ('a : value_or_null) ('b : value_or_null).
    ('a * 'b) list -> equal:local_ ('a -> 'a -> bool) -> ('a, 'b list) t

  (** Converts an association list with potential duplicate keys into an association list
      of (non-empty) lists with no duplicate keys. *)
  val sort_and_group
    : ('a : value_or_null) ('b : value_or_null).
    ('a * 'b) list -> compare:local_ ('a -> 'a -> int) -> ('a, 'b list) t
end

(** [sub pos len l] is the [len]-element sublist of [l], starting at [pos]. *)
val sub : ('a : value_or_null). 'a t -> pos:int -> len:int -> 'a t

[%%template:
[@@@alloc.default a @ l = (heap_global, stack_local)]

(** [take l n] returns the first [n] elements of [l], or all of [l] if [n > length l].
    [take l n = fst (split_n l n)]. If [n >= length l], returns [l] rather than a copy. *)
val take : ('a : value_or_null). 'a t @ l -> int -> 'a t @ l]

(** [drop l n] returns [l] without the first [n] elements, or the empty list if
    [n > length l]. [drop l n] is equivalent to [snd (split_n l n)]. If [n <= 0], returns
    [l] rather than a copy. *)
val drop : ('a : value_or_null). 'a t -> int -> 'a t

(** [take_while l ~f] returns the longest prefix of [l] for which [f] is [true]. *)
val take_while : ('a : value_or_null). 'a t -> f:local_ ('a -> bool) -> 'a t

(** [drop_while l ~f] drops the longest prefix of [l] for which [f] is [true]. *)
val drop_while : ('a : value_or_null). 'a t -> f:local_ ('a -> bool) -> 'a t

(** [split_while xs ~f = (take_while xs ~f, drop_while xs ~f)]. *)
val split_while : ('a : value_or_null). 'a t -> f:local_ ('a -> bool) -> 'a t * 'a t

(** [drop_last l] drops the last element of [l], returning [None] if [l] is [empty]. *)
val drop_last : ('a : value_or_null). 'a t -> 'a t option

val drop_last_exn : ('a : value_or_null). 'a t -> 'a t

(** Like [concat], but faster and without preserving any ordering (i.e., for lists that
    are essentially viewed as multi-sets). *)
val concat_no_order : ('a : value_or_null). 'a t t -> 'a t

val cons : ('a : value_or_null). 'a -> 'a t -> 'a t

(** Returns a list with all possible pairs -- if the input lists have length [len1] and
    [len2], the resulting list will have length [len1 * len2]. *)
val%template cartesian_product
  : ('a : value_or_null) ('b : value_or_null).
  'a t @ l -> 'b t @ l -> ('a * 'b) t @ l
[@@alloc __ @ l = (heap_global, stack_local)]

(** [permute ?random_state t] returns a permutation of [t].

    [permute] side-effects [random_state] by repeated calls to [Random.State.int]. If
    [random_state] is not supplied, [permute] uses [Random.State.default]. *)
val permute : ('a : value). ?random_state:Random.State.t -> 'a t -> 'a t

(** [random_element ?random_state t] is [None] if [t] is empty, else it is [Some x] for
    some [x] chosen uniformly at random from [t].

    [random_element] side-effects [random_state] by calling [Random.State.int]. If
    [random_state] is not supplied, [random_element] uses [Random.State.default]. *)
val random_element
  : ('a : value_or_null).
  ?random_state:Random.State.t -> 'a t -> 'a option

val random_element_exn : ('a : value_or_null). ?random_state:Random.State.t -> 'a t -> 'a

(** [is_sorted t ~compare] returns [true] iff for all adjacent [a1; a2] in [t],
    [compare a1 a2 <= 0].

    [is_sorted_strictly] is similar, except it uses [<] instead of [<=]. *)
val is_sorted : ('a : value_or_null). 'a t -> compare:local_ ('a -> 'a -> int) -> bool

val is_sorted_strictly
  : ('a : value_or_null).
  'a t -> compare:local_ ('a -> 'a -> int) -> bool

module Infix : sig
  val ( @ ) : ('a : value_or_null). 'a t -> 'a t -> 'a t
end

[%%template:
[@@@alloc.default __ @ l = (heap_global, stack_local)]

(** [transpose m] transposes the rows and columns of the matrix [m], considered as either
    a row of column lists or (dually) a column of row lists.

    Example:

    {[
      transpose [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ] = [ [ 1; 4 ]; [ 2; 5 ]; [ 3; 6 ] ]
    ]}

    On non-empty rectangular matrices, [transpose] is an involution (i.e.,
    [transpose (transpose m) = m]). Transpose returns [None] when called on lists of lists
    with non-uniform lengths. *)
val transpose : ('a : value_or_null). 'a t t @ l -> 'a t t option @ l

(** [transpose_exn] transposes the rows and columns of its argument, throwing an exception
    if the list is not rectangular. *)
val transpose_exn : ('a : value_or_null). 'a t t @ l -> 'a t t @ l]

(** [intersperse xs ~sep] places [sep] between adjacent elements of [xs]. For example,
    [intersperse [1;2;3] ~sep:0 = [1;0;2;0;3]]. *)
val intersperse : ('a : value_or_null). 'a t -> sep:'a -> 'a t

module Local : sig
  module Let_syntax : sig
    val return : ('a : value_or_null). 'a @ local -> 'a t @ local

    (** Pronounced "map". Infix form of [map]. *)
    val ( >>| )
      : ('a : value_or_null) ('b : value_or_null).
      'a t @ local -> ('a @ local -> 'b @ local) @ local -> 'b t @ local

    (** Pronounced "bind". Infix form of [bind]. *)
    val ( >>= )
      : ('a : value_or_null) ('b : value_or_null).
      'a t @ local -> ('a @ local -> 'b t @ local) @ local -> 'b t @ local

    module Let_syntax : sig
      val return : ('a : value_or_null). 'a @ local -> 'a t @ local

      val map
        : ('a : value_or_null) ('b : value_or_null).
        'a t @ local -> f:('a @ local -> 'b @ local) @ local -> 'b t @ local

      val bind
        : ('a : value_or_null) ('b : value_or_null).
        'a t @ local -> f:('a @ local -> 'b t @ local) @ local -> 'b t @ local

      val both
        : ('a : value_or_null) ('b : value_or_null).
        'a t @ local -> 'b t @ local -> ('a * 'b) t @ local

      module Open_on_rhs : sig end
    end
  end
end

module Private : sig
  val max_non_tailcall : int
end]
