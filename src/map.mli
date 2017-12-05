open! Import
open! Map_intf

type ('key, +'value, 'cmp) t

type ('k, 'cmp) comparator =
  (module Comparator.S with type t = 'k and type comparator_witness = 'cmp)

(** Test if invariants of internal AVL search tree hold. *)
val invariants : (_, _, _) t -> bool

val comparator : ('a, _, 'cmp) t -> ('a, 'cmp) Comparator.t

(** the empty map *)
val empty : ('a, 'cmp) comparator -> ('a, 'b, 'cmp) t

(** map with one key, data pair *)
val singleton : ('a, 'cmp) comparator -> 'a -> 'b -> ('a, 'b, 'cmp) t

(** creates map from association list with unique keys *)
val of_alist
  :  ('a, 'cmp) comparator
  -> ('a * 'b) list
  -> [ `Ok of ('a, 'b, 'cmp) t | `Duplicate_key of 'a ]

(** creates map from association list with unique keys.  Returns an error if duplicate 'a
    keys are found. *)
val of_alist_or_error
  :  ('a, 'cmp) comparator
  -> ('a * 'b) list -> ('a, 'b, 'cmp) t Or_error.t

(** creates map from association list with unique keys.  Raises an exception if duplicate
    'a keys are found. *)
val of_alist_exn
  :  ('a, 'cmp) comparator
  -> ('a * 'b) list -> ('a, 'b, 'cmp) t

(** creates map from association list with possibly repeated keys. *)
val of_alist_multi
  :  ('a, 'cmp) comparator
  -> ('a * 'b) list -> ('a, 'b list, 'cmp) t

(** combines an association list into a map, folding together bound values with common
    keys *)
val of_alist_fold
  :  ('a, 'cmp) comparator
  -> ('a * 'b) list -> init:'c -> f:('c -> 'b -> 'c) -> ('a, 'c, 'cmp) t

(** combines an association list into a map, reducing together bound values with common
    keys *)
val of_alist_reduce
  :  ('a, 'cmp) comparator
  -> ('a * 'b) list -> f:('b -> 'b -> 'b) -> ('a, 'b, 'cmp) t

(** [of_iteri ~iteri] behaves like [of_alist], except that instead of taking a concrete
    datastruture, it takes an iteration function.  For instance, to convert a string table
    into a map: [of_iteri (module String) ~f:(Hashtbl.iteri table)].  It is faster than
    adding the elements one by one. *)
val of_iteri
  :  ('a, 'cmp) comparator
  -> iteri:(f:(key:'a -> data:'b -> unit) -> unit)
  -> [ `Ok of ('a, 'b, 'cmp) t
     | `Duplicate_key of 'a ]

(** creates map from sorted array of key-data pairs. The input array must be sorted, as
    given by the relevant comparator (either in ascending or descending order), and must
    not contain any duplicate keys.  If either of these conditions do not hold, an error
    is returned.  *)
val of_sorted_array
  :  ('a, 'cmp) comparator
  -> ('a * 'b) array -> ('a, 'b, 'cmp) t Or_error.t

(** Like [of_sorted_array] except it returns a map with broken invariants when an [Error]
    would have been returned. *)
val of_sorted_array_unchecked
  :  ('a, 'cmp) comparator
  -> ('a * 'b) array -> ('a, 'b, 'cmp) t

(** [of_increasing_iterator_unchecked c ~len ~f] behaves like [of_sorted_array_unchecked c
    (Array.init len ~f)], with the additional restriction that a decreasing order is not
    supported.  The advantage is not requiring you to allocate an intermediate array.  [f]
    will be called with 0, 1, ... [len - 1], in order. *)
val of_increasing_iterator_unchecked
  :  ('a, 'cmp) comparator
  -> len:int
  -> f:(int -> 'a * 'b)
  -> ('a, 'b, 'cmp) t

(** [of_increasing_sequence c seq] behaves like [of_sorted_array c (Sequence.to_array seq)],
    but does not allocate the intermediate array.

    The sequence will be folded over once, and the additional time complexity is /O(n)/.
*)
val of_increasing_sequence
  :  ('k, 'cmp) comparator
  -> ('k * 'v) Sequence.t
  -> ('k, 'v, 'cmp) t Or_error.t

(** Test whether a map is empty or not. *)
val is_empty : (_, _, _) t -> bool

(** [length map] @return number of elements in [map].  O(1), but [Tree.length] is O(n). *)
val length : (_, _, _) t -> int

(** returns a new map with the specified new binding; if the key was already bound, its
    previous binding disappears. *)
val set : ('k, 'v, 'cmp) t -> key:'k -> data:'v -> ('k, 'v, 'cmp) t

val add : ('k, 'v, 'cmp) t -> key:'k -> data:'v -> ('k, 'v, 'cmp) t
[@@deprecated "[since 2017-11] Use [set] instead"]

(** if key is not present then add a singleton list, otherwise, cons data on the head of
    the existing list. *)
val add_multi
  :  ('k, 'v list, 'cmp) t
  -> key:'k
  -> data:'v
  -> ('k, 'v list, 'cmp) t

(** if key is present then remove its head element; if result is empty, remove the key. *)
val remove_multi
  :  ('k, 'v list, 'cmp) t
  -> 'k
  -> ('k, 'v list, 'cmp) t

(** returns the value bound to the given key, or the empty list if there is none. *)
val find_multi
  : ('k, 'v list, 'cmp) t
  -> 'k
  -> 'v list

(** [change t key ~f] returns a new map [m] that is the same as [t] on all keys except for
    [key], and whose value for [key] is defined by [f], i.e. [find m key = f (find t
    key)]. *)
val change
  :  ('k, 'v, 'cmp) t
  -> 'k
  -> f:('v option -> 'v option)
  -> ('k, 'v, 'cmp) t

(** [update t key ~f] is [change t key ~f:(fun o -> Some (f o))]. *)
val update
  :  ('k, 'v, 'cmp) t
  -> 'k
  -> f:('v option -> 'v)
  -> ('k, 'v, 'cmp) t

(** returns the value bound to the given key, raising [Not_found] if none such exists *)
val find     : ('k, 'v, 'cmp) t -> 'k -> 'v option
val find_exn : ('k, 'v, 'cmp) t -> 'k -> 'v

(** returns a new map with any binding for the key in question removed *)
val remove : ('k, 'v, 'cmp) t -> 'k -> ('k, 'v, 'cmp) t

(** [mem map key] tests whether [map] contains a binding for [key] *)
val mem : ('k, _, 'cmp) t -> 'k -> bool

val iter_keys : ('k, _, _) t -> f:('k -> unit) -> unit
val iter      : (_, 'v, _) t -> f:('v -> unit) -> unit
val iteri     : ('k, 'v, _) t -> f:(key:'k -> data:'v -> unit) -> unit

(** Iterate two maps side by side.  Complexity of this function is O(M+N).  If two inputs
    are [(0, a); (1, a)] and [(1, b); (2, b)], [f] will be called with [(0, `Left a); (1,
    `Both (a, b)); (2, `Right b)] *)
val iter2
  :  ('k, 'v1, 'cmp) t
  -> ('k, 'v2, 'cmp) t
  -> f:(key:'k -> data:[ `Left of 'v1 | `Right of 'v2 | `Both of 'v1 * 'v2 ] -> unit)
  -> unit

(** returns new map with bound values replaced by f applied to the bound values *)
val map : ('k, 'v1, 'cmp) t -> f:('v1 -> 'v2) -> ('k, 'v2, 'cmp) t

(** like [map], but function takes both key and data as arguments *)
val mapi
  :  ('k, 'v1, 'cmp) t
  -> f:(key:'k -> data:'v1 -> 'v2)
  -> ('k, 'v2, 'cmp) t

(** folds over keys and data in map in increasing order of key. *)
val fold : ('k, 'v, _) t -> init:'a -> f:(key:'k -> data:'v -> 'a -> 'a) -> 'a

(** folds over keys and data in map in decreasing order of key. *)
val fold_right : ('k, 'v, _) t -> init:'a -> f:(key:'k -> data:'v -> 'a -> 'a) -> 'a

(** folds over two maps side by side, like [iter2]. *)
val fold2
  :  ('k, 'v1, 'cmp) t
  -> ('k, 'v2, 'cmp) t
  -> init:'a
  -> f:(key:'k -> data:[ `Left of 'v1 | `Right of 'v2 | `Both of 'v1 * 'v2 ] -> 'a -> 'a)
  -> 'a

(** [filter], [filteri], [filter_keys], [filter_map], and [filter_mapi] run in O(n * lg n)
    time; they simply accumulate each key & data retained by [f] into a new map using
    [add]. *)
val filter_keys : ('k, 'v, 'cmp) t -> f:('k -> bool) -> ('k, 'v, 'cmp) t
val filter      : ('k, 'v, 'cmp) t -> f:('v -> bool) -> ('k, 'v, 'cmp) t
val filteri     : ('k, 'v, 'cmp) t -> f:(key:'k -> data:'v -> bool) -> ('k, 'v, 'cmp) t

(** returns new map with bound values filtered by f applied to the bound values *)
val filter_map
  :  ('k, 'v1, 'cmp) t
  -> f:('v1 -> 'v2 option)
  -> ('k, 'v2, 'cmp) t

(** like [filter_map], but function takes both key and data as arguments*)
val filter_mapi
  :  ('k, 'v1, 'cmp) t
  -> f:(key:'k -> data:'v1 -> 'v2 option)
  -> ('k, 'v2, 'cmp) t

(** [partition_mapi t ~f] returns two new [t]s, with each key in [t] appearing in exactly
    one of the result maps depending on its mapping in [f]. *)
val partition_mapi
  :  ('k, 'v1, 'cmp) t
  -> f:(key:'k -> data:'v1 -> [`Fst of 'v2 | `Snd of 'v3])
  -> ('k, 'v2, 'cmp) t * ('k, 'v3, 'cmp) t

(** [partition_map t ~f = partition_mapi t ~f:(fun ~key:_ ~data -> f data)] *)
val partition_map
  :  ('k, 'v1, 'cmp) t
  -> f:('v1 -> [`Fst of 'v2 | `Snd of 'v3])
  -> ('k, 'v2, 'cmp) t * ('k, 'v3, 'cmp) t

(**
   {[
     partitioni_tf t ~f
     =
     partition_mapi t ~f:(fun ~key ~data ->
       if f ~key ~data
       then `Fst data
       else `Snd data)
   ]} *)
val partitioni_tf
  :  ('k, 'v, 'cmp) t
  -> f:(key:'k -> data:'v -> bool)
  -> ('k, 'v, 'cmp) t * ('k, 'v, 'cmp) t

(** [partition_tf t ~f = partitioni_tf t ~f:(fun ~key:_ ~data -> f data)] *)
val partition_tf
  :  ('k, 'v, 'cmp) t
  -> f:('v -> bool)
  -> ('k, 'v, 'cmp) t * ('k, 'v, 'cmp) t

(** Total ordering between maps.  The first argument is a total ordering used to compare
    data associated with equal keys in the two maps. *)
val compare_direct
  :  ('v -> 'v -> int)
  -> ('k, 'v, 'cmp) t
  -> ('k, 'v, 'cmp) t
  -> int

(** Hash function: a building block to use when hashing data structures containing
    maps in them. [hash_fold_direct hash_fold_key] is compatible with
    [compare_direct] iff [hash_fold_key] is compatible with [(comparator m).compare]
    of the map [m] being hashed. *)
val hash_fold_direct
  :  'k Hash.folder
  -> 'v Hash.folder
  -> ('k, 'v, 'cmp) t Hash.folder

(** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are equal, that is, contain
    equal keys and associate them with equal data.  [cmp] is the equality predicate used
    to compare the data associated with the keys. *)
val equal
  :  ('v -> 'v -> bool)
  -> ('k, 'v, 'cmp) t
  -> ('k, 'v, 'cmp) t
  -> bool

(** returns list of keys in map *)
val keys : ('k, _, _) t -> 'k list

(** returns list of data in map *)
val data : (_, 'v, _) t -> 'v list

(** creates association list from map. *)
val to_alist
  :  ?key_order : [ `Increasing | `Decreasing ]  (** default is [`Increasing] *)
  -> ('k, 'v, _) t
  -> ('k * 'v) list

val validate : name:('k -> string) -> 'v Validate.check -> ('k, 'v, _) t Validate.check

(** {6 Additional operations on maps} *)

(** Merges two maps. The runtime is O(length(t1) + length(t2)). In particular,
    you shouldn't use this function to merge a list of maps. Consider using
    [merge_skewed] instead. *)
val merge
  :  ('k, 'v1, 'cmp) t
  -> ('k, 'v2, 'cmp) t
  -> f:(key:'k
        -> [ `Left of 'v1 | `Right of 'v2 | `Both of 'v1 * 'v2 ]
        -> 'v3 option)
  -> ('k, 'v3, 'cmp) t

(** A special case of [merge], [merge_skewed t1 t2] is a map containing all the
    bindings of [t1] and [t2]. Bindings that appear in both [t1] and [t2] are
    combined in to a single value using the [combine] function. In a call
    [combine ~key v1 v2] the value [v1] comes from [t1] and [v2] from [t2].

    The runtime of [merge_skewed] is [O(l1 * log(l2))], where [l1] is the length
    of the smaller map and [l2] the length of the larger map. This is likely to
    be faster than [merge] when one of the maps is a lot smaller, or when you
    merge a list of maps. *)
val merge_skewed
  :  ('k, 'v, 'cmp) t
  -> ('k, 'v, 'cmp) t
  -> combine:(key:'k -> 'v -> 'v -> 'v)
  -> ('k, 'v, 'cmp) t

module Symmetric_diff_element : sig
  type ('k, 'v) t = 'k * [ `Left of 'v | `Right of 'v | `Unequal of 'v * 'v ]
  [@@deriving_inline compare, sexp]
  include
  sig
    [@@@ocaml.warning "-32"]
    val compare :
      ('k -> 'k -> int) -> ('v -> 'v -> int) -> ('k,'v) t -> ('k,'v) t -> int
    val t_of_sexp :
      (Sexplib.Sexp.t -> 'k) ->
      (Sexplib.Sexp.t -> 'v) -> Sexplib.Sexp.t -> ('k,'v) t
    val sexp_of_t :
      ('k -> Sexplib.Sexp.t) ->
      ('v -> Sexplib.Sexp.t) -> ('k,'v) t -> Sexplib.Sexp.t
  end
  [@@@end]
end

(** [symmetric_diff t1 t2 ~data_equal] returns a list of changes between [t1] and [t2].
    It is intended to be efficient in the case where [t1] and [t2] share a large amount of
    structure.  The keys in the output sequence will be in sorted order. *)
val symmetric_diff
  :  ('k, 'v, 'cmp) t
  -> ('k, 'v, 'cmp) t
  -> data_equal:('v -> 'v -> bool)
  -> ('k, 'v) Symmetric_diff_element.t Sequence.t

(** [min_elt map] @return Some [(key, data)] pair corresponding to the minimum key in
    [map], None if empty. *)
val min_elt     : ('k, 'v, _) t -> ('k * 'v) option
val min_elt_exn : ('k, 'v, _) t ->  'k * 'v

(** [max_elt map] @return Some [(key, data)] pair corresponding to the maximum key in
    [map], and None if [map] is empty. *)
val max_elt     : ('k, 'v, _) t -> ('k * 'v) option
val max_elt_exn : ('k, 'v, _) t ->  'k * 'v

(** same semantics as similar functions in List *)
val for_all  : ('k, 'v, _) t -> f:(               'v -> bool) -> bool
val for_alli : ('k, 'v, _) t -> f:(key:'k -> data:'v -> bool) -> bool
val exists   : ('k, 'v, _) t -> f:(               'v -> bool) -> bool
val existsi  : ('k, 'v, _) t -> f:(key:'k -> data:'v -> bool) -> bool
val count    : ('k, 'v, _) t -> f:(               'v -> bool) -> int
val counti   : ('k, 'v, _) t -> f:(key:'k -> data:'v -> bool) -> int

(** [split t key] returns a map of keys strictly less than [key], the mapping of [key] if
    any, and a map of keys strictly greater than [key].

    Runtime is O(m + log n) where n is the size of the input map, and m is the size of the
    smaller of the two output maps.  The O(m) term is due to the need to calculate the
    length of the output maps. **)
val split
  :  ('k, 'v, 'cmp) t
  -> 'k
  -> ('k, 'v, 'cmp) t * ('k * 'v) option * ('k, 'v, 'cmp) t

(** [append ~lower_part ~upper_part] returns [`Ok map]
    where map contains all the [(key, value)] pairs from the two input maps
    if all the keys from [lower_part] are less than all the keys from [upper_part].
    Otherwise it returns [`Overlapping_key_ranges].

    Runtime is O(log n) where n is the size of the larger input map.  This can be
    significantly faster than [Map.merge] or repeated [Map.add].

    {[
      assert (match Map.append ~lower_part ~upper_part with
        | `Ok whole_map ->
          whole_map
          = Map.(of_alist_exn (List.append (to_alist lower_part) (to_alist upper_part)))
        | `Overlapping_key_ranges -> true);
    ]} *)
val append
  :  lower_part:('k, 'v, 'cmp) t
  -> upper_part:('k, 'v, 'cmp) t
  -> [ `Ok of ('k, 'v, 'cmp) t
     | `Overlapping_key_ranges ]

(** [subrange t ~lower_bound ~upper_bound] returns a map containing all the entries from
    [t] whose keys lie inside the interval indicated by [~lower_bound] and [~upper_bound].
    If this interval is empty, an empty map is returned.

    Runtime is O(m + log n) where n is the size of the input map, and m is the size of the
    output map.  The O(m) term is due to the need to calculate the length of the output
    map. *)
val subrange
  :  ('k, 'v, 'cmp) t
  -> lower_bound:'k Maybe_bound.t
  -> upper_bound:'k Maybe_bound.t
  -> ('k, 'v, 'cmp) t

(** [fold_range_inclusive t ~min ~max ~init ~f]
    folds f (with initial value ~init) over all keys (and their associated values)
    that are in the range [min, max] (inclusive).  *)
val fold_range_inclusive
  :  ('k, 'v, 'cmp) t
  -> min:'k
  -> max:'k
  -> init:'a
  -> f:(key:'k -> data:'v -> 'a -> 'a)
  -> 'a

(** [range_to_alist t ~min ~max] returns an associative list of the elements whose
    keys lie in [min, max] (inclusive), with the smallest key being at the head of the
    list. *)
val range_to_alist : ('k, 'v, 'cmp) t -> min:'k -> max:'k -> ('k * 'v) list

(** [closest_key t dir k] returns the [(key, value)] pair in [t] with [key] closest to
    [k], which satisfies the given inequality bound.

    For example, [closest_key t `Less_than k] would be the pair with the closest key to
    [k] where [key < k].

    [to_sequence] can be used to get the same results as [closest_key].  It is less
    efficient for individual lookups but more efficient for finding many elements starting
    at some value. *)
val closest_key
  :  ('k, 'v, 'cmp) t
  -> [ `Greater_or_equal_to
     | `Greater_than
     | `Less_or_equal_to
     | `Less_than
     ]
  -> 'k
  -> ('k * 'v) option

(** [nth t n] finds the (key, value) pair of rank n (i.e. such that there are exactly n
    keys strictly less than the found key), if one exists.  O(log(length t) + n) time. *)
val nth     : ('k, 'v, _) t -> int -> ('k * 'v) option
val nth_exn : ('k, 'v, _) t -> int -> ('k * 'v)

(** [rank t k] if k is in t, returns the number of keys strictly less than k in t,
    otherwise None *)
val rank : ('k, 'v, 'cmp) t -> 'k -> int option

(** [to_sequence ?order ?keys_greater_or_equal_to ?keys_less_or_equal_to t] gives a
    sequence of key-value pairs between [keys_less_or_equal_to] and
    [keys_greater_or_equal_to] inclusive, presented in [order].  If
    [keys_greater_or_equal_to > keys_less_or_equal_to], the sequence is empty.  Cost is
    O(log n) up front and amortized O(1) to produce each element. *)
val to_sequence
  :  ?order                    : [ `Increasing_key (** default *) | `Decreasing_key ]
  -> ?keys_greater_or_equal_to : 'k
  -> ?keys_less_or_equal_to    : 'k
  -> ('k, 'v, 'cmp) t
  -> ('k * 'v) Sequence.t

(** [M] is meant to be used in combination with OCaml applicative functor types:

    {[
      type string_to_int_map = int Map.M(String).t
    ]}

    which stands for:

    {[
      type string_to_int_map = (String.t, int, String.comparator_witness) Map.t
    ]}

    The point is that [int Map.M(String).t] supports deriving, whereas the second syntax
    doesn't (because there is no such thing as, say, String.sexp_of_comparator_witness,
    instead you would want to pass the comparator directly).

    In addition, the requirement of [@@deriving_inline][@@@end] on the key module are only what is needed
    to satisfy what you are trying to derive on the map itself. Say you write:

    {[
      type t = int Map.M(X).t [@@deriving_inline hash][@@@end]
    ]}

    then this will be well typed exactly if X contains at least:
    - a type t with no parameters
    - a comparator witness
    - a hash_fold_t function with the right type *)
module M (K : sig type t type comparator_witness end) : sig
  type nonrec 'v t = (K.t, 'v, K.comparator_witness) t
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

val sexp_of_m__t
  :  (module Sexp_of_m with type t = 'k)
  -> ('v -> Sexp.t)
  -> ('k, 'v, 'cmp) t
  -> Sexp.t

val m__t_of_sexp
  :  (module M_of_sexp with type t = 'k and type comparator_witness = 'cmp)
  -> (Sexp.t -> 'v)
  -> Sexp.t
  -> ('k, 'v, 'cmp) t

val compare_m__t
  :  (module Compare_m)
  -> ('v -> 'v -> int)
  -> ('k, 'v, 'cmp) t
  -> ('k, 'v, 'cmp) t
  -> int

val hash_fold_m__t
  :  (module Hash_fold_m with type t = 'k)
  -> (Hash.state -> 'v -> Hash.state)
  -> (Hash.state -> ('k, 'v, _) t -> Hash.state)

(** Using comparator is a similar interface as the toplevel of [Map], except the functions
    take a [~comparator:('k, 'cmp) Comparator.t] where the functions at the toplevel of
    [Map] takes a [('k, 'cmp) comparator]. *)
module Using_comparator : sig
  type nonrec ('k, +'v, 'cmp) t = ('k, 'v, 'cmp) t [@@deriving_inline sexp_of]
  include
  sig
    [@@@ocaml.warning "-32"]
    val sexp_of_t :
      ('k -> Sexplib.Sexp.t) ->
      ('v -> Sexplib.Sexp.t) ->
      ('cmp -> Sexplib.Sexp.t) -> ('k,'v,'cmp) t -> Sexplib.Sexp.t
  end
  [@@@end]

  val t_of_sexp_direct
    :  comparator:('k, 'cmp) Comparator.t
    -> (Sexp.t -> 'k)
    -> (Sexp.t -> 'v)
    -> Sexp.t
    -> ('k, 'v, 'cmp) t

  module Tree : sig
    type ('k, +'v, 'cmp) t [@@deriving_inline sexp_of]
    include
    sig
      [@@@ocaml.warning "-32"]
      val sexp_of_t :
        ('k -> Sexplib.Sexp.t) ->
        ('v -> Sexplib.Sexp.t) ->
        ('cmp -> Sexplib.Sexp.t) -> ('k,'v,'cmp) t -> Sexplib.Sexp.t
    end
    [@@@end]

    val t_of_sexp_direct
      :  comparator:('k, 'cmp) Comparator.t
      -> (Sexp.t -> 'k)
      -> (Sexp.t -> 'v)
      -> Sexp.t
      -> ('k, 'v, 'cmp) t

    include Creators_and_accessors3_with_comparator
      with type ('a, 'b, 'c) t    := ('a, 'b, 'c) t
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) t

    val empty_without_value_restriction : (_, _, _) t
  end

  include Accessors3
    with type ('a, 'b, 'c) t    := ('a, 'b, 'c) t
    with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Tree.t
  include Creators3_with_comparator
    with type ('a, 'b, 'c) t    := ('a, 'b, 'c) t
    with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Tree.t

  val comparator : ('a, _, 'cmp) t -> ('a, 'cmp) Comparator.t

  val hash_fold_direct
    :  'k Hash.folder
    -> 'v Hash.folder
    -> ('k, 'v, 'cmp) t Hash.folder

  module Empty_without_value_restriction (K : Comparator.S1) : sig
    val empty : ('a K.t, 'v, K.comparator_witness) t
  end
end

