@@ portable

(** Same semantics as ['a Array.t], except it's guaranteed that the representation array
    is not tagged with [Double_array_tag], the tag for float arrays.

    This means it's safer to use in the presence of [Obj.magic], but it's slower than
    normal [Array] if you use it with floats.

    It can often be faster than [Array] if you use it with non-floats. *)

open! Import

(** See [Base.Array] for comments. *)
type ('a : value_or_null) t : mutable_data with 'a
[@@deriving sexp, sexp_grammar, compare ~localize]

val invariant : ('a : value_or_null). 'a t -> unit
val empty : ('a : value_or_null). 'a t

(** For obtaining uncontended access to [empty] from a portable function. *)
val get_empty : unit -> _ t

val create : ('a : value_or_null). len:int -> 'a -> 'a t
val singleton : ('a : value_or_null). 'a -> 'a t
val init : ('a : value_or_null). int -> f:local_ (int -> 'a) -> 'a t
val length : ('a : value_or_null). local_ 'a t @ contended -> int
val get : ('a : value_or_null). local_ 'a t -> int -> 'a
val unsafe_get : ('a : value_or_null). local_ 'a t -> int -> 'a
val set : ('a : value_or_null). local_ 'a t -> int -> 'a -> unit
val unsafe_set : ('a : value_or_null). local_ 'a t -> int -> 'a -> unit
val swap : ('a : value_or_null). local_ 'a t -> int -> int -> unit

(** [unsafe_set_omit_phys_equal_check] is like [unsafe_set], except it doesn't do a
    [phys_equal] check to try to skip [caml_modify]. It is safe to call this even if the
    values are [phys_equal]. *)
val unsafe_set_omit_phys_equal_check
  : ('a : value_or_null).
  local_ 'a t -> int -> 'a -> unit

(** [unsafe_set_with_caml_modify] always calls [caml_modify] before setting and never gets
    the old value. This is like [unsafe_set_omit_phys_equal_check] except it doesn't check
    whether the old value and the value being set are integers to try to skip
    [caml_modify]. *)
val unsafe_set_with_caml_modify : ('a : value_or_null). local_ 'a t -> int -> 'a -> unit

(** Same as [unsafe_set_with_caml_modify], but with bounds check. *)
val set_with_caml_modify : ('a : value_or_null). local_ 'a t -> int -> 'a -> unit

val map : ('a : value_or_null) ('b : value_or_null). 'a t -> f:local_ ('a -> 'b) -> 'b t

val mapi
  : ('a : value_or_null) ('b : value_or_null).
  'a t -> f:local_ (int -> 'a -> 'b) -> 'b t

val iter : ('a : value_or_null). 'a t -> f:local_ ('a -> unit) -> unit

(** Like {!iter}, but the function is applied to the index of the element as first
    argument, and the element itself as second argument. *)
val iteri : ('a : value_or_null). 'a t -> f:local_ (int -> 'a -> unit) -> unit

val fold
  : ('a : value_or_null) 'acc.
  'a t -> init:'acc -> f:local_ ('acc -> 'a -> 'acc) -> 'acc

val foldi
  : ('a : value_or_null) 'acc.
  'a t -> init:'acc -> f:local_ (int -> 'acc -> 'a -> 'acc) -> 'acc

(** [unsafe_to_array_inplace__promise_not_a_float] converts from a [t] to an [array] in
    place. This function is unsafe if the underlying type is a float. *)
val unsafe_to_array_inplace__promise_not_a_float : 'a t -> 'a array

(** [of_array] and [to_array] return fresh arrays with the same contents rather than
    returning a reference to the underlying array. *)
val of_array : 'a array -> 'a t

val%template to_array : 'a t @ m -> 'a array @ m
[@@alloc __ @ m = (heap_global, stack_local)]

val of_list : ('a : value_or_null). 'a list -> 'a t
val of_list_rev : ('a : value_or_null). 'a list -> 'a t
val to_list : ('a : value_or_null). 'a t -> 'a list

include Blit.S1 with type 'a t := 'a t

val copy : ('a : value_or_null). 'a t -> 'a t
val exists : ('a : value_or_null). 'a t -> f:local_ ('a -> bool) -> bool
val existsi : ('a : value_or_null). 'a t -> f:local_ (int -> 'a -> bool) -> bool
val for_all : ('a : value_or_null). 'a t -> f:local_ ('a -> bool) -> bool
val for_alli : ('a : value_or_null). 'a t -> f:local_ (int -> 'a -> bool) -> bool
val concat : ('a : value_or_null). 'a t list -> 'a t

val concat_map
  : ('a : value_or_null) ('b : value_or_null).
  'a t -> f:local_ ('a -> 'b t) -> 'b t

val concat_mapi
  : ('a : value_or_null) ('b : value_or_null).
  'a t -> f:local_ (int -> 'a -> 'b t) -> 'b t

val partition_map
  : ('a : value_or_null) 'b 'c.
  'a t -> f:local_ ('a -> ('b, 'c) Either.t) -> 'b t * 'c t

val filter : ('a : value_or_null). 'a t -> f:local_ ('a -> bool) -> 'a t
val filteri : ('a : value_or_null). 'a t -> f:local_ (int -> 'a -> bool) -> 'a t

val filter_map
  : ('a : value_or_null) ('b : value_or_null).
  'a t -> f:local_ ('a -> 'b option) -> 'b t

val filter_mapi
  : ('a : value_or_null) ('b : value_or_null).
  'a t -> f:local_ (int -> 'a -> 'b option) -> 'b t

val find : ('a : value_or_null). 'a t -> f:local_ ('a -> bool) -> 'a option

val findi
  : ('a : value_or_null).
  'a t -> f:local_ (int -> 'a -> bool) -> (int * 'a) option

val find_map : ('a : value_or_null) 'b. 'a t -> f:local_ ('a -> 'b option) -> 'b option

val find_mapi
  : ('a : value_or_null) 'b.
  'a t -> f:local_ (int -> 'a -> 'b option) -> 'b option

(** Functions with the 2 suffix raise an exception if the lengths of the two given arrays
    aren't the same. *)
val map2_exn
  : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
  'a t -> 'b t -> f:local_ ('a -> 'b -> 'c) -> 'c t

val fold2_exn
  : ('a : value_or_null) ('b : value_or_null) 'acc.
  'a t -> 'b t -> init:'acc -> f:local_ ('acc -> 'a -> 'b -> 'acc) -> 'acc

val min_elt : 'a t -> compare:local_ ('a -> 'a -> int) -> 'a option
val max_elt : 'a t -> compare:local_ ('a -> 'a -> int) -> 'a option

(** [sort] uses constant heap space.

    To sort only part of the array, specify [pos] to be the index to start sorting from
    and [len] indicating how many elements to sort. *)
val sort : ?pos:int -> ?len:int -> local_ 'a t -> compare:local_ ('a -> 'a -> int) -> unit

include Binary_searchable.S1 with type 'a t := 'a t

(** {2 Extra lowlevel and unsafe functions} *)

(** The behavior is undefined if you access an element before setting it. *)
val unsafe_create_uninitialized : ('a : value_or_null). len:int -> 'a t

(** New obj array filled with [Obj.repr 0] *)
val create_obj_array : len:int -> Stdlib.Obj.t t

(** [unsafe_set_assuming_currently_int t i obj] sets index [i] of [t] to [obj], but only
    works correctly if the value there is an immediate, i.e.
    [Stdlib.Obj.is_int (get t i)]. This precondition saves a dynamic check.

    [unsafe_set_int_assuming_currently_int] is similar, except the value being set is an
    int.

    [unsafe_set_int] is similar but does not assume anything about the target. *)
val unsafe_set_assuming_currently_int
  :  local_ Stdlib.Obj.t t
  -> int
  -> Stdlib.Obj.t
  -> unit

val unsafe_set_int_assuming_currently_int : local_ Stdlib.Obj.t t -> int -> int -> unit
val unsafe_set_int : local_ Stdlib.Obj.t t -> int -> int -> unit

(** [unsafe_clear_if_pointer t i] prevents [t.(i)] from pointing to anything to prevent
    space leaks. It does this by setting [t.(i)] to [Stdlib.Obj.repr 0]. As a performance
    hack, it only does this when [not (Stdlib.Obj.is_int t.(i))]. It is an error to access
    the cleared index before setting it again. *)
val unsafe_clear_if_pointer : local_ Stdlib.Obj.t t -> int -> unit
