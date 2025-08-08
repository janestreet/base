(** Same semantics as ['a Array.t], except it's guaranteed that the representation array
    is not tagged with [Double_array_tag], the tag for float arrays.

    This means it's safer to use in the presence of [Obj.magic], but it's slower than
    normal [Array] if you use it with floats.

    It can often be faster than [Array] if you use it with non-floats. *)

open! Import

(** See [Base.Array] for comments. *)
type 'a t [@@deriving sexp, sexp_grammar, compare ~localize]

val invariant : 'a. 'a t -> unit
val empty : 'a. 'a t

(** For obtaining uncontended access to [empty] from a portable function. *)
val get_empty : unit -> _ t

val create : 'a. len:int -> 'a -> 'a t
val singleton : 'a. 'a -> 'a t
val init : 'a. int -> f:(int -> 'a) -> 'a t
val length : 'a. 'a t -> int
val get : 'a. 'a t -> int -> 'a
val unsafe_get : 'a. 'a t -> int -> 'a
val set : 'a. 'a t -> int -> 'a -> unit
val unsafe_set : 'a. 'a t -> int -> 'a -> unit
val swap : 'a. 'a t -> int -> int -> unit

(** [unsafe_set_omit_phys_equal_check] is like [unsafe_set], except it doesn't do a
    [phys_equal] check to try to skip [caml_modify]. It is safe to call this even if the
    values are [phys_equal]. *)
val unsafe_set_omit_phys_equal_check : 'a. 'a t -> int -> 'a -> unit

(** [unsafe_set_with_caml_modify] always calls [caml_modify] before setting and never gets
    the old value. This is like [unsafe_set_omit_phys_equal_check] except it doesn't check
    whether the old value and the value being set are integers to try to skip
    [caml_modify]. *)
val unsafe_set_with_caml_modify : 'a. 'a t -> int -> 'a -> unit

(** Same as [unsafe_set_with_caml_modify], but with bounds check. *)
val set_with_caml_modify : 'a. 'a t -> int -> 'a -> unit

val map : 'a 'b. 'a t -> f:('a -> 'b) -> 'b t
val mapi : 'a 'b. 'a t -> f:(int -> 'a -> 'b) -> 'b t
val iter : 'a. 'a t -> f:('a -> unit) -> unit

(** Like {!iter}, but the function is applied to the index of the element as first
    argument, and the element itself as second argument. *)
val iteri : 'a. 'a t -> f:(int -> 'a -> unit) -> unit

val fold : 'a 'acc. 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
val foldi : 'a 'acc. 'a t -> init:'acc -> f:(int -> 'acc -> 'a -> 'acc) -> 'acc

(** [unsafe_to_array_inplace__promise_not_a_float] converts from a [t] to an [array] in
    place. This function is unsafe if the underlying type is a float. *)
val unsafe_to_array_inplace__promise_not_a_float : 'a t -> 'a array

(** [of_array] and [to_array] return fresh arrays with the same contents rather than
    returning a reference to the underlying array. *)
val of_array : 'a array -> 'a t

val%template to_array : 'a t -> 'a array [@@alloc __ @ m = (heap_global, stack_local)]

val of_list : 'a. 'a list -> 'a t
val of_list_rev : 'a. 'a list -> 'a t
val to_list : 'a. 'a t -> 'a list

include Blit.S1 with type 'a t := 'a t

val copy : 'a. 'a t -> 'a t
val exists : 'a. 'a t -> f:('a -> bool) -> bool
val existsi : 'a. 'a t -> f:(int -> 'a -> bool) -> bool
val for_all : 'a. 'a t -> f:('a -> bool) -> bool
val for_alli : 'a. 'a t -> f:(int -> 'a -> bool) -> bool
val concat : 'a. 'a t list -> 'a t
val concat_map : 'a 'b. 'a t -> f:('a -> 'b t) -> 'b t
val concat_mapi : 'a 'b. 'a t -> f:(int -> 'a -> 'b t) -> 'b t
val partition_map : 'a 'b 'c. 'a t -> f:('a -> ('b, 'c) Either.t) -> 'b t * 'c t
val filter : 'a. 'a t -> f:('a -> bool) -> 'a t
val filteri : 'a. 'a t -> f:(int -> 'a -> bool) -> 'a t
val filter_map : 'a 'b. 'a t -> f:('a -> 'b option) -> 'b t
val filter_mapi : 'a 'b. 'a t -> f:(int -> 'a -> 'b option) -> 'b t
val find : 'a. 'a t -> f:('a -> bool) -> 'a option
val findi : 'a. 'a t -> f:(int -> 'a -> bool) -> (int * 'a) option
val find_map : 'a 'b. 'a t -> f:('a -> 'b option) -> 'b option
val find_mapi : 'a 'b. 'a t -> f:(int -> 'a -> 'b option) -> 'b option

(** Functions with the 2 suffix raise an exception if the lengths of the two given arrays
    aren't the same. *)
val map2_exn : 'a 'b 'c. 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

val fold2_exn
  : 'a 'b 'acc.
  'a t -> 'b t -> init:'acc -> f:('acc -> 'a -> 'b -> 'acc) -> 'acc

val min_elt : 'a t -> compare:('a -> 'a -> int) -> 'a option
val max_elt : 'a t -> compare:('a -> 'a -> int) -> 'a option

(** [sort] uses constant heap space.

    To sort only part of the array, specify [pos] to be the index to start sorting from
    and [len] indicating how many elements to sort. *)
val sort : ?pos:int -> ?len:int -> 'a t -> compare:('a -> 'a -> int) -> unit

include Binary_searchable.S1 with type 'a t := 'a t

(** {2 Extra lowlevel and unsafe functions} *)

(** The behavior is undefined if you access an element before setting it. *)
val unsafe_create_uninitialized : 'a. len:int -> 'a t

(** New obj array filled with [Obj.repr 0] *)
val create_obj_array : len:int -> Stdlib.Obj.t t

(** [unsafe_set_assuming_currently_int t i obj] sets index [i] of [t] to [obj], but only
    works correctly if the value there is an immediate, i.e.
    [Stdlib.Obj.is_int (get t i)]. This precondition saves a dynamic check.

    [unsafe_set_int_assuming_currently_int] is similar, except the value being set is an
    int.

    [unsafe_set_int] is similar but does not assume anything about the target. *)
val unsafe_set_assuming_currently_int : Stdlib.Obj.t t -> int -> Stdlib.Obj.t -> unit

val unsafe_set_int_assuming_currently_int : Stdlib.Obj.t t -> int -> int -> unit
val unsafe_set_int : Stdlib.Obj.t t -> int -> int -> unit

(** [unsafe_clear_if_pointer t i] prevents [t.(i)] from pointing to anything to prevent
    space leaks. It does this by setting [t.(i)] to [Stdlib.Obj.repr 0]. As a performance
    hack, it only does this when [not (Stdlib.Obj.is_int t.(i))]. It is an error to access
    the cleared index before setting it again. *)
val unsafe_clear_if_pointer : Stdlib.Obj.t t -> int -> unit
