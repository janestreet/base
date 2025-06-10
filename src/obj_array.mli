@@ portable

(** This module is not exposed for external use, and is only here for the implementation
    of [Uniform_array] internally. [Obj.t Uniform_array.t] should be used in place of
    [Obj_array.t]. *)

open! Import

type t [@@deriving sexp_of]

include Blit.S with type t := t
include Invariant.S with type t := t

(** [create ~len x] returns an obj-array of length [len], all of whose indices have value
    [x]. *)
val create : len:int -> Stdlib.Obj.t -> t

(** [create_zero ~len] returns an obj-array of length [len], all of whose indices have
    value [Stdlib.Obj.repr 0]. *)
val create_zero : len:int -> t

(** [copy t] returns a new array with the same elements as [t]. *)
val copy : local_ t -> t

val singleton : Stdlib.Obj.t -> t
val empty : t
val get_empty : unit -> t
val length : local_ t @ contended -> int

(** [get t i] and [unsafe_get t i] return the object at index [i]. [set t i o] and
    [unsafe_set t i o] set index [i] to [o]. In no case is the object copied. The
    [unsafe_*] variants omit the bounds check of [i]. *)
val get : local_ t -> int -> Stdlib.Obj.t
[@@zero_alloc]

val unsafe_get : local_ t -> int -> Stdlib.Obj.t [@@zero_alloc]
val set : local_ t -> int -> Stdlib.Obj.t -> unit
val unsafe_set : local_ t -> int -> Stdlib.Obj.t -> unit
val swap : local_ t -> int -> int -> unit

(** [set_with_caml_modify] simply sets the value in the array with no bells and whistles,
    unlike [set] which first reads the value to optimize immediate values and setting the
    index to its current value. This can be used when these optimizations are not useful,
    but the noise in generated code is annoying (and might have an impact on performance,
    although this is pure speculation). *)
val set_with_caml_modify : local_ t -> int -> Stdlib.Obj.t -> unit

(** [unsafe_set_assuming_currently_int t i obj] sets index [i] of [t] to [obj], but only
    works correctly if [Stdlib.Obj.is_int (get t i)]. This precondition saves a dynamic
    check.

    [unsafe_set_int_assuming_currently_int] is similar, except the value being set is an
    int.

    [unsafe_set_int] is similar but does not assume anything about the target. *)
val unsafe_set_assuming_currently_int : local_ t -> int -> Stdlib.Obj.t -> unit

val unsafe_set_int_assuming_currently_int : local_ t -> int -> int -> unit
val unsafe_set_int : local_ t -> int -> int -> unit

(** [unsafe_set_omit_phys_equal_check] is like [unsafe_set], except it doesn't do a
    [phys_equal] check to try to skip [caml_modify]. It is safe to call this even if the
    values are [phys_equal]. *)
val unsafe_set_omit_phys_equal_check : local_ t -> int -> Stdlib.Obj.t -> unit

(** Same as [set_with_caml_modify], but without bounds checks. This is like
    [unsafe_set_omit_phys_equal_check] except it doesn't check whether the old value and
    the value being set are integers to try to skip [caml_modify]. *)
val unsafe_set_with_caml_modify : local_ t -> int -> Stdlib.Obj.t -> unit

(** [unsafe_clear_if_pointer t i] prevents [t.(i)] from pointing to anything to prevent
    space leaks. It does this by setting [t.(i)] to [Stdlib.Obj.repr 0]. As a performance
    hack, it only does this when [not (Stdlib.Obj.is_int t.(i))]. *)
val unsafe_clear_if_pointer : local_ t -> int -> unit

val sub : t -> pos:int -> len:int -> t
