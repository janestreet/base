(** ['a Option_array.t] is a compact representation of ['a option array]: it avoids
    allocating heap objects representing [Some x], usually representing them with [x]
    instead.  It uses a special representation for [None] that's guaranteed to never
    collide with any representation of [Some x]. *)

open! Import

type 'a t [@@deriving_inline sexp]
include
sig
  [@@@ocaml.warning "-32"]
  include Ppx_sexp_conv_lib.Sexpable.S1 with type 'a t :=  'a t
end[@@ocaml.doc "@inline"]
[@@@end]

val empty : _ t

(** Initially filled with all [None] *)
val create : len:int -> _ t


val init_some : int -> f:(int -> 'a       ) -> 'a t
val init      : int -> f:(int -> 'a option) -> 'a t

val length : _ t -> int

(** [get t i] returns the element number [i] of array [t], raising if [i] is outside the
    range 0 to [length t - 1]. *)
val get : 'a t -> int -> 'a option

(** Raises if the element number [i] is [None]. *)
val get_some_exn : 'a t -> int -> 'a

(** [is_none t i = Option.is_none (get t i)] *)
val is_none : _ t -> int -> bool

(** [is_some t i = Option.is_some (get t i)] *)
val is_some : _ t -> int -> bool

(** These can cause arbitrary behavior when used for an out-of-bounds array access. *)

val unsafe_get          : 'a t -> int -> 'a option
val unsafe_get_some_exn : 'a t -> int -> 'a
val unsafe_is_some      : _  t -> int -> bool

(** [set t i x] modifies array [t] in place, replacing element number [i] with [x],
    raising if [i] is outside the range 0 to [length t - 1]. *)
val set      : 'a t -> int -> 'a option -> unit
val set_some : 'a t -> int -> 'a ->        unit
val set_none : _  t -> int ->              unit

val swap : _ t -> int -> int -> unit

(** Replaces all the elements of the array with [None]. *)
val clear : _ t -> unit

(** Unsafe versions of [set*]. Can cause arbitrary behaviour when used for an
    out-of-bounds array access. *)

val unsafe_set      : 'a t -> int -> 'a option -> unit
val unsafe_set_some : 'a t -> int -> 'a ->        unit
val unsafe_set_none : _  t -> int ->              unit

include Blit.S1 with type 'a t := 'a t

(** Makes a (shallow) copy of the array. *)
val copy : 'a t -> 'a t

(**/**)

module For_testing : sig
  module Unsafe_cheap_option : sig
    type 'a t [@@deriving_inline sexp]
    include
    sig
      [@@@ocaml.warning "-32"]
      include Ppx_sexp_conv_lib.Sexpable.S1 with type 'a t :=  'a t
    end[@@ocaml.doc "@inline"]
    [@@@end]

    val none : _ t
    val some : 'a -> 'a t

    val is_none : _ t -> bool
    val is_some : _ t -> bool

    val value_exn    : 'a t -> 'a
    val value_unsafe : 'a t -> 'a

    val to_option : 'a t -> 'a Option.t
    val of_option : 'a Option.t -> 'a t
  end
end
