@@ portable

(** ['a Option_array.t] is a compact representation of ['a option array]: it avoids
    allocating heap objects representing [Some x], usually representing them with [x]
    instead. It uses a special representation for [None] that's guaranteed to never
    collide with any representation of [Some x]. *)

open! Import

type 'a t : mutable_data with 'a [@@deriving sexp, sexp_grammar]

val empty : _ t

(** For obtaining uncontended access to [empty] from a portable function. *)
val get_empty : unit -> _ t

(** Initially filled with all [None] *)
val create : len:int -> _ t

include
  Indexed_container.Generic with type ('a, _, _) t := 'a t and type 'a elt := 'a option

val length : local_ _ t -> int
val init_some : int -> f:local_ (int -> 'a) -> 'a t
val init : int -> f:local_ (int -> 'a option) -> 'a t
val of_array : 'a option array -> 'a t
val of_array_some : 'a array -> 'a t
val to_array : 'a t -> 'a option Array.t

(** [get t i] returns the element number [i] of array [t], raising if [i] is outside the
    range 0 to [length t - 1]. *)
val get : 'a t -> int -> 'a option

(** Similar to [get], but allocates result in the caller's stack region instead of heap. *)
val get_local : local_ 'a t -> int -> local_ 'a option

(** Raises if the element number [i] is [None]. *)
val get_some_exn : 'a t -> int -> 'a

(** [is_none t i = Option.is_none (get t i)] *)
val is_none : _ t -> int -> bool

(** [is_some t i = Option.is_some (get t i)] *)
val is_some : _ t -> int -> bool

(** These can cause arbitrary behavior when used for an out-of-bounds array access. *)

val unsafe_get : 'a t -> int -> 'a option

(** [unsafe_get_some_exn t i] is unsafe because it does not bounds check [i]. It does,
    however check whether the value at index [i] is none or some, and raises if it is
    none. *)
val unsafe_get_some_exn : 'a t -> int -> 'a

(** [unsafe_get_some_assuming_some t i] is unsafe both because it does not bounds check
    [i] and because it does not check whether the value at index [i] is none or some,
    assuming that it is some. *)
val unsafe_get_some_assuming_some : 'a t -> int -> 'a

val unsafe_is_some : _ t -> int -> bool

(** [set t i x] modifies array [t] in place, replacing element number [i] with [x],
    raising if [i] is outside the range 0 to [length t - 1]. *)
val set : 'a t -> int -> 'a option -> unit

val set_some : 'a t -> int -> 'a -> unit
val set_none : _ t -> int -> unit
val swap : _ t -> int -> int -> unit

(** Replaces all the elements of the array with [None]. *)
val clear : _ t -> unit

(** [map f [|a1; ...; an|]] applies function [f] to [a1], [a2], ..., [an], in order, and
    builds the option_array [[|f a1; ...; f an|]] with the results returned by [f]. *)
val map : 'a t -> f:local_ ('a option -> 'b option) -> 'b t

(** [map_some t ~f] is like [map], but [None] elements always map to [None] and [Some]
    always map to [Some]. *)
val map_some : 'a t -> f:local_ ('a -> 'b) -> 'b t

(** Unsafe versions of [set*]. Can cause arbitrary behaviour when used for an
    out-of-bounds array access. *)

val unsafe_set : 'a t -> int -> 'a option -> unit
val unsafe_set_some : 'a t -> int -> 'a -> unit
val unsafe_set_none : _ t -> int -> unit

include Blit.S1 with type 'a t := 'a t

(** Makes a (shallow) copy of the array. *)
val copy : 'a t -> 'a t

(**/**)

module For_testing : sig
  module Unsafe_cheap_option : sig
    type 'a t [@@deriving sexp]

    val none : _ t
    val some : 'a -> 'a t
    val is_none : _ t -> bool
    val is_some : _ t -> bool
    val value_exn : 'a t -> 'a
    val value_unsafe : 'a t -> 'a
    val to_option : 'a t -> 'a Option.t
    val of_option : 'a Option.t -> 'a t
  end
end
