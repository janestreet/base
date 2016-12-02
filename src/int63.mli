(** 63-bit integers.

    The size of Int63 is always 63 bits.  On a 64-bit platform it is just an int
    (63-bits), and on a 32-bit platform it is an int64 wrapped to respect the
    semantic of 63bit integers.

    Because Int63 has different representations on 32-bit and 64-bit platforms,
    marshalling Int63 will not work between 32-bit and 64-bit platforms.
    unmarshal will segfault. *)

open! Import

(** In 64bit architectures, we expose [type t = private int] so that the compiler can
    omit caml_modify when dealing with record fields holding [Int63.t].
    Code should not explicitly make use of the [private], e.g. via [(i :> int)], since
    such code will not compile on 32-bit platforms. *)
include Int_intf.S with type t = Int63_backend.t

(** Unlike the usual operations, these never overflow, preferring instead to raise. *)
module Overflow_exn : sig
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val abs : t -> t
  val neg : t -> t
end

val of_int : int -> t
val to_int : t -> int option

(** [random ~state bound] returns a random integer between 0 (inclusive) and [bound]
    (exclusive).  [bound] must be greater than 0.

    The default [~state] is [Random.State.default]. *)
val random : ?state : Random.State.t -> t -> t

(**/**)
module Private : sig
  module Repr : sig
    type 'a t = Int : int t | Int64 : int64 t
  end
  val repr : t Repr.t
end
