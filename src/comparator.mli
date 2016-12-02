(** A type-indexed value that allows one to compare (and for generating error messages,
    serialize) values of the type in question.

    One of the type parameters is a phantom parameter used to distinguish comparators
    potentially built on different comparison functions.  In particular, we want to
    distinguish those using polymorphic compare and those using a monomorphic compare. *)

open! Import

type ('a, 'witness) t =
  private
  { compare   : 'a -> 'a -> int
  ; sexp_of_t : 'a -> Sexp.t
  }

type ('a, 'b) comparator = ('a, 'b) t

module type S = sig
  type t
  type comparator_witness
  val comparator : (t, comparator_witness) comparator
end

module type S1 = sig
  type 'a t
  type comparator_witness
  val comparator : ('a t, comparator_witness) comparator
end

module type S_fc = sig
  type comparable_t
  include S with type t := comparable_t
end

(** [make] creates a comparator witness for the given comparison. It is intended as a
    lightweight alternative to the functors below, to be used like so:
    [include (val Comparator.make ~compare ~sexp_of_t)] *)
val make
  :  compare:('a -> 'a -> int)
  -> sexp_of_t:('a -> Sexp.t)
  -> (module S_fc with type comparable_t = 'a)

module Poly : S1 with type 'a t = 'a

module S_to_S1 (S : S) : S1
  with type 'a t = S.t
  with type comparator_witness = S.comparator_witness

(** [Make] creates a [comparator] value and its phantom [comparator_witness] type for a
    nullary type. *)
module Make (M : sig
    type t [@@deriving_inline compare, sexp_of]
    include
    sig
      [@@@ocaml.warning "-32"]
      val sexp_of_t : t -> Sexplib.Sexp.t
      val compare : t -> t -> int
    end
    [@@@end]
  end) : S with type t := M.t

(** [Make1] creates a [comparator] value and its phantom [comparator_witness] type for a
    unary type.  It takes a [compare] and [sexp_of_t] that have
    non-standard types because the [Comparator.t] type doesn't allow passing in
    additional values for the type argument. *)
module Make1 (M : sig
    type 'a t
    val compare : 'a t -> 'a t -> int
    val sexp_of_t : _ t -> Sexp.t
  end) : S1 with type 'a t := 'a M.t
