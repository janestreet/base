(** A signature for a commutative group (in the group-theory sense).

    An implementation of this interface should have the following properties:

    + associativity: [(a+b)+c = a+(b+c)] for all elt's [a,b,c]
    + identity: [zero+a = a+zero = a] for all elt's [a]
    + inverses: given any elt [a] there exists a (unique) elt [b] such that [a+b=b+a=zero]
    + commutativity: [a+b = b+a] *)

open! Import

module type S = sig
  type t [@@deriving_inline sexp]
  include
  sig
    [@@@ocaml.warning "-32"]
    val t_of_sexp : Sexplib.Sexp.t -> t
    val sexp_of_t : t -> Sexplib.Sexp.t
  end
  [@@@end]  (* an element of the group *)

  val zero : t
  val (+)  : t -> t -> t
  val (-)  : t -> t -> t
end
