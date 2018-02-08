(** A signature for a commutative group (in the group-theory sense).

    An implementation of this interface should have the following properties:

    + associativity: [(a + b) + c = a + (b + c)]
    + identity: [zero + a = a + zero = a]
    + inverses: given any [a] there exists a (unique) elt [b] such that [a + b = b + a =
    zero]
    + commutativity: [a + b = b + a] *)

open! Import

module type S = sig
  type t [@@deriving_inline sexp]
  include
  sig
    [@@@ocaml.warning "-32"]
    val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  end
  [@@@end]

  val zero : t
  val (+)  : t -> t -> t
  val (-)  : t -> t -> t
end
