@@ portable

(** Boolean type extended to be enumerable, hashable, sexpable, comparable, and
    stringable. *)

open! Import

type t = bool [@@deriving enumerate, globalize, sexp ~stackify ~unboxed, sexp_grammar]

include%template Identifiable.S [@mode local] [@modality portable] with type t := t

include Invariant.S with type t := t

(** - [to_int true = 1]
    - [to_int false = 0] *)
val to_int : t -> int

include module type of Bool0

module Non_short_circuiting : sig
  (** Non-short circuiting and branch-free boolean operators.

      The default versions of these infix operators are short circuiting, which requires
      branching instructions to implement. The operators below are instead branch-free,
      and therefore not short-circuiting. *)

  val ( && ) : t -> t -> t [@@zero_alloc]
  val ( || ) : t -> t -> t [@@zero_alloc]
end

(** [Branch_free] aliases the [Non_short_circuiting] module to make it more readable in
    code if you want to avoid branching because you want to have both sides of the infix
    operator execute (implied by using [Non_short_circuiting]) or because you just want to
    avoid the branch (implied by using [Branch_free]) *)
module Branch_free : module type of Non_short_circuiting
