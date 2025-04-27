@@ portable

(** Boolean type extended to be enumerable, hashable, sexpable, comparable, and
    stringable. *)

open! Import

type t = bool [@@deriving enumerate, globalize, sexp ~localize, sexp_grammar]

include Identifiable.S with type t := t
include Ppx_compare_lib.Comparable.S__local with type t := t
include Ppx_compare_lib.Equal.S__local with type t := t
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

  val ( && ) : t -> t -> t
  val ( || ) : t -> t -> t
end
