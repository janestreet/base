(** Boolean type extended to be enumerable, hashable, sexpable, comparable, and
    stringable. *)

open! Import

type t = bool [@@deriving_inline enumerate, globalize, sexp, sexp_grammar]

include Ppx_enumerate_lib.Enumerable.S with type t := t

val globalize : t -> t

include Sexplib0.Sexpable.S with type t := t

val t_sexp_grammar : t Sexplib0.Sexp_grammar.t

[@@@end]

include Identifiable.S with type t := t
include Ppx_compare_lib.Comparable.S_local with type t := t
include Ppx_compare_lib.Equal.S_local with type t := t
include Invariant.S with type t := t

(**
   - [to_int true = 1]
   - [to_int false = 0] *)
val to_int : t -> int

external select
  :  bool
  -> ('a[@local_opt])
  -> ('a[@local_opt])
  -> ('a[@local_opt])
  = "caml_csel_value"
  [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

module Non_short_circuiting : sig
  (** Non-short circuiting and branch-free boolean operators.

      The default versions of these infix operators are short circuiting, which
      requires branching instructions to implement. The operators below are
      instead branch-free, and therefore not short-circuiting. *)

  val ( && ) : t -> t -> t
  val ( || ) : t -> t -> t
end
