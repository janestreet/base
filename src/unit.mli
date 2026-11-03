(** Module for the type [unit]. *)

open! Import0

type t = unit [@@deriving enumerate, globalize, sexp ~unboxed, sexp_grammar]

include Identifiable.S with type t := t
include Invariant.S with type t := t
