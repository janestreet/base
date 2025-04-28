(** Module for the type [unit]. *)

open! Import

type t = unit [@@deriving enumerate, globalize, sexp ~localize, sexp_grammar]

include Identifiable.S with type t := t
include Ppx_compare_lib.Equal.S__local with type t := t
include Ppx_compare_lib.Comparable.S__local with type t := t
include Invariant.S with type t := t
