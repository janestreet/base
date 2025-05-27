@@ portable

(** Module for the type [unit]. *)

open! Import

type t = unit [@@deriving enumerate, globalize, sexp ~localize, sexp_grammar]

include%template Identifiable.S [@mode local] [@modality portable] with type t := t

include Invariant.S with type t := t
