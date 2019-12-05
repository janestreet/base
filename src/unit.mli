(** Module for the type [unit]. *)

open! Import

type t = unit [@@deriving_inline enumerate, sexp]

val all : t list

include Ppx_sexp_conv_lib.Sexpable.S with type t := t

[@@@end]

include Identifiable.S with type t := t
include Invariant.S with type t := t
