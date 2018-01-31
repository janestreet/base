(** Module for the type [unit]. *)

open! Import

type t = unit [@@deriving_inline compare, enumerate, hash, sexp]
include
sig
  [@@@ocaml.warning "-32"]
  val compare : t -> t -> int
  val all : t list
  val hash_fold_t :
    Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state
  val hash : t -> Ppx_hash_lib.Std.Hash.hash_value
  val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
end
[@@@end]

include Identifiable.S with type t := t
include Invariant.S    with type t := t
