(** Module for the type [unit] *)

open! Import

type t = unit [@@deriving_inline compare, hash, sexp]
include
sig
  [@@@ocaml.warning "-32"]
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val hash_fold_t :
    Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state
  val hash : t -> Ppx_hash_lib.Std.Hash.hash_value
  val compare : t -> t -> int
end
[@@@end]

include Identifiable.S with type t := t
include Invariant.S    with type t := t
