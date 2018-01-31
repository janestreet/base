(** Boolean type extended to be enumerable, hashable, sexpable, comparable, and
    stringable. *)

open! Import

type t = bool [@@deriving_inline enumerate, hash, sexp]
include
sig
  [@@@ocaml.warning "-32"]
  val all : t list
  val hash_fold_t :
    Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state
  val hash : t -> Ppx_hash_lib.Std.Hash.hash_value
  val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
end
[@@@end]

include Comparable.S with type t := t
include Stringable.S with type t := t

(**
   - [to_int true = 1]
   - [to_int false = 0] *)
val to_int : t -> int
