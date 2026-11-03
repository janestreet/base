open! Import0
open Hash.Builtin
open Ppx_compare_lib.Builtin
include Sexplib0.Sexp

(** Type of S-expressions *)
type t = Sexplib0.Sexp.t =
  | Atom of string
  | List of t list
[@@deriving compare, hash]

let t_sexp_grammar = Sexplib0.Sexp_conv.sexp_t_sexp_grammar
let of_string = ()
let invariant (_ : t) = ()
