(** One typically obtains a [Source_code_position.t] using a [[%here]] expression, which
    is implemented by the [ppx_here] preprocessor. *)

open! Import

(** See INRIA's OCaml documentation for a description of these fields. *)
type t
  = Lexing.position
  = { pos_fname : string
    ; pos_lnum  : int
    ; pos_bol   : int
    ; pos_cnum  : int
    }
  [@@deriving hash, sexp_of]
(** [sexp_of_t] uses the form ["FILE:LINE:COL"], and does not have a corresponding
    [of_sexp]. *)

include Comparable.S with type t := t
include Hashable.S   with type t := t

(** [to_string t] converts [t] to the form ["FILE:LINE:COL"]. *)
val to_string : t -> string

