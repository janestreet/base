@@ portable

(** One typically obtains a [Source_code_position.t] using a [[%here]] expression, which
    is implemented by the [ppx_here] preprocessor. *)

open! Import

(** See INRIA's OCaml documentation for a description of these fields.

    [sexp_of_t] uses the form ["FILE:LINE:COL"], and does not have a corresponding
    [of_sexp]. *)
type t = Stdlib.Lexing.position =
  { pos_fname : string
  ; pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  }
[@@deriving hash, sexp_of ~stackify]

include%template Comparable.S [@mode local] [@modality portable] with type t := t

(** [to_string t] converts [t] to the form ["FILE:LINE:COL"]. *)
val to_string : t -> string

(** [of_pos Stdlib.__POS__] is like [[%here]] but without using ppx. *)
val of_pos : string * int * int * int -> t

(** [here_or_there (Some there)] returns [there]. [here_or_there None] returns [~here],
    which may be defined implicitly on compilers supporting [[%call_pos]]. *)
val here_or_there : here:[%call_pos] -> t option -> t

(** [is_dummy pos] returns true if [pos] is equal to [Stdlib.Lexing.dummy_pos].

    [Stdlib.Lexing.dummy_pos] is a position guaranteed to be different from any valid
    position. *)
val is_dummy : local_ t -> bool
