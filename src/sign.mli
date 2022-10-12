(** A type for representing the sign of a numeric value. *)

open! Import

type t = Sign0.t =
  | Neg
  | Zero
  | Pos
[@@deriving_inline enumerate, sexp_grammar]

include Ppx_enumerate_lib.Enumerable.S with type t := t

val t_sexp_grammar : t Sexplib0.Sexp_grammar.t

[@@@end]

(** This provides [to_string]/[of_string], sexp conversion, Map, Hashtbl, etc. *)
include Identifiable.S with type t := t

(** Returns the human-readable strings "positive", "negative", "zero". *)
val to_string_hum : t -> string

val of_int : int -> t

(** Map [Neg/Zero/Pos] to [-1/0/1] respectively. *)
val to_int : t -> int

(** Map [Neg/Zero/Pos] to [-1./0./1.] respectively.
    (There is no [of_float] here, but see {!Float.sign_exn}.) *)
val to_float : t -> float

(** Map [Neg/Zero/Pos] to [Pos/Zero/Neg] respectively. *)
val flip : t -> t

(** [Neg * Neg = Pos], etc. *)
val ( * ) : t -> t -> t
