(** An extension to [Sign] with a [Nan] constructor, for representing the sign
    of float-like numeric values. *)

open! Import

type t =
  | Neg
  | Zero
  | Pos
  | Nan
[@@deriving_inline enumerate, sexp_grammar]

include Ppx_enumerate_lib.Enumerable.S with type t := t

val t_sexp_grammar : t Sexplib0.Sexp_grammar.t

[@@@end]

(** This provides [to_string]/[of_string], sexp conversion, Map, Hashtbl, etc. *)
include Identifiable.S with type t := t

include Ppx_compare_lib.Comparable.S_local with type t := t
include Ppx_compare_lib.Equal.S_local with type t := t

(** Returns the human-readable strings "positive", "negative", "zero", "not-a-number". *)
val to_string_hum : t -> string

val of_int : int -> t

(** Map [Neg/Zero/Pos] to [-1/0/1] respectively.  [Nan] raises. *)
val to_int_exn : t -> int

val of_sign : Sign.t -> t

(** [Nan] raises. *)
val to_sign_exn : t -> Sign.t

(** Map [Neg/Zero/Pos/Nan] to [Pos/Zero/Neg/Nan] respectively. *)
val flip : t -> t

(** [Neg * Neg = Pos], etc.  If either argument is [Nan] then the result is [Nan]. *)
val ( * ) : t -> t -> t
