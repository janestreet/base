@@ portable

(** A lazy string, implemented with [Info], but intended specifically for error messages. *)

open! Import
module Sexp := Sexp0

type t : value mod contended = private Info.t

include Info.S with type t := t (** @open *)

(** Note that the exception raised by this function maintains a reference to the [t]
    passed in. *)
val raise : t -> _ @ portable

val raise_s : Sexp.t -> _ @ portable

(** Re-raise exceptions raised from [f] with this [t] as context. If this function raises,
    the exception maintains a reference to the [t] passed in. *)
val reraise_uncaught : t -> f:(unit -> 'a) -> 'a

val to_info : t -> Info.t
val of_info : Info.t -> t
