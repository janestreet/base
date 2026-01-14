(** An inefficient, dead-simple implementation of percentile statistics. *)

open! Base

type t

(** Create an empty [t]. *)
val create : unit -> t

(** Add a measurement. *)
val add : t -> int -> unit

(** Print out percentiles from [t]. *)
val print : t -> unit
