@@ portable

(** Functions for ordered collections. *)

open! Import

include module type of Ordered_collection_common0 (** @inline *)

(** Like [get_pos_len_exn]. Returns an [Or_error.t]. *)
val get_pos_len
  :  ?pos:local_ int
  -> ?len:local_ int
  -> unit
  -> total_length:int
  -> local_ (int * int) Or_error.t
