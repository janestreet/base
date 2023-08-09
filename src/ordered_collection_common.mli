(** Functions for ordered collections. *)

open! Import

include module type of Ordered_collection_common0 (** @inline *)

(** Like [get_pos_len_exn]. Returns an [Or_error.t]. *)
val get_pos_len
  :  ?pos:(int[@local])
  -> ?len:(int[@local])
  -> unit
  -> total_length:int
  -> ((int * int) Or_error.t[@local])
