(** This module is not exposed in Base.  Instead, these functions are accessed and
    commented in Int. *)

open! Import

val is_pow2 : int -> bool
val ceil_pow2  : int -> int
val floor_pow2 : int -> int

val ceil_log2  : int -> int
val floor_log2 : int -> int
