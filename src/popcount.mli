@@ portable

(** This module exposes popcount functions (which count the number of ones in a bitstring)
    for the various integer types.

    Functions are exposed in their respective modules. *)

open! Import0

val int_popcount : int @ local -> int
val int32_popcount : int32 @ local -> int32
val int64_popcount : int64 @ local -> int64
val nativeint_popcount : nativeint @ local -> nativeint
