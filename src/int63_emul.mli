@@ portable

(** [Int63_emul] implements 63-bit integers using the [int64] type. It is is used to
    implement [Int63] on 32-bit platforms; see [Int63_backends.Emulated]. *)

open! Import0

type t = int64 [@@deriving globalize]

include Int.S with type t := t
include Replace_polymorphic_compare.S with type t := t

val of_int : int -> t
val to_int : t @ local -> int option
val to_int_trunc : t @ local -> int
val of_int32 : int32 @ local -> t
val to_int32 : t @ local -> int32 option
val to_int32_trunc : t @ local -> int32
val of_int64 : int64 @ local -> t option
val of_int64_exn : int64 @ local -> t
val of_int64_trunc : int64 @ local -> t
val of_nativeint : nativeint @ local -> t option
val to_nativeint : t @ local -> nativeint option
val of_nativeint_trunc : nativeint @ local -> t
val to_nativeint_trunc : t @ local -> nativeint
val bswap16 : t @ local -> t
val bswap32 : t @ local -> t
val bswap48 : t @ local -> t

(*_ exported for Core *)
module W : sig
  val wrap_exn : int64 @ local -> t @ local
  val unwrap : t @ local -> int64 @ local
end

val repr : t Int63_type.Repr.t
