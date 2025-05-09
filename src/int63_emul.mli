@@ portable

(** [Int63_emul] implements 63-bit integers using the [int64] type. It is is used to
    implement [Int63] on 32-bit platforms; see [Int63_backends.Emulated]. *)

open! Import

type t [@@deriving globalize]

include Int.S with type t := t
include Replace_polymorphic_compare.S with type t := t

val of_int : int -> t
val to_int : local_ t -> int option
val to_int_trunc : local_ t -> int
val of_int32 : local_ int32 -> t
val to_int32 : local_ t -> int32 option
val to_int32_trunc : local_ t -> int32
val of_int64 : local_ int64 -> t option
val of_int64_exn : local_ int64 -> t
val of_int64_trunc : local_ int64 -> t
val of_nativeint : local_ nativeint -> t option
val to_nativeint : local_ t -> nativeint option
val of_nativeint_trunc : local_ nativeint -> t
val to_nativeint_trunc : local_ t -> nativeint
val bswap16 : local_ t -> t
val bswap32 : local_ t -> t
val bswap48 : local_ t -> t

(*_ exported for Core *)
module W : sig
  val wrap_exn : local_ int64 -> local_ t
  val unwrap : local_ t -> local_ int64
end

module Repr : sig
    type emulated = t

    type ('underlying_type, 'intermediate_type) t =
      | Int : (int, int) t
      | Int64 : (int64, emulated) t
  end
  with type emulated := t

val repr : (t, t) Repr.t
