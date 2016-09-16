open! Import

include Int_intf.S with type t = nativeint

val of_int : int -> t
val to_int : t -> int option

val of_int32 : int32 -> t
val to_int32 : t -> int32 option

val of_nativeint : nativeint -> t
val to_nativeint : t -> nativeint

val of_int64 : int64 -> t option
