open! Import

include Int_intf.S

val of_int : int -> t
val to_int : t -> int option

val of_int32 : int32 -> t

(*_ exported for Core_kernel *)
module W : sig
  val wrap_exn : int64 -> t
  val unwrap : t -> int64
end

module Repr : sig
  type emulated = t
  type ('underlying_type, 'intermediate_type) t =
    | Int   : (int   , int     ) t
    | Int64 : (int64 , emulated) t
end with type emulated := t

val repr : (t, t) Repr.t
