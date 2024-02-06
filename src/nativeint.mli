(** Processor-native integers. *)

open! Import

type t = nativeint [@@deriving_inline globalize]

val globalize : t -> t

[@@@end]

include Int_intf.S with type t := t

(** {2 Conversion functions} *)

val of_int : int -> t
val to_int : t -> int option
val of_int32 : int32 -> t
val to_int32 : t -> int32 option
val of_nativeint : nativeint -> t
val to_nativeint : t -> nativeint
val of_int64 : int64 -> t option

(** {3 Truncating conversions}

    These functions return the least-significant bits of the input. In cases where
    optional conversions return [Some x], truncating conversions return [x]. *)

val to_int_trunc : t -> int

external to_int32_trunc
  :  (nativeint[@local_opt])
  -> (int32[@local_opt])
  = "%nativeint_to_int32"

val of_int64_trunc : int64 -> t

(** {2 Byte swap functions}

    See {{!modtype:Int.Int_without_module_types}[Int]'s byte swap section} for
    a description of Base's approach to exposing byte swap primitives. *)

val bswap : t -> t
