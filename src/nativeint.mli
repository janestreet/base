(** Processor-native integers. *)

open! Import

type t = nativeint [@@deriving globalize]

include Int.S with type t := t
include Replace_polymorphic_compare.S with type t := t

(** {2 Conversion functions} *)

external of_int : int -> (t[@local_opt]) = "%nativeint_of_int"
val to_int : t -> int option
external of_int32 : int32 -> (t[@local_opt]) = "%nativeint_of_int32"
val to_int32 : t -> int32 option
external of_nativeint : (nativeint[@local_opt]) -> (t[@local_opt]) = "%identity"
external to_nativeint : (t[@local_opt]) -> (nativeint[@local_opt]) = "%identity"
val of_int64 : int64 -> t option

(** {3 Truncating conversions}

    These functions return the least-significant bits of the input. In cases where
    optional conversions return [Some x], truncating conversions return [x]. *)

val to_int_trunc : t -> int
external to_int32_trunc : t -> (int32[@local_opt]) = "%nativeint_to_int32"
external of_int64_trunc : int64 -> (t[@local_opt]) = "%int64_to_nativeint"

(** {2 Byte swap functions}

    See {{!modtype:Int.Int_without_module_types} [Int]'s byte swap section} for a
    description of Base's approach to exposing byte swap primitives. *)

external bswap : t -> (t[@local_opt]) = "%bswap_native"
