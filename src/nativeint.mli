@@ portable

(** Processor-native integers. *)

open! Import0

type t = nativeint [@@deriving globalize, sexp ~stackify]

val%template sexp_of_t_u : Nativeint_u.t -> Sexp0.t @ m
[@@alloc a @ m = (heap_global, stack_local)]

val t_u_of_sexp : Sexp0.t -> Nativeint_u.t

include Int.S with type t := t
include Replace_polymorphic_compare.S with type t := t

(** {2 Conversion functions} *)

external of_int : int -> (t[@local_opt]) = "%nativeint_of_int"
val to_int : t @ local -> int option
external of_int32 : int32 @ local -> (t[@local_opt]) = "%nativeint_of_int32"
val to_int32 : t @ local -> int32 option
external of_nativeint : (nativeint[@local_opt]) -> (t[@local_opt]) = "%identity"
external to_nativeint : (t[@local_opt]) -> (nativeint[@local_opt]) = "%identity"
val of_int64 : int64 @ local -> t option

(** {3 Truncating conversions}

    These functions return the least-significant bits of the input. In cases where
    optional conversions return [Some x], truncating conversions return [x]. *)

val to_int_trunc : t @ local -> int
external to_int32_trunc : t @ local -> (int32[@local_opt]) = "%nativeint_to_int32"
external of_int64_trunc : int64 @ local -> (t[@local_opt]) = "%int64_to_nativeint"

(** {2 Byte swap functions}

    See {{!modtype:Int.Int_without_module_types} [Int]'s byte swap section} for a
    description of Base's approach to exposing byte swap primitives. *)

external bswap : t @ local -> (t[@local_opt]) = "%bswap_native"
