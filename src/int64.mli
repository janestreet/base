@@ portable

(** 64-bit integers. *)

open! Import

type t = int64 [@@deriving globalize]

include Int.S with type t := t
include Replace_polymorphic_compare.S with type t := t

module O : sig
  (*_ Declared as externals so that the compiler skips the caml_apply_X wrapping even when
      compiling without cross library inlining. *)
  external ( + ) : local_ t -> local_ t -> (t[@local_opt]) = "%int64_add"
  external ( - ) : local_ t -> local_ t -> (t[@local_opt]) = "%int64_sub"
  external ( * ) : local_ t -> local_ t -> (t[@local_opt]) = "%int64_mul"
  external ( / ) : local_ t -> local_ t -> (t[@local_opt]) = "%int64_div"
  external ( ~- ) : local_ t -> (t[@local_opt]) = "%int64_neg"
  val ( ** ) : local_ t -> local_ t -> t
  external ( = ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%equal"
  external ( <> ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%notequal"
  external ( < ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%lessthan"
  external ( > ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%greaterthan"
  external ( <= ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%lessequal"
  external ( >= ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%greaterequal"
  external ( land ) : local_ t -> local_ t -> (t[@local_opt]) = "%int64_and"
  external ( lor ) : local_ t -> local_ t -> (t[@local_opt]) = "%int64_or"
  external ( lxor ) : local_ t -> local_ t -> (t[@local_opt]) = "%int64_xor"
  val lnot : local_ t -> t
  val abs : t -> t
  val abs_local : local_ t -> local_ t
  external neg : local_ t -> t = "%int64_neg"
  val zero : t
  val ( % ) : local_ t -> local_ t -> t
  val ( /% ) : local_ t -> local_ t -> t
  val ( // ) : local_ t -> local_ t -> float
  external ( lsl ) : local_ t -> int -> (t[@local_opt]) = "%int64_lsl"
  external ( asr ) : local_ t -> int -> (t[@local_opt]) = "%int64_asr"
  external ( lsr ) : local_ t -> int -> (t[@local_opt]) = "%int64_lsr"
end

include module type of O

(** {2 Conversion functions} *)

(*_ Declared as externals so that the compiler skips the caml_apply_X wrapping even when
    compiling without cross library inlining. *)
external of_int : int -> (t[@local_opt]) = "%int64_of_int"
external of_int32 : local_ int32 -> (t[@local_opt]) = "%int64_of_int32"
external of_int64 : (t[@local_opt]) -> (t[@local_opt]) = "%identity"
val to_int : local_ t -> int option
val to_int32 : local_ t -> int32 option
external of_nativeint : local_ nativeint -> (t[@local_opt]) = "%int64_of_nativeint"
val to_nativeint : local_ t -> nativeint option

(** {3 Truncating conversions}

    These functions return the least-significant bits of the input. In cases where
    optional conversions return [Some x], truncating conversions return [x]. *)

(*_ Declared as externals so that the compiler skips the caml_apply_X wrapping even when
    compiling without cross library inlining. *)
external to_int_trunc : local_ t -> int = "%int64_to_int"
external to_int32_trunc : local_ t -> (int32[@local_opt]) = "%int64_to_int32"
external to_nativeint_trunc : local_ t -> (nativeint[@local_opt]) = "%int64_to_nativeint"

(** {3 Low-level float conversions} *)

(** [bits_of_float] will always allocate its result on the heap unless the [_unboxed] C
    function call is chosen by the compiler. *)
external bits_of_float
  :  local_ float
  -> t
  = "caml_int64_bits_of_float" "caml_int64_bits_of_float_unboxed"
[@@unboxed] [@@noalloc]

(** [float_of_bits] will always allocate its result on the heap unless the [_unboxed] C
    function call is chosen by the compiler. *)
external float_of_bits
  :  local_ t
  -> float
  = "caml_int64_float_of_bits" "caml_int64_float_of_bits_unboxed"
[@@unboxed] [@@noalloc]

(** {2 Byte swap operations}

    See {{!modtype:Int.Int_without_module_types} [Int]'s byte swap section} for a
    description of Base's approach to exposing byte swap primitives.

    As of writing, these operations do not sign extend unnecessarily on 64 bit machines,
    unlike their int32 counterparts, and hence, are more performant. See the {!Int32}
    module for more details of the overhead entailed by the int32 byteswap functions. *)

val bswap16 : local_ t -> t
val bswap32 : local_ t -> t
val bswap48 : local_ t -> t

(*_ Declared as an external so that the compiler skips the caml_apply_X wrapping even when
    compiling without cross library inlining. *)
external bswap64 : local_ t -> (t[@local_opt]) = "%bswap_int64"
