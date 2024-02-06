(** 64-bit integers. *)

open! Import

type t = int64 [@@deriving_inline globalize]

val globalize : t -> t

[@@@end]

include Int_intf.S with type t := t

module O : sig
  (*_ Declared as externals
    so that the compiler skips the caml_apply_X wrapping even when
    compiling without cross library inlining. *)
  external ( + ) : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt]) = "%int64_add"
  external ( - ) : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt]) = "%int64_sub"
  external ( * ) : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt]) = "%int64_mul"
  external ( / ) : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt]) = "%int64_div"
  external ( ~- ) : (t[@local_opt]) -> (t[@local_opt]) = "%int64_neg"
  val ( ** ) : t -> t -> t
  external ( = ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%equal"
  external ( <> ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%notequal"
  external ( < ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%lessthan"
  external ( > ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%greaterthan"
  external ( <= ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%lessequal"
  external ( >= ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%greaterequal"
  external ( land ) : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt]) = "%int64_and"
  external ( lor ) : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt]) = "%int64_or"
  external ( lxor ) : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt]) = "%int64_xor"
  val lnot : t -> t
  val abs : t -> t
  external neg : t -> t = "%int64_neg"
  val zero : t
  val ( % ) : t -> t -> t
  val ( /% ) : t -> t -> t
  val ( // ) : t -> t -> float

  external ( lsl )
    :  (t[@local_opt])
    -> (int[@local_opt])
    -> (t[@local_opt])
    = "%int64_lsl"

  external ( asr )
    :  (t[@local_opt])
    -> (int[@local_opt])
    -> (t[@local_opt])
    = "%int64_asr"

  external ( lsr )
    :  (t[@local_opt])
    -> (int[@local_opt])
    -> (t[@local_opt])
    = "%int64_lsr"
end

include module type of O

(** {2 Conversion functions} *)

(*_ Declared as externals so that the compiler skips the caml_apply_X wrapping even when
  compiling without cross library inlining. *)
external of_int : (int[@local_opt]) -> (t[@local_opt]) = "%int64_of_int"
external of_int32 : (int32[@local_opt]) -> (t[@local_opt]) = "%int64_of_int32"
external of_int64 : (t[@local_opt]) -> (t[@local_opt]) = "%identity"
val to_int : t -> int option
val to_int32 : t -> int32 option
external of_nativeint : (nativeint[@local_opt]) -> (t[@local_opt]) = "%int64_of_nativeint"
val to_nativeint : t -> nativeint option

(** {3 Truncating conversions}

    These functions return the least-significant bits of the input. In cases where
    optional conversions return [Some x], truncating conversions return [x]. *)

(*_ Declared as externals so that the compiler skips the caml_apply_X wrapping even when
  compiling without cross library inlining. *)
external to_int_trunc : (t[@local_opt]) -> int = "%int64_to_int"
external to_int32_trunc : (int64[@local_opt]) -> (int32[@local_opt]) = "%int64_to_int32"

external to_nativeint_trunc
  :  (int64[@local_opt])
  -> (nativeint[@local_opt])
  = "%int64_to_nativeint"

(** {3 Low-level float conversions} *)

(** [bits_of_float] will always allocate its result on the heap unless the [_unboxed]
    C function call is chosen by the compiler. *)
external bits_of_float
  :  (float[@local_opt])
  -> (int64[@local_opt])
  = "caml_int64_bits_of_float" "caml_int64_bits_of_float_unboxed"
  [@@unboxed] [@@noalloc]

(** [float_of_bits] will always allocate its result on the heap unless the [_unboxed]
    C function call is chosen by the compiler. *)
external float_of_bits
  :  (int64[@local_opt])
  -> (float[@local_opt])
  = "caml_int64_float_of_bits" "caml_int64_float_of_bits_unboxed"
  [@@unboxed] [@@noalloc]

(** {2 Byte swap operations}

    See {{!modtype:Int.Int_without_module_types}[Int]'s byte swap section} for
    a description of Base's approach to exposing byte swap primitives.

    As of writing, these operations do not sign extend unnecessarily on 64 bit machines,
    unlike their int32 counterparts, and hence, are more performant. See the {!Int32}
    module for more details of the overhead entailed by the int32 byteswap functions.
*)

val bswap16 : t -> t
val bswap32 : t -> t
val bswap48 : t -> t

(*_ Declared as an external so that the compiler skips the caml_apply_X wrapping even when
  compiling without cross library inlining. *)
external bswap64 : (t[@local_opt]) -> (t[@local_opt]) = "%bswap_int64"
