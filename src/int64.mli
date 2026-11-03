@@ portable

(** 64-bit integers. *)

open! Import0

type t = int64 [@@deriving globalize, sexp ~stackify]

val%template sexp_of_t_u : Int64_u.t -> Sexp0.t @ m
[@@alloc a @ m = (heap_global, stack_local)]

val t_u_of_sexp : Sexp0.t -> Int64_u.t

include Int.S with type t := t
include Replace_polymorphic_compare.S with type t := t

module O : sig
  (*_ Declared as externals so that the compiler skips the caml_apply_X wrapping even when
      compiling without cross library inlining. *)
  external ( + ) : t @ local -> t @ local -> (t[@local_opt]) = "%int64_add"
  external ( - ) : t @ local -> t @ local -> (t[@local_opt]) = "%int64_sub"
  external ( * ) : t @ local -> t @ local -> (t[@local_opt]) = "%int64_mul"
  external ( / ) : t @ local -> t @ local -> (t[@local_opt]) = "%int64_div"
  external ( ~- ) : t @ local -> (t[@local_opt]) = "%int64_neg"
  val ( ** ) : t @ local -> t @ local -> t
  external ( = ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%equal"
  external ( <> ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%notequal"
  external ( < ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%lessthan"
  external ( > ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%greaterthan"
  external ( <= ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%lessequal"
  external ( >= ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%greaterequal"
  external ( land ) : t @ local -> t @ local -> (t[@local_opt]) = "%int64_and"
  external ( lor ) : t @ local -> t @ local -> (t[@local_opt]) = "%int64_or"
  external ( lxor ) : t @ local -> t @ local -> (t[@local_opt]) = "%int64_xor"
  val lnot : t @ local -> t
  val abs : t -> t
  val abs_local : t @ local -> t @ local
  external neg : t @ local -> t = "%int64_neg"
  val zero : t
  val ( % ) : t @ local -> t @ local -> t
  val ( /% ) : t @ local -> t @ local -> t
  val ( // ) : t @ local -> t @ local -> float
  external ( lsl ) : t @ local -> int -> (t[@local_opt]) = "%int64_lsl"
  external ( asr ) : t @ local -> int -> (t[@local_opt]) = "%int64_asr"
  external ( lsr ) : t @ local -> int -> (t[@local_opt]) = "%int64_lsr"
end

include module type of O

(** {2 Conversion functions} *)

(*_ Declared as externals so that the compiler skips the caml_apply_X wrapping even when
    compiling without cross library inlining. *)
external of_int : int -> (t[@local_opt]) = "%int64_of_int"
external of_int32 : int32 @ local -> (t[@local_opt]) = "%int64_of_int32"
external of_int64 : (t[@local_opt]) -> (t[@local_opt]) = "%identity"
val to_int : t @ local -> int option
val to_int32 : t @ local -> int32 option
external of_nativeint : nativeint @ local -> (t[@local_opt]) = "%int64_of_nativeint"
val to_nativeint : t @ local -> nativeint option

(** {3 Truncating conversions}

    These functions return the least-significant bits of the input. In cases where
    optional conversions return [Some x], truncating conversions return [x]. *)

(*_ Declared as externals so that the compiler skips the caml_apply_X wrapping even when
    compiling without cross library inlining. *)
external to_int_trunc : t @ local -> int = "%int64_to_int"
external to_int32_trunc : t @ local -> (int32[@local_opt]) = "%int64_to_int32"
external to_nativeint_trunc : t @ local -> (nativeint[@local_opt]) = "%int64_to_nativeint"

(** {3 Low-level float conversions} *)

(** [bits_of_float] will always allocate its result on the heap unless the [_unboxed] C
    function call is chosen by the compiler. *)
external bits_of_float
  :  float @ local
  -> t
  = "caml_int64_bits_of_float" "caml_int64_bits_of_float_unboxed"
[@@unboxed] [@@noalloc] [@@builtin]

(** [float_of_bits] will always allocate its result on the heap unless the [_unboxed] C
    function call is chosen by the compiler. *)
external float_of_bits
  :  t @ local
  -> float
  = "caml_int64_float_of_bits" "caml_int64_float_of_bits_unboxed"
[@@unboxed] [@@noalloc] [@@builtin]

(** {2 Byte swap operations}

    See {{!modtype:Int.Int_without_module_types} [Int]'s byte swap section} for a
    description of Base's approach to exposing byte swap primitives.

    As of writing, these operations do not sign extend unnecessarily on 64 bit machines,
    unlike their int32 counterparts, and hence, are more performant. See the {!Int32}
    module for more details of the overhead entailed by the int32 byteswap functions. *)

val bswap16 : t @ local -> t
val bswap32 : t @ local -> t
val bswap48 : t @ local -> t

(*_ Declared as an external so that the compiler skips the caml_apply_X wrapping even when
    compiling without cross library inlining. *)
external bswap64 : t @ local -> (t[@local_opt]) = "%bswap_int64"
