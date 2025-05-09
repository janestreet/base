@@ portable

(** An int of exactly 32 bits, regardless of the machine.

    Side note: There's not much reason to want an int of at least 32 bits (i.e., 32 on
    32-bit machines and 63 on 64-bit machines) because [Int63] is basically just as
    efficient.

    Overflow issues are {i not} generally considered and explicitly handled. This may be
    more of an issue for 32-bit ints than 64-bit ints.

    [Int32.t] is boxed on both 32-bit and 64-bit machines. *)

open! Import

type t = int32 [@@deriving globalize]

include Int.S with type t := t
include Replace_polymorphic_compare.S with type t := t

(** {2 Conversion functions} *)

val of_int : int -> t option
val to_int : local_ t -> int option
external of_int32 : (int32[@local_opt]) -> (t[@local_opt]) = "%identity"
external to_int32 : (t[@local_opt]) -> (int32[@local_opt]) = "%identity"
val of_nativeint : local_ nativeint -> t option
external to_nativeint : local_ t -> (nativeint[@local_opt]) = "%nativeint_of_int32"
val of_int64 : local_ int64 -> t option
val of_local_int64_exn : local_ int64 -> local_ t [@@zero_alloc]

(** {3 Truncating conversions}

    These functions return the least-significant bits of the input. In cases where
    optional conversions return [Some x], truncating conversions return [x]. *)

external of_int_trunc : int -> (t[@local_opt]) = "%int32_of_int"
val to_int_trunc : local_ t -> int
external of_nativeint_trunc : local_ nativeint -> (t[@local_opt]) = "%nativeint_to_int32"
external of_int64_trunc : local_ int64 -> (t[@local_opt]) = "%int64_to_int32"

(** {3 Low-level float conversions} *)

(** Rounds a regular 64-bit OCaml float to a 32-bit IEEE-754 "single" float, and returns
    its bit representation. We make no promises about the exact rounding behavior, or what
    happens in case of over- or underflow. *)
external bits_of_float
  :  local_ float
  -> t
  = "caml_int32_bits_of_float" "caml_int32_bits_of_float_unboxed"
[@@unboxed] [@@noalloc]

(** Creates a 32-bit IEEE-754 "single" float from the given bits, and converts it to a
    regular 64-bit OCaml float. *)
external float_of_bits
  :  local_ t
  -> float
  = "caml_int32_float_of_bits" "caml_int32_float_of_bits_unboxed"
[@@unboxed] [@@noalloc]

(** {2 Byte swap operations}

    See {{!modtype:Int.Int_without_module_types} [Int]'s byte swap section} for a
    description of Base's approach to exposing byte swap primitives.

    When compiling for 64-bit machines, if signedness of the output value does not matter,
    use byteswap functions for [int64], if possible, for better performance. As of
    writing, 32-bit byte swap operations on 64-bit machines have extra overhead for moving
    to 32-bit registers and sign-extending values when returning to 64-bit registers.

    The x86 instruction sequence that demonstrates the overhead is in
    [base/bench/bench_int.ml] *)

val bswap16 : local_ t -> t
external bswap32 : local_ t -> (t[@local_opt]) = "%bswap_int32"
