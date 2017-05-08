(** OCaml's native integer type.

    The number of bits in an integer is platform dependent, being 31-bits on a 32-bit
    platform, and 63-bits on a 64-bit platform.  [int] is a signed integer type.  [int]s
    are also subject to overflow, meaning that [Int.max_value + 1 = Int.min_value].

    [int]s always fit in a machine word. *)

open! Import

include Int_intf.S with type t = int

(** [max_value_30_bits = 2^30 - 1].  It is useful for writing tests that work on both
    64-bit and 32-bit platforms. *)
val max_value_30_bits : t

(** [ceil_pow2 x] returns the smallest power of 2 that is greater than or equal to [x].
    The implementation may only be called for [x > 0].  Example: [ceil_pow2 17 = 32] *)
val ceil_pow2 : int -> int

(** [floor_pow2 x] returns the largest power of 2 that is less than or equal to [x]. The
    implementation may only be called for [x > 0].  Example: [floor_pow2 17 = 16] *)
val floor_pow2 : int -> int

(** [ceil_log2 x] returns the ceiling of log-base-2 of [x], and raises if [x <= 0]. *)
val ceil_log2 : int -> int

(** [floor_log2 x] returns the floor of log-base-2 of [x], and raises if [x <= 0]. *)
val floor_log2 : int -> int

(** [is_pow2 x] returns true iff [x] is a power of 2.  [is_pow2] raises if [x <= 0]. *)
val is_pow2 : int -> bool

(** {2 Conversion functions} *)

val of_int : int -> t
val to_int : t -> int
val of_int32 : int32 -> t option
val to_int32 : t -> int32 option
val of_int64 : int64 -> t option
val of_nativeint : nativeint -> t option
val to_nativeint : t -> nativeint

(**/**)
module Private : sig
  (*_ For ../bench/bench_int.ml *)
  module O_F : sig
    val ( %  ) : int -> int -> int
    val ( /% ) : int -> int -> int
    val ( // ) : int -> int -> float
  end
end
