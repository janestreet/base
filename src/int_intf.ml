(** An interface to use for int-like types, e.g., {{!Base.Int} [Int]} and {{!Base.Int64}
    [Int64]}. *)

open! Import

module Definitions = struct
  module type Round = sig
    type t

    (** [round] rounds an int to a multiple of a given [to_multiple_of] argument,
        according to a direction [dir], with default [dir] being [`Nearest]. [round] will
        raise if [to_multiple_of <= 0]. If the result overflows (too far positive or too
        far negative), [round] returns an incorrect result.

        {v
       | `Down    | rounds toward Int.neg_infinity                          |
       | `Up      | rounds toward Int.infinity                              |
       | `Nearest | rounds to the nearest multiple, or `Up in case of a tie |
       | `Zero    | rounds toward zero                                      |
        v}

        Here are some examples for [round ~to_multiple_of:10] for each direction:

        {v
       | `Down    | {10 .. 19} --> 10 | { 0 ... 9} --> 0 | {-10 ... -1} --> -10 |
       | `Up      | { 1 .. 10} --> 10 | {-9 ... 0} --> 0 | {-19 .. -10} --> -10 |
       | `Zero    | {10 .. 19} --> 10 | {-9 ... 9} --> 0 | {-19 .. -10} --> -10 |
       | `Nearest | { 5 .. 14} --> 10 | {-5 ... 4} --> 0 | {-15 ... -6} --> -10 |
        v}

        For convenience and performance, there are variants of [round] with [dir]
        hard-coded. If you are writing performance-critical code you should use these. *)

    val round
      :  ?dir:local_ [ `Zero | `Nearest | `Up | `Down ]
      -> local_ t
      -> to_multiple_of:local_ t
      -> t

    val round_towards_zero : local_ t -> to_multiple_of:local_ t -> t
    val round_down : local_ t -> to_multiple_of:local_ t -> t
    val round_up : local_ t -> to_multiple_of:local_ t -> t
    val round_nearest : local_ t -> to_multiple_of:local_ t -> t
  end

  (** Human-readable serialization. *)
  module type To_string_hum = sig
    type t

    val to_string_hum : ?delimiter:char -> local_ t -> string

    (** Alias for [to_string_hum] where a submodule is more convenient, for instance:
        [%string "%{x#Int.Hum}"] *)
    module Hum : sig
      val to_string : ?delimiter:char -> local_ t -> string
    end
  end

  (** String format for integers, [to_string] / [sexp_of_t] direction only. Includes
      comparisons and hash functions for [[@@deriving]]. *)
  module type To_string_format = sig
    type t [@@deriving sexp_of, compare ~localize, hash]

    val to_string : local_ t -> string

    include To_string_hum with type t := t
  end

  (** String format for integers, including both [to_string] / [sexp_of_t] and [of_string]
      / [t_of_sexp] directions. Includes comparisons and hash functions for
      [[@@deriving]]. *)
  module type String_format = sig
    type t [@@deriving sexp, sexp_grammar, compare ~localize, hash]

    include Stringable.S with type t := t
    include To_string_hum with type t := t
  end

  (** Binary format for integers, unsigned and starting with [0b]. *)
  module type Binaryable = sig
    type t

    module Binary : To_string_format with type t = t
  end

  (** Hex format for integers, signed and starting with [0x]. *)
  module type Hexable = sig
    type t

    module Hex : String_format with type t = t
  end

  module type S_common = sig
    type t [@@deriving sexp ~stackify, sexp_grammar]

    include Floatable.S_local_input with type t := t
    include Intable.S with type t := t

    include%template Identifiable.S [@mode local] [@modality portable] with type t := t

    include Stringable.S_local_input with type t := t
    include Comparable.With_zero with type t := t
    include Invariant.S with type t := t
    include Hexable with type t := t
    include Binaryable with type t := t

    val of_string_opt : local_ string -> t option

    (** [delimiter] is an underscore by default. *)
    val to_string_hum : ?delimiter:char -> local_ t -> string

    (** {2 Infix operators and constants} *)

    val zero : t
    val one : t
    val minus_one : t
    val ( + ) : local_ t -> local_ t -> t
    val ( - ) : local_ t -> local_ t -> t
    val ( * ) : local_ t -> local_ t -> t

    (** Integer exponentiation *)
    val ( ** ) : local_ t -> local_ t -> t

    (** Negation *)

    val neg : local_ t -> t
    val ( ~- ) : local_ t -> t

    (** There are two pairs of integer division and remainder functions, [/%] and [%], and
        [/] and [rem]. They both satisfy the same equation relating the quotient and the
        remainder:

        {[
          x = (x /% y * y) + (x % y);
          x = (x / y * y) + rem x y
        ]}

        The functions return the same values if [x] and [y] are positive. They all raise
        if [y = 0].

        The functions differ if [x < 0] or [y < 0].

        If [y < 0], then [%] and [/%] raise, whereas [/] and [rem] do not.

        [x % y] always returns a value between 0 and [y - 1], even when [x < 0]. On the
        other hand, [rem x y] returns a negative value if and only if [x < 0]; that value
        satisfies [abs (rem x y) <= abs y - 1]. *)

    val ( /% ) : local_ t -> local_ t -> t
    val ( % ) : local_ t -> local_ t -> t
    val ( / ) : local_ t -> local_ t -> t
    val rem : local_ t -> local_ t -> t

    (** Float division of integers. *)
    val ( // ) : local_ t -> local_ t -> float

    (** Same as [bit_and]. *)
    val ( land ) : local_ t -> local_ t -> t

    (** Same as [bit_or]. *)
    val ( lor ) : local_ t -> local_ t -> t

    (** Same as [bit_xor]. *)
    val ( lxor ) : local_ t -> local_ t -> t

    (** Same as [bit_not]. *)
    val lnot : local_ t -> t

    (** Same as [shift_left]. *)
    val ( lsl ) : local_ t -> int -> t

    (** Same as [shift_right]. *)
    val ( asr ) : local_ t -> int -> t

    (** {2 Other common functions} *)

    include Round with type t := t

    (** Returns the absolute value of the argument. May be negative if the input is
        [min_value]. *)
    val abs : t -> t

    (** Like [abs], but for locally-allocated values. If the argument is already positive,
        then this is the identity (as is [abs] itself). *)
    val abs_local : local_ t -> local_ t

    (** {2 Successor and predecessor functions} *)

    val succ : local_ t -> t
    val pred : local_ t -> t

    (** {2 Exponentiation} *)

    (** [pow base exponent] returns [base] raised to the power of [exponent]. It is OK if
        [base <= 0]. [pow] raises if [exponent < 0], or an integer overflow would occur. *)
    val pow : local_ t -> local_ t -> t

    (** {2 Bit-wise logical operations} *)

    (** These are identical to [land], [lor], etc. except they're not infix and have
        different names. *)
    val bit_and : local_ t -> local_ t -> t

    val bit_or : local_ t -> local_ t -> t
    val bit_xor : local_ t -> local_ t -> t
    val bit_not : local_ t -> t

    (** Returns the number of 1 bits in the binary representation of the input. *)
    val popcount : local_ t -> t

    (** {2 Bit-shifting operations}

        The results are unspecified for negative shifts and shifts [>= num_bits]. *)

    (** Shifts left, filling in with zeroes. *)
    val shift_left : local_ t -> int -> t

    (** Shifts right, preserving the sign of the input. *)
    val shift_right : local_ t -> int -> t

    (** {2 Increment and decrement functions for integer references} *)

    val decr : local_ t ref -> unit
    val incr : local_ t ref -> unit

    (** {2 Conversion functions to related integer types} *)

    val of_int32_exn : int32 -> t
    val to_int32_exn : t -> int32
    val of_int64_exn : int64 -> t
    val to_int64 : t -> int64
    val of_nativeint_exn : nativeint -> t
    val to_nativeint_exn : t -> nativeint

    (*_ N.b. we provide separate local-accepting versions so that they may be implemented
        as the identity function, as an optimization. *)
    val of_local_int32_exn : local_ int32 -> local_ t
    val to_local_int32_exn : local_ t -> local_ int32
    val of_local_int64_exn : local_ int64 -> local_ t
    val to_local_int64 : local_ t -> local_ int64
    val of_local_nativeint_exn : local_ nativeint -> local_ t
    val to_local_nativeint_exn : local_ t -> local_ nativeint

    (** [of_float_unchecked] truncates the given floating point number to an integer,
        rounding towards zero. The result is unspecified if the argument is nan or falls
        outside the range of representable integers. *)
    val of_float_unchecked : local_ float -> t

    module Summable : sig
      type nonrec t = t

      val zero : t
      val ( + ) : t -> t -> t
      val ( - ) : t -> t -> t
    end
  end

  module type Operators_unbounded = sig
    type t

    val ( + ) : local_ t -> local_ t -> t
    val ( - ) : local_ t -> local_ t -> t
    val ( * ) : local_ t -> local_ t -> t
    val ( / ) : local_ t -> local_ t -> t
    val ( ~- ) : local_ t -> t
    val ( ** ) : local_ t -> local_ t -> t

    include Comparisons.Infix with type t := t

    val abs : t -> t
    val abs_local : local_ t -> local_ t
    val neg : local_ t -> t
    val zero : t
    val ( % ) : local_ t -> local_ t -> t
    val ( /% ) : local_ t -> local_ t -> t
    val ( // ) : local_ t -> local_ t -> float
    val ( land ) : local_ t -> local_ t -> t
    val ( lor ) : local_ t -> local_ t -> t
    val ( lxor ) : local_ t -> local_ t -> t
    val lnot : local_ t -> t
    val ( lsl ) : local_ t -> int -> t
    val ( asr ) : local_ t -> int -> t
  end

  module type Operators = sig
    include Operators_unbounded

    val ( lsr ) : local_ t -> int -> t
  end

  (** [S_unbounded] is a generic interface for unbounded integers, e.g. [Bignum.Bigint].
      [S_unbounded] is a restriction of [S] (below) that omits values that depend on
      fixed-size integers. *)
  module type S_unbounded = sig
    include S_common (** @inline *)

    (** A sub-module designed to be opened to make working with ints more convenient. *)
    module O : Operators_unbounded with type t := t
  end

  (** [S] is a generic interface for fixed-size integers. *)
  module type S = sig
    include S_common (** @inline *)

    (** The number of bits available in this integer type. Note that the integer
        representations are signed. *)
    val num_bits : t

    (** The largest representable integer. *)
    val max_value : t

    (** The smallest representable integer. *)
    val min_value : t

    (** Same as [shift_right_logical]. *)
    val ( lsr ) : local_ t -> int -> t

    (** Shifts right, filling in with zeroes, which will not preserve the sign of the
        input. *)
    val shift_right_logical : local_ t -> int -> t

    (** [ceil_pow2 x] returns the smallest power of 2 that is greater than or equal to
        [x]. The implementation may only be called for [x > 0]. Example:
        [ceil_pow2 17 = 32] *)
    val ceil_pow2 : local_ t -> t

    (** [floor_pow2 x] returns the largest power of 2 that is less than or equal to [x].
        The implementation may only be called for [x > 0]. Example: [floor_pow2 17 = 16] *)
    val floor_pow2 : local_ t -> t

    (** [ceil_log2 x] returns the ceiling of log-base-2 of [x], and raises if [x <= 0]. *)
    val ceil_log2 : local_ t -> t

    (** [floor_log2 x] returns the floor of log-base-2 of [x], and raises if [x <= 0]. *)
    val floor_log2 : local_ t -> t

    (** [is_pow2 x] returns true iff [x] is a power of 2. [is_pow2] raises if [x <= 0]. *)
    val is_pow2 : local_ t -> bool

    (** Returns the number of leading zeros in the binary representation of the input, as
        an integer between 0 and one less than [num_bits].

        The results are unspecified for [t = 0]. *)
    val clz : local_ t -> t

    (** Returns the number of trailing zeros in the binary representation of the input, as
        an integer between 0 and one less than [num_bits].

        The results are unspecified for [t = 0]. *)
    val ctz : local_ t -> t

    (** A sub-module designed to be opened to make working with ints more convenient. *)
    module O : Operators with type t := t
  end

  module type Int_without_module_types = sig
    (** OCaml's native integer type.

        The number of bits in an integer is platform dependent, being 31-bits on a 32-bit
        platform, and 63-bits on a 64-bit platform. [int] is a signed integer type. [int]s
        are also subject to overflow, meaning that [Int.max_value + 1 = Int.min_value].

        [int]s always fit in a machine word. *)

    type t = int [@@deriving globalize]

    include S with type t := t (** @inline *)

    module O : sig
      (*_ Declared as externals so that the compiler skips the caml_apply_X wrapping even
          when compiling without cross library inlining. *)
      external ( + ) : (t[@local_opt]) -> (t[@local_opt]) -> t = "%addint"
      external ( - ) : (t[@local_opt]) -> (t[@local_opt]) -> t = "%subint"
      external ( * ) : (t[@local_opt]) -> (t[@local_opt]) -> t = "%mulint"
      external ( / ) : (t[@local_opt]) -> (t[@local_opt]) -> t = "%divint"
      external ( ~- ) : (t[@local_opt]) -> t = "%negint"
      val ( ** ) : local_ t -> local_ t -> t
      external ( = ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%equal"
      external ( <> ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%notequal"
      external ( < ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%lessthan"
      external ( > ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%greaterthan"
      external ( <= ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%lessequal"
      external ( >= ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%greaterequal"
      external ( land ) : (t[@local_opt]) -> (t[@local_opt]) -> t = "%andint"
      external ( lor ) : (t[@local_opt]) -> (t[@local_opt]) -> t = "%orint"
      external ( lxor ) : (t[@local_opt]) -> (t[@local_opt]) -> t = "%xorint"
      val lnot : local_ t -> t
      val abs : t -> t
      val abs_local : local_ t -> local_ t
      external neg : (t[@local_opt]) -> t = "%negint"
      val zero : t
      val ( % ) : local_ t -> local_ t -> t
      val ( /% ) : local_ t -> local_ t -> t
      val ( // ) : local_ t -> local_ t -> float
      external ( lsl ) : (t[@local_opt]) -> (int[@local_opt]) -> t = "%lslint"
      external ( asr ) : (t[@local_opt]) -> (int[@local_opt]) -> t = "%asrint"
      external ( lsr ) : (t[@local_opt]) -> (int[@local_opt]) -> t = "%lsrint"
    end

    include module type of O
    include Replace_polymorphic_compare.S with type t := t

    (** [max_value_30_bits = 2^30 - 1]. It is useful for writing tests that work on both
        64-bit and 32-bit platforms. *)
    val max_value_30_bits : t

    (** {2 Conversion functions} *)

    val of_int : int -> t
    val to_int : t -> int
    val of_int32 : local_ int32 -> t option
    val to_int32 : t -> int32 option
    val of_int64 : local_ int64 -> t option
    val of_int64_exn : local_ int64 -> t
    val of_nativeint : local_ nativeint -> t option
    external to_nativeint : t -> (nativeint[@local_opt]) = "%nativeint_of_int"

    (** {3 Truncating conversions}

        These functions return the least-significant bits of the input. In cases where
        optional conversions return [Some x], truncating conversions return [x]. *)

    (*_ Declared as externals so that the compiler skips the caml_apply_X wrapping even
        when compiling without cross library inlining. *)
    external to_int32_trunc : t -> (int32[@local_opt]) = "%int32_of_int"
    external of_int32_trunc : local_ int32 -> t = "%int32_to_int"
    external of_int64_trunc : local_ int64 -> t = "%int64_to_int"
    external of_nativeint_trunc : local_ nativeint -> t = "%nativeint_to_int"

    (** {2 Byte swap operations}

        Byte swap operations reverse the order of bytes in an integer. For example,
        {!Int32.bswap32} reorders the bottom 32 bits (or 4 bytes), turning [0x1122_3344]
        to [0x4433_2211]. Byte swap functions exposed by Base use OCaml primitives to
        generate assembly instructions to perform the relevant byte swaps.

        For a more extensive list of byteswap functions, see {!Int32} and {!Int64}. *)

    (** Byte swaps bottom 16 bits (2 bytes). The values of the remaining bytes are
        undefined. *)
    external bswap16 : t -> t = "%bswap16"
    (*_ Declared as an external so that the compiler skips the caml_apply_X wrapping even
        when compiling without cross library inlining. *)

    (**/**)

    (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

        https://opensource.janestreet.com/standards/#private-submodules *)
    module Private : sig
      (*_ For ../bench/bench_int.ml *)
      module O_F : sig
        val ( % ) : local_ int -> local_ int -> int
        val ( /% ) : local_ int -> local_ int -> int
        val ( // ) : local_ int -> local_ int -> float
      end
    end
  end
end

module type Int = sig @@ portable
  include module type of struct
    include Definitions
  end

  include Int_without_module_types (** @inline *)
end
