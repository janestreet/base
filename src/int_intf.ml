open! Import

module type Round = sig
  type t

  (** [round] rounds an int to a multiple of a given [to_multiple_of] argument, according
     to a direction [dir], with default [dir] being [`Nearest].  [round] will raise if
     [to_multiple_of <= 0].

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

     For convenience and performance, there are variants of [round] with [dir] hard-coded.
     If you are writing performance-critical code you should use these.
  *)

  val round : ?dir:[ `Zero | `Nearest | `Up | `Down ] -> t -> to_multiple_of:t -> t

  val round_towards_zero : t -> to_multiple_of:t -> t
  val round_down         : t -> to_multiple_of:t -> t
  val round_up           : t -> to_multiple_of:t -> t
  val round_nearest      : t -> to_multiple_of:t -> t
end

module type Hexable = sig
  type t
  module Hex : sig
    type nonrec t = t [@@deriving sexp, compare, hash]

    include Stringable.S with type t := t

    val to_string_hum : ?delimiter:char -> t -> string
  end
end

module type S = sig
  type t [@@deriving hash, sexp]

  include Floatable.S          with type t := t
  include Intable.S            with type t := t
  include Identifiable.S       with type t := t
  include Comparable.With_zero with type t := t
  include Hexable              with type t := t

  (** [delimiter] is underscore by default *)
  val to_string_hum : ?delimiter:char -> t -> string

  (** The number of bits available in this integer type.  Note that the integer
      representations are signed *)
  val num_bits : int

  (** {9 Infix operators and constants } *)

  val zero : t
  val one : t
  val minus_one : t

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t

  (** Negation *)
  val neg : t -> t
  val ( ~- ) : t -> t

  (** There are two pairs of integer division and remainder functions, [/%] and [%], and
      [/] and [rem].  They both satisfy the same equation relating the quotient and the
      remainder:

      {[
        x = (x /% y) * y + (x % y);
        x = (x /  y) * y + (rem x y);
      ]}

      The functions return the same values if [x] and [y] are positive.  They all raise
      if [y = 0].

      The functions differ if [x < 0] or [y < 0].

      If [y < 0], then [%] and [/%] raise, whereas [/] and [rem] do not.

      [x % y] always returns a value between 0 and [y - 1], even when [x < 0].  On the
      other hand, [rem x y] returns a negative value if and only if [x < 0]; that value
      satisfies [abs (rem x y) <= abs y - 1]. *)
  val ( /% ) : t -> t -> t
  val ( %  ) : t -> t -> t
  val ( / )  : t -> t -> t
  val rem    : t -> t -> t

  (** float division of integers *)
  val ( // ) : t -> t -> float

  (** A sub-module designed to be opened to make working with ints more convenient.  *)
  module O : sig

    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( / ) : t -> t -> t
    val ( ~- ) : t -> t
    include Polymorphic_compare_intf.Infix with type t := t

    val abs    : t -> t
    val neg    : t -> t
    val zero   : t
    val of_int_exn : int -> t

    val ( % )  : t -> t -> t
    val ( /% ) : t -> t -> t
    val ( // ) : t -> t -> float
  end

  (** {9 Successor and predecessor functions } *)

  val succ : t -> t
  val pred : t -> t

  (** Returns the absolute value of the argument.  May be negative if the input is
      [min_value] *)
  val abs : t -> t

  include Round with type t := t

  (** [pow base exponent] returns [base] raised to the power of [exponent].  It is OK if
      [base <= 0].  [pow] raises if [exponent < 0], or an integer overflow would occur. *)
  val pow : t -> t -> t

  (** The largest representable integer *)
  val max_value : t

  (** The smallest representable integer *)
  val min_value : t

  (** {9 Bit-wise logical operations } *)

  val bit_and : t -> t -> t
  val bit_or : t -> t -> t
  val bit_xor : t -> t -> t
  val bit_not : t -> t

  (** {9 Bit-shifting operations }

      The results are unspecified for negative shifts and shifts [>= num_bits] *)

  (** shifts left, filling in with zeroes *)
  val shift_left : t -> int -> t

  (** shifts right, preserving the sign of the input. *)
  val shift_right : t -> int -> t

  (** shifts right, filling in with zeroes, which will not preserve the sign of the
      input *)
  val shift_right_logical : t -> int -> t

  (** {9 Increment and decrement functions for integer references } *)

  val decr : t ref -> unit
  val incr : t ref -> unit

  (** {9 Population count} *)

  (** returns the number of 1 bits in the binary representation of the input *)
  val popcount : t -> int

  (** {9 Conversion functions to related integer types} *)

  val of_int32_exn : int32 -> t
  val to_int32_exn : t -> int32
  val of_int64_exn : int64 -> t
  val to_int64 : t -> int64
  val of_nativeint_exn : nativeint -> t
  val to_nativeint_exn : t -> nativeint

  (** [of_float_unchecked] truncates the given floating point number to an integer,
      rounding towards zero.
      The result is unspecified if the argument is nan or falls outside the range
      of representable integers. *)
  val of_float_unchecked : float -> t

end

let%test_module _ = (module struct
  (** this functor's type-correctness ensures that every value in [S.O] is also in [S]. *)
  module Check_O_contained_in_S (M : S) : sig end = (M : module type of M.O)
end)
