@@ portable

open! Import0

[%%template:
[@@@alloc.default a @ m = (heap_global, stack_local)]

val int64_u_to_string : Int64_u.t -> string @ m
val int_to_string : int -> string @ m
val nativeint_to_string : nativeint @ local -> string @ m
val int32_to_string : int32 @ local -> string @ m
val int64_to_string : int64 @ local -> string @ m]

[@@@ocaml.text {|/*|}]

module I64u : sig
  type t = Int64_u.t

  val num_digits_nonneg : t -> t
  [@@ocaml.doc {| The number of decimal digits in the argument, which must be >=0. |}]

  val unsafe_write_nonnegative_decimal
    :  bytes @ local
    -> pos:t
    -> num_digits:t
    -> t
    -> unit
  [@@ocaml.doc
    {| Pokes this number into the destination buffer at the given position; we do no bounds
      checking. We will write exactly [num_digits] characters, starting from the least
      significant, padding with zeroes as necessary. If the number does not fit in
      [num_digits], we may produce garbage. |}]
end

module Private : sig
  module Constants : sig
    val pow10 : string
    val digit_pairs : string
  end
end
