@@ portable

open! Import

val int64_u_to_string : int64# -> string
val int_to_string : int -> string
val nativeint_to_string : nativeint @ local -> string
val int32_to_string : int32 @ local -> string
val int64_to_string : int64 @ local -> string

[@@@ocaml.text {|/*|}]

module Private : sig
  module Constants : sig
    val pow10 : string
    val digit_pairs : string
  end
end
