@@ portable

type t = Stdlib.Uchar.t

val succ : t -> t
val pred : t -> t
val is_valid : int -> bool
val is_char : t -> bool
val unsafe_to_char : t -> char
val unsafe_of_int : int -> t
val of_int : int -> t
val to_int : t -> int
val of_char : char -> t
val compare : t -> t -> int
val equal : t -> t -> bool
val min_value : t
val max_value : t
val byte_order_mark : t
val replacement_char : t
val utf_8_byte_length : t -> int
val utf_16_byte_length : t -> int

type utf_decode = Stdlib.Uchar.utf_decode

val utf_decode_is_valid : utf_decode -> bool
val utf_decode_uchar : utf_decode -> t
val utf_decode_length : utf_decode -> int
val utf_decode : int -> t -> utf_decode
val utf_decode_invalid : int -> utf_decode
