open! Import0

type t = Stdlib.Uchar.t

let succ = Stdlib.Uchar.succ
let pred = Stdlib.Uchar.pred
let is_valid = Stdlib.Uchar.is_valid
let is_char = Stdlib.Uchar.is_char
let unsafe_to_char = Stdlib.Uchar.unsafe_to_char
let unsafe_of_int = Stdlib.Uchar.unsafe_of_int
let of_int = Stdlib.Uchar.of_int
let to_int = Stdlib.Uchar.to_int
let of_char = Stdlib.Uchar.of_char
let compare = Stdlib.Uchar.compare
let equal = Stdlib.Uchar.equal
let min_value = Stdlib.Uchar.min
let max_value = Stdlib.Uchar.max
let byte_order_mark = Stdlib.Uchar.bom
let replacement_char = Stdlib.Uchar.rep
let utf_8_byte_length = Stdlib.Uchar.utf_8_byte_length
let utf_16_byte_length = Stdlib.Uchar.utf_16_byte_length

type utf_decode = Stdlib.Uchar.utf_decode

let utf_decode_is_valid = Stdlib.Uchar.utf_decode_is_valid
let utf_decode_uchar = Stdlib.Uchar.utf_decode_uchar
let utf_decode_length = Stdlib.Uchar.utf_decode_length
let utf_decode = Stdlib.Uchar.utf_decode
let utf_decode_invalid = Stdlib.Uchar.utf_decode_invalid
