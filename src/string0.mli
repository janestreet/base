module Uchar := Uchar0

external get : (string[@local_opt]) -> (int[@local_opt]) -> char = "%string_safe_get"
external get_uint8 : string -> int -> int = "%string_safe_get"
external length : (string[@local_opt]) -> int = "%string_length"

external unsafe_get
  :  (string[@local_opt])
  -> (int[@local_opt])
  -> char
  = "%string_unsafe_get"

external unsafe_get_uint8 : string -> int -> int = "%string_unsafe_get"
external unsafe_get_uint16_ne : string -> int -> int = "%caml_string_get16u"
external swap16 : int -> int = "%bswap16"
external unsafe_get_int32_ne : string -> int -> int32 = "%caml_string_get32u"
external swap32 : int32 -> int32 = "%bswap_int32"
val unsafe_get_uint16_le : string -> int -> int
val unsafe_get_uint16_be : string -> int -> int
val unsafe_get_int32_le : string -> int -> int32
val unsafe_get_int32_be : string -> int -> int32
val max_length : int
val ( ^ ) : string -> string -> string
val capitalize : string -> string
val compare : string -> string -> int
val escaped : string -> string
val sub : string -> pos:int -> len:int -> string
val uncapitalize : string -> string
val is_valid_utf_8 : string -> bool
val is_valid_utf_16be : string -> bool
val is_valid_utf_16le : string -> bool
val get_utf_8_uchar : string -> byte_pos:int -> Uchar.utf_decode
val get_utf_16be_uchar : string -> byte_pos:int -> Uchar.utf_decode
val get_utf_16le_uchar : string -> byte_pos:int -> Uchar.utf_decode
val get_utf_32le_uchar : string -> byte_pos:int -> Uchar.utf_decode
val get_utf_32be_uchar : string -> byte_pos:int -> Uchar.utf_decode

[%%template:
[@@@alloc.default a @ m = (stack_local, heap_global)]

val make : int -> char -> string
val concat : ?sep:string -> string list -> string
val append : string -> string -> string
val lowercase : string -> string
val uppercase : string -> string]

val iter : string -> f:(char -> unit) -> unit
val split_lines : string -> string list
