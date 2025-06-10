@@ portable

module Uchar := Uchar0

external get
  :  (string[@local_opt])
  -> (int[@local_opt])
  -> char
  @@ portable
  = "%string_safe_get"

external get_uint8 : string @ local -> int -> int @@ portable = "%string_safe_get"
external length : (string[@local_opt]) -> int @@ portable = "%string_length"

external unsafe_get
  :  (string[@local_opt])
  -> (int[@local_opt])
  -> char
  @@ portable
  = "%string_unsafe_get"

external unsafe_get_uint8
  :  string @ local
  -> int
  -> int
  @@ portable
  = "%string_unsafe_get"

external unsafe_get_uint16_ne
  :  string @ local
  -> int
  -> int
  @@ portable
  = "%caml_string_get16u"

external swap16 : int -> int @@ portable = "%bswap16"

external unsafe_get_int32_ne
  :  string @ local
  -> int
  -> int32
  @@ portable
  = "%caml_string_get32u"

external swap32 : int32 -> int32 @@ portable = "%bswap_int32"
val unsafe_get_uint16_le : string @ local -> int -> int
val unsafe_get_uint16_be : string @ local -> int -> int
val unsafe_get_int32_le : string @ local -> int -> int32
val unsafe_get_int32_be : string @ local -> int -> int32
val max_length : int
val ( ^ ) : string @ local -> string @ local -> string
val capitalize : string -> string
val compare : string -> string -> int
val escaped : string -> string
val make : int -> char -> string
val sub : string -> pos:int -> len:int -> string
val uncapitalize : string -> string
val is_valid_utf_8 : string @ local -> bool
val is_valid_utf_16be : string @ local -> bool
val is_valid_utf_16le : string @ local -> bool
val get_utf_8_uchar : string @ local -> byte_pos:int -> Uchar.utf_decode
val get_utf_16be_uchar : string @ local -> byte_pos:int -> Uchar.utf_decode
val get_utf_16le_uchar : string @ local -> byte_pos:int -> Uchar.utf_decode
val get_utf_32le_uchar : string @ local -> byte_pos:int -> Uchar.utf_decode
val get_utf_32be_uchar : string @ local -> byte_pos:int -> Uchar.utf_decode
val concat : ?sep:string -> string list @ local -> string

val%template lowercase : string @ m -> string @ m
[@@alloc a @ m = (stack_local, heap_global)]

val%template uppercase : string @ m -> string @ m
[@@alloc a @ m = (stack_local, heap_global)]

val iter : string -> f:(char -> unit) @ local -> unit
val split_lines : string -> string list
