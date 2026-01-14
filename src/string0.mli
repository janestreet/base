@@ portable

module Uchar := Uchar0

external get : (string[@local_opt]) -> (int[@local_opt]) -> char = "%string_safe_get"
external get_uint8 : string @ local -> int -> int = "%string_safe_get"
external length : (string[@local_opt]) -> int = "%string_length"

external unsafe_get
  :  (string[@local_opt])
  -> (int[@local_opt])
  -> char
  = "%string_unsafe_get"

external unsafe_get_uint8 : string @ local -> int -> int = "%string_unsafe_get"
external unsafe_get_uint16_ne : string @ local -> int -> int = "%caml_string_get16u"
external swap16 : int -> int = "%bswap16"
external unsafe_get_int32_ne : string @ local -> int -> int32 = "%caml_string_get32u"
external swap32 : int32 -> int32 = "%bswap_int32"
val unsafe_get_uint16_le : string @ local -> int -> int
val unsafe_get_uint16_be : string @ local -> int -> int
val unsafe_get_int32_le : string @ local -> int -> int32
val unsafe_get_int32_be : string @ local -> int -> int32
val max_length : int
val ( ^ ) : string @ local -> string @ local -> string
val capitalize : string -> string
val compare : string -> string -> int
val escaped : string -> string
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

[%%template:
[@@@alloc.default a @ m = (stack_local, heap_global)]

val make : int -> char -> string @ m
val concat : ?sep:string -> string list @ local -> string @ m
val append : string @ local -> string @ local -> string @ m
val lowercase : string @ m -> string @ m
val uppercase : string @ m -> string @ m]

val%template iter : string @ m -> f:(char -> unit) @ local -> unit
[@@mode m = (global, local)]

val split_lines : string -> string list

val unsafe_blits
  :  dst:bytes @ local
  -> dst_pos:int
  -> sep:string @ local
  -> sep_len:int
  -> string list @ local
  -> unit

[%%template:
[@@@alloc.default a @ m = (heap @ global, stack @ local)]

(** [init n ~f] is equivalent to [of_list [f 0; f 1; ...; f (n-1)]]. It raises an
    exception if [n < 0]. *)
val init : int -> f:(int -> char @ m) @ local -> string @ m

(** [filter t ~f] returns all the elements of [t] that satisfy the predicate [f]. *)
val filter : string @ m -> f:(char -> bool) @ local -> string @ m

val filteri : string @ m -> f:(int -> char -> bool) @ local -> string @ m]

val%template smart_globalize : string @ local -> string @ m
[@@alloc __ @ m = (heap_global, stack_local)]
