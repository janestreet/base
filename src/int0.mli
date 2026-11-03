external format : string -> int -> string = "caml_format_int"
val to_string : int -> string
external of_string : string -> int = "caml_int_of_string"
val of_string_opt : string -> int option
val to_float : int -> float
val of_float : float -> int
val max_value : int
val min_value : int
val succ : int -> int
val pred : int -> int
val ceil_pow2 : int -> int
val floor_pow2 : int -> int
val is_pow2 : int -> bool
val clz : int -> int
val ctz : int -> int
val floor_log2 : int -> int
val ceil_log2 : int -> int
