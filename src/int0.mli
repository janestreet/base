@@ portable

external format : string @ local -> int -> string = "caml_format_int"
val to_string : int -> string
external of_string : string @ local -> int = "caml_int_of_string"
val of_string_opt : string @ local -> int option
val to_float : int -> float
val of_float : float -> int
val max_value : int
val min_value : int
val succ : int -> int
val pred : int -> int
