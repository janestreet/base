type t

val compare : t -> t -> int
val compare__local : t -> t -> int

(** [length = 16] is the size of the digest in bytes. *)
val length : int

val to_binary : t -> string
val to_binary_local : t -> string
val of_binary_exn : string -> t

(** assumes the input is 16 bytes without checking *)
val unsafe_of_binary : string -> t

val unsafe_of_binary_local : string -> t
val unsafe_of_binary__local : string -> t
val globalize : t -> t
val to_hex : t -> string
val of_hex_exn : string -> t
val string : string -> t
val bytes : bytes -> t
val subbytes : bytes -> pos:int -> len:int -> t
