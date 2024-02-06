(**
   `globalize` functions for the builtin types.

   These functions are equivalent to the identity function, except that they copy their
   input rather than return it. They only copy as much as is required to match that type:
   `global_` and mutable subcomponents are not copied since those are already global.
   Globalizing a type with mutable contents (e.g., ['a array] or ['a ref]) will therefore
   create a non-shared copy; mutating the copy won't affect the original and vice versa.

   Further globalize functions can be generated with `ppx_globalize`. *)

val globalize_bool : bool -> bool
val globalize_char : char -> char
val globalize_float : float -> float
val globalize_int : int -> int
val globalize_int32 : int32 -> int32
val globalize_int64 : int64 -> int64
val globalize_nativeint : nativeint -> nativeint
val globalize_bytes : bytes -> bytes
val globalize_string : string -> string
val globalize_unit : unit -> unit
val globalize_array : ('a -> 'b) -> 'a array -> 'a array
val globalize_lazy_t : ('a -> 'b) -> 'a lazy_t -> 'a lazy_t
val globalize_list : ('a -> 'b) -> 'a list -> 'b list
val globalize_option : ('a -> 'b) -> 'a option -> 'b option

val globalize_result
  :  ('ok -> 'ok)
  -> ('err -> 'err)
  -> ('ok, 'err) result
  -> ('ok, 'err) result

val globalize_ref : ('a -> 'b) -> 'a ref -> 'a ref
