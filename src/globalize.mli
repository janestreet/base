@@ portable

(** `globalize` functions for the builtin types.

    These functions are equivalent to the identity function, except that they copy their
    input rather than return it. They only copy as much as is required to match that type:
    `global_` and mutable subcomponents are not copied since those are already global.
    Globalizing a type with mutable contents (e.g., ['a array] or ['a ref]) will therefore
    create a non-shared copy; mutating the copy won't affect the original and vice versa.

    Further globalize functions can be generated with `ppx_globalize`. *)

val globalize_bool : local_ bool -> bool
val globalize_char : local_ char -> char
val globalize_float : local_ float -> float
val globalize_int : local_ int -> int
val globalize_int32 : local_ int32 -> int32
val globalize_int64 : local_ int64 -> int64
val globalize_nativeint : local_ nativeint -> nativeint
val globalize_bytes : local_ bytes -> bytes
val globalize_string : local_ string -> string
val globalize_unit : local_ unit -> unit
val globalize_array : (local_ 'a -> 'b) -> local_ 'a array -> 'a array
val globalize_floatarray : local_ floatarray -> floatarray
val globalize_lazy_t : (local_ 'a -> 'b) -> local_ 'a lazy_t -> 'a lazy_t
val globalize_list : (local_ 'a -> 'b) -> local_ 'a list -> 'b list
val globalize_option : (local_ 'a -> 'b) -> local_ 'a option -> 'b option

val globalize_or_null
  :  (local_ 'a -> 'b)
  -> local_ 'a Basement.Or_null_shim.t
  -> 'b Basement.Or_null_shim.t

val globalize_result
  :  (local_ 'ok -> 'ok)
  -> (local_ 'err -> 'err)
  -> local_ ('ok, 'err) result
  -> ('ok, 'err) result

val globalize_ref : (local_ 'a -> 'b) -> local_ 'a ref -> 'a ref
