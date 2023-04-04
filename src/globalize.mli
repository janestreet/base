(**
   `globalize` functions for the builtin types.

   These functions are equivalent to the identity function, except that they copy their
   input rather than return it. They only copy as much as is required to match that type:
   `global_` and mutable subcomponents are not copied since those are already global.
   Globalizing a type with mutable contents (e.g., ['a array] or ['a ref]) will therefore
   create a non-shared copy; mutating the copy won't affect the original and vice versa.

   Further globalize functions can be generated with `ppx_globalize`. *)

val globalize_bool : (bool[@local]) -> bool
val globalize_char : (char[@local]) -> char
val globalize_float : (float[@local]) -> float
val globalize_int : (int[@local]) -> int
val globalize_int32 : (int32[@local]) -> int32
val globalize_int64 : (int64[@local]) -> int64
val globalize_nativeint : (nativeint[@local]) -> nativeint
val globalize_bytes : (bytes[@local]) -> bytes
val globalize_string : (string[@local]) -> string
val globalize_unit : (unit[@local]) -> unit
val globalize_array : (('a[@local]) -> 'b) -> ('a array[@local]) -> 'a array
val globalize_list : (('a[@local]) -> 'b) -> ('a list[@local]) -> 'b list
val globalize_option : (('a[@local]) -> 'b) -> ('a option[@local]) -> 'b option

val globalize_result
  :  (('ok[@ocaml.local]) -> 'ok)
  -> (('err[@ocaml.local]) -> 'err)
  -> (('ok, 'err) result[@ocaml.local])
  -> ('ok, 'err) result

val globalize_ref : (('a[@local]) -> 'b) -> ('a ref[@local]) -> 'a ref
