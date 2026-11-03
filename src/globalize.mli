(** `globalize` functions for the builtin types.

    These functions are equivalent to the identity function, except that they copy their
    input rather than return it. They only copy as much as is required to match that type:
    `global_` and mutable subcomponents are not copied since those are already global.
    Globalizing a type with mutable contents (e.g., ['a array] or ['a ref]) will therefore
    create a non-shared copy; mutating the copy won't affect the original and vice versa.

    Further globalize functions can be generated with `ppx_globalize`. *)

(*_ It's tempting to make [maybe_globalize] templated over the mode of the input too,
    however it's not clear whether we'll be able to support that in the future (once it
    works via first-class language features rather than ppx_template).

    The input can't be mode-polymorphic over (local, global) as there is no way to tell if
    a value is local or global at runtime. Specifically, we can't look at whether it's on
    the heap as a proxy because borrowing will mean that things on the heap can be truly
    local.

    The input also can't have its own allocator acting as a mode witness. It's plausible
    allocators will be covariant over their mode parameter but that means you would be
    able to pass a heap allocator in as the witness of a local value, so it wouldn't get
    globalized, effectively magic-ing something from local to global. *)
val maybe_globalize : ('a -> 'a) -> 'a -> 'a
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
val globalize_array : 'a 'b. ('a -> 'b) -> 'a array -> 'a array
val globalize_floatarray : floatarray -> floatarray
val globalize_lazy_t : ('a -> 'b) -> 'a lazy_t -> 'a lazy_t
val globalize_list : 'a 'b. ('a -> 'b) -> 'a list -> 'b list
val globalize_option : 'a 'b. ('a -> 'b) -> 'a option -> 'b option

val globalize_or_null
  :  ('a -> 'b)
  -> 'a Basement.Or_null_shim.t
  -> 'b Basement.Or_null_shim.t

val globalize_result
  : 'ok 'err.
  ('ok -> 'ok) -> ('err -> 'err) -> ('ok, 'err) result -> ('ok, 'err) result

val globalize_ref : 'a 'b. ('a -> 'b) -> 'a ref -> 'a ref
