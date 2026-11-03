@@ portable

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
  val%template maybe_globalize : ('a @ local -> 'a) -> 'a @ local -> 'a @ l
  [@@alloc a @ l = (heap_global, stack_local)]

val globalize_bool : bool @ local -> bool
val globalize_char : char @ local -> char
val globalize_float : float @ local -> float
val globalize_int : int @ local -> int
val globalize_int32 : int32 @ local -> int32
val globalize_int64 : int64 @ local -> int64
val globalize_nativeint : nativeint @ local -> nativeint
val globalize_bytes : bytes @ local -> bytes @ unique
val globalize_string : string @ local -> string @ unique
val globalize_unit : unit @ local -> unit

val%template globalize_array
  : ('a : k mod separable) ('b : any).
  ('a @ local -> 'b) -> 'a array @ local -> 'a array
[@@kind k = (base_or_null_with_imm, value_or_null mod external64)]

val globalize_floatarray : floatarray @ local -> floatarray @ unique
val globalize_lazy_t : ('a @ local -> 'b) -> 'a lazy_t @ local -> 'a lazy_t

val globalize_list
  : ('a : value_or_null) ('b : value_or_null).
  ('a @ local -> 'b) -> 'a list @ local -> 'b list

val globalize_option
  : ('a : value_or_null) ('b : value_or_null).
  ('a @ local -> 'b) -> 'a option @ local -> 'b option

val globalize_or_null
  :  ('a @ local -> 'b)
  -> 'a Basement.Or_null_shim.t @ local
  -> 'b Basement.Or_null_shim.t

val globalize_result
  : ('ok : value_or_null) ('err : value_or_null).
  ('ok @ local -> 'ok)
  -> ('err @ local -> 'err)
  -> ('ok, 'err) result @ local
  -> ('ok, 'err) result

val globalize_ref
  : ('a : value_or_null) ('b : value_or_null).
  ('a @ local -> 'b) -> 'a ref @ local -> 'a ref
