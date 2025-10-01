@@ portable

[@@@warning "-incompatible-with-upstream"]

[%%template:
  external create
    : ('a : any mod separable).
    len:int -> 'a -> 'a array @ m
    = "%makearray_dynamic"
  [@@alloc __ @ m = (heap_global, stack_local)] [@@layout_poly]]

external create_local
  : ('a : any mod separable).
  len:int -> 'a -> local_ 'a array
  = "%makearray_dynamic"
[@@layout_poly]

external magic_create_uninitialized
  : ('a : any mod separable).
  len:int -> ('a array[@local_opt])
  = "%makearray_dynamic_uninit"
[@@layout_poly]

external%template get
  : ('a : any mod separable).
  ('a array[@local_opt]) @ m -> (int[@local_opt]) -> 'a @ m
  = "%array_safe_get"
[@@layout_poly] [@@mode m = (uncontended, shared)]

external length
  : ('a : any mod separable).
  ('a array[@local_opt]) @ immutable -> int
  = "%array_length"
[@@layout_poly]

external set
  : ('a : any mod separable).
  ('a array[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
  = "%array_safe_set"
[@@layout_poly]

external%template unsafe_get
  : ('a : any mod separable).
  ('a array[@local_opt]) @ m -> (int[@local_opt]) -> 'a @ m
  = "%array_unsafe_get"
[@@mode m = (uncontended, shared)] [@@layout_poly]

external unsafe_set
  : ('a : any mod separable).
  ('a array[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
  = "%array_unsafe_set"
[@@layout_poly]

[%%template:
[@@@kind.default k = (value, immediate, immediate64)]

external unsafe_fill
  : ('a : k).
  local_ 'a array -> int -> int -> 'a -> unit
  = "caml_array_fill"

external unsafe_sub
  : ('a : k).
  local_ 'a array -> int -> int -> 'a array
  = "caml_array_sub"

external concat : ('a : k). local_ 'a array list -> 'a array = "caml_array_concat"]

val%template unsafe_blit
  : ('a : k).
  src:'a array @ local
  -> src_pos:int
  -> dst:'a array @ local
  -> dst_pos:int
  -> len:int
  -> unit
[@@kind k = (value, immediate, immediate64, bits64, bits32, word, float64)]

[%%template:
[@@@kind.default k = (float64, bits32, bits64, word)]

val unsafe_sub : ('a : k). 'a array @ local -> int -> int -> 'a array
val concat : ('a : k). 'a array list @ local -> 'a array]

val max_length : int
val create_float_uninitialized : len:int -> float array

val blit
  :  src:'a array @ local
  -> src_pos:int
  -> dst:'a array @ local
  -> dst_pos:int
  -> len:int
  -> unit

val make_matrix : dimx:int -> dimy:int -> 'a -> 'a array array

val%template fold_right : 'a array @ m -> f:('a @ m -> 'b -> 'b) @ local -> init:'b -> 'b
[@@mode m = (uncontended, shared)]

val stable_sort : 'a array -> compare:('a -> 'a -> int) -> unit

[%%template:
[@@@kind.default k1 = (value, immediate, immediate64, float64, bits32, bits64, word)]

val init : ('a : k1). int -> f:(int -> 'a) @ local -> 'a array @ m
[@@alloc __ @ m = (heap_global, stack_local)]

val iter : ('a : k1). 'a array -> f:('a -> unit) @ local -> unit
val iteri : ('a : k1). 'a array -> f:(int -> 'a -> unit) @ local -> unit
val to_list : ('a : k1). 'a array -> ('a List0.Constructors.t[@kind k1])
val of_list : ('a List0.Constructors.t[@kind k1]) -> 'a array
val sub : ('a : k1). 'a array @ local -> pos:int -> len:int -> 'a array
val append : ('a : k1). 'a array -> 'a array -> 'a array
val fill : ('a : k1). 'a array @ local -> pos:int -> len:int -> 'a -> unit
val swap : ('a : k1). 'a array @ local -> int -> int -> unit

[@@@kind.default k2 = (value, immediate, immediate64, float64, bits32, bits64, word)]

val fold : ('a : k1) ('b : k2). 'a array -> init:'b -> f:('b -> 'a -> 'b) @ local -> 'b
val map : ('a : k1) ('b : k2). 'a array @ local -> f:('a -> 'b) @ local -> 'b array
val mapi : ('a : k1) ('b : k2). 'a array -> f:(int -> 'a -> 'b) @ local -> 'b array]
