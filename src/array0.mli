@@ portable

[%%template:
  external create
    : ('a : any_non_null).
    len:int -> 'a -> 'a array @ m
    = "%makearray_dynamic"
  [@@alloc __ @ m = (heap_global, stack_local)] [@@layout_poly]]

external create_local
  : ('a : any_non_null).
  len:int -> 'a -> local_ 'a array
  = "%makearray_dynamic"
[@@layout_poly]

external magic_create_uninitialized
  : ('a : any_non_null).
  len:int -> ('a array[@local_opt])
  = "%makearray_dynamic_uninit"
[@@layout_poly]

external%template get
  : ('a : any_non_null).
  ('a array[@local_opt]) @ m -> (int[@local_opt]) -> 'a @ m
  = "%array_safe_get"
[@@layout_poly] [@@mode m = (uncontended, shared)]

external length
  : ('a : any_non_null).
  ('a array[@local_opt]) @ contended -> int
  = "%array_length"
[@@layout_poly]

external set
  : ('a : any_non_null).
  ('a array[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
  = "%array_safe_set"
[@@layout_poly]

external%template unsafe_get
  : ('a : any_non_null).
  ('a array[@local_opt]) @ m -> (int[@local_opt]) -> 'a @ m
  = "%array_unsafe_get"
[@@mode m = (uncontended, shared)] [@@layout_poly]

external unsafe_set
  : ('a : any_non_null).
  ('a array[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
  = "%array_unsafe_set"
[@@layout_poly]

external unsafe_blit
  :  src:('a array[@local_opt])
  -> src_pos:int
  -> dst:('a array[@local_opt])
  -> dst_pos:int
  -> len:int
  -> unit
  = "caml_array_blit"

external unsafe_fill : local_ 'a array -> int -> int -> 'a -> unit = "caml_array_fill"
external unsafe_sub : local_ 'a array -> int -> int -> 'a array = "caml_array_sub"
external concat : local_ 'a array list -> 'a array = "caml_array_concat"
val max_length : int
val create_float_uninitialized : len:int -> float array
val append : 'a array -> 'a array -> 'a array

val blit
  :  src:'a array @ local
  -> src_pos:int
  -> dst:'a array @ local
  -> dst_pos:int
  -> len:int
  -> unit

val fill : 'a array @ local -> pos:int -> len:int -> 'a -> unit

val%template init : int -> f:(int -> 'a) @ local -> 'a array @ m
[@@alloc __ @ m = (heap_global, stack_local)]

val make_matrix : dimx:int -> dimy:int -> 'a -> 'a array array
val of_list : 'a list -> 'a array
val sub : 'a array @ local -> pos:int -> len:int -> 'a array
val to_list : 'a array -> 'a list
val fold : 'a array -> init:'b -> f:('b -> 'a -> 'b) @ local -> 'b

val%template fold_right : 'a array @ m -> f:('a @ m -> 'b -> 'b) @ local -> init:'b -> 'b
[@@mode m = (uncontended, shared)]

val iter : 'a array -> f:('a -> unit) @ local -> unit
val iteri : 'a array -> f:(int -> 'a -> unit) @ local -> unit

[@@@warning "-incompatible-with-upstream"]

val%template map
  : ('a : ki) ('b : ko).
  'a array @ local -> f:('a -> 'b) @ local -> 'b array
[@@kind
  ki = (value, immediate, immediate64, float64, bits32, bits64, word)
  , ko = (value, immediate, immediate64, float64, bits32, bits64, word)]

val mapi : 'a array -> f:(int -> 'a -> 'b) @ local -> 'b array
val stable_sort : 'a array -> compare:('a -> 'a -> int) -> unit
val swap : 'a array @ local -> int -> int -> unit
