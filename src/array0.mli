[%%template:
  external create : len:int -> 'a -> 'a array = "caml_make_vect"
  [@@alloc __ = (heap, stack)]]

val create_local : len:int -> 'a -> 'a array
val magic_create_uninitialized : len:int -> ('a array[@local_opt])

external%template get
  : 'a.
  ('a array[@local_opt]) -> (int[@local_opt]) -> 'a
  = "%array_safe_get"
[@@layout_poly] [@@mode m = (uncontended, shared)]

external length : 'a. ('a array[@local_opt]) -> int = "%array_length" [@@layout_poly]

external set
  : 'a.
  ('a array[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
  = "%array_safe_set"
[@@layout_poly]

external%template unsafe_get
  : 'a.
  ('a array[@local_opt]) -> (int[@local_opt]) -> 'a
  = "%array_unsafe_get"
[@@mode m = (uncontended, shared)] [@@layout_poly]

external unsafe_set
  : 'a.
  ('a array[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
  = "%array_unsafe_set"
[@@layout_poly]

[%%template:
[@@@kind.default k = (value, value mod external64)]

external unsafe_fill : 'a. 'a array -> int -> int -> 'a -> unit = "caml_array_fill"
external unsafe_sub : 'a. 'a array -> int -> int -> 'a array = "caml_array_sub"
external concat : 'a. 'a array list -> 'a array = "caml_array_concat"]

val%template unsafe_blit
  : 'a.
  src:'a array -> src_pos:int -> dst:'a array -> dst_pos:int -> len:int -> unit
[@@kind k = (base, value mod external64)]

[%%template:
[@@@kind.default k = base_non_value]

val unsafe_sub : 'a. 'a array -> int -> int -> 'a array
val concat : 'a. 'a array list -> 'a array]

val max_length : int
val create_float_uninitialized : len:int -> float array
val blit : src:'a array -> src_pos:int -> dst:'a array -> dst_pos:int -> len:int -> unit
val make_matrix : dimx:int -> dimy:int -> 'a -> 'a array array

val%template fold_right : 'a array -> f:('a -> 'b -> 'b) -> init:'b -> 'b
[@@mode m = (uncontended, shared)]

val stable_sort : 'a array -> compare:('a -> 'a -> int) -> unit

[%%template:
[@@@kind.default k' = (base_or_null, value mod external64)]
[@@@kind k = k' mod separable]

val init : 'a. int -> f:(int -> 'a) -> 'a array
[@@alloc __ @ m = (heap_global, stack_local)]

val iter : 'a. 'a array -> f:('a -> unit) -> unit
val iteri : 'a. 'a array -> f:(int -> 'a -> unit) -> unit

val to_list : 'a. 'a array -> ('a List0.Constructors.t[@kind k' or value_or_null])
[@@alloc __ @ m = (heap_global, stack_local)]

val of_list : 'a. ('a List0.Constructors.t[@kind k' or value_or_null]) -> 'a array
val sub : 'a. 'a array -> pos:int -> len:int -> 'a array
val append : 'a. 'a array -> 'a array -> 'a array
val fill : 'a. 'a array -> pos:int -> len:int -> 'a -> unit
val swap : 'a. 'a array -> int -> int -> unit]

[%%template:
[@@@kind.default k1 = (base, value mod external64), k2 = (base, value mod external64)]

val fold : 'a 'b. 'a array -> init:'b -> f:('b -> 'a -> 'b) -> 'b
val map : 'a 'b. 'a array -> f:('a -> 'b) -> 'b array
val mapi : 'a 'b. 'a array -> f:(int -> 'a -> 'b) -> 'b array]
