@@ portable

[%%template:
  external create
    : ('a : any mod separable).
    len:int -> 'a -> 'a array @ m unique
    = "%makearray_dynamic"
  [@@alloc __ @ m = (heap_global, stack_local)] [@@layout_poly]]

external create_local
  : ('a : any mod separable).
  len:int -> 'a -> 'a array @ local unique
  = "%makearray_dynamic"
[@@layout_poly]

external magic_create_uninitialized
  : ('a : any mod separable).
  len:int -> ('a array[@local_opt]) @ unique
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

external unsafe_fill
  : ('a : value_or_null mod separable).
  local_ 'a array -> int -> int -> 'a -> unit
  = "caml_array_fill"

external unsafe_sub
  : ('a : value_or_null mod separable).
  local_ 'a array -> int -> int -> 'a array
  = "caml_array_sub"

external concat
  : ('a : value_or_null mod separable).
  local_ 'a array list -> 'a array
  = "caml_array_concat"

val%template unsafe_blit
  : ('a : k mod separable).
  src:'a array @ local
  -> src_pos:int
  -> dst:'a array @ local
  -> dst_pos:int
  -> len:int
  -> unit
[@@kind k = (base_or_null, value_or_null mod external64)]

[%%template:
[@@@kind.default k' = (base_non_value, value_or_null mod external64)]
[@@@kind k = k' mod separable]

val unsafe_sub : ('a : k). 'a array @ local -> int -> int -> 'a array
val concat : ('a : k). 'a array list @ local -> 'a array]

val max_length : int
val create_float_uninitialized : len:int -> float array

include sig
  [@@@implicit_kind: ('a : value_or_null mod separable)]
  [@@@implicit_kind: ('b : value_or_null)]

  val blit
    :  src:'a array @ local
    -> src_pos:int
    -> dst:'a array @ local
    -> dst_pos:int
    -> len:int
    -> unit

  val make_matrix : dimx:int -> dimy:int -> 'a -> 'a array array

  val%template fold_right
    :  'a array @ m
    -> f:('a @ m -> 'b -> 'b) @ local
    -> init:'b
    -> 'b
  [@@mode m = (uncontended, shared)]

  val stable_sort : 'a array -> compare:('a -> 'a -> int) -> unit
end

[%%template:
[@@@kind.default k' = (base_or_null, value_or_null mod external64)]
[@@@kind k = k' mod separable]

val init : ('a : k). int -> f:(int -> 'a) @ local -> 'a array @ m unique
[@@alloc __ @ m = (heap_global, stack_local)]

val iter : ('a : k). 'a array -> f:('a -> unit) @ local -> unit
val iteri : ('a : k). 'a array -> f:(int -> 'a -> unit) @ local -> unit

val to_list
  : ('a : k).
  'a array @ m -> ('a List0.Constructors.t[@kind k' or value_or_null]) @ m
[@@alloc __ @ m = (heap_global, stack_local)]

val of_list : ('a : k). ('a List0.Constructors.t[@kind k' or value_or_null]) -> 'a array
val sub : ('a : k). 'a array @ local -> pos:int -> len:int -> 'a array
val append : ('a : k). 'a array -> 'a array -> 'a array
val fill : ('a : k). 'a array @ local -> pos:int -> len:int -> 'a -> unit
val swap : ('a : k). 'a array @ local -> int -> int -> unit]

[%%template:
[@@@kind.default
  k1' = (base_or_null, value_or_null mod external64)
  , k2' = (base_or_null, value_or_null mod external64)]

[@@@kind k1 = k1' mod separable, k2 = k2' mod separable]

val fold : ('a : k1) ('b : k2'). 'a array -> init:'b -> f:('b -> 'a -> 'b) @ local -> 'b
val map : ('a : k1) ('b : k2). 'a array @ local -> f:('a -> 'b) @ local -> 'b array
val mapi : ('a : k1) ('b : k2). 'a array -> f:(int -> 'a -> 'b) @ local -> 'b array]
