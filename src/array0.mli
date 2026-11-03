module List := List0

external create : len:int -> 'a -> 'a array = "caml_make_vect"
val create_local : len:int -> 'a -> 'a array
val magic_create_uninitialized : len:int -> ('a array[@local_opt])

external get : 'a. ('a array[@local_opt]) -> (int[@local_opt]) -> 'a = "%array_safe_get"
[@@layout_poly]

external length : 'a. ('a array[@local_opt]) -> int = "%array_length" [@@layout_poly]

external set
  : 'a.
  ('a array[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
  = "%array_safe_set"
[@@layout_poly]

external unsafe_get
  : 'a.
  ('a array[@local_opt]) -> (int[@local_opt]) -> 'a
  = "%array_unsafe_get"
[@@layout_poly]

external unsafe_set
  : 'a.
  ('a array[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
  = "%array_unsafe_set"
[@@layout_poly]

external unsafe_fill : 'a. 'a array -> int -> int -> 'a -> unit = "caml_array_fill"
external unsafe_sub : 'a. 'a array -> int -> int -> 'a array = "caml_array_sub"
external concat : 'a. 'a array list -> 'a array = "caml_array_concat"

val unsafe_blit
  : 'a.
  src:'a array -> src_pos:int -> dst:'a array -> dst_pos:int -> len:int -> unit

val max_length : int
val create_float_uninitialized : len:int -> float array

include sig
  [@@@implicit_kind: 'a]
  [@@@implicit_kind: 'b]

  val blit : src:'a array -> src_pos:int -> dst:'a array -> dst_pos:int -> len:int -> unit
  val make_matrix : dimx:int -> dimy:int -> 'a -> 'a array array
  val fold_right : 'a array -> f:('a -> 'b -> 'b) -> init:'b -> 'b
  val stable_sort : 'a array -> compare:('a -> 'a -> int) -> unit
end

val init : 'a. int -> f:(int -> 'a) -> 'a array
val iter : 'a. 'a array -> f:('a -> unit) -> unit
val iteri : 'a. 'a array -> f:(int -> 'a -> unit) -> unit
val to_list : 'a. 'a array -> 'a List.Constructors.t
val of_list : 'a. 'a List.Constructors.t -> 'a array
val sub : 'a. 'a array -> pos:int -> len:int -> 'a array
val append : 'a. 'a array -> 'a array -> 'a array
val fill : 'a. 'a array -> pos:int -> len:int -> 'a -> unit
val swap : 'a. 'a array -> int -> int -> unit
val fold : 'a 'b. 'a array -> init:'b -> f:('b -> 'a -> 'b) -> 'b
val map : 'a 'b. 'a array -> f:('a -> 'b) -> 'b array
val mapi : 'a 'b. 'a array -> f:(int -> 'a -> 'b) -> 'b array
