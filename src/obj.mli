open! Import

(** Versions of [Stdlib.Obj] functions that work with modes. Note it is never safe to
    magic a local value to global. *)

type t = Stdlib.Obj.t
type raw_data = Stdlib.Obj.raw_data

external tag : t -> int = "caml_obj_tag" [@@noalloc]
val size : t -> int [@@zero_alloc]
external is_int : t -> bool = "%obj_is_int"
val is_block : t -> bool [@@zero_alloc]

(** [dup t] returns a shallow copy of [t]. However if [t] is immutable then it might be
    returned unchanged. *)
val dup : t -> t

(** Checks whether a value is stack-allocated. *)
external is_stack : t -> bool = "caml_dummy_obj_is_stack"
[@@noalloc]

type stack_or_heap =
  | Immediate
  | Stack
  | Heap
[@@deriving sexp ~localize, compare]

(** Checks if a value is immediate, stack-allocated, or heap-allocated. *)
val stack_or_heap : t -> stack_or_heap
[@@zero_alloc]

type uniform_or_mixed =
  | Immediate
  | Uniform
  | Mixed of { scannable_prefix_len : int }
[@@deriving sexp ~localize, compare ~localize, globalize]

(** Checks if a value is immediate, a uniform block, or a mixed block. *)
val uniform_or_mixed : t -> uniform_or_mixed
[@@zero_alloc]

(** Computes the total size (in words, including the headers) of all heap blocks
    accessible from the argument. Statically allocated blocks are included. *)
val reachable_words : t -> int

val first_non_constant_constructor_tag : int
val last_non_constant_constructor_tag : int
val forcing_tag : int
val cont_tag : int
val lazy_tag : int
val closure_tag : int
val object_tag : int
val infix_tag : int
val forward_tag : int
val no_scan_tag : int
val abstract_tag : int
val string_tag : int
val double_tag : int
val double_array_tag : int
val custom_tag : int
val int_tag : int
val out_of_heap_tag : int
val unaligned_tag : int

external%template repr : ('a[@local_opt]) -> (t[@local_opt]) = "%identity"
[@@mode
  c = (uncontended, shared, contended)
  , o = (many, once)
  , p = (nonportable, portable)
  , u = (aliased, unique)]

external%template magic : 'a 'b. ('a[@local_opt]) -> ('b[@local_opt]) = "%identity"
[@@layout_poly]
[@@mode
  c = (uncontended, shared, contended)
  , o = (many, once)
  , p = (nonportable, portable)
  , u = (aliased, unique)]

external%template magic_portable : 'a. ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
[@@layout_poly]
[@@mode c = (uncontended, shared, contended), o = (many, once), u = (aliased, unique)]

external%template magic_uncontended
  : 'a.
  ('a[@local_opt]) -> ('a[@local_opt])
  = "%identity"
[@@layout_poly]
[@@mode o = (many, once), p = (nonportable, portable), u = (aliased, unique)]

external%template magic_many : 'a. ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
[@@layout_poly]
[@@mode
  c = (uncontended, shared, contended), p = (nonportable, portable), u = (aliased, unique)]

external%template magic_unique : 'a. ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
[@@layout_poly]
[@@mode
  c = (uncontended, shared, contended), o = (many, once), p = (nonportable, portable)]

module Ephemeron : sig
  include module type of struct
    include Stdlib.Ephemeron
  end
end

module Expert : sig
  include module type of struct
    include Stdlib.Obj
  end

  external%template obj : (t[@local_opt]) -> ('a[@local_opt]) = "%identity"
  [@@mode
    c = (uncontended, shared, contended)
    , o = (many, once)
    , p = (nonportable, portable)
    , u = (aliased, unique)]

  val%template field : t -> int -> t [@@mode l = (local, global)]
  val%template set_field : t -> int -> t -> unit [@@mode l = (local, global)]

  external raw_field : (t[@local_opt]) -> int -> raw_data = "caml_obj_raw_field"

  external set_raw_field
    :  (t[@local_opt])
    -> int
    -> raw_data
    -> unit
    = "caml_obj_set_raw_field"
end
