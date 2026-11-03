open! Import

module Definitions = struct
  type raw_data = Stdlib.Obj.raw_data

  type stack_or_heap =
    | Immediate
    | Stack
    | Heap
  [@@deriving sexp, compare]

  type uniform_or_mixed =
    | Immediate
    | Uniform
    | Mixed of { scannable_prefix_len : int }
  [@@deriving sexp, compare, globalize]
end

module type S = sig
  open Definitions

  type t

  external repr : 'a. ('a[@local_opt]) -> (t[@local_opt]) = "%identity"

  (** [dup t] returns a shallow copy of [t]. However if [t] is immutable then it might be
      returned unchanged. *)
  external dup : t -> t = "caml_obj_dup"

  (** Returns the tag of the block. *)
  external tag : t -> int = "caml_obj_tag" [@@noalloc]

  (** Computes the total size (in words) of the value. *)
  val size : t -> int
  [@@zero_alloc strict]

  (** Tests if the argument is an immediate value (int or variant constructor). *)
  external is_int : t -> bool = "%obj_is_int"

  (** Tests if the argument is a heap-allocated block. *)
  val is_block : t -> bool
  [@@zero_alloc strict]

  (** Checks whether a value is stack-allocated. *)
  external is_stack : t -> bool = "caml_dummy_obj_is_stack"
  [@@noalloc]

  (** Checks if a value is immediate, stack-allocated, or heap-allocated. *)
  val stack_or_heap : t -> stack_or_heap
  [@@zero_alloc strict]

  (** Checks if a value is immediate, a uniform block, or a mixed block. *)
  val uniform_or_mixed : t -> uniform_or_mixed
  [@@zero_alloc strict]

  (** Computes the total size (in words, including the headers) of all heap blocks
      accessible from the argument. Statically allocated blocks are included. *)
  external reachable_words : t -> int = "caml_obj_reachable_words"

  module Expert : sig
    external obj : 'a. (t[@local_opt]) -> ('a[@local_opt]) = "%identity"

    (*_ [field] has no [@@zero_alloc] annotation because reading a field of a flat float
        array (a block with [double_array_tag]) allocates a boxed float. *)
    val field : t -> int -> t
    val set_field : t -> int -> t -> unit [@@zero_alloc strict]
    external raw_field : (t[@local_opt]) -> int -> raw_data = "caml_obj_raw_field"

    external set_raw_field
      :  (t[@local_opt])
      -> int
      -> raw_data
      -> unit
      = "caml_obj_set_raw_field"
  end
end

module type Obj = sig
  (** Versions of [Stdlib.Obj] functions that work with modes. Note it is never safe to
      magic a local value to global. *)

  include module type of struct
    include Definitions
  end

  type t = Stdlib.Obj.t

  include S with type t := t (** @inline *)

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

  external magic : 'a 'b. ('a[@local_opt]) -> ('b[@local_opt]) = "%identity"
  [@@layout_poly]

  external magic_portable : 'a. ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
  [@@layout_poly]

  external magic_uncontended : 'a. ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
  [@@layout_poly]

  external magic_many : 'a. ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
  [@@layout_poly]

  external magic_unyielding : 'a. 'a -> 'a = "%identity" [@@layout_poly]

  external magic_unique : 'a. ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
  [@@layout_poly]

  external magic_read_write_uncontended
    : 'a.
    ('a[@local_opt]) -> ('a[@local_opt])
    = "%identity"
  [@@layout_poly]

  module Ephemeron : sig
    include module type of struct
      include Stdlib.Ephemeron
    end
  end

  module Expert : sig
    include module type of struct
      include Stdlib.Obj
    end

    include module type of Expert

    external new_mixed_block
      :  tag:int
      -> total_words:int
      -> scannable_words:int
      -> t
      = "Base_obj_new_mixed_block"
  end

  module Nullable : sig
    (** Versions of [Obj] functions that work on [maybe_null] values.

        Defines a distinct type to avoid optimizations assuming its values are [non_null]. *)

    type t

    include S with type t := t (** @inline *)

    (** Tests if the argument is an immediate value (int, variant constructor, or null). *)
    val is_immediate : t -> bool
    [@@zero_alloc strict]

    external of_or_null
      :  (Stdlib.Obj.t or_null[@local_opt])
      -> (t[@local_opt])
      = "%identity"

    external to_or_null
      :  (t[@local_opt])
      -> (Stdlib.Obj.t or_null[@local_opt])
      = "%identity"
  end

  external nullable : (t[@local_opt]) -> (Nullable.t[@local_opt]) = "%identity"
end
