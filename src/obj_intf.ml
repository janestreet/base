open! Import

module Definitions = struct
  type raw_data = Stdlib.Obj.raw_data

  type stack_or_heap =
    | Immediate
    | Stack
    | Heap
  [@@deriving sexp ~stackify, compare]

  type uniform_or_mixed =
    | Immediate
    | Uniform
    | Mixed of { scannable_prefix_len : int }
  [@@deriving sexp ~stackify, compare ~localize, globalize]
end

module type%template [@kind.explicit k = (value, value_or_null)] S = sig
  open Definitions

  type t

  external%template repr : 'a. ('a[@local_opt]) -> (t[@local_opt]) = "%identity"
  [@@mode
    c = (uncontended, shared, contended)
    , o = (many, once)
    , p = (nonportable, portable)
    , u = (aliased, unique)]

  (** [dup t] returns a shallow copy of [t]. However if [t] is immutable then it might be
      returned unchanged. *)
  external dup : t -> t = "caml_obj_dup"

  (** Returns the tag of the block. *)
  external tag : t -> int = "caml_obj_tag" [@@noalloc]

  (** Computes the total size (in words) of the value. *)
  val size : t -> int
  [@@zero_alloc]

  (** Tests if the argument is an immediate value (int or variant constructor). *)
  external is_int : t -> bool = "%obj_is_int"

  (** Tests if the argument is a heap-allocated block. *)
  val is_block : t -> bool
  [@@zero_alloc]

  (** Checks whether a value is stack-allocated. *)
  external is_stack : t -> bool = "caml_dummy_obj_is_stack"
  [@@noalloc]

  (** Checks if a value is immediate, stack-allocated, or heap-allocated. *)
  val stack_or_heap : t -> stack_or_heap
  [@@zero_alloc]

  (** Checks if a value is immediate, a uniform block, or a mixed block. *)
  val uniform_or_mixed : t -> uniform_or_mixed
  [@@zero_alloc]

  (** Computes the total size (in words, including the headers) of all heap blocks
      accessible from the argument. Statically allocated blocks are included. *)
  external reachable_words : t -> int = "caml_obj_reachable_words"

  module Expert : sig
    external%template obj : 'a. (t[@local_opt]) -> ('a[@local_opt]) = "%identity"
    [@@mode
      c = (uncontended, shared, contended)
      , o = (many, once)
      , p = (nonportable, portable)
      , u = (aliased, unique)]

    [%%template:
    [@@@mode.default l = (local, global)]

    val field : t -> int -> t
    val set_field : t -> int -> t -> unit]

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

  include%template S [@kind.explicit value] with type t := t (** @inline *)

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

  external%template magic : 'a 'b. ('a[@local_opt]) -> ('b[@local_opt]) = "%identity"
  [@@layout_poly]
  [@@mode
    c = (uncontended, shared, contended)
    , o = (many, once)
    , p = (nonportable, portable)
    , u = (aliased, unique)]

  external%template magic_portable
    : 'a.
    ('a[@local_opt]) -> ('a[@local_opt])
    = "%identity"
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
    c = (uncontended, shared, contended)
    , p = (nonportable, portable)
    , u = (aliased, unique)]

  external%template magic_unique : 'a. ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
  [@@layout_poly]
  [@@mode
    c = (uncontended, shared, contended), o = (many, once), p = (nonportable, portable)]

  external%template magic_read_write_uncontended
    : 'a.
    ('a[@local_opt]) -> ('a[@local_opt])
    = "%identity"
  [@@layout_poly]
  [@@mode o = (many, once), p = (nonportable, portable), u = (aliased, unique)]

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

    include%template S [@kind.explicit value_or_null] with type t := t (** @inline *)

    (** Tests if the argument is an immediate value (int, variant constructor, or null). *)
    val is_immediate : t -> bool
    [@@zero_alloc]
  end
end
