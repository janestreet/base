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

  type t : k

  external%template repr
    : ('a : k).
    ('a[@local_opt]) @ c o p u -> (t[@local_opt]) @ c o p u
    = "%obj_magic"
  [@@mode
    c = (uncontended, shared, contended)
    , o = (many, once)
    , p = (nonportable, portable)
    , u = (aliased, unique)]

  (** [dup t] returns a shallow copy of [t]. However if [t] is immutable then it might be
      returned unchanged. *)
  external dup : t -> t = "%obj_dup"

  (** Returns the tag of the block. *)
  external tag : t @ contended local once -> int = "caml_obj_tag"
  [@@noalloc]

  (** Computes the total size (in words) of the value. *)
  val size : t @ contended local once -> int
  [@@zero_alloc]

  (** Tests if the argument is an immediate value (int or variant constructor). *)
  external is_int : t @ contended local once -> bool = "%obj_is_int"

  (** Tests if the argument is a heap-allocated block. *)
  val is_block : t @ contended local once -> bool
  [@@zero_alloc]

  (** Checks whether a value is stack-allocated. *)
  external is_stack : t @ contended local once -> bool = "caml_obj_is_stack"
  [@@noalloc]

  (** Checks if a value is immediate, stack-allocated, or heap-allocated. *)
  val stack_or_heap : t @ contended local once -> stack_or_heap
  [@@zero_alloc]

  (** Checks if a value is immediate, a uniform block, or a mixed block. *)
  val uniform_or_mixed : t @ contended local once -> uniform_or_mixed @ local
  [@@zero_alloc]

  (** Computes the total size (in words, including the headers) of all heap blocks
      accessible from the argument. Statically allocated blocks are included. *)
  external reachable_words : t -> int @@ portable = "caml_obj_reachable_words"

  module Expert : sig
    external%template obj
      : ('a : k).
      (t[@local_opt]) @ c o p u -> ('a[@local_opt]) @ c o p u
      = "%obj_magic"
    [@@mode
      c = (uncontended, shared, contended)
      , o = (many, once)
      , p = (nonportable, portable)
      , u = (aliased, unique)]

    [%%template:
    [@@@mode.default l = (local, global)]

    val field : t @ l -> int -> t @ l
    val set_field : t @ l -> int -> t -> unit]

    external raw_field : (t[@local_opt]) -> int -> raw_data = "caml_obj_raw_field"

    external set_raw_field
      :  (t[@local_opt])
      -> int
      -> raw_data
      -> unit
      = "caml_obj_set_raw_field"
  end
end

module type Obj = sig @@ portable
  (** Versions of [Stdlib.Obj] functions that work with modes. Note it is never safe to
      magic a local value to global. *)

  include module type of struct
    include Definitions
  end

  type t = Stdlib.Obj.t

  include%template S [@kind.explicit value] with type t := t (** @inline *)

  val uniquely_reachable_words : t array -> int array * int
  [@@ocaml.doc
    {| For each element of the array, computes the total size (as defined above by
        [reachable_words]) of all heap blocks accessible from the argument but excluding
        all blocks accessible from any other arguments.

        Also returns a single number denoting the total memory reachable from at least two
        of the roots. We make no attempt to classify which two (or more) roots are
        responsible for this memory. |}]

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

  external%template magic
    : ('a : any) ('b : any).
    ('a[@local_opt]) @ c o p u -> ('b[@local_opt]) @ c o p u
    = "%obj_magic"
  [@@layout_poly]
  [@@mode
    c = (uncontended, shared, contended)
    , o = (many, once)
    , p = (nonportable, portable)
    , u = (aliased, unique)]

  external%template magic_portable
    : ('a : any).
    ('a[@local_opt]) @ c o u -> ('a[@local_opt]) @ c o portable u
    = "%identity"
  [@@layout_poly]
  [@@mode c = (uncontended, shared, contended), o = (many, once), u = (aliased, unique)]

  external%template magic_uncontended
    : ('a : any).
    ('a[@local_opt]) @ contended o p u -> ('a[@local_opt]) @ o p u
    = "%identity"
  [@@layout_poly]
  [@@mode o = (many, once), p = (nonportable, portable), u = (aliased, unique)]

  external%template magic_many
    : ('a : any).
    ('a[@local_opt]) @ c once p u -> ('a[@local_opt]) @ c p u
    = "%identity"
  [@@layout_poly]
  [@@mode
    c = (uncontended, shared, contended)
    , p = (nonportable, portable)
    , u = (aliased, unique)]

  external%template magic_unique
    : ('a : any).
    ('a[@local_opt]) @ c o p -> ('a[@local_opt]) @ c o p unique
    = "%identity"
  [@@layout_poly]
  [@@mode
    c = (uncontended, shared, contended), o = (many, once), p = (nonportable, portable)]

  external%template magic_read_write_uncontended
    : ('a : any).
    ('a[@local_opt]) @ immutable o p u -> ('a[@local_opt]) @ o p u
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

    type t : value_or_null

    include%template S [@kind.explicit value_or_null] with type t := t (** @inline *)

    (** Tests if the argument is an immediate value (int, variant constructor, or null). *)
    val is_immediate : t @ contended local once -> bool
    [@@zero_alloc]

    external is_null : t @ contended local once -> bool = "%is_null"
    val null_tag : int
  end
end
