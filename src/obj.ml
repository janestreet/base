open! Import
include Obj_intf.Definitions

external%template magic
  : ('a : any) ('b : any).
  ('a[@local_opt]) @ c o p u -> ('b[@local_opt]) @ c o p u
  @@ portable
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
  @@ portable
  = "%identity"
[@@layout_poly]
[@@mode c = (uncontended, shared, contended), o = (many, once), u = (aliased, unique)]

external%template magic_uncontended
  : ('a : any).
  ('a[@local_opt]) @ contended o p u -> ('a[@local_opt]) @ o p u
  @@ portable
  = "%identity"
[@@layout_poly]
[@@mode o = (many, once), p = (nonportable, portable), u = (aliased, unique)]

external%template magic_many
  : ('a : any).
  ('a[@local_opt]) @ c once p u -> ('a[@local_opt]) @ c p u
  @@ portable
  = "%identity"
[@@layout_poly]
[@@mode
  c = (uncontended, shared, contended), p = (nonportable, portable), u = (aliased, unique)]

external%template magic_unique
  : ('a : any).
  ('a[@local_opt]) @ c o p -> ('a[@local_opt]) @ c o p unique
  @@ portable
  = "%identity"
[@@layout_poly]
[@@mode
  c = (uncontended, shared, contended), o = (many, once), p = (nonportable, portable)]

external%template magic_read_write_uncontended
  : ('a : any).
  ('a[@local_opt]) @ immutable o p u -> ('a[@local_opt]) @ o p u
  @@ portable
  = "%identity"
[@@layout_poly]
[@@mode o = (many, once), p = (nonportable, portable), u = (aliased, unique)]

module%template
  [@inline] [@kind.explicit k = (value, value_or_null)] Make (T : sig
  @@ portable
    type t : k

    val is_immediate : t @ contended local once -> bool [@@zero_alloc]
  end) =
struct
  open T

  external%template repr
    : ('a : k).
    ('a[@local_opt]) @ c o p u -> (t[@local_opt]) @ c o p u
    @@ portable
    = "%obj_magic"
  [@@mode
    c = (uncontended, shared, contended)
    , o = (many, once)
    , p = (nonportable, portable)
    , u = (aliased, unique)]

  external dup : t -> t @@ portable = "%obj_dup"
  external tag : t @ contended local once -> int @@ portable = "caml_obj_tag" [@@noalloc]
  external size : t @ contended local once -> int @@ portable = "%obj_size"

  (** Flambda2 attempts to track whether a block is an array, and %obj_size and %obj_field
      may only be used on unspecialized blocks. Hence we wrap them with [opaque_identity],
      which makes Flambda2 forget any additional information about the block. *)
  let%template[@inline always] size t =
    size ((Sys.opaque_identity [@mode contended once nonportable aliased]) t)
  ;;

  let[@inline] is_immediate t = is_immediate t
  let[@inline] is_block t = not (is_immediate t)

  external is_stack : t @ contended local once -> bool @@ portable = "caml_obj_is_stack"
  [@@noalloc]

  let%template stack_or_heap repr : stack_or_heap =
    (* [is_immediate] and [is_stack] do not actually consume [repr]. *)
    let repr = (magic_many [@mode contended nonportable aliased]) repr in
    if is_immediate repr
    then Immediate
    else (
      match Sys.backend_type with
      | Sys.Native -> if is_stack repr then Stack else Heap
      | Sys.Bytecode -> Heap
      | Sys.Other _ -> Heap)
  ;;

  external reserved_header_bits
    :  t @ contended local once
    -> int
    @@ portable
    = "caml_succ_scannable_prefix_len"
  [@@noalloc]

  let%template uniform_or_mixed repr : uniform_or_mixed = exclave_
    (* [is_immediate] and [reserved_header_bits] do not actually consume [repr]. *)
    let repr = (magic_many [@mode contended nonportable aliased]) repr in
    if is_immediate repr
    then Immediate
    else (
      match Sys.backend_type with
      | Sys.Native ->
        (match reserved_header_bits repr with
         | 0 -> Uniform
         | n -> Mixed { scannable_prefix_len = n - 1 })
      | Sys.Bytecode -> Uniform
      | Sys.Other _ -> Uniform)
  ;;

  external reachable_words : t -> int @@ portable = "caml_obj_reachable_words"

  module Expert = struct
    external%template obj
      : ('a : k).
      (t[@local_opt]) @ c o p u -> ('a[@local_opt]) @ c o p u
      @@ portable
      = "%obj_magic"
    [@@mode
      c = (uncontended, shared, contended)
      , o = (many, once)
      , p = (nonportable, portable)
      , u = (aliased, unique)]

    external field : (t[@local_opt]) -> int -> (t[@local_opt]) @@ portable = "%obj_field"

    (* We have to write both implementations because it doesn't make sense to template
       over an allocator, but the local version needs exclave. If [Sys.opaque_identity]
       preserved the fact that [t] is regional rather than local to this function, we
       could instead write
       {[
         let%template[@mode m = (global, local)] [@inline always] field t i =
           let t = Sys.opaque_identity t in
           field i [@exclave_if_local m]
         ;;
       ]} *)
    let[@inline always] field__local t i = exclave_ field (Sys.opaque_identity t) i
    let[@inline always] field t i = field (Sys.opaque_identity t) i

    external set_field
      :  (t[@local_opt])
      -> int
      -> t
      -> unit
      @@ portable
      = "%obj_set_field"

    let%template[@inline always] set_field t i f = set_field (Sys.opaque_identity t) i f
    [@@mode l = (local, global)]
    ;;

    external raw_field
      :  (t[@local_opt])
      -> int
      -> raw_data
      @@ portable
      = "caml_obj_raw_field"

    external set_raw_field
      :  (t[@local_opt])
      -> int
      -> raw_data
      -> unit
      @@ portable
      = "caml_obj_set_raw_field"
  end
end

type t = Stdlib.Obj.t

external is_int : t @ contended local once -> bool @@ portable = "%obj_is_int"

include%template Make [@kind.explicit value] (struct
    type nonrec t = t

    let[@inline] is_immediate t = is_int t
  end)

external uniquely_reachable_words
  :  t array
  -> int array * int
  @@ portable
  = "caml_obj_uniquely_reachable_words"

let first_non_constant_constructor_tag = Stdlib.Obj.first_non_constant_constructor_tag
let last_non_constant_constructor_tag = Stdlib.Obj.last_non_constant_constructor_tag
let forcing_tag = Stdlib.Obj.forcing_tag
let cont_tag = Stdlib.Obj.cont_tag
let lazy_tag = Stdlib.Obj.lazy_tag
let closure_tag = Stdlib.Obj.closure_tag
let object_tag = Stdlib.Obj.object_tag
let infix_tag = Stdlib.Obj.infix_tag
let forward_tag = Stdlib.Obj.forward_tag
let no_scan_tag = Stdlib.Obj.no_scan_tag
let abstract_tag = Stdlib.Obj.abstract_tag
let string_tag = Stdlib.Obj.string_tag
let double_tag = Stdlib.Obj.double_tag
let double_array_tag = Stdlib.Obj.double_array_tag
let custom_tag = Stdlib.Obj.custom_tag
let int_tag = Stdlib.Obj.int_tag
let out_of_heap_tag = Stdlib.Obj.out_of_heap_tag
let unaligned_tag = Stdlib.Obj.unaligned_tag

module Ephemeron = struct
  include Stdlib.Ephemeron
end

module Expert = struct
  include Stdlib.Obj
  include Expert

  external new_mixed_block
    :  tag:int
    -> total_words:int
    -> scannable_words:int
    -> t
    @@ portable
    = "Base_obj_new_mixed_block"
end

module Nullable = struct
  type t : value_or_null

  external is_int : t @ contended local once -> bool @@ portable = "%obj_is_int"
  external is_null : t @ contended local once -> bool @@ portable = "%is_null"

  include%template Make [@kind.explicit value_or_null] (struct
      type nonrec t = t

      [%%template
        let is_immediate t =
          let t = (magic_many [@mode contended nonportable aliased]) t in
          is_int t || is_null t
        [@@inline]
        ;;]
    end)

  let null_tag = 1010
end
