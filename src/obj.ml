open! Import
include Obj_intf.Definitions

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

module%template
  [@inline] [@kind.explicit k = (value, value_or_null)] Make (T : sig
    type t

    val is_immediate : t -> bool [@@zero_alloc]
  end) =
struct
  open T

  external%template repr : 'a. ('a[@local_opt]) -> (t[@local_opt]) = "%identity"
  [@@mode
    c = (uncontended, shared, contended)
    , o = (many, once)
    , p = (nonportable, portable)
    , u = (aliased, unique)]

  external dup : t -> t = "caml_obj_dup"
  external tag : t -> int = "caml_obj_tag" [@@noalloc]
  external size : t -> int = "%obj_size"

  (** Flambda2 attempts to track whether a block is an array, and %obj_size and %obj_field
      may only be used on unspecialized blocks. Hence we wrap them with [opaque_identity],
      which makes Flambda2 forget any additional information about the block. *)
  let%template[@inline always] size t =
    size ((Sys.opaque_identity [@mode contended once nonportable aliased]) t)
  ;;

  let[@inline] is_immediate t = is_immediate t
  let[@inline] is_block t = not (is_immediate t)

  external is_stack : t -> bool = "caml_dummy_obj_is_stack" [@@noalloc]

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

  external reserved_header_bits : t -> int = "caml_dummy_succ_scannable_prefix_len"
  [@@noalloc]

  let%template uniform_or_mixed repr : uniform_or_mixed =
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

  external reachable_words : t -> int = "caml_obj_reachable_words"

  module Expert = struct
    external%template obj : 'a. (t[@local_opt]) -> ('a[@local_opt]) = "%identity"
    [@@mode
      c = (uncontended, shared, contended)
      , o = (many, once)
      , p = (nonportable, portable)
      , u = (aliased, unique)]

    external field : (t[@local_opt]) -> int -> (t[@local_opt]) = "%obj_field"

    (* We have to write both implementations because it doesn't make sense to template
       over an allocator, but the local version needs exclave. If [Sys.opaque_identity]
       preserved the fact that [t] is regional rather than local to this function, we
       could instead write {[
         let%template[@mode m = (global, local)] [@inline always] field t i =
           let t = Sys.opaque_identity t in
           field i [@exclave_if_local m]
         ;;
       ]} *)
    let[@inline always] field__local t i = field (Sys.opaque_identity t) i
    let[@inline always] field t i = field (Sys.opaque_identity t) i

    external set_field : (t[@local_opt]) -> int -> t -> unit = "%obj_set_field"

    let%template[@inline always] set_field t i f = set_field (Sys.opaque_identity t) i f
    [@@mode l = (local, global)]
    ;;

    external raw_field : (t[@local_opt]) -> int -> raw_data = "caml_obj_raw_field"

    external set_raw_field
      :  (t[@local_opt])
      -> int
      -> raw_data
      -> unit
      = "caml_obj_set_raw_field"
  end
end

type t = Stdlib.Obj.t

external is_int : t -> bool = "%obj_is_int"

include%template Make [@kind.explicit value] (struct
    type nonrec t = t

    let[@inline] is_immediate t = is_int t
  end)

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
    = "Base_obj_new_mixed_block"
end

module Nullable = struct
  type t

  external is_int : t -> bool = "%obj_is_int"

  include%template Make [@kind.explicit value_or_null] (struct
      type nonrec t = t

      let is_immediate t = is_int t [@@inline]
    end)
end
