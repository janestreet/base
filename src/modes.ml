open! Import

open struct
  module Result = Result0
end

type%template 'a t = { modal : 'a }
[@@unboxed]
[@@modality
  g = (local, global)
  , p = (nonportable, portable)
  , c = (uncontended, shared, contended)
  , m = (once, many)
  , a = (unique, aliased)]
[@@kind k = base_or_null]

module Global = struct
  include Modes_intf.Definitions.Global

  type 'a t = { global : 'a } [@@unboxed]

  let compare__local compare a b = compare a.global b.global
  let compare compare a b = compare__local compare a b
  let equal__local equal a b = equal a.global b.global
  let equal equal a b = equal__local equal a b
  let hash_fold_t hash state t = hash state t.global
  let t_of_sexp of_sexp sexp = { global = of_sexp sexp }
  let sexp_of_t sexp_of t = sexp_of t.global
  let sexp_of_t__stack sexp_of t = sexp_of t.global

  let t_sexp_grammar : 'a. 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t =
    Sexplib0.Sexp_grammar.coerce
  ;;

  let globalize _ { global } = { global }
  let wrap global = { global }
  let unwrap { global } = global
  let map { global = x } ~f = { global = f x }

  external wrap_list : 'a list -> 'a t list = "%identity"
  external unwrap_list : ('a t list[@local_opt]) -> ('a list[@local_opt]) = "%identity"
  external wrap_iarray : 'a iarray -> 'a t iarray = "%identity"

  external unwrap_iarray
    :  ('a t iarray[@local_opt])
    -> ('a iarray[@local_opt])
    = "%identity"

  external wrap_or_null : 'a or_null -> 'a t or_null = "%identity"
  external unwrap_or_null : ('a t or_null[@local_opt]) -> 'a or_null = "%identity"
  external wrap_option : 'a option -> 'a t option = "%identity"

  external unwrap_option
    :  ('a t option[@local_opt])
    -> ('a option[@local_opt])
    = "%identity"

  external wrap_either : ('a, 'b) Either0.t -> ('a t, 'b t) Either0.t = "%identity"

  external unwrap_either
    :  (('a t, 'b t) Either0.t[@local_opt])
    -> (('a, 'b) Either0.t[@local_opt])
    = "%identity"

  external wrap_first : ('a, 'b) Either0.t -> ('a t, 'b) Either0.t = "%identity"

  external unwrap_first
    :  (('a t, 'b) Either0.t[@local_opt])
    -> (('a, 'b) Either0.t[@local_opt])
    = "%identity"

  external wrap_second : ('a, 'b) Either0.t -> ('a, 'b t) Either0.t = "%identity"

  external unwrap_second
    :  (('a, 'b t) Either0.t[@local_opt])
    -> (('a, 'b) Either0.t[@local_opt])
    = "%identity"

  external wrap_result : ('a, 'b) Result.t -> ('a t, 'b t) Result.t = "%identity"

  external unwrap_result
    :  (('a t, 'b t) Result.t[@local_opt])
    -> (('a, 'b) Result.t[@local_opt])
    = "%identity"

  external wrap_ok : ('a, 'b) Result.t -> ('a t, 'b) Result.t = "%identity"

  external unwrap_ok
    :  (('a t, 'b) Result.t[@local_opt])
    -> (('a, 'b) Result.t[@local_opt])
    = "%identity"

  external wrap_error : ('a, 'b) Result.t -> ('a, 'b t) Result.t = "%identity"

  external unwrap_error
    :  (('a, 'b t) Result.t[@local_opt])
    -> (('a, 'b) Result.t[@local_opt])
    = "%identity"

  external wrap_tuple2 : 'a * 'b -> 'a t * 'b t = "%identity"
  external wrap_fst : 'a * 'b -> 'a t * 'b = "%identity"
  external wrap_snd : 'a * 'b -> 'a * 'b t = "%identity"

  external unwrap_tuple2
    :  ('a t * 'b t[@local_opt])
    -> ('a * 'b[@local_opt])
    = "%identity"

  external unwrap_fst : ('a t * 'b[@local_opt]) -> ('a * 'b[@local_opt]) = "%identity"
  external unwrap_snd : ('a * 'b t[@local_opt]) -> ('a * 'b[@local_opt]) = "%identity"

  module Global_wrapper = struct
    type nonrec 'a t = 'a t

    let wrap = wrap
    let unwrap = unwrap
  end

  module Local_wrapper = struct
    type 'a t = 'a

    let wrap = Fn.id
    let unwrap = Fn.id
  end

  module Poly_fn1 (Input : T) (Output : T) (F : Wrapped_fn1(Input)(Output).S) :
    Poly_fn1 with type input := Input.t and type output := Output.t = struct
    module Global = F (Global_wrapper)
    module Local = F (Local_wrapper)

    let fn_global a = unwrap (Global.fn (wrap a)) [@nontail]
    let fn_local = Local.fn
  end

  module Poly_fn2
      (Input1 : T)
      (Input2 : T)
      (Output : T)
      (F : Wrapped_fn2(Input1)(Input2)(Output).S) :
    Poly_fn2
    with type input1 := Input1.t
     and type input2 := Input2.t
     and type output := Output.t = struct
    module Global = F (Global_wrapper)
    module Local = F (Local_wrapper)

    let fn_global a b = unwrap (Global.fn (wrap a) (wrap b)) [@nontail]
    let fn_local = Local.fn
  end

  module Poly_fn3
      (Input1 : T)
      (Input2 : T)
      (Input3 : T)
      (Output : T)
      (F : Wrapped_fn3(Input1)(Input2)(Input3)(Output).S) :
    Poly_fn3
    with type input1 := Input1.t
     and type input2 := Input2.t
     and type input3 := Input3.t
     and type output := Output.t = struct
    module Global = F (Global_wrapper)
    module Local = F (Local_wrapper)

    let fn_global a b c = unwrap (Global.fn (wrap a) (wrap b) (wrap c)) [@nontail]
    let fn_local = Local.fn
  end
end

module Portable = struct
  include Modes_intf.Definitions.Portable

  type 'a t = { portable : 'a }
  [@@unboxed] [@@deriving compare ~localize, equal ~localize, hash]

  let%template sexp_of_t sexp_of_a { portable } = sexp_of_a portable [@exclave_if_stack a]
  [@@alloc a = (heap, stack)]
  ;;

  let%template[@alloc stack] sexp_of_t = (sexp_of_t [@alloc stack])
  let t_of_sexp a_of_sexp sexp = { portable = a_of_sexp sexp }

  let t_sexp_grammar : type a. a Sexplib0.Sexp_grammar.t -> a t Sexplib0.Sexp_grammar.t =
    Sexplib0.Sexp_grammar.coerce
  ;;

  external cross : 'a. 'a -> 'a = "%identity"
  external wrap : ('a[@local_opt]) -> ('a t[@local_opt]) = "%identity"
  external unwrap : ('a t[@local_opt]) -> ('a[@local_opt]) = "%identity"

  let map { portable } ~f = { portable = f portable }

  external wrap_list : ('a list[@local_opt]) -> ('a t list[@local_opt]) = "%identity"
  external unwrap_list : ('a t list[@local_opt]) -> ('a list[@local_opt]) = "%identity"

  external wrap_iarray
    :  ('a iarray[@local_opt])
    -> ('a t iarray[@local_opt])
    = "%identity"

  external unwrap_iarray
    :  ('a t iarray[@local_opt])
    -> ('a iarray[@local_opt])
    = "%identity"

  external wrap_or_null
    :  ('a or_null[@local_opt])
    -> ('a t or_null[@local_opt])
    = "%identity"

  external unwrap_or_null
    :  ('a t or_null[@local_opt])
    -> ('a or_null[@local_opt])
    = "%identity"

  external wrap_option
    :  ('a option[@local_opt])
    -> ('a t option[@local_opt])
    = "%identity"

  external unwrap_option
    :  ('a t option[@local_opt])
    -> ('a option[@local_opt])
    = "%identity"

  external wrap_either
    :  (('a, 'b) Either0.t[@local_opt])
    -> (('a t, 'b t) Either0.t[@local_opt])
    = "%identity"

  external unwrap_either
    :  (('a t, 'b t) Either0.t[@local_opt])
    -> (('a, 'b) Either0.t[@local_opt])
    = "%identity"

  external wrap_first
    :  (('a, 'b) Either0.t[@local_opt])
    -> (('a t, 'b) Either0.t[@local_opt])
    = "%identity"

  external unwrap_first
    :  (('a t, 'b) Either0.t[@local_opt])
    -> (('a, 'b) Either0.t[@local_opt])
    = "%identity"

  external unwrap_first__portable
    :  (('a t, 'b) Either0.t[@local_opt])
    -> (('a, 'b) Either0.t[@local_opt])
    = "%identity"

  external wrap_second
    :  (('a, 'b) Either0.t[@local_opt])
    -> (('a, 'b t) Either0.t[@local_opt])
    = "%identity"

  external unwrap_second
    :  (('a, 'b t) Either0.t[@local_opt])
    -> (('a, 'b) Either0.t[@local_opt])
    = "%identity"

  external unwrap_second__portable
    :  (('a, 'b t) Either0.t[@local_opt])
    -> (('a, 'b) Either0.t[@local_opt])
    = "%identity"

  external wrap_result
    :  (('a, 'b) Result.t[@local_opt])
    -> (('a t, 'b t) Result.t[@local_opt])
    = "%identity"

  external unwrap_result
    :  (('a t, 'b t) Result.t[@local_opt])
    -> (('a, 'b) Result.t[@local_opt])
    = "%identity"

  external wrap_result_list
    :  (('a, 'b) Result.t list[@local_opt])
    -> (('a t, 'b t) Result.t list[@local_opt])
    = "%identity"

  external unwrap_result_list
    :  (('a t, 'b t) Result.t list[@local_opt])
    -> (('a, 'b) Result.t list[@local_opt])
    = "%identity"

  external wrap_ok
    :  (('a, 'b) Result.t[@local_opt])
    -> (('a t, 'b) Result.t[@local_opt])
    = "%identity"

  external unwrap_ok
    :  (('a t, 'b) Result.t[@local_opt])
    -> (('a, 'b) Result.t[@local_opt])
    = "%identity"

  external unwrap_ok__portable
    :  (('a t, 'b) Result.t[@local_opt])
    -> (('a, 'b) Result.t[@local_opt])
    = "%identity"

  external wrap_error
    :  (('a, 'b) Result.t[@local_opt])
    -> (('a, 'b t) Result.t[@local_opt])
    = "%identity"

  external unwrap_error
    :  (('a, 'b t) Result.t[@local_opt])
    -> (('a, 'b) Result.t[@local_opt])
    = "%identity"

  external unwrap_error__portable
    :  (('a, 'b t) Result.t[@local_opt])
    -> (('a, 'b) Result.t[@local_opt])
    = "%identity"

  external wrap_tuple2 : ('a * 'b[@local_opt]) -> ('a t * 'b t[@local_opt]) = "%identity"
  external wrap_fst : ('a * 'b[@local_opt]) -> ('a t * 'b[@local_opt]) = "%identity"
  external wrap_snd : ('a * 'b[@local_opt]) -> ('a * 'b t[@local_opt]) = "%identity"

  external unwrap_tuple2
    :  ('a t * 'b t[@local_opt])
    -> ('a * 'b[@local_opt])
    = "%identity"

  external%template unwrap_fst
    :  ('a t * 'b[@local_opt])
    -> ('a * 'b[@local_opt])
    = "%identity"
  [@@mode p = (portable, nonportable)]

  external%template unwrap_snd
    :  ('a * 'b t[@local_opt])
    -> ('a * 'b[@local_opt])
    = "%identity"
  [@@mode p = (portable, nonportable)]
end

module Contended = struct
  type 'a t = { contended : 'a } [@@unboxed]

  external cross : 'a. 'a -> 'a = "%identity"
end

module Shared = struct
  type 'a t = { shared : 'a } [@@unboxed]
end

module Portended = struct
  type 'a t = { portended : 'a } [@@unboxed]
end

module Many = struct
  type 'a t = { many : 'a } [@@unboxed]
end

module Aliased = struct
  type 'a t = { aliased : 'a } [@@unboxed]
end

module Forkable = struct
  type 'a t = { forkable : 'a } [@@unboxed]
end

module Unyielding = struct
  type 'a t = { unyielding : 'a } [@@unboxed]
end

module Immutable_data = struct
  type 'a t = { immutable_data : 'a } [@@unboxed]
end

module At_locality = struct
  include Modes_intf.Definitions.At_locality

  (* The safety of this module is tested in `test/test_modes.ml`, which reimplements its
     interface without using [external] definitions. *)

  (* This type must not cross locality. *)
  type actually_local

  type global = [ `global ]
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]
  [@@immediate]

  type local =
    [ `global
    | `local of (actually_local[@compare.ignore] [@equal.ignore] [@sexp.opaque])
    ]
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]

  type (+'a, +'locality) t

  external wrap : 'a 'loc. 'a -> ('a, 'loc) t = "%identity"
  external unwrap : 'a 'loc. ('a, 'loc) t -> 'a = "%identity"
  external wrap_local : 'a. 'a -> ('a, local) t = "%identity"
  external unwrap_local : 'a 'loc. ('a, 'loc) t -> 'a = "%identity"
  external unwrap_global : 'a. ('a, global) t -> 'a = "%identity"

  let to_local t = wrap_local (unwrap_local t)
  let to_global t = wrap (unwrap t)
  let globalize globalize_a _ t = wrap (globalize_a (unwrap_local t))
  let globalize_global t = wrap (unwrap_global t)
  let equal equal_a _ x y = equal_a (unwrap x) (unwrap y)
  let compare compare_a _ x y = compare_a (unwrap x) (unwrap y)
  let equal__local equal_a _ x y = equal_a (unwrap_local x) (unwrap_local y) [@nontail]

  let compare__local compare_a _ x y =
    compare_a (unwrap_local x) (unwrap_local y) [@nontail]
  ;;

  let hash_fold_t hash_fold_a _ state t = hash_fold_a state (unwrap t)
  let sexp_of_t sexp_of_a _ t = sexp_of_a (unwrap t)

  let t_sexp_grammar
    : type a. a Sexplib0.Sexp_grammar.t -> _ -> (a, _) t Sexplib0.Sexp_grammar.t
    =
    fun grammar _ -> Sexplib0.Sexp_grammar.coerce grammar
  ;;
end

module At_portability = struct
  type nonportable_
  [@@allow_redundant_modalities
    "While [global] implies [forkable unyielding], we include them for clarity"]
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]

  (* We only need [hash_fold] and local comparisons. *)
  let _ = [%compare: nonportable_]
  let _ = [%equal: nonportable_]
  let _ = [%hash: nonportable_]

  type portable = [ `portable ]
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]
  [@@immediate]

  type nonportable =
    [ `portable
    | `nonportable of nonportable_
    ]
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]

  type (+!'a, +'portability) t
  [@@allow_redundant_modalities
    "While [global] implies [forkable unyielding], we include them for clarity"]

  external wrap_portable : ('a[@local_opt]) -> (('a, _) t[@local_opt]) = "%identity"

  external unwrap_portable
    :  (('a, portable) t[@local_opt])
    -> ('a[@local_opt])
    = "%identity"

  external wrap_nonportable
    :  ('a[@local_opt])
    -> (('a, nonportable) t[@local_opt])
    = "%identity"

  external unwrap_nonportable : (('a, _) t[@local_opt]) -> ('a[@local_opt]) = "%identity"

  [%%template
  type portability = nonportable
  [@@mode nonportable]
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]

  type portability = portable
  [@@mode portable]
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]

  external wrap
    :  ('a[@local_opt])
    -> (('a, (portability[@mode p])) t[@local_opt])
    = "%identity"
  [@@mode p = (portable, nonportable)]

  external unwrap
    :  (('a, (portability[@mode p])) t[@local_opt])
    -> ('a[@local_opt])
    = "%identity"
  [@@mode p = portable]

  external unwrap : (('a, _) t[@local_opt]) -> ('a[@local_opt]) = "%identity"
  [@@mode __ = nonportable]]

  let%template equal equal_a _ x y =
    equal_a (unwrap_nonportable x) (unwrap_nonportable y) [@nontail]
  [@@mode __ = (local, global)]
  ;;

  let%template compare compare_a _ x y =
    compare_a (unwrap_nonportable x) (unwrap_nonportable y) [@nontail]
  [@@mode __ = (local, global)]
  ;;

  let hash_fold_t hash_fold_a _ state t = hash_fold_a state (unwrap_nonportable t)
  let sexp_of_t sexp_of_a _ t = sexp_of_a (unwrap_nonportable t)

  let t_sexp_grammar
    : type a. a Sexplib0.Sexp_grammar.t -> _ -> (a, _) t Sexplib0.Sexp_grammar.t
    =
    fun grammar _ -> Sexplib0.Sexp_grammar.coerce grammar
  ;;
end

module Portable_via_contended = struct
  type +'a t

  external wrap : ('a[@local_opt]) -> ('a t[@local_opt]) = "%identity"
  external unwrap : ('a t[@local_opt]) -> ('a[@local_opt]) = "%identity"
  external unwrap_contended : 'a. ('a t[@local_opt]) -> ('a[@local_opt]) = "%identity"
end

module Contended_via_portable = struct
  type +'a t

  external wrap : ('a[@local_opt]) -> ('a t[@local_opt]) = "%identity"
  external unwrap : ('a t[@local_opt]) -> ('a[@local_opt]) = "%identity"

  let sexp_of_t sexp_of_a t = sexp_of_a (unwrap t)
  let refute_portable _ = assert false
end

module Mod = struct
  type%template 'a t = Mod : 'a. ('a t[@modality g p c m a])
  [@@modality
    g = (local, global)
    , p = (nonportable, portable)
    , c = (uncontended, shared, contended)
    , m = (once, many)
    , a = (unique, aliased)]

  module Global = struct
    type%template 'a t = ('a t[@modality global]) = Mod : 'a. 'a t
  end

  module Portable = struct
    type%template 'a t = ('a t[@modality portable]) = Mod : 'a. 'a t
  end

  module Contended = struct
    type%template 'a t = ('a t[@modality contended]) = Mod : 'a. 'a t
  end

  module Shared = struct
    type%template 'a t = ('a t[@modality shared]) = Mod : 'a. 'a t
  end

  module Portended = struct
    type%template 'a t = ('a t[@modality portable contended]) = Mod : 'a. 'a t
  end

  module Many = struct
    type%template 'a t = ('a t[@modality many]) = Mod : 'a. 'a t
  end

  module Aliased = struct
    type%template 'a t = ('a t[@modality aliased]) = Mod : 'a. 'a t
  end
end

module Export = struct
  type 'a global = 'a Global.t = { global : 'a } [@@unboxed]
  type 'a portable = 'a Portable.t = { portable : 'a } [@@unboxed]
  type 'a contended = 'a Contended.t = { contended : 'a } [@@unboxed]
  type 'a shared = 'a Shared.t = { shared : 'a } [@@unboxed]
  type 'a portended = 'a Portended.t = { portended : 'a } [@@unboxed]
  type 'a many = 'a Many.t = { many : 'a } [@@unboxed]
  type 'a aliased = 'a Aliased.t = { aliased : 'a } [@@unboxed]
  type 'a forkable = 'a Forkable.t = { forkable : 'a } [@@unboxed]
  type 'a unyielding = 'a Unyielding.t = { unyielding : 'a } [@@unboxed]
  type 'a immutable_data = 'a Immutable_data.t = { immutable_data : 'a } [@@unboxed]
end
