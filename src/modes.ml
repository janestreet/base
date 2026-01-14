open! Import

open struct
  module Result = Result0
end

type%template ('a : k) t = { modal : 'a @@ a c m p }
[@@unboxed]
[@@modality
  a = aliased
  , p = (nonportable, portable)
  , c = (uncontended, shared, contended)
  , m = (once, many)]
[@@kind k = base_or_null]

type%template ('a : k) t = { modal : 'a @@ c g m p }
[@@unboxed]
[@@modality
  g = (local, global)
  , p = (nonportable, portable)
  , c = (uncontended, shared, contended)
  , m = (once, many)]
[@@kind k = base_or_null]

module Global = struct
  include Modes_intf.Definitions.Global

  type ('a : value_or_null) t : value_or_null mod global = { global : 'a @@ global }
  [@@unboxed]

  let compare__local compare a b = compare a.global b.global
  let compare compare a b = compare__local compare a b
  let equal__local equal a b = equal a.global b.global
  let equal equal a b = equal__local equal a b
  let hash_fold_t hash state t = hash state t.global
  let t_of_sexp of_sexp sexp = { global = of_sexp sexp }
  let sexp_of_t sexp_of t = sexp_of t.global
  let sexp_of_t__stack sexp_of t = exclave_ sexp_of t.global

  let t_sexp_grammar : 'a. 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t =
    Sexplib0.Sexp_grammar.coerce
  ;;

  let globalize _ { global } = { global }
  let wrap global = { global }
  let unwrap { global } = global
  let map { global = x } ~f = { global = f x }

  external wrap_list : 'a list -> 'a t list @@ portable = "%identity"

  external unwrap_list
    :  ('a t list[@local_opt])
    -> ('a list[@local_opt])
    @@ portable
    = "%identity"

  external wrap_iarray : 'a iarray -> 'a t iarray @@ portable = "%identity"

  external unwrap_iarray
    :  ('a t iarray[@local_opt])
    -> ('a iarray[@local_opt])
    @@ portable
    = "%identity"

  external wrap_or_null : 'a or_null -> 'a t or_null @@ portable = "%identity"

  external unwrap_or_null
    :  ('a t or_null[@local_opt])
    -> 'a or_null
    @@ portable
    = "%identity"

  external wrap_option : 'a option -> 'a t option @@ portable = "%identity"

  external unwrap_option
    :  ('a t option[@local_opt])
    -> ('a option[@local_opt])
    @@ portable
    = "%identity"

  external wrap_either
    :  ('a, 'b) Either0.t
    -> ('a t, 'b t) Either0.t
    @@ portable
    = "%identity"

  external unwrap_either
    :  (('a t, 'b t) Either0.t[@local_opt])
    -> (('a, 'b) Either0.t[@local_opt])
    @@ portable
    = "%identity"

  external wrap_first
    :  ('a, 'b) Either0.t
    -> ('a t, 'b) Either0.t
    @@ portable
    = "%identity"

  external unwrap_first
    :  (('a t, 'b) Either0.t[@local_opt])
    -> (('a, 'b) Either0.t[@local_opt])
    @@ portable
    = "%identity"

  external wrap_second
    :  ('a, 'b) Either0.t
    -> ('a, 'b t) Either0.t
    @@ portable
    = "%identity"

  external unwrap_second
    :  (('a, 'b t) Either0.t[@local_opt])
    -> (('a, 'b) Either0.t[@local_opt])
    @@ portable
    = "%identity"

  external wrap_result
    :  ('a, 'b) Result.t
    -> ('a t, 'b t) Result.t
    @@ portable
    = "%identity"

  external unwrap_result
    :  (('a t, 'b t) Result.t[@local_opt])
    -> (('a, 'b) Result.t[@local_opt])
    @@ portable
    = "%identity"

  external wrap_ok : ('a, 'b) Result.t -> ('a t, 'b) Result.t @@ portable = "%identity"

  external unwrap_ok
    :  (('a t, 'b) Result.t[@local_opt])
    -> (('a, 'b) Result.t[@local_opt])
    @@ portable
    = "%identity"

  external wrap_error : ('a, 'b) Result.t -> ('a, 'b t) Result.t @@ portable = "%identity"

  external unwrap_error
    :  (('a, 'b t) Result.t[@local_opt])
    -> (('a, 'b) Result.t[@local_opt])
    @@ portable
    = "%identity"

  external wrap_tuple2 : 'a * 'b -> 'a t * 'b t @@ portable = "%identity"
  external wrap_fst : 'a * 'b -> 'a t * 'b @@ portable = "%identity"
  external wrap_snd : 'a * 'b -> 'a * 'b t @@ portable = "%identity"

  external unwrap_tuple2
    :  ('a t * 'b t[@local_opt])
    -> ('a * 'b[@local_opt])
    @@ portable
    = "%identity"

  external unwrap_fst
    :  ('a t * 'b[@local_opt])
    -> ('a * 'b[@local_opt])
    @@ portable
    = "%identity"

  external unwrap_snd
    :  ('a * 'b t[@local_opt])
    -> ('a * 'b[@local_opt])
    @@ portable
    = "%identity"

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

  type ('a : value_or_null) t : value_or_null mod portable = { portable : 'a @@ portable }
  [@@unboxed] [@@deriving compare ~localize, equal ~localize, hash]

  let%template sexp_of_t sexp_of_a { portable } = sexp_of_a portable [@exclave_if_stack a]
  [@@alloc a = (heap, stack)]
  ;;

  let%template[@alloc stack] sexp_of_t = (sexp_of_t [@alloc stack])
  let t_of_sexp a_of_sexp sexp = { portable = a_of_sexp sexp }

  let t_sexp_grammar : type a. a Sexplib0.Sexp_grammar.t -> a t Sexplib0.Sexp_grammar.t =
    Sexplib0.Sexp_grammar.coerce
  ;;

  external cross
    : ('a : value mod portable).
    'a -> 'a @ portable
    @@ portable
    = "%identity"

  external wrap
    :  ('a[@local_opt]) @ portable
    -> ('a t[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external unwrap
    :  ('a t[@local_opt])
    -> ('a[@local_opt]) @ portable
    @@ portable
    = "%identity"

  let map { portable } ~f = { portable = f portable }

  external wrap_list
    :  ('a list[@local_opt]) @ portable
    -> ('a t list[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external unwrap_list
    :  ('a t list[@local_opt])
    -> ('a list[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external wrap_iarray
    :  ('a iarray[@local_opt]) @ portable
    -> ('a t iarray[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external unwrap_iarray
    :  ('a t iarray[@local_opt])
    -> ('a iarray[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external wrap_or_null
    :  ('a or_null[@local_opt]) @ portable
    -> ('a t or_null[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external unwrap_or_null
    :  ('a t or_null[@local_opt])
    -> ('a or_null[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external wrap_option
    :  ('a option[@local_opt]) @ portable
    -> ('a t option[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external unwrap_option
    :  ('a t option[@local_opt])
    -> ('a option[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external wrap_either
    :  (('a, 'b) Either0.t[@local_opt]) @ portable
    -> (('a t, 'b t) Either0.t[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external unwrap_either
    :  (('a t, 'b t) Either0.t[@local_opt])
    -> (('a, 'b) Either0.t[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external wrap_first
    :  (('a, 'b) Either0.t[@local_opt]) @ portable
    -> (('a t, 'b) Either0.t[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external unwrap_first
    :  (('a t, 'b) Either0.t[@local_opt])
    -> (('a, 'b) Either0.t[@local_opt])
    @@ portable
    = "%identity"

  external unwrap_first__portable
    :  (('a t, 'b) Either0.t[@local_opt]) @ portable
    -> (('a, 'b) Either0.t[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external wrap_second
    :  (('a, 'b) Either0.t[@local_opt]) @ portable
    -> (('a, 'b t) Either0.t[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external unwrap_second
    :  (('a, 'b t) Either0.t[@local_opt])
    -> (('a, 'b) Either0.t[@local_opt])
    @@ portable
    = "%identity"

  external unwrap_second__portable
    :  (('a, 'b t) Either0.t[@local_opt]) @ portable
    -> (('a, 'b) Either0.t[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external wrap_result
    :  (('a, 'b) Result.t[@local_opt]) @ portable
    -> (('a t, 'b t) Result.t[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external unwrap_result
    :  (('a t, 'b t) Result.t[@local_opt])
    -> (('a, 'b) Result.t[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external wrap_result_list
    :  (('a, 'b) Result.t list[@local_opt]) @ portable
    -> (('a t, 'b t) Result.t list[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external unwrap_result_list
    :  (('a t, 'b t) Result.t list[@local_opt])
    -> (('a, 'b) Result.t list[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external wrap_ok
    :  (('a, 'b) Result.t[@local_opt]) @ portable
    -> (('a t, 'b) Result.t[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external unwrap_ok
    :  (('a t, 'b) Result.t[@local_opt])
    -> (('a, 'b) Result.t[@local_opt])
    @@ portable
    = "%identity"

  external unwrap_ok__portable
    :  (('a t, 'b) Result.t[@local_opt]) @ portable
    -> (('a, 'b) Result.t[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external wrap_error
    :  (('a, 'b) Result.t[@local_opt]) @ portable
    -> (('a, 'b t) Result.t[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external unwrap_error
    :  (('a, 'b t) Result.t[@local_opt])
    -> (('a, 'b) Result.t[@local_opt])
    @@ portable
    = "%identity"

  external unwrap_error__portable
    :  (('a, 'b t) Result.t[@local_opt]) @ portable
    -> (('a, 'b) Result.t[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external wrap_tuple2
    :  ('a * 'b[@local_opt]) @ portable
    -> ('a t * 'b t[@local_opt])
    @@ portable
    = "%identity"

  external wrap_fst
    :  ('a * 'b[@local_opt]) @ portable
    -> ('a t * 'b[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external wrap_snd
    :  ('a * 'b[@local_opt]) @ portable
    -> ('a * 'b t[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external unwrap_tuple2
    :  ('a t * 'b t[@local_opt])
    -> ('a * 'b[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external%template unwrap_fst
    :  ('a t * 'b[@local_opt]) @ p
    -> ('a * 'b[@local_opt]) @ p
    @@ portable
    = "%identity"
  [@@mode p = (portable, nonportable)]

  external%template unwrap_snd
    :  ('a * 'b t[@local_opt]) @ p
    -> ('a * 'b[@local_opt]) @ p
    @@ portable
    = "%identity"
  [@@mode p = (portable, nonportable)]
end

module Contended = struct
  type ('a : value_or_null) t : value_or_null mod contended =
    { contended : 'a @@ contended }
  [@@unboxed]

  let t_of_sexp of_sexp sexp = { contended = of_sexp sexp }

  external cross
    : ('a : value mod contended).
    'a @ contended -> 'a
    @@ portable
    = "%identity"
end

module Shared = struct
  type ('a : value_or_null) t = { shared : 'a @@ shared } [@@unboxed]

  let t_of_sexp of_sexp sexp = { shared = of_sexp sexp }
end

module Portended = struct
  type ('a : value_or_null) t : value_or_null mod contended portable =
    { portended : 'a @@ contended portable }
  [@@unboxed]
end

module Many = struct
  type ('a : value_or_null) t : value_or_null mod many = { many : 'a @@ many }
  [@@unboxed]
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of ~stackify, sexp_grammar]
end

module Aliased = struct
  type ('a : value_or_null) t : value_or_null mod aliased = { aliased : 'a @@ aliased }
  [@@unboxed]
end

module Aliased_many = struct
  type ('a : value_or_null) t : value_or_null mod aliased many =
    { aliased_many : 'a @@ aliased many }
  [@@unboxed]
end

module Forkable = struct
  type ('a : value_or_null) t : value_or_null mod forkable = { forkable : 'a @@ forkable }
  [@@unboxed]
end

module Unyielding = struct
  type ('a : value_or_null) t : value_or_null mod unyielding =
    { unyielding : 'a @@ unyielding }
  [@@unboxed]
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of ~stackify, sexp_grammar]

  let t_of_sexp of_sexp sexp = { unyielding = of_sexp sexp }
end

module Stateless = struct
  type ('a : value_or_null) t : value_or_null mod stateless =
    { stateless : 'a @@ stateless }
  [@@unboxed]
end

module Observing = struct
  type ('a : value_or_null) t = { observing : 'a @@ observing } [@@unboxed]
end

module Immutable = struct
  type ('a : value_or_null) t : value_or_null mod immutable =
    { immutable : 'a @@ immutable }
  [@@unboxed]
end

module Read = struct
  type ('a : value_or_null) t = { read : 'a @@ read } [@@unboxed]
end

module Immutable_data = struct
  type ('a : value mod non_float) t : immutable_data =
    { immutable_data : 'a @@ forkable immutable many stateless unyielding }
  [@@unboxed]
end

module At_locality = struct
  include Modes_intf.Definitions.At_locality

  (* The safety of this module is tested in `test/test_modes.ml`, which reimplements its
     interface without using [external] definitions. *)

  (* This type must not cross locality. *)
  type actually_local :
    value mod aliased contended external_ forkable many non_null portable unyielding

  type global : immediate = [ `global ]
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]

  type local =
    [ `global
    | `local of (actually_local[@compare.ignore] [@equal.ignore] [@sexp.opaque])
    ]
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]

  type (+'a
       , +'locality)
       t :
       immediate
       with 'a @@ global
       with 'locality @@ aliased contended forkable many portable unyielding

  external wrap : 'a 'loc. 'a -> ('a, 'loc) t @@ portable = "%identity"
  external unwrap : 'a 'loc. ('a, 'loc) t -> 'a @@ portable = "%identity"
  external wrap_local : 'a. local_ 'a -> local_ ('a, local) t @@ portable = "%identity"

  external unwrap_local
    : 'a 'loc.
    local_ ('a, 'loc) t -> local_ 'a
    @@ portable
    = "%identity"

  external unwrap_global : 'a. local_ ('a, global) t -> 'a @@ portable = "%identity"

  let to_local t = exclave_ wrap_local (unwrap_local t)
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
  type nonportable_ :
    value mod aliased contended external_ forkable global many non_null unyielding
  [@@allow_redundant_modalities
    "While [global] implies [forkable unyielding], we include them for clarity"]
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]

  (* We only need [hash_fold] and local comparisons. *)
  let _ = [%compare: nonportable_]
  let _ = [%equal: nonportable_]
  let _ = [%hash: nonportable_]

  type portable : immediate = [ `portable ]
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]

  type nonportable =
    [ `portable
    | `nonportable of nonportable_
    ]
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]

  type (+!'a
       , +'portability)
       t :
       immediate
       with 'a @@ portable
       with 'portability @@ aliased contended forkable global many unyielding
  [@@allow_redundant_modalities
    "While [global] implies [forkable unyielding], we include them for clarity"]

  external wrap_portable
    :  ('a[@local_opt]) @ portable
    -> (('a, _) t[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external unwrap_portable
    :  (('a, portable) t[@local_opt])
    -> ('a[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external wrap_nonportable
    :  ('a[@local_opt])
    -> (('a, nonportable) t[@local_opt])
    @@ portable
    = "%identity"

  external unwrap_nonportable
    :  (('a, _) t[@local_opt])
    -> ('a[@local_opt])
    @@ portable
    = "%identity"

  [%%template
  type portability = nonportable
  [@@mode nonportable]
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]

  type portability = portable
  [@@mode portable]
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]

  external wrap
    :  ('a[@local_opt]) @ p
    -> (('a, (portability[@mode p])) t[@local_opt]) @ p
    @@ portable
    = "%identity"
  [@@mode p = (portable, nonportable)]

  external unwrap
    :  (('a, (portability[@mode p])) t[@local_opt])
    -> ('a[@local_opt]) @ p
    @@ portable
    = "%identity"
  [@@mode p = portable]

  external unwrap : (('a, _) t[@local_opt]) -> ('a[@local_opt]) @@ portable = "%identity"
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
  type +'a t : value mod portable

  external wrap : ('a[@local_opt]) -> ('a t[@local_opt]) @@ portable = "%identity"
  external unwrap : ('a t[@local_opt]) -> ('a[@local_opt]) @@ portable = "%identity"

  external unwrap_contended
    : ('a : value mod portable).
    ('a t[@local_opt]) @ contended -> ('a[@local_opt]) @ contended
    @@ portable
    = "%identity"
end

module Contended_via_portable = struct
  type +'a t : value mod contended

  external wrap : ('a[@local_opt]) -> ('a t[@local_opt]) @@ portable = "%identity"
  external unwrap : ('a t[@local_opt]) -> ('a[@local_opt]) @@ portable = "%identity"

  let sexp_of_t sexp_of_a t = sexp_of_a (unwrap t)
  let refute_portable (_ @ portable) = assert false
end

module Mod = struct
  type%template ('a : any) t = Mod : ('a : any mod a c m p). ('a t[@modality a p c m])
  [@@modality
    a = aliased
    , p = (nonportable, portable)
    , c = (uncontended, shared, contended)
    , m = (once, many)]

  type%template ('a : any) t = Mod : ('a : any mod c g m p). ('a t[@modality g p c m])
  [@@modality
    g = (local, global)
    , p = (nonportable, portable)
    , c = (uncontended, shared, contended)
    , m = (once, many)]

  module Global = struct
    type%template ('a : any) t = ('a t[@modality global]) =
      | Mod : ('a : any mod global). 'a t
  end

  module Portable = struct
    type%template ('a : any) t = ('a t[@modality portable]) =
      | Mod : ('a : any mod portable). 'a t
  end

  module Contended = struct
    type%template ('a : any) t = ('a t[@modality contended]) =
      | Mod : ('a : any mod contended). 'a t
  end

  module Shared = struct
    type%template ('a : any) t = ('a t[@modality shared]) =
      | Mod : ('a : any mod shared). 'a t
  end

  module Portended = struct
    type%template ('a : any) t = ('a t[@modality portable contended]) =
      | Mod : ('a : any mod contended portable). 'a t
  end

  module Many = struct
    type%template ('a : any) t = ('a t[@modality many]) =
      | Mod : ('a : any mod many). 'a t
  end

  module Aliased = struct
    type%template ('a : any) t = ('a t[@modality aliased]) =
      | Mod : ('a : any mod aliased). 'a t
  end
end

module Export = struct
  type ('a : value_or_null) global : value_or_null mod global = 'a Global.t =
    { global : 'a @@ global }
  [@@unboxed]
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of ~stackify, sexp_grammar]

  let global_of_sexp = Global.t_of_sexp

  type ('a : value_or_null) portable : value_or_null mod portable = 'a Portable.t =
    { portable : 'a @@ portable }
  [@@unboxed]
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of ~stackify, sexp_grammar]

  type ('a : value_or_null) contended : value_or_null mod contended = 'a Contended.t =
    { contended : 'a @@ contended }
  [@@unboxed]

  let contended_of_sexp = Contended.t_of_sexp

  type ('a : value_or_null) shared = 'a Shared.t = { shared : 'a @@ shared } [@@unboxed]

  let shared_of_sexp = Shared.t_of_sexp

  type ('a : value_or_null) portended : value_or_null mod contended portable =
        'a Portended.t =
    { portended : 'a @@ contended portable }
  [@@unboxed]

  type ('a : value_or_null) many : value_or_null mod many = 'a Many.t =
    { many : 'a @@ many }
  [@@unboxed]
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of ~stackify, sexp_grammar]

  type ('a : value_or_null) aliased : value_or_null mod aliased = 'a Aliased.t =
    { aliased : 'a @@ aliased }
  [@@unboxed]

  type ('a : value_or_null) aliased_many : value_or_null mod aliased many =
        'a Aliased_many.t =
    { aliased_many : 'a @@ aliased many }
  [@@unboxed]

  type ('a : value_or_null) forkable : value_or_null mod forkable = 'a Forkable.t =
    { forkable : 'a @@ forkable }
  [@@unboxed]

  type ('a : value_or_null) unyielding : value_or_null mod unyielding = 'a Unyielding.t =
    { unyielding : 'a @@ unyielding }
  [@@unboxed]
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of ~stackify, sexp_grammar]

  let unyielding_of_sexp = Unyielding.t_of_sexp

  type ('a : value_or_null) stateless : value_or_null mod stateless = 'a Stateless.t =
    { stateless : 'a @@ stateless }
  [@@unboxed]

  type ('a : value_or_null) observing = 'a Observing.t = { observing : 'a @@ observing }
  [@@unboxed]

  type ('a : value_or_null) immutable : value_or_null mod immutable = 'a Immutable.t =
    { immutable : 'a @@ immutable }
  [@@unboxed]

  type ('a : value_or_null) read = 'a Read.t = { read : 'a @@ read } [@@unboxed]

  type ('a : value mod non_float) immutable_data : immutable_data = 'a Immutable_data.t =
    { immutable_data : 'a @@ forkable immutable many stateless unyielding }
  [@@unboxed]
end
