(** Modes are an experimental compiler feature, supported in the compiler branch found at:
    https://github.com/ocaml-flambda/ocaml-jst

    This module provides types that wrap a value in a different mode from its context. In
    the standard OCaml compiler, these types are all no-op wrappers. *)

open! Import

open struct
  module Result = Result0
end

module Definitions = struct
  module Global = struct
    (** Abstraction over modality. Used in [Wrapped_fn*] signatures, below.

        Locality-polymorphic functions must be functorized over an arbitrary [Wrapper].
        The [Poly_fn*] functors instantiate a function using ['a t = 'a] for the local
        version and ['a t = Modes.Global.t] for the global version. *)
    module type Wrapper = sig
      type 'a t

      val wrap : 'a -> local_ 'a t
      val unwrap : local_ 'a t -> local_ 'a
    end

    (** Locality-polymorphic 1-argument function. Passed to [Poly_fn1] functor, below. *)
    module Wrapped_fn1 (Input : T) (Output : T) = struct
      module type S = functor (W : Wrapper) -> sig
        val fn : local_ Input.t W.t -> local_ Output.t W.t
      end
    end

    (** Locality-polymorphic 2-argument function. Passed to [Poly_fn2] functor, below. *)
    module Wrapped_fn2 (Input1 : T) (Input2 : T) (Output : T) = struct
      module type S = functor (W : Wrapper) -> sig
        val fn : local_ Input1.t W.t -> local_ Input2.t W.t -> local_ Output.t W.t
      end
    end

    (** Locality-polymorphic 3-argument function. Passed to [Poly_fn3] functor, below. *)
    module Wrapped_fn3 (Input1 : T) (Input2 : T) (Input3 : T) (Output : T) = struct
      module type S = functor (W : Wrapper) -> sig
        val fn
          :  local_ Input1.t W.t
          -> local_ Input2.t W.t
          -> local_ Input3.t W.t
          -> local_ Output.t W.t
      end
    end

    (** Local and global 1-argument function. Provided by [Poly_fn1] functor, below. *)
    module type Poly_fn1 = sig
      type input
      type output

      val fn_local : local_ input -> local_ output
      val fn_global : input -> output
    end

    (** Local and global 2-argument function. Provided by [Poly_fn2] functor, below. *)
    module type Poly_fn2 = sig
      type input1
      type input2
      type output

      val fn_local : local_ input1 -> local_ input2 -> local_ output
      val fn_global : input1 -> input2 -> output
    end

    (** Local and global 3-argument function. Provided by [Poly_fn3] functor, below. *)
    module type Poly_fn3 = sig
      type input1
      type input2
      type input3
      type output

      val fn_local : local_ input1 -> local_ input2 -> local_ input3 -> local_ output
      val fn_global : input1 -> input2 -> input3 -> output
    end

    module type Global = sig
      type 'a t
      [@@deriving compare ~localize, equal ~localize, hash, sexp ~stackify, sexp_grammar]

      (** Globalize a [t]. Takes an argument because [[%globalize]] will pass one for
          ['a], but [globalize] is a no-op so it discards the argument. *)
      val globalize : local_ _ -> local_ 'a t -> 'a t

      (** Construct a [t]. Returns a global [t], which may be used as a local value. *)
      val wrap : 'a -> 'a t

      (** Access the contents of a [t]. Accepts a local [t], to which global values may be
          passed. *)
      val unwrap : local_ 'a t -> 'a

      (** Transform the contents of a [t]. Accepts a local [t], to which global values may
          be passed. Returns a global [t], which may be used as a local value. *)
      val map : local_ 'a t -> f:local_ ('a -> 'b) -> 'b t

      (** {2 Wrap and unwrap within other types}

          It can be safe to wrap or unwrap a [Global.t] inside other types as a coercion
          rather than a conversion, i.e. without copying any data. Since the type system
          does not provide coercions for this, we provide explicit operations to convert
          as a no-op. We export them as [external ... = "%identity"] to make this clear.

          The [wrap_*] functions produce global values, which may be used as local values.

          The [unwrap_*] functions produce local or global values, depending on which they
          are passed. If used for globals, the result may be used as a local value. If
          used for locals, the result must be local because the outer constructor may not
          be used as a global value.

          In general, it is safe to wrap or unwrap a type parameter ['a] in this way if
          the wrapping and unwrapping do not pass through any arrow type. *)

      (** Wrapping and unwrapping [List]. *)

      external wrap_list : 'a list -> 'a t list = "%identity"

      external unwrap_list
        :  ('a t list[@local_opt])
        -> ('a list[@local_opt])
        = "%identity"

      (** Wrapping and unwrapping [Iarray]. *)

      external wrap_iarray : 'a iarray -> 'a t iarray = "%identity"

      external unwrap_iarray
        :  ('a t iarray[@local_opt])
        -> ('a iarray[@local_opt])
        = "%identity"

      (** Wrapping and unwrapping [Or_null]. *)

      external wrap_or_null : 'a or_null -> 'a t or_null = "%identity"
      external unwrap_or_null : ('a t or_null[@local_opt]) -> 'a or_null = "%identity"

      (** Wrapping and unwrapping [Option]. *)

      external wrap_option : 'a option -> 'a t option = "%identity"

      external unwrap_option
        :  ('a t option[@local_opt])
        -> ('a option[@local_opt])
        = "%identity"

      (** Wrapping and unwrapping [Either]. *)

      external wrap_either : ('a, 'b) Either0.t -> ('a t, 'b t) Either0.t = "%identity"

      external unwrap_either
        :  (('a t, 'b t) Either0.t[@local_opt])
        -> (('a, 'b) Either0.t[@local_opt])
        = "%identity"

      (** Wrapping and unwrapping [Either.First]. *)

      external wrap_first : ('a, 'b) Either0.t -> ('a t, 'b) Either0.t = "%identity"

      external unwrap_first
        :  (('a t, 'b) Either0.t[@local_opt])
        -> (('a, 'b) Either0.t[@local_opt])
        = "%identity"

      (** Wrapping and unwrapping [Either.Second]. *)

      external wrap_second : ('a, 'b) Either0.t -> ('a, 'b t) Either0.t = "%identity"

      external unwrap_second
        :  (('a, 'b t) Either0.t[@local_opt])
        -> (('a, 'b) Either0.t[@local_opt])
        = "%identity"

      (** Wrapping and unwrapping [Result]. *)

      external wrap_result : ('a, 'b) Result.t -> ('a t, 'b t) Result.t = "%identity"

      external unwrap_result
        :  (('a t, 'b t) Result.t[@local_opt])
        -> (('a, 'b) Result.t[@local_opt])
        = "%identity"

      (** Wrapping and unwrapping [Result.Ok]. *)

      external wrap_ok : ('a, 'b) Result.t -> ('a t, 'b) Result.t = "%identity"

      external unwrap_ok
        :  (('a t, 'b) Result.t[@local_opt])
        -> (('a, 'b) Result.t[@local_opt])
        = "%identity"

      (** Wrapping and unwrapping [Result.Error]. *)

      external wrap_error : ('a, 'b) Result.t -> ('a, 'b t) Result.t = "%identity"

      external unwrap_error
        :  (('a, 'b t) Result.t[@local_opt])
        -> (('a, 'b) Result.t[@local_opt])
        = "%identity"

      (** Wrapping and unwrapping two-tuples. *)

      external wrap_tuple2 : 'a * 'b -> 'a t * 'b t = "%identity"
      external wrap_fst : 'a * 'b -> 'a t * 'b = "%identity"
      external wrap_snd : 'a * 'b -> 'a * 'b t = "%identity"

      external unwrap_tuple2
        :  ('a t * 'b t[@local_opt])
        -> ('a * 'b[@local_opt])
        = "%identity"

      external unwrap_fst : ('a t * 'b[@local_opt]) -> ('a * 'b[@local_opt]) = "%identity"
      external unwrap_snd : ('a * 'b t[@local_opt]) -> ('a * 'b[@local_opt]) = "%identity"

      (** {2 Simulating mode polymorphism}

          Until the mode extension to the OCaml compiler supports polymorphism over modes,
          we have to simulate it by writing multiple instances of some functions. These
          functors handle simple cases of local-vs-global versions of functions.

          Given a function that is parameterized over the [Wrapper] signature above, these
          functors provide two versions of the function, one that operates on and returns
          local values, and one that operates on and returns global values.

          Performance caveat:

          The function implementation itself is only compiled once. Inputs it unwraps will
          be local, and may need to be [globalize]d. Outputs it produces will be global,
          and will be allocated on the heap. To avoid these performance concerns, you will
          need to actually write two versions of the function. *)

      (** Instantiate local and global versions of a 1-argument function. *)
      module Poly_fn1 (Input : T) (Output : T) (F : Wrapped_fn1(Input)(Output).S) :
        Poly_fn1 with type input := Input.t and type output := Output.t

      (** Instantiate local and global versions of a 2-argument function. *)
      module Poly_fn2
          (Input1 : T)
          (Input2 : T)
          (Output : T)
          (F : Wrapped_fn2(Input1)(Input2)(Output).S) :
        Poly_fn2
        with type input1 := Input1.t
         and type input2 := Input2.t
         and type output := Output.t

      (** Instantiate local and global versions of a 3-argument function. *)
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
         and type output := Output.t
    end
  end

  module Portable = struct
    module type Portable = sig
      type 'a t
      [@@deriving
        compare ~localize, equal ~localize, hash, sexp_of ~stackify, sexp_grammar]

      val t_of_sexp : (Sexplib0.Sexp.t -> 'a @ portable) -> Sexplib0.Sexp.t -> 'a t

      (** Require a value has a type that mode-crosses portability. This is useful for
          assisting type inference as well as improving error messages. *)
      external cross : ('a : value mod portable). 'a -> 'a @ portable = "%identity"

      (** Construct a [t]. *)
      external wrap
        :  ('a[@local_opt]) @ portable
        -> ('a t[@local_opt]) @ portable
        = "%identity"

      (** Access the contents of a [t]. *)
      external unwrap : ('a t[@local_opt]) -> ('a[@local_opt]) @ portable = "%identity"

      (** Transform the contents of a [t]. *)
      val map : 'a t -> f:local_ ('a @ portable -> 'b @ portable) -> 'b t @ portable

      (** {2 Wrap and unwrap within other types}

          It can be safe to wrap or unwrap a [Portable.t] inside other types as a coercion
          rather than a conversion, i.e. without copying any data. Since the type system
          does not provide coercions for this, we provide explicit operations to convert
          as a no-op. We export them as [external ... = "%identity"] to make this clear.

          In general, it is safe to wrap or unwrap a type parameter ['a] in this way if
          the wrapping and unwrapping do not pass through any arrow type. *)

      (** Wrapping and unwrapping [List]. *)

      external wrap_list
        :  ('a list[@local_opt]) @ portable
        -> ('a t list[@local_opt]) @ portable
        = "%identity"

      external unwrap_list
        :  ('a t list[@local_opt])
        -> ('a list[@local_opt]) @ portable
        = "%identity"

      (** Wrapping and unwrapping [Iarray]. *)

      external wrap_iarray
        :  ('a iarray[@local_opt]) @ portable
        -> ('a t iarray[@local_opt]) @ portable
        = "%identity"

      external unwrap_iarray
        :  ('a t iarray[@local_opt])
        -> ('a iarray[@local_opt]) @ portable
        = "%identity"

      (** Wrapping and unwrapping [Or_null]. *)

      external wrap_or_null
        :  ('a or_null[@local_opt]) @ portable
        -> ('a t or_null[@local_opt]) @ portable
        = "%identity"

      external unwrap_or_null
        :  ('a t or_null[@local_opt])
        -> ('a or_null[@local_opt]) @ portable
        = "%identity"

      (** Wrapping and unwrapping [Option]. *)

      external wrap_option
        :  ('a option[@local_opt]) @ portable
        -> ('a t option[@local_opt]) @ portable
        = "%identity"

      external unwrap_option
        :  ('a t option[@local_opt])
        -> ('a option[@local_opt]) @ portable
        = "%identity"

      (** Wrapping and unwrapping [Either]. *)

      external wrap_either
        :  (('a, 'b) Either0.t[@local_opt]) @ portable
        -> (('a t, 'b t) Either0.t[@local_opt]) @ portable
        = "%identity"

      external unwrap_either
        :  (('a t, 'b t) Either0.t[@local_opt])
        -> (('a, 'b) Either0.t[@local_opt]) @ portable
        = "%identity"

      (** Wrapping and unwrapping [Either.First]. *)

      external wrap_first
        :  (('a, 'b) Either0.t[@local_opt]) @ portable
        -> (('a t, 'b) Either0.t[@local_opt]) @ portable
        = "%identity"

      external unwrap_first
        :  (('a t, 'b) Either0.t[@local_opt])
        -> (('a, 'b) Either0.t[@local_opt])
        = "%identity"

      external unwrap_first__portable
        :  (('a t, 'b) Either0.t[@local_opt]) @ portable
        -> (('a, 'b) Either0.t[@local_opt]) @ portable
        = "%identity"

      (** Wrapping and unwrapping [Either.Second]. *)

      external wrap_second
        :  (('a, 'b) Either0.t[@local_opt]) @ portable
        -> (('a, 'b t) Either0.t[@local_opt]) @ portable
        = "%identity"

      external unwrap_second
        :  (('a, 'b t) Either0.t[@local_opt])
        -> (('a, 'b) Either0.t[@local_opt])
        = "%identity"

      external unwrap_second__portable
        :  (('a, 'b t) Either0.t[@local_opt]) @ portable
        -> (('a, 'b) Either0.t[@local_opt]) @ portable
        = "%identity"

      (** Wrapping and unwrapping [Result]. *)

      external wrap_result
        :  (('a, 'b) Result.t[@local_opt]) @ portable
        -> (('a t, 'b t) Result.t[@local_opt]) @ portable
        = "%identity"

      external unwrap_result
        :  (('a t, 'b t) Result.t[@local_opt])
        -> (('a, 'b) Result.t[@local_opt]) @ portable
        = "%identity"

      (** Wrapping and unwrapping [Result.t list]. *)

      external wrap_result_list
        :  (('a, 'b) Result.t list[@local_opt]) @ portable
        -> (('a t, 'b t) Result.t list[@local_opt]) @ portable
        = "%identity"

      external unwrap_result_list
        :  (('a t, 'b t) Result.t list[@local_opt])
        -> (('a, 'b) Result.t list[@local_opt]) @ portable
        = "%identity"

      (** Wrapping and unwrapping [Result.Ok]. *)

      external wrap_ok
        :  (('a, 'b) Result.t[@local_opt]) @ portable
        -> (('a t, 'b) Result.t[@local_opt]) @ portable
        = "%identity"

      external unwrap_ok
        :  (('a t, 'b) Result.t[@local_opt])
        -> (('a, 'b) Result.t[@local_opt])
        = "%identity"

      external unwrap_ok__portable
        :  (('a t, 'b) Result.t[@local_opt]) @ portable
        -> (('a, 'b) Result.t[@local_opt]) @ portable
        = "%identity"

      (** Wrapping and unwrapping [Result.Error]. *)

      external wrap_error
        :  (('a, 'b) Result.t[@local_opt]) @ portable
        -> (('a, 'b t) Result.t[@local_opt]) @ portable
        = "%identity"

      external unwrap_error
        :  (('a, 'b t) Result.t[@local_opt])
        -> (('a, 'b) Result.t[@local_opt])
        = "%identity"

      external unwrap_error__portable
        :  (('a, 'b t) Result.t[@local_opt]) @ portable
        -> (('a, 'b) Result.t[@local_opt]) @ portable
        = "%identity"

      (** Wrapping and unwrapping two-tuples. *)

      external wrap_tuple2
        :  ('a * 'b[@local_opt]) @ portable
        -> ('a t * 'b t[@local_opt])
        = "%identity"

      external wrap_fst
        :  ('a * 'b[@local_opt]) @ portable
        -> ('a t * 'b[@local_opt]) @ portable
        = "%identity"

      external wrap_snd
        :  ('a * 'b[@local_opt]) @ portable
        -> ('a * 'b t[@local_opt]) @ portable
        = "%identity"

      external unwrap_tuple2
        :  ('a t * 'b t[@local_opt])
        -> ('a * 'b[@local_opt]) @ portable
        = "%identity"

      external%template unwrap_fst
        :  ('a t * 'b[@local_opt]) @ p
        -> ('a * 'b[@local_opt]) @ p
        = "%identity"
      [@@mode p = (portable, nonportable)]

      external%template unwrap_snd
        :  ('a * 'b t[@local_opt]) @ p
        -> ('a * 'b[@local_opt]) @ p
        = "%identity"
      [@@mode p = (portable, nonportable)]
    end
  end

  module At_locality = struct
    (** Abstract over whether a value is [local] or [global]. *)
    module type Without_crossing = sig
      type actually_local

      type local =
        [ `global
        | `local of actually_local
        ]
      [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]

      type global = [ `global ]
      [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]

      type (+'a, 'locality) t
      [@@deriving
        compare ~localize, equal ~localize, hash, globalize, sexp_of, sexp_grammar]

      (** Create a global wrapper. Can have any phantom type. *)
      val wrap : 'a -> ('a, _) t

      (** Unwrap a global wrapper. When the wrapper is global, the contents are too. *)
      val unwrap : ('a, _) t -> 'a

      (** Wrap local contents. *)
      val wrap_local : local_ 'a -> local_ ('a, local) t

      (** Unwrap local contents. Global or local contents can be unwrapped as local. *)
      val unwrap_local : local_ ('a, _) t -> local_ 'a

      (** Unwrap global contents. *)
      val unwrap_global : local_ ('a, global) t -> 'a

      (** Convert phantom type of a wrapper to local. *)
      val to_local : local_ ('a, _) t -> local_ ('a, local) t

      (** Convert phantom type of a global wrapper to global. *)
      val to_global : ('a, _) t -> ('a, global) t

      (** Globalize a wrapper where the contents are already global. *)
      val globalize_global : local_ ('a, global) t -> ('a, global) t
    end

    module type At_locality = sig
      (** Phantom type parameter for {!t} representing that the inhabitant may be local.
          This type does not cross locality to enforce the relationship between
          mode-crossing and phantom types in the [with] clauses on [type t] below. *)
      type actually_local :
        value mod aliased contended external_ forkable many non_null portable unyielding

      (** Phantom type parameter for {!t} which represents that the inhabitant is known to
          be [global]. *)
      type global : immediate = [ `global ]

      (** Phantom type parameter for {!t} which represents that the locality of the
          inhabitant is unknown, and so must be assumed to be [local]. *)
      type local =
        [ `global
        | `local of actually_local
        ]

      (** Abstract over whether a value is [global] or [local], with zero runtime cost,
          supporting mode crossing locality in the global case

          [('a, global) t] represents a ['a] that is known to be [global]. This type
          importantly mode-crosses along the locality axis, even if ['a] usually does not.

          [('a, local) t] represents a ['a] whose locality is unknown. This type does not
          mode-cross along the locality axis, even if ['a] usually does. *)
      type (+'a
           , +'locality)
           t :
           immediate
           with 'a @@ global
           with 'locality @@ aliased contended forkable many portable unyielding

      include
        Without_crossing
        with type ('a, 'loc) t := ('a, 'loc) t
         and type actually_local := actually_local
         and type global := global
         and type local := local
    end
  end
end

module type Modes = sig @@ portable
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

  (** Wrap values in the [global_] mode, even in a [local_] context. *)
  module Global : sig
    include module type of struct
      include Definitions.Global
    end

    type ('a : value_or_null) t : value_or_null mod global = { global : 'a @@ global }
    [@@unboxed]

    include Global with type 'a t := 'a t (** @inline *)
  end

  module Portable : sig
    include module type of struct
      include Definitions.Portable
    end

    type ('a : value_or_null) t : value_or_null mod portable =
      { portable : 'a @@ portable }
    [@@unboxed]

    include Portable with type 'a t := 'a t
  end

  module Contended : sig
    type ('a : value_or_null) t : value_or_null mod contended =
      { contended : 'a @@ contended }
    [@@unboxed] [@@deriving of_sexp]

    (** Require a value has a type that mode-crosses contention. This is useful for
        assisting type inference as well as improving error messages. *)
    external cross : ('a : value mod contended). 'a @ contended -> 'a = "%identity"
  end

  module Shared : sig
    type ('a : value_or_null) t = { shared : 'a @@ shared }
    [@@unboxed] [@@deriving of_sexp]
  end

  module Portended : sig
    type ('a : value_or_null) t : value_or_null mod contended portable =
      { portended : 'a @@ contended portable }
    [@@unboxed]
  end

  module Many : sig
    type ('a : value_or_null) t : value_or_null mod many = { many : 'a @@ many }
    [@@unboxed]
    [@@deriving compare ~localize, equal ~localize, hash, sexp_of ~stackify, sexp_grammar]
  end

  module Aliased : sig
    type ('a : value_or_null) t : value_or_null mod aliased = { aliased : 'a @@ aliased }
    [@@unboxed]
  end

  module Aliased_many : sig
    type ('a : value_or_null) t : value_or_null mod aliased many =
      { aliased_many : 'a @@ aliased many }
    [@@unboxed]
  end

  module Forkable : sig
    type ('a : value_or_null) t : value_or_null mod forkable =
      { forkable : 'a @@ forkable }
    [@@unboxed]
  end

  module Unyielding : sig
    type ('a : value_or_null) t : value_or_null mod unyielding =
      { unyielding : 'a @@ unyielding }
    [@@unboxed]
    [@@deriving compare ~localize, equal ~localize, hash, sexp ~stackify, sexp_grammar]
  end

  module Stateless : sig
    type ('a : value_or_null) t : value_or_null mod stateless =
      { stateless : 'a @@ stateless }
    [@@unboxed]
  end

  module Observing : sig
    type ('a : value_or_null) t = { observing : 'a @@ observing } [@@unboxed]
  end

  module Immutable : sig
    type ('a : value_or_null) t : value_or_null mod immutable =
      { immutable : 'a @@ immutable }
    [@@unboxed]
  end

  module Read : sig
    type ('a : value_or_null) t = { read : 'a @@ read } [@@unboxed]
  end

  module Immutable_data : sig
    type ('a : value mod non_float) t : immutable_data =
      { immutable_data : 'a @@ forkable immutable many stateless unyielding }
    [@@unboxed]
  end

  (** Abstract over whether a value is [local] or [global]. *)
  module At_locality : sig
    include module type of struct
      include Definitions.At_locality
    end

    include At_locality
  end

  (** Abstract over whether a value is [portable] or [nonportable] *)
  module At_portability : sig
    type nonportable_ :
      value mod aliased contended external_ forkable global many non_null unyielding
    [@@allow_redundant_modalities
      "While [global] implies [forkable unyielding], we include them for clarity"]

    (** Phantom type parameter for {!t} which represents that the inhabitant is known to
        be [portable]. *)
    type portable : immediate = [ `portable ]
    [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]

    (** Phantom type parameter for {!t} which represents that the portability of the
        inhabitant is unknown, and so must be assumed to be [nonportable]. *)
    type nonportable =
      [ `portable
      | `nonportable of nonportable_
      ]
    [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]

    (** Abstract over whether a value is [portable] or [nonportable], with zero runtime
        cost.

        [('a, portable) t] represents a ['a] that is known to be [portable]. This type
        importantly mode-crosses along the portability axis, even if ['a] usually does
        not.

        [('a, nonportable) t] represents a ['a] whose portability is unknown. This type
        does not mode-cross along the portability axis, even if ['a] usually does. *)
    type (+!'a
         , +'portability)
         t :
         immediate
         with 'a @@ portable
         with 'portability @@ aliased contended forkable global many unyielding
    [@@allow_redundant_modalities
      "While [global] implies [forkable unyielding], we include them for clarity"]
    [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]

    external wrap_portable
      :  ('a[@local_opt]) @ portable
      -> (('a, _) t[@local_opt]) @ portable
      = "%identity"

    external unwrap_portable
      :  (('a, portable) t[@local_opt])
      -> ('a[@local_opt]) @ portable
      = "%identity"

    external wrap_nonportable
      :  ('a[@local_opt])
      -> (('a, nonportable) t[@local_opt])
      = "%identity"

    external unwrap_nonportable
      :  (('a, _) t[@local_opt])
      -> ('a[@local_opt])
      = "%identity"

    (** The following two types are intended to be used with [ppx_template] to template
        over the ['portability] type parameter itself, for example if it appears as the
        parameter of another type.

        For example:

        {[
          type%template ('a, 'b) t =
            { maybe_portable :
                ( 'a -> 'b
                  , (Modes.At_portability.portability[@modality p]) )
                  Modes.At_portability.t
            }
          [@@modality p = (portable, nonportable)]
        ]} *)

    [%%template:
    type portability = nonportable
    [@@mode nonportable]
    [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]

    type portability = portable
    [@@mode portable]
    [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]

    external wrap
      :  ('a[@local_opt]) @ p
      -> (('a, (portability[@mode p])) t[@local_opt]) @ p
      = "%identity"
    [@@mode p = (portable, nonportable)]

    external unwrap
      :  (('a, (portability[@mode p])) t[@local_opt])
      -> ('a[@local_opt]) @ p
      = "%identity"
    [@@mode p = portable]

    external unwrap : (('a, _) t[@local_opt]) -> ('a[@local_opt]) = "%identity"
    [@@mode __ = nonportable]]
  end

  (** Allow a type to mode-cross portability by disallowing contended access to it. This
      type is useful for making a value portable (or a type mode-cross portability) when
      you don't actually need to use it in another capsule.

      Example: Suppose we have the type
      {[
        type t =
          { init : unit -> unit
          ; users : string list
          }
      ]}
      where all fields except for [init] are [immutable_data]. If we know that we will
      only ever call [t.init] from the current capsule, but then want to pass the record
      to other capsules, we can wrap [init] in [Portable_via_contended.t] so that it can
      be read and run from the current capsule, but is safe to send to other capsules
      since they won't be able to extract the contained function (even if it was actually
      portable):

      {[
        type t : value mod portable =
          { init : (unit -> unit) Portable_via_contended.t
          ; users : string list
          }
      ]} *)
  module Portable_via_contended : sig
    type +'a t : value mod portable

    (** The [uncontended] annotations below are redundant, but present to emphasize their
        importance for safety. *)

    external wrap
      :  ('a[@local_opt]) @ uncontended
      -> ('a t[@local_opt]) @ uncontended
      = "%identity"

    external unwrap
      :  ('a t[@local_opt]) @ uncontended
      -> ('a[@local_opt]) @ uncontended
      = "%identity"

    (** If the contained value is of a type that crosses portability, it's safe to extract
        it, even if it has crossed a capsule boundary. *)
    external unwrap_contended
      : ('a : value mod portable).
      ('a t[@local_opt]) @ contended -> ('a[@local_opt]) @ contended
      = "%identity"
  end

  (** Allow a type to mode-cross from contended by prohibiting it from being moved to
      another domain (i.e., by ensuring it's nonportable). *)
  module Contended_via_portable : sig
    type +'a t : value mod contended [@@deriving sexp_of]

    external wrap : ('a[@local_opt]) -> ('a t[@local_opt]) = "%identity"
    external unwrap : ('a t[@local_opt]) -> ('a[@local_opt]) = "%identity"

    (** It's impossible to construct a portable [t]. [refute_portable] lets you make use
        of that fact: having a portable [t] in hand means that you're in unreachable code
        and don't have to produce a result. *)
    val refute_portable : _ t @ portable -> _ @ portable
  end

  (** A witness of mode-crossing. In spirit, it's similar to [Type_equal.t]: there's no
      runtime content, but the fact that you have a ['a Mod.t] in hand lets you recover
      mode-crossing information about ['a]. *)
  module Mod : sig
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

    (** Aliases of [Mod] for specific modalities. These aliases aren't necessary, but they
        are a bit more convenient than ppx_template (e.g. they are auto-completed in
        editors). *)

    module Global : sig
      type%template ('a : any) t = ('a t[@modality global]) =
        | Mod : ('a : any mod global). 'a t
    end

    module Portable : sig
      type%template ('a : any) t = ('a t[@modality portable]) =
        | Mod : ('a : any mod portable). 'a t
    end

    module Contended : sig
      type%template ('a : any) t = ('a t[@modality contended]) =
        | Mod : ('a : any mod contended). 'a t
    end

    module Shared : sig
      type%template ('a : any) t = ('a t[@modality shared]) =
        | Mod : ('a : any mod shared). 'a t
    end

    module Portended : sig
      type%template ('a : any) t = ('a t[@modality portable contended]) =
        | Mod : ('a : any mod contended portable). 'a t
    end

    module Many : sig
      type%template ('a : any) t = ('a t[@modality many]) =
        | Mod : ('a : any mod many). 'a t
    end

    module Aliased : sig
      type%template ('a : any) t = ('a t[@modality aliased]) =
        | Mod : ('a : any mod aliased). 'a t
    end
  end

  (** Can be [open]ed or [include]d to bring field names into scope. *)
  module Export : sig
    type ('a : value_or_null) global : value_or_null mod global = 'a Global.t =
      { global : 'a @@ global }
    [@@unboxed]
    [@@deriving compare ~localize, equal ~localize, hash, sexp ~stackify, sexp_grammar]

    type ('a : value_or_null) portable : value_or_null mod portable = 'a Portable.t =
      { portable : 'a @@ portable }
    [@@unboxed]
    [@@deriving compare ~localize, equal ~localize, hash, sexp_of ~stackify, sexp_grammar]

    type ('a : value_or_null) contended : value_or_null mod contended = 'a Contended.t =
      { contended : 'a @@ contended }
    [@@unboxed] [@@deriving of_sexp]

    type ('a : value_or_null) shared = 'a Shared.t = { shared : 'a @@ shared }
    [@@unboxed] [@@deriving of_sexp]

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

    type ('a : value_or_null) unyielding : value_or_null mod unyielding =
          'a Unyielding.t =
      { unyielding : 'a @@ unyielding }
    [@@unboxed]
    [@@deriving compare ~localize, equal ~localize, hash, sexp ~stackify, sexp_grammar]

    type ('a : value_or_null) stateless : value_or_null mod stateless = 'a Stateless.t =
      { stateless : 'a @@ stateless }
    [@@unboxed]

    type ('a : value_or_null) observing = 'a Observing.t = { observing : 'a @@ observing }
    [@@unboxed]

    type ('a : value_or_null) immutable : value_or_null mod immutable = 'a Immutable.t =
      { immutable : 'a @@ immutable }
    [@@unboxed]

    type ('a : value_or_null) read = 'a Read.t = { read : 'a @@ read } [@@unboxed]

    type ('a : value mod non_float) immutable_data : immutable_data =
          'a Immutable_data.t =
      { immutable_data : 'a @@ forkable immutable many stateless unyielding }
    [@@unboxed]
  end
end
