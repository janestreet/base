(** Modes are an experimental compiler feature, supported in the compiler branch found at:
    https://github.com/ocaml-flambda/ocaml-jst

    This module provides types that wrap a value in a different mode from its context. In
    the standard OCaml compiler, these types are all no-op wrappers. *)

open! Import

module Definitions = struct
  module Global = struct
    (** Abstraction over modality. Used in [Wrapped_fn*] signatures, below.

        Locality-polymorphic functions must be functorized over an arbitrary [Wrapper].
        The [Poly_fn*] functors instantiate a function using ['a t = 'a] for the local
        version and ['a t = Modes.Global.t] for the global version. *)
    module type Wrapper = sig
      type 'a t

      val wrap : 'a -> 'a t
      val unwrap : 'a t -> 'a
    end

    (** Locality-polymorphic 1-argument function. Passed to [Poly_fn1] functor, below. *)
    module Wrapped_fn1 (Input : T) (Output : T) = struct
      module type S = functor (W : Wrapper) -> sig
        val fn : Input.t W.t -> Output.t W.t
      end
    end

    (** Locality-polymorphic 2-argument function. Passed to [Poly_fn2] functor, below. *)
    module Wrapped_fn2 (Input1 : T) (Input2 : T) (Output : T) = struct
      module type S = functor (W : Wrapper) -> sig
        val fn : Input1.t W.t -> Input2.t W.t -> Output.t W.t
      end
    end

    (** Locality-polymorphic 3-argument function. Passed to [Poly_fn3] functor, below. *)
    module Wrapped_fn3 (Input1 : T) (Input2 : T) (Input3 : T) (Output : T) = struct
      module type S = functor (W : Wrapper) -> sig
        val fn : Input1.t W.t -> Input2.t W.t -> Input3.t W.t -> Output.t W.t
      end
    end

    (** Local and global 1-argument function. Provided by [Poly_fn1] functor, below. *)
    module type Poly_fn1 = sig
      type input
      type output

      val fn_local : input -> output
      val fn_global : input -> output
    end

    (** Local and global 2-argument function. Provided by [Poly_fn2] functor, below. *)
    module type Poly_fn2 = sig
      type input1
      type input2
      type output

      val fn_local : input1 -> input2 -> output
      val fn_global : input1 -> input2 -> output
    end

    (** Local and global 3-argument function. Provided by [Poly_fn3] functor, below. *)
    module type Poly_fn3 = sig
      type input1
      type input2
      type input3
      type output

      val fn_local : input1 -> input2 -> input3 -> output
      val fn_global : input1 -> input2 -> input3 -> output
    end

    module type Global = sig
      type 'a t
      [@@deriving compare ~localize, equal ~localize, hash, sexp ~localize, sexp_grammar]

      (** Globalize a [t]. Takes an argument because [[%globalize]] will pass one for
          ['a], but [globalize] is a no-op so it discards the argument. *)
      val globalize : _ -> 'a t -> 'a t

      (** Construct a [t]. Returns a global [t], which may be used as a local value. *)
      val wrap : 'a -> 'a t

      (** Access the contents of a [t]. Accepts a local [t], to which global values may be
          passed. *)
      val unwrap : 'a t -> 'a

      (** Transform the contents of a [t]. Accepts a local [t], to which global values may
          be passed. Returns a global [t], which may be used as a local value. *)
      val map : 'a t -> f:('a -> 'b) -> 'b t

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

      (** Require a value has a type that mode-crosses portability. This is useful for
          assisting type inference as well as improving error messages. *)
      external cross : 'a. 'a -> 'a = "%identity"

      (** Construct a [t]. *)
      external wrap : ('a[@local_opt]) -> ('a t[@local_opt]) = "%identity"

      (** Access the contents of a [t]. *)
      external unwrap : ('a t[@local_opt]) -> ('a[@local_opt]) = "%identity"

      (** Transform the contents of a [t]. *)
      val map : 'a t -> f:('a -> 'b) -> 'b t

      (** {2 Wrap and unwrap within other types}

          It can be safe to wrap or unwrap a [Portable.t] inside other types as a coercion
          rather than a conversion, i.e. without copying any data. Since the type system
          does not provide coercions for this, we provide explicit operations to convert
          as a no-op. We export them as [external ... = "%identity"] to make this clear.

          In general, it is safe to wrap or unwrap a type parameter ['a] in this way if
          the wrapping and unwrapping do not pass through any arrow type. *)

      (** Wrapping and unwrapping [List]. *)

      external wrap_list : ('a list[@local_opt]) -> ('a t list[@local_opt]) = "%identity"

      external unwrap_list
        :  ('a t list[@local_opt])
        -> ('a list[@local_opt])
        = "%identity"

      (** Wrapping and unwrapping [Iarray]. *)

      external wrap_iarray
        :  ('a iarray[@local_opt])
        -> ('a t iarray[@local_opt])
        = "%identity"

      external unwrap_iarray
        :  ('a t iarray[@local_opt])
        -> ('a iarray[@local_opt])
        = "%identity"

      (** Wrapping and unwrapping [Or_null]. *)

      external wrap_or_null
        :  ('a or_null[@local_opt])
        -> ('a t or_null[@local_opt])
        = "%identity"

      external unwrap_or_null
        :  ('a t or_null[@local_opt])
        -> ('a or_null[@local_opt])
        = "%identity"

      (** Wrapping and unwrapping [Option]. *)

      external wrap_option
        :  ('a option[@local_opt])
        -> ('a t option[@local_opt])
        = "%identity"

      external unwrap_option
        :  ('a t option[@local_opt])
        -> ('a option[@local_opt])
        = "%identity"

      (** Wrapping and unwrapping [Either]. *)

      external wrap_either
        :  (('a, 'b) Either0.t[@local_opt])
        -> (('a t, 'b t) Either0.t[@local_opt])
        = "%identity"

      external unwrap_either
        :  (('a t, 'b t) Either0.t[@local_opt])
        -> (('a, 'b) Either0.t[@local_opt])
        = "%identity"

      (** Wrapping and unwrapping [Either.First]. *)

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

      (** Wrapping and unwrapping [Either.Second]. *)

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

      (** Wrapping and unwrapping [Result]. *)

      external wrap_result
        :  (('a, 'b) Result.t[@local_opt])
        -> (('a t, 'b t) Result.t[@local_opt])
        = "%identity"

      external unwrap_result
        :  (('a t, 'b t) Result.t[@local_opt])
        -> (('a, 'b) Result.t[@local_opt])
        = "%identity"

      (** Wrapping and unwrapping [Result.Ok]. *)

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

      (** Wrapping and unwrapping [Result.Error]. *)

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
      val wrap_local : 'a -> ('a, local) t

      (** Unwrap local contents. Global or local contents can be unwrapped as local. *)
      val unwrap_local : ('a, _) t -> 'a

      (** Unwrap global contents. *)
      val unwrap_global : ('a, global) t -> 'a

      (** Convert phantom type of a wrapper to local. *)
      val to_local : ('a, _) t -> ('a, local) t

      (** Convert phantom type of a global wrapper to global. *)
      val to_global : ('a, _) t -> ('a, global) t

      (** Globalize a wrapper where the contents are already global. *)
      val globalize_global : ('a, global) t -> ('a, global) t
    end

    module type At_locality = sig
      type actually_local

      (** Phantom type parameter for {!t} which represents that the inhabitant is known to
          be [global]. *)
      type global = [ `global ] [@@immediate]

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
      type (+'a, +'locality) t

      include
        Without_crossing
        with type ('a, 'loc) t := ('a, 'loc) t
         and type actually_local := actually_local
         and type global := global
         and type local := local
    end
  end
end

module type Modes = sig
  (** Wrap values in the [global_] mode, even in a [local_] context. *)
  module Global : sig
    include module type of struct
      include Definitions.Global
    end

    type 'a t = { global : 'a } [@@unboxed]

    include Global with type 'a t := 'a t (** @inline *)
  end

  module Portable : sig
    include module type of struct
      include Definitions.Portable
    end

    type 'a t = { portable : 'a } [@@unboxed]

    include Portable with type 'a t := 'a t
  end

  module Contended : sig
    type 'a t = { contended : 'a } [@@unboxed]

    (** Require a value has a type that mode-crosses contention. This is useful for
        assisting type inference as well as improving error messages. *)
    external cross : 'a. 'a -> 'a = "%identity"
  end

  module Portended : sig
    type 'a t = { portended : 'a } [@@unboxed]
  end

  module Many : sig
    type 'a t = { many : 'a } [@@unboxed]
  end

  module Aliased : sig
    type 'a t = { aliased : 'a } [@@unboxed]
  end

  module Immutable_data : sig
    type 'a t = { immutable_data : 'a } [@@unboxed]
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
    type nonportable_

    (** Phantom type parameter for {!t} which represents that the inhabitant is known to
        be [portable]. *)
    type portable = [ `portable ]
    [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]
    [@@immediate]

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
    type (+!'a, +'portability) t
    [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]

    external wrap_portable : ('a[@local_opt]) -> (('a, _) t[@local_opt]) = "%identity"

    external unwrap_portable
      :  (('a, portable) t[@local_opt])
      -> ('a[@local_opt])
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
        type t =
          { init : (unit -> unit) Portable_via_contended.t
          ; users : string list
          }
      ]} *)
  module Portable_via_contended : sig
    type +'a t

    (** The [uncontended] annotations below are redundant, but present to emphasize their
        importance for safety. *)

    external wrap : ('a[@local_opt]) -> ('a t[@local_opt]) = "%identity"
    external unwrap : ('a t[@local_opt]) -> ('a[@local_opt]) = "%identity"

    (** If the contained value is of a type that crosses portability, it's safe to extract
        it, even if it has crossed a capsule boundary. *)
    external unwrap_contended : 'a. ('a t[@local_opt]) -> ('a[@local_opt]) = "%identity"
  end

  (** Can be [open]ed or [include]d to bring field names into scope. *)
  module Export : sig
    type 'a global = 'a Global.t = { global : 'a } [@@unboxed]
    type 'a portable = 'a Portable.t = { portable : 'a } [@@unboxed]
    type 'a contended = 'a Contended.t = { contended : 'a } [@@unboxed]
    type 'a portended = 'a Portended.t = { portended : 'a } [@@unboxed]
    type 'a many = 'a Many.t = { many : 'a } [@@unboxed]
    type 'a aliased = 'a Aliased.t = { aliased : 'a } [@@unboxed]
    type 'a immutable_data = 'a Immutable_data.t = { immutable_data : 'a } [@@unboxed]
  end
end
