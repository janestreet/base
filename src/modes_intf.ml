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
  end
end

module type Modes = sig
  (** Wrap values in the [global_] mode, even in a [local_] context. *)
  module Global : sig
    include module type of struct
      include Definitions.Global
    end

    type 'a t = { global_ global : 'a }
    [@@unboxed]
    [@@deriving_inline compare ~localize, equal ~localize, hash, sexp, sexp_grammar]

    include Ppx_compare_lib.Comparable.S1 with type 'a t := 'a t
    include Ppx_compare_lib.Comparable.S_local1 with type 'a t := 'a t
    include Ppx_compare_lib.Equal.S1 with type 'a t := 'a t
    include Ppx_compare_lib.Equal.S_local1 with type 'a t := 'a t
    include Ppx_hash_lib.Hashable.S1 with type 'a t := 'a t
    include Sexplib0.Sexpable.S1 with type 'a t := 'a t

    val t_sexp_grammar : 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t

    [@@@end]

    (** Globalize a [t]. Takes an argument because [[%globalize]] will pass one for ['a],
        but [globalize] is a no-op so it discards the argument. *)
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
        does not provide coercions for this, we provide explicit operations to convert as
        a no-op. We export them as [external ... = "%identity"] to make this clear.

        The [wrap_*] functions produce global values, which may be used as local values.

        The [unwrap_*] functions produce local or global values, depending on which they
        are passed. If used for globals, the result may be used as a local value. If used
        for locals, the result must be local because the outer constructor may not be used
        as a global value.

        In general, it is safe to wrap or unwrap a type parameter ['a] in this way if the
        wrapping and unwrapping do not pass through any arrow type. *)

    (** Wrapping and unwrapping [List]. *)

    external wrap_list : 'a list -> 'a t list = "%identity"
    external unwrap_list : ('a t list[@local_opt]) -> ('a list[@local_opt]) = "%identity"

    (** Wrapping and unwrapping [Option]. *)

    external wrap_option : 'a option -> 'a t option = "%identity"

    external unwrap_option
      :  ('a t option[@local_opt])
      -> ('a option[@local_opt])
      = "%identity"

    (** Wrapping and unwrapping [Either]. *)

    external wrap_either : ('a, 'b) Either.t -> ('a t, 'b t) Either.t = "%identity"

    external unwrap_either
      :  (('a t, 'b t) Either.t[@local_opt])
      -> (('a, 'b) Either.t[@local_opt])
      = "%identity"

    (** Wrapping and unwrapping [Either.First]. *)

    external wrap_first : ('a, 'b) Either.t -> ('a t, 'b) Either.t = "%identity"

    external unwrap_first
      :  (('a t, 'b) Either.t[@local_opt])
      -> (('a, 'b) Either.t[@local_opt])
      = "%identity"

    (** Wrapping and unwrapping [Either.Second]. *)

    external wrap_second : ('a, 'b) Either.t -> ('a, 'b t) Either.t = "%identity"

    external unwrap_second
      :  (('a, 'b t) Either.t[@local_opt])
      -> (('a, 'b) Either.t[@local_opt])
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
        be local, and may need to be [globalize]d. Outputs it produces will be global, and
        will be allocated on the heap. To avoid these performance concerns, you will need
        to actually write two versions of the function. *)

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

  (** Can be [open]ed or [include]d to bring field names into scope. *)
  module Export : sig
    type 'a _global = 'a Global.t = { global_ global : 'a } [@@unboxed]
  end
end
