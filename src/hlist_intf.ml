open! Import

module Definitions = struct
  module type S = sig
    (** The type of elements of the heterogeneous list, e.g. ['a elt = 'a option]. *)
    type 'a elt

    (** Heterogenous list with elements' types determined by ['a]. The type parameter of
        the heterogeneous list determines the ['a] of each ['a elt], combined with [->]
        and ending in [unit].

        Arrow types are chosen for human-friendly printing. This does put each ['a] in a
        negative position; [t] is invariant due to GADT restrictions so putting ['a] in
        positive position would not change the variance of [t].

        Uses list constructor syntax so that values can be written like [[ a; b; c ]]. *)
    type _ t =
      | [] : unit t
      | ( :: ) : 'head elt * 'tail t -> ('head -> 'tail) t
  end

  (** Polymorphic function passed to [Map(_)(_).map]; see below. *)
  module Map_f (Input : S) (Output : S) = struct
    type t = { f : 'a. 'a Input.elt -> 'a Output.elt } [@@unboxed]
  end

  (** Polymorphic function passed to [Map2(_)(_)(_).map2]; see below. *)
  module Map2_f (Input1 : S) (Input2 : S) (Output : S) = struct
    type t = { f : 'a. 'a Input1.elt -> 'a Input2.elt -> 'a Output.elt } [@@unboxed]
  end

  (** Polymorphic function passed to [Lift_map(_)(_)(_).lift_map]; see below. *)
  module Lift_map_f (Liftable : T1) (Input : S) (Output : S) = struct
    type t = { f : 'a. 'a Input.elt -> 'a Output.elt Liftable.t } [@@unboxed]
  end

  (** Subset of [Applicative.S] used by [Lift] and [Lift_map], below.

      To use with [Monad.S], see [Liftable_monad], below. We could use [map] and [both] to
      accommodate both module types, but when available, a single [map2] can be more
      efficient than [both] + [map]. *)
  module type Applicative_like = sig
    type 'a t

    val return : 'a -> 'a t
    val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  end

  (** Subset of monad interface that [Liftable_monad] uses to implement
      [Applicative_like]. *)
  module type Monad_like = sig
    type 'a t

    val return : 'a -> 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t
  end
end

(** Heterogeneous list datatype. Can be specialized for elements ['a elt] to be of any
    unary type, e.g. ['a option] or ['a -> Sexp.t]. *)
module type Hlist = sig @@ portable
  include module type of struct
    include Definitions
  end

  (** [Hlist.t] is a heterogenous list with arbitrary element types. *)
  include S with type 'a elt = 'a

  (** Can be used to implement a type, or just to refer to it in a type expression, e.g.
      [Make(M).t] *)
  module Make (Elt : T1) : S with type 'a elt = 'a Elt.t

  (** Creates a [map] function from a heterogeneous list of one element type to another.
      Leaves each element's ['a] the same, but can map, e.g., from [_ option] to [_ list]. *)
  module Map (Input : S) (Output : S) : sig
    val map : 'a Input.t -> f:Map_f(Input)(Output).t -> 'a Output.t
  end

  (** Creates a [map2] for combining two heterogeneous lists with corresponding elements. *)
  module Map2 (Input1 : S) (Input2 : S) (Output : S) : sig
    val map2
      :  'a Input1.t
      -> 'a Input2.t
      -> f:Map2_f(Input1)(Input2)(Output).t
      -> 'a Output.t
  end

  (** Implements [map2] using [both] and [map]. Allows using [Lift] and [Lift_map] with a
      module satisfying [Monad.S] but not [Applicative.S]. *)
  module Liftable_monad (Make : Monad_like) : Applicative_like with type 'a t = 'a Make.t

  (** Lifts an applicative or monad type out of a heterogeneous list. *)
  module Lift
      (Liftable : Applicative_like)
      (Inner : S)
      (Outer : S with type 'a elt = 'a Inner.elt Liftable.t) : sig
    val lift : 'a Outer.t -> 'a Inner.t Liftable.t
  end

  (** Maps an applicative or monad function over a heterogenous list, lifting the
      applicative or monad out of each element to the whole result. *)
  module Lift_map (Liftable : Applicative_like) (Input : S) (Output : S) : sig
    val lift_map
      :  'a Input.t
      -> f:Lift_map_f(Liftable)(Input)(Output).t
      -> 'a Output.t Liftable.t
  end

  (** Specialized modules for 1-, 2-, and 3-element heterogenous lists. Allows derivers to
      be applied naturally. Can be useful for, e.g., testing. *)

  module T1 : sig
    type nonrec 'a t = ('a -> unit) t
    [@@deriving
      compare ~localize, equal ~localize, globalize, hash, sexp ~stackify, sexp_grammar]
  end

  module T2 : sig
    type nonrec ('a, 'b) t = ('a -> 'b -> unit) t
    [@@deriving
      compare ~localize, equal ~localize, globalize, hash, sexp ~stackify, sexp_grammar]
  end

  module T3 : sig
    type nonrec ('a, 'b, 'c) t = ('a -> 'b -> 'c -> unit) t
    [@@deriving
      compare ~localize, equal ~localize, globalize, hash, sexp ~stackify, sexp_grammar]
  end

  module T4 : sig
    type nonrec ('a, 'b, 'c, 'd) t = ('a -> 'b -> 'c -> 'd -> unit) t
    [@@deriving
      compare ~localize, equal ~localize, globalize, hash, sexp ~stackify, sexp_grammar]
  end
end
