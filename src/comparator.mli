@@ portable

(** Comparison and serialization for a type, using a witness type to distinguish between
    comparison functions with different behavior. *)

open! Import
module Sexp := Sexp0

(** [('a, 'witness) t] contains a comparison function for values of type ['a]. Two values
    of type [t] with the same ['witness] are guaranteed to have the same comparison
    function.

    In OxCaml, [('a, 'witness) t] additionally tracks whether or not the underlying
    comparison function is portable using the ['witness] parameter - if the ['witness]
    type crosses portability, then the comparison function is known to be portable. *)
type ('a, 'witness) t : value mod contended portable with 'witness @@ contended

(*_ See the SAFETY comment in the .ml file *)

val compare : ('a, 'witness) t -> 'a -> 'a -> int
val sexp_of_t : ('a, 'witness) t -> 'a -> Sexp.t

type ('a, 'b) comparator = ('a, 'b) t

module type S = sig
  type t
  type comparator_witness : value

  val comparator : (t, comparator_witness) comparator
end

module type%template [@modality portable] S = sig @@ portable
  type comparator_witness : value mod portable

  include S with type comparator_witness := comparator_witness
end

module type S1 = sig
  type 'a t
  type comparator_witness

  val comparator : ('a t, comparator_witness) comparator
end

module type%template [@modality portable] S1 = sig @@ portable
  type comparator_witness : value mod portable

  include S1 with type comparator_witness := comparator_witness
end

module type%template S_fc = sig @@ p
  type comparable_t

  include S [@modality p] with type t := comparable_t
end
[@@modality p = (nonportable, portable)]

(** [make] creates a comparator witness for the given comparison. It is intended as a
    lightweight alternative to the functors below, to be used like so:

    {[
      include (val Comparator.make ~compare ~sexp_of_t)
    ]} *)
val%template make
  :  compare:('a -> 'a -> int) @ p
  -> sexp_of_t:('a -> Sexp.t) @ p
  -> ((module S_fc with type comparable_t = 'a)[@mode p])
[@@mode p = (nonportable, portable)]

module%template Poly : S1 [@modality portable] with type 'a t = 'a

module Module : sig
  (** First-class module providing a comparator and witness type. *)
  type ('a, 'b) t = (module S with type t = 'a and type comparator_witness = 'b)
end

val of_module : ('a, 'b) Module.t -> ('a, 'b) t
val to_module : ('a, 'b) t -> ('a, 'b) Module.t

module%template.portable S_to_S1 (S : S) :
  S1 with type 'a t = S.t with type comparator_witness = S.comparator_witness

(** [Make] creates a [comparator] value and its phantom [comparator_witness] type for a
    nullary type. *)
module%template.portable
  [@modality p] Make (M : sig
    type t [@@deriving compare, sexp_of]
  end) : S [@modality p] with type t := M.t

(** [Make1] creates a [comparator] value and its phantom [comparator_witness] type for a
    unary type. It takes a [compare] and [sexp_of_t] that have non-standard types because
    the [Comparator.t] type doesn't allow passing in additional values for the type
    argument. *)
module%template.portable
  [@modality p] Make1 (M : sig
    type 'a t

    val compare : 'a t -> 'a t -> int
    val sexp_of_t : _ t -> Sexp.t
  end) : S1 [@modality p] with type 'a t := 'a M.t

module type Derived = sig
  type 'a t
  type !'cmp comparator_witness

  val comparator : ('a, 'cmp) comparator -> ('a t, 'cmp comparator_witness) comparator
end

(** [Derived] creates a [comparator] function that constructs a comparator for the type
    ['a t] given a comparator for the type ['a]. *)
module%template.portable Derived (M : sig
    type 'a t [@@deriving compare, sexp_of]
  end) : Derived with type 'a t := 'a M.t

module type Derived2 = sig
  type ('a, 'b) t
  type (!'cmp_a, !'cmp_b) comparator_witness

  val comparator
    :  ('a, 'cmp_a) comparator
    -> ('b, 'cmp_b) comparator
    -> (('a, 'b) t, ('cmp_a, 'cmp_b) comparator_witness) comparator
end

(** [Derived2] creates a [comparator] function that constructs a comparator for the type
    [('a, 'b) t] given comparators for the type ['a] and ['b]. *)
module%template.portable Derived2 (M : sig
    type ('a, 'b) t [@@deriving compare, sexp_of]
  end) : Derived2 with type ('a, 'b) t := ('a, 'b) M.t

module type Derived_phantom = sig
  type ('a, 'b) t
  type 'cmp comparator_witness

  val comparator
    :  ('a, 'cmp) comparator
    -> (('a, _) t, 'cmp comparator_witness) comparator
end

(** [Derived_phantom] creates a [comparator] function that constructs a comparator for the
    type [('a, 'b) t] given a comparator for the type ['a]. *)
module%template.portable Derived_phantom (M : sig
    type ('a, 'b) t

    val compare : ('a -> 'a -> int) -> ('a, 'b) t -> ('a, 'b) t -> int
    val sexp_of_t : ('a -> Sexp.t) -> ('a, _) t -> Sexp.t
  end) : Derived_phantom with type ('a, 'b) t := ('a, 'b) M.t

module type Derived2_phantom = sig
  type ('a, 'b, 'c) t
  type (!'cmp_a, !'cmp_b) comparator_witness

  val comparator
    :  ('a, 'cmp_a) comparator
    -> ('b, 'cmp_b) comparator
    -> (('a, 'b, _) t, ('cmp_a, 'cmp_b) comparator_witness) comparator
end

(** [Derived2_phantom] creates a [comparator] function that constructs a comparator for
    the type [('a, 'b, 'c) t] given a comparator for the types ['a] and ['b]. *)

module%template.portable Derived2_phantom (M : sig
    type ('a, 'b, 'c) t

    val compare
      :  ('a -> 'a -> int)
      -> ('b -> 'b -> int)
      -> ('a, 'b, 'c) t
      -> ('a, 'b, 'c) t
      -> int

    val sexp_of_t : ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b, _) t -> Sexp.t
  end) : Derived2_phantom with type ('a, 'b, 'c) t := ('a, 'b, 'c) M.t
