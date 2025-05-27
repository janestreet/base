(** Comparison and serialization for a type, using a witness type to distinguish between
    comparison functions with different behavior. *)

open! Import
module Sexp = Sexp0

module Definitions (T : sig
    type (_, _) comparator
  end) =
struct
  open T

  [%%template
  [@@@modality.default p = (nonportable, portable)]

  module type S = sig
    type t
    type comparator_witness : value mod p

    val comparator : (t, comparator_witness) comparator
  end

  module type S1 = sig
    type 'a t
    type comparator_witness : value mod p

    val comparator : ('a t, comparator_witness) comparator
  end

  module type S_fc = sig
    type comparable_t

    include S [@modality p] with type t := comparable_t
  end

  module type Derived = sig
    type 'a t
    type !'cmp comparator_witness : value mod p with 'cmp

    val comparator : ('a, 'cmp) comparator -> ('a t, 'cmp comparator_witness) comparator
  end

  module type Derived2 = sig
    type ('a, 'b) t
    type (!'cmp_a, !'cmp_b) comparator_witness : value mod p with 'cmp_a with 'cmp_b

    val comparator
      :  ('a, 'cmp_a) comparator
      -> ('b, 'cmp_b) comparator
      -> (('a, 'b) t, ('cmp_a, 'cmp_b) comparator_witness) comparator
  end

  module type Derived_phantom = sig
    type ('a, 'b) t
    type !'cmp comparator_witness : value mod p with 'cmp

    val comparator
      :  ('a, 'cmp) comparator
      -> (('a, _) t, 'cmp comparator_witness) comparator
  end

  module type Derived2_phantom = sig
    type ('a, 'b, 'c) t
    type (!'cmp_a, !'cmp_b) comparator_witness : value mod p with 'cmp_a with 'cmp_b

    val comparator
      :  ('a, 'cmp_a) comparator
      -> ('b, 'cmp_b) comparator
      -> (('a, 'b, _) t, ('cmp_a, 'cmp_b) comparator_witness) comparator
  end]
end

module type Comparator = sig @@ portable
  (** [('a, 'witness) t] contains a comparison function for values of type ['a]. Two
      values of type [t] with the same ['witness] are guaranteed to have the same
      comparison function.

      In OxCaml, [('a, 'witness) t] additionally tracks whether or not the underlying
      comparison function is portable using the ['witness] parameter - if the ['witness]
      type crosses portability, then the comparison function is known to be portable. *)
  type ('a, 'witness) t : value mod contended portable with 'witness @@ contended

  (*_ See the SAFETY comment in the .ml file *)

  val compare : ('a, 'witness) t -> 'a -> 'a -> int
  val sexp_of_t : ('a, 'witness) t -> 'a -> Sexp.t

  module T : sig
    type ('a, 'b) comparator = ('a, 'b) t
  end

  include module type of struct
    include T
    include Definitions (T)
  end

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

  [%%template:
  [@@@mode.default m = (local, global)]

  (** [Make] creates a [comparator] value and its phantom [comparator_witness] type for a
      nullary type. *)
  module%template.portable
    [@modality p] Make (M : sig
      type t [@@deriving (compare [@mode m]), sexp_of]
    end) : S [@modality p] with type t := M.t

  (** [Make1] creates a [comparator] value and its phantom [comparator_witness] type for a
      unary type. It takes a [compare] and [sexp_of_t] that have non-standard types
      because the [Comparator.t] type doesn't allow passing in additional values for the
      type argument. *)
  module%template.portable
    [@modality p] Make1 (M : sig
      type 'a t

      val compare : 'a t @ m -> 'a t @ m -> int [@@mode m = (global, m)]
      val sexp_of_t : _ t -> Sexp.t
    end) : S1 [@modality p] with type 'a t := 'a M.t

  (** [Derived] creates a [comparator] function that constructs a comparator for the type
      ['a t] given a comparator for the type ['a]. *)
  module%template.portable
    [@modality p] Derived (M : sig
      type 'a t [@@deriving (compare [@mode m]), sexp_of]
    end) : Derived [@modality p] with type 'a t := 'a M.t

  (** [Derived2] creates a [comparator] function that constructs a comparator for the type
      [('a, 'b) t] given comparators for the type ['a] and ['b]. *)
  module%template.portable
    [@modality p] Derived2 (M : sig
      type ('a, 'b) t [@@deriving (compare [@mode m]), sexp_of]
    end) : Derived2 [@modality p] with type ('a, 'b) t := ('a, 'b) M.t

  (** [Derived_phantom] creates a [comparator] function that constructs a comparator for
      the type [('a, 'b) t] given a comparator for the type ['a]. *)
  module%template.portable
    [@modality p] Derived_phantom (M : sig
      type ('a, 'b) t

      val compare : ('a @ m -> 'a @ m -> int) -> ('a, 'b) t @ m -> ('a, 'b) t @ m -> int
      [@@mode m = (global, m)]

      val sexp_of_t : ('a -> Sexp.t) -> ('a, _) t -> Sexp.t
    end) : Derived_phantom [@modality p] with type ('a, 'b) t := ('a, 'b) M.t

  (** [Derived2_phantom] creates a [comparator] function that constructs a comparator for
      the type [('a, 'b, 'c) t] given a comparator for the types ['a] and ['b]. *)
  module%template.portable
    [@modality p] Derived2_phantom (M : sig
      type ('a, 'b, 'c) t

      val compare
        :  ('a @ m -> 'a @ m -> int)
        -> ('b @ m -> 'b @ m -> int)
        -> ('a, 'b, 'c) t @ m
        -> ('a, 'b, 'c) t @ m
        -> int
      [@@mode m = (global, m)]

      val sexp_of_t : ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b, _) t -> Sexp.t
    end) : Derived2_phantom [@modality p] with type ('a, 'b, 'c) t := ('a, 'b, 'c) M.t]
end
