open! Import
module Capsule = Basement.Capsule

module Definitions = struct
  module type With_mutex = sig
    type k

    val mutex : k Capsule.Mutex.t
  end
end

(** For now, see [Basement.Capsule] for documentation of capsule types.

    This module provides an interface to capsules that is a small subset of
    [Basement.Capsule], starting with the most common entry points. The interface is also
    cleaned up to be somewhat easier to use, and more consistent with other conventions in
    [Base].

    Over time we will provide more of [Basement.Capsule]'s functionality. *)
module type Capsule = sig @@ portable
  include module type of struct
    include Definitions
  end

  module Password : sig
    type 'k t : value mod contended portable = 'k Capsule.Password.t
  end

  module Data : sig
    type ('a, 'k) t : value mod contended portable = ('a, 'k) Capsule.Data.t

    (** These functions are the most common way to interact with capsules. *)

    val create : (unit -> 'a) @ local portable -> ('a, 'k) t

    (** Retrieve a value using the state stored in a capsule. *)
    val get
      : 'a 'k ('b : value mod contended portable).
      ('a, 'k) t -> f:('a -> 'b) @ local portable -> password:'k Password.t @ local -> 'b

    (** Like [get], for types that do not cross portability and contention. *)
    val get_contended
      :  ('a, 'k) t
      -> f:('a -> 'b @ contended portable) @ local portable
      -> password:'k Password.t @ local
      -> 'b @ contended portable

    (** A constrained form of [get] specialized to return [unit]. *)
    val iter
      :  ('a, 'k) t
      -> f:('a -> unit) @ local portable
      -> password:'k Password.t @ local
      -> unit

    (** These functions enable more complicated manipulation of capsules. *)

    val return : ('a : value mod contended) @ portable -> ('a, 'k) t
    val both : ('a, 'k) t -> ('b, 'k) t -> ('a * 'b, 'k) t
    val fst : ('a * _, 'k) t -> ('a, 'k) t
    val snd : (_ * 'b, 'k) t -> ('b, 'k) t

    val map
      :  ('a, 'k) t
      -> f:('a -> 'b) @ local portable
      -> password:'k Password.t @ local
      -> ('b, 'k) t

    val bind
      :  ('a, 'k1) t
      -> f:('a -> ('b, 'k2) t) @ local portable
      -> password:'k1 Password.t @ local
      -> ('b, 'k2) t

    (** Retrieve the value in a capsule directly. Likely only useful if the capsule has
        already been [map]'d, as capsules do not usually contain portable values. *)
    val get_id : ('a : value mod contended portable) 'k. ('a, 'k) t -> 'a

    (** Like [get_id], for types that do not cross contention. *)
    val get_id_contended : ('a : value mod portable, 'k) t -> 'a @ contended
  end

  module Mutex : sig
    type 'k t : value mod contended portable = 'k Capsule.Mutex.t

    type packed : value mod contended portable = Capsule.Mutex.packed =
      | P : 'k t -> packed
    [@@unboxed] [@@unsafe_allow_any_mode_crossing]

    (** Creates a mutex with a fresh existential key type. *)
    val create : unit -> packed

    (** Like [create]. Useful in module definitions, where GADTs cannot be unpacked. *)
    module Create () : With_mutex

    val with_lock : 'k t -> f:('k Password.t @ local -> 'a) @ local -> 'a
  end

  module Isolated : sig @@ portable
    (** A value isolated within its own capsule.

        A primary use-case for this type is to use aliasing as a proxy for contention.
        [unique] access to an ['a Capsule.Isolated.t] allows [uncontended] access to the
        underlying ['a]. [aliased] access to an ['a Capsule.Isolated.t] allows [shared]
        access to the underlying ['a]. *)
    type 'a t : value mod contended portable

    (** [create f] runs [f] within a fresh capsule, and creates a [Capsule.Isolated.t]
        containing the result. *)
    val create : (unit -> 'a) @ local portable -> 'a t @ unique

    (** [with_unique t ~f] takes a [unique] isolated capsule [t], calls [f] with its
        value, and returns a tuple of the unique isolated capsule and the result of [f]. *)
    val with_unique
      : 'a ('b : value mod contended portable).
      'a t @ unique
      -> f:('a -> 'b) @ local once portable
      -> 'a t * 'b Modes.Aliased.t @ unique

    (** Like [with_unique], but with the most general mode annotations. *)
    val with_unique_gen
      :  'a t @ unique
      -> f:('a -> 'b @ contended portable unique) @ local once portable
      -> 'a t * 'b @ contended portable unique

    (** [with_unique t ~f] takes an [aliased] isolated capsule [t], calls [f] with shared
        access to its value, and returns a tuple of the unique isolated capsule and the
        result of [f]. *)
    val with_shared
      : ('a : value mod portable) ('b : value mod contended portable).
      'a t -> f:('a @ shared -> 'b) @ local once portable -> 'b

    (** Like [with_shared], but with the most general mode annotations. *)
    val with_shared_gen
      : ('a : value mod portable) 'b.
      'a t
      -> f:('a @ shared -> 'b @ contended portable) @ local once portable
      -> 'b @ contended portable
  end

  module Expert = Basement.Capsule
end
