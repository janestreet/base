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
module type Capsule = sig
  include module type of struct
    include Definitions
  end

  module Password : sig
    type 'k t = 'k Capsule.Password.t
  end

  module Data : sig
    type ('a, 'k) t = ('a, 'k) Capsule.Data.t

    (** These functions are the most common way to interact with capsules. *)

    val create : (unit -> 'a) -> ('a, 'k) t

    (** Retrieve a value using the state stored in a capsule. *)
    val get : 'a 'k 'b. ('a, 'k) t -> f:('a -> 'b) -> password:'k Password.t -> 'b

    (** Like [get], for types that do not cross portability and contention. *)
    val get_contended : ('a, 'k) t -> f:('a -> 'b) -> password:'k Password.t -> 'b

    (** A constrained form of [get] specialized to return [unit]. *)
    val iter : ('a, 'k) t -> f:('a -> unit) -> password:'k Password.t -> unit

    (** These functions enable more complicated manipulation of capsules. *)

    val return : 'a -> ('a, 'k) t
    val both : ('a, 'k) t -> ('b, 'k) t -> ('a * 'b, 'k) t
    val fst : ('a * _, 'k) t -> ('a, 'k) t
    val snd : (_ * 'b, 'k) t -> ('b, 'k) t
    val map : ('a, 'k) t -> f:('a -> 'b) -> password:'k Password.t -> ('b, 'k) t

    val bind
      :  ('a, 'k1) t
      -> f:('a -> ('b, 'k2) t)
      -> password:'k1 Password.t
      -> ('b, 'k2) t

    (** Retrieve the value in a capsule directly. Likely only useful if the capsule has
        already been [map]'d, as capsules do not usually contain portable values. *)
    val get_id : 'a 'k. ('a, 'k) t -> 'a

    (** Like [get_id], for types that do not cross contention. *)
    val get_id_contended : ('a, 'k) t -> 'a
  end

  module Mutex : sig
    type 'k t = 'k Capsule.Mutex.t

    type packed = Capsule.Mutex.packed = P : 'k t -> packed
    [@@unboxed] [@@unsafe_allow_any_mode_crossing]

    (** Creates a mutex with a fresh existential key type. *)
    val create : unit -> packed

    (** Like [create]. Useful in module definitions, where GADTs cannot be unpacked. *)
    module Create () : With_mutex

    val with_lock : 'k t -> f:('k Password.t -> 'a) -> 'a
  end

  module Expert = Basement.Capsule
end
