open! Import0
module Sexp = Sexp0

module Definitions = struct
  (** We give a name to [Key__portable], even though it could normally be written as
      [sig @@ portable include Key end]. This is so [of_key__portable] can use it for
      first-class modules.

      @canonical Base.Hashable.Key *)
  module type Key = sig
    type t [@@deriving sexp_of]

    val compare : [%compare: t]

    (** This used to be documented to guarantee that hash functions don't return negative
        numbers, but as of 2026-04-16 this guarantee was seldom true. *)
    val hash : t -> int
  end
end

module type Hashable = sig
  include module type of struct
    include Definitions
  end

  type 'a t =
    { hash : 'a -> int
    ; compare : 'a -> 'a -> int
    ; sexp_of_t : 'a -> Sexp.t
    }

  val equal : 'a t -> 'a t -> bool
  val poly : 'a t
  val of_key : 'a. (module Key with type t = 'a) -> 'a t
  val to_key : 'a. 'a t -> (module Key with type t = 'a)
  val hash_param : int -> int -> 'a -> int
  val hash : 'a -> int
end
