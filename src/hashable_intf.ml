open! Import0
module Sexp = Sexp0

module Definitions = struct
  (** We give a name to [Key__portable], even though it could normally be written as
      [sig @@ portable include Key end]. This is so [of_key__portable] can use it for
      first-class modules.

      @canonical Base.Hashable.Key *)
  module type%template
    [@modality p = (portable, nonportable)] [@mode m = (local, global)]
    Key = sig
    @@ p
    type t : any [@@deriving sexp_of]

    val compare : ([%compare: t][@mode.explicit m]) [@@mode m = (global, m)]

    (** This used to be documented to guarantee that hash functions don't return negative
        numbers, but as of 2026-04-16 this guarantee was seldom true. *)
    val hash : t -> int
  end
end

module type Hashable = sig @@ portable
  include module type of struct
    include Definitions
  end

  type ('a : any) t =
    { hash : 'a -> int
    ; compare : 'a -> 'a -> int
    ; sexp_of_t : 'a -> Sexp.t
    }

  val equal : 'a t -> 'a t -> bool
  val poly : 'a t

  val%template of_key
    : ('a : any).
    ((module Key with type t = 'a)[@modality p]) @ immutable -> 'a t @ p
  [@@modality p = (portable, nonportable)]

  val%template to_key
    : ('a : any).
    'a t @ p -> ((module Key with type t = 'a)[@modality p]) @ p
  [@@modality p = (portable, nonportable)]

  val hash_param : int -> int -> 'a -> int
  val hash : 'a -> int
end
