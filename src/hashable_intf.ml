open! Import
module Sexp = Sexp0

module Definitions = struct
  (** We give a name to [Key__portable], even though it could normally be written as
      [sig @@ portable include Key end]. This is so [of_key__portable] can use it for
      first-class modules.

      @canonical Base.Hashable.Key *)
  module type%template
    [@kind k = (value, float64, bits64)] [@modality p = (portable, nonportable)] Key = sig
    @@ p
    type t : k [@@deriving sexp_of]

    val compare : [%compare: t]

    (** Values returned by [hash] must be non-negative. An exception will be raised in the
        case that [hash] returns a negative value. *)
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

  val%template of_key : ((module Key with type t = 'a)[@kind k] [@modality p]) -> 'a t @ p
  [@@kind k = (value, float64, bits64)] [@@modality p = (portable, nonportable)]

  val%template to_key
    :  'a t @ p
    -> ((module Key with type t = 'a)[@kind k] [@modality p]) @ p
  [@@kind k = (value, float64, bits64)] [@@modality p = (portable, nonportable)]

  val hash_param : int -> int -> 'a -> int
  val hash : 'a -> int
end
