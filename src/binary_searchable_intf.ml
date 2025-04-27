(** Module types for a [binary_search] function for a sequence, and functors for building
    [binary_search] functions. *)

open! Import

(** An [Indexable] type is a finite sequence of elements indexed by consecutive integers
    [0] ... [length t - 1]. [get] and [length] must be O(1) for the resulting
    [binary_search] to be lg(n). *)
module type Indexable = sig
  type elt
  type t

  val get : t -> int -> elt
  val length : t -> int
end

module type Indexable1 = sig
  type 'a t

  val get : 'a t -> int -> 'a
  val length : _ t -> int
end

module Which_target_by_key = struct
  type t =
    [ `Last_strictly_less_than (** [         | < elt X |                       ] *)
    | `Last_less_than_or_equal_to (** [      |      <= elt       X |           ] *)
    | `Last_equal_to (** [                             |   = elt X |           ] *)
    | `First_equal_to (** [                            | X = elt   |           ] *)
    | `First_greater_than_or_equal_to (** [            | X       >= elt      | ] *)
    | `First_strictly_greater_than (** [                           | X > elt | ] *)
    ]
  [@@deriving enumerate]
end

module Which_target_by_segment = struct
  type t =
    [ `Last_on_left
    | `First_on_right
    ]
  [@@deriving enumerate]
end

type ('t, 'elt, 'key) binary_search =
  ?pos:int
  -> ?len:int
  -> 't
  -> compare:local_ ('elt -> 'key -> int)
  -> Which_target_by_key.t
  -> 'key
  -> local_ int option

type ('t, 'elt) binary_search_segmented =
  ?pos:int
  -> ?len:int
  -> 't
  -> segment_of:local_ ('elt -> [ `Left | `Right ])
  -> Which_target_by_segment.t
  -> local_ int option

module type S = sig
  type elt
  type t

  (** See [Binary_search.binary_search] in binary_search.ml *)
  val binary_search : (t, elt, 'key) binary_search

  (** See [Binary_search.binary_search_segmented] in binary_search.ml *)
  val binary_search_segmented : (t, elt) binary_search_segmented
end

module type S1 = sig
  type 'a t

  val binary_search : ('a t, 'a, 'key) binary_search
  val binary_search_segmented : ('a t, 'a) binary_search_segmented
end

module type Binary_searchable = sig @@ portable
  module type S = S
  module type S1 = S1
  module type Indexable = Indexable
  module type Indexable1 = Indexable1

  module Which_target_by_key = Which_target_by_key
  module Which_target_by_segment = Which_target_by_segment

  type nonrec ('t, 'elt, 'key) binary_search = ('t, 'elt, 'key) binary_search
  type nonrec ('t, 'elt) binary_search_segmented = ('t, 'elt) binary_search_segmented

  module%template.portable Make (T : Indexable) :
    S with type t := T.t with type elt := T.elt

  module%template.portable Make1 (T : Indexable1) : S1 with type 'a t := 'a T.t
end
