(** Module types for a [binary_search] function for a sequence, and functors for building
    [binary_search] functions. *)

open! Import

[@@@warning "-incompatible-with-upstream"]

[%%template
[@@@kind_set.define values = (value, value mod external64)]

module Definitions = struct
  [%%template
  [@@@mode.default m = (global, local)]

  (** An [Indexable] type is a finite sequence of elements indexed by consecutive integers
      [0] ... [length t - 1]. [get] and [length] must be O(1) for the resulting
      [binary_search] to be lg(n). *)
  module type Indexable = sig
    type elt
    type t

    val get : t @ m' -> int -> elt @ m' [@@mode m' = (global, m)]
    val length : t @ m' -> int [@@mode m' = (global, m)]
  end

  module type Indexable1 = sig
    type ('a : k) t

    val get : 'a t @ m' -> int -> 'a @ m' [@@mode m' = (global, m)]
    val length : _ t @ m' -> int [@@mode m' = (global, m)]
  end
  [@@kind k = values]]

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

  [%%template
  [@@@mode.default m = (global, local)]

  type ('t, 'elt, 'key) binary_search =
    ?pos:int
    -> ?len:int
    -> 't @ m
    -> compare:local_ ('elt @ m -> 'key @ m -> int)
    -> Which_target_by_key.t
    -> 'key @ m
    -> local_ int option

  type ('t, 'elt) binary_search_segmented =
    ?pos:int
    -> ?len:int
    -> 't @ m
    -> segment_of:local_ ('elt @ m -> [ `Left | `Right ])
    -> Which_target_by_segment.t
    -> local_ int option

  module type S = sig
    type elt
    type t

    (** See [Binary_search.binary_search] in binary_search.ml *)

    val binary_search : ((t, elt, 'key) binary_search[@mode m']) [@@mode m' = (global, m)]

    (** See [Binary_search.binary_search_segmented] in binary_search.ml *)

    val binary_search_segmented : ((t, elt) binary_search_segmented[@mode m'])
    [@@mode m' = (global, m)]
  end

  module type S1 = sig
    type ('a : k) t

    (** See [Binary_search.binary_search] in binary_search.ml *)

    val binary_search : (('a t, 'a, 'key) binary_search[@mode m'])
    [@@mode m' = (global, m)]

    (** See [Binary_search.binary_search_segmented] in binary_search.ml *)

    val binary_search_segmented : (('a t, 'a) binary_search_segmented[@mode m'])
    [@@mode m' = (global, m)]
  end
  [@@kind k = values]]
end

module type Binary_searchable = sig @@ portable
  include module type of struct
    include Definitions
  end

  [%%template:
  [@@@mode.default m = (global, local)]

  module%template.portable Make (T : Indexable [@mode m]) :
    S [@mode m] with type t := T.t with type elt := T.elt

  module%template.portable Make1 (T : Indexable1 [@mode m] [@kind k]) :
    S1 [@mode m] [@kind k] with type 'a t := 'a T.t
  [@@kind k = values]]
end]
