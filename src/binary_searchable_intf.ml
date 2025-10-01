(** Module types for a [binary_search] function for a sequence, and functors for building
    [binary_search] functions. *)

open! Import

module Definitions = struct
  [%%template
  [@@@mode.default m = (global, local)]

  (** An [Indexable] type is a finite sequence of elements indexed by consecutive integers
      [0] ... [length t - 1]. [get] and [length] must be O(1) for the resulting
      [binary_search] to be lg(n). *)
  module type Indexable = sig
    type elt
    type t

    val get : t -> int -> elt [@@mode m' = (global, m)]
    val length : t -> int [@@mode m' = (global, m)]
  end

  module type Indexable1 = sig
    type 'a t

    val get : 'a t -> int -> 'a [@@mode m' = (global, m)]
    val length : _ t -> int [@@mode m' = (global, m)]
  end
  [@@kind k = (value, immediate, immediate64, value mod external_, value mod external64)]]

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
    -> 't
    -> compare:('elt -> 'key -> int)
    -> Which_target_by_key.t
    -> 'key
    -> int option

  type ('t, 'elt) binary_search_segmented =
    ?pos:int
    -> ?len:int
    -> 't
    -> segment_of:('elt -> [ `Left | `Right ])
    -> Which_target_by_segment.t
    -> int option

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
    type 'a t

    (** See [Binary_search.binary_search] in binary_search.ml *)

    val binary_search : (('a t, 'a, 'key) binary_search[@mode m'])
    [@@mode m' = (global, m)]

    (** See [Binary_search.binary_search_segmented] in binary_search.ml *)

    val binary_search_segmented : (('a t, 'a) binary_search_segmented[@mode m'])
    [@@mode m' = (global, m)]
  end
  [@@kind k = (value, immediate, immediate64, value mod external_, value mod external64)]]
end

module type Binary_searchable = sig
  include module type of struct
    include Definitions
  end

  [%%template:
  [@@@mode.default m = (global, local)]

  module%template.portable Make (T : Indexable [@mode m]) :
    S [@mode m] with type t := T.t with type elt := T.elt

  module%template.portable Make1 (T : Indexable1 [@mode m] [@kind k]) :
    S1 [@mode m] [@kind k] with type 'a t := 'a T.t
  [@@kind k = (value, immediate, immediate64, value mod external_, value mod external64)]]
end
