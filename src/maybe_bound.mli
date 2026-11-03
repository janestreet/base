@@ portable

(** Used for specifying a bound (either upper or lower) as inclusive, exclusive, or
    unbounded. *)

open! Import0
module List := List0

type interval_comparison =
  | Below_lower_bound
  | In_range
  | Above_upper_bound
[@@deriving sexp ~stackify, sexp_grammar, compare ~localize, hash]

[%%template:
[@@@kind k = base_or_null]

include sig
  [@@@implicit_kind: ('a : k)]
  [@@@implicit_kind: ('b : k)]
  [@@@kind.default k]

  type 'a t =
    | Incl of 'a
    | Excl of 'a
    | Unbounded
  [@@deriving sexp ~stackify, sexp_grammar, globalize]

  val all : ('a List.Constructors.t[@kind k]) -> ('a t[@kind k]) list
  val map : ('a t[@kind k]) -> f:('a -> 'b) @ local -> ('b t[@kind k])

  val is_lower_bound
    :  ('a t[@kind k])
    -> of_:'a
    -> compare:('a -> 'a -> int) @ local
    -> bool

  val is_upper_bound
    :  ('a t[@kind k])
    -> of_:'a
    -> compare:('a -> 'a -> int) @ local
    -> bool

  (** [interval_contains_exn ~lower ~upper x ~compare] raises if [lower] and [upper] are
      crossed. *)
  val interval_contains_exn
    :  lower:('a t[@kind k])
    -> upper:('a t[@kind k])
    -> 'a
    -> compare:('a -> 'a -> int) @ local
    -> bool

  (** [bounds_crossed ~lower ~upper ~compare] returns true if [lower > upper].

      It ignores whether the bounds are [Incl] or [Excl]. *)
  val bounds_crossed
    :  lower:('a t[@kind k])
    -> upper:('a t[@kind k])
    -> compare:('a -> 'a -> int) @ local
    -> bool

  (** [compare_to_interval_exn ~lower ~upper x ~compare] raises if [lower] and [upper] are
      crossed. *)
  val compare_to_interval_exn
    :  lower:('a t[@kind k])
    -> upper:('a t[@kind k])
    -> 'a
    -> compare:('a -> 'a -> int) @ local
    -> interval_comparison
end]
