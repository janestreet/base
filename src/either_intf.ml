(** A type that represents values with two possibilities.

    [Either] can be seen as a generic sum type, the dual of [Tuple]. [First] is neither
    more important nor less important than [Second].

    Many functions in [Either] focus on just one constructor. The [Focused] signature
    abstracts over which constructor is the focus. To use these functions, use the [First]
    or [Second] modules in [S]. *)

[@@@warning "-incompatible-with-upstream"]

open! Import

module Definitions = struct
  module type Focused = sig
    type (+'focus : value_or_null, +'other) t

    include%template
      Monad.S2
      [@kind value_or_null mod maybe_null] [@mode local]
      with type ('a : value_or_null, 'b) t := ('a, 'b) t

    include%template
      Applicative.S2
      [@kind value_or_null mod maybe_null] [@mode local]
      with type ('a : value_or_null, 'b) t := ('a, 'b) t

    val value : ('a : value_or_null) 'b. ('a, 'b) t -> default:'a -> 'a
    val to_option : ('a : value_or_null) 'b. ('a, 'b) t -> 'a option

    val with_return
      : ('a : value_or_null) 'b.
      local_ (local_ 'a With_return.return -> 'b) -> ('a, 'b) t

    val combine
      : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null) 'd.
      ('a, 'd) t
      -> ('b, 'd) t
      -> f:local_ ('a -> 'b -> 'c)
      -> other:local_ ('d -> 'd -> 'd)
      -> ('c, 'd) t

    val combine_all
      : ('a : value_or_null) 'b.
      ('a, 'b) t list -> f:local_ ('b -> 'b -> 'b) -> ('a list, 'b) t

    val combine_all_unit
      : 'b.
      (unit, 'b) t list -> f:local_ ('b -> 'b -> 'b) -> (unit, 'b) t
  end
end

module type Either = sig @@ portable
  include module type of struct
    include Definitions
  end

  type ('f : value_or_null, 's : value_or_null) t = ('f, 's) Either0.t =
    | First of 'f
    | Second of 's

  include sig
      type%template ('f : kf, 's : ks) t = (('f, 's) Either0.t[@kind kf ks])
      [@@kind kf = base_or_null, ks = base_or_null]
      [@@deriving compare ~localize, equal ~localize, sexp ~stackify, sexp_grammar]
    end
    with type ('f : value_or_null, 's : value_or_null) t := ('f, 's) t

  [%%rederive:
    type nonrec ('f : value_or_null, 's : value_or_null) t = ('f, 's) t =
      | First of 'f
      | Second of 's
    [@@deriving globalize, hash]]

  include Invariant.S2 with type ('a, 'b) t := ('a, 'b) t

  val swap : ('f : value_or_null) ('s : value_or_null). ('f, 's) t -> ('s, 'f) t
  val value : ('a : value_or_null). ('a, 'a) t -> 'a

  val iter
    : ('a : value_or_null) ('b : value_or_null).
    ('a, 'b) t -> first:local_ ('a -> unit) -> second:local_ ('b -> unit) -> unit

  val value_map
    : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
    ('a, 'b) t -> first:local_ ('a -> 'c) -> second:local_ ('b -> 'c) -> 'c

  val map
    : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null) ('d : value_or_null).
    ('a, 'b) t -> first:local_ ('a -> 'c) -> second:local_ ('b -> 'd) -> ('c, 'd) t

  module First : Focused with type ('a : value_or_null, 'b) t = ('a, 'b) t
  module Second : Focused with type ('a : value_or_null, 'b) t = ('b, 'a) t

  val is_first : ('f : value_or_null) ('s : value_or_null). ('f, 's) t -> bool
  val is_second : ('f : value_or_null) ('s : value_or_null). ('f, 's) t -> bool

  (** [first] and [second] are [First.return] and [Second.return]. *)
  val first : ('f : value_or_null) ('s : value_or_null). 'f -> ('f, 's) t

  val second : ('f : value_or_null) ('s : value_or_null). 's -> ('f, 's) t

  (**/**)

  module Export : sig
    type ('f : value_or_null, 's : value_or_null) _either = ('f, 's) t =
      | First of 'f
      | Second of 's
  end
end
