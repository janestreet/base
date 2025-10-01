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
    type (+'focus, +'other) t

    include Monad.S2__local with type ('a, 'b) t := ('a, 'b) t
    include Applicative.S2__local with type ('a, 'b) t := ('a, 'b) t

    val value : ('a, _) t -> default:'a -> 'a
    val to_option : ('a, _) t -> 'a option
    val with_return : local_ (local_ 'a With_return.return -> 'b) -> ('a, 'b) t

    val combine
      :  ('a, 'd) t
      -> ('b, 'd) t
      -> f:local_ ('a -> 'b -> 'c)
      -> other:local_ ('d -> 'd -> 'd)
      -> ('c, 'd) t

    val combine_all : ('a, 'b) t list -> f:local_ ('b -> 'b -> 'b) -> ('a list, 'b) t
    val combine_all_unit : (unit, 'b) t list -> f:local_ ('b -> 'b -> 'b) -> (unit, 'b) t
  end
end

module type Either = sig @@ portable
  include module type of struct
    include Definitions
  end

  type ('f, 's) t = ('f, 's) Either0.t =
    | First of 'f
    | Second of 's

  include sig
      type%template ('f : kf, 's : ks) t = (('f, 's) Either0.t[@kind kf ks])
      [@@kind
        kf = (float64, bits32, bits64, word, immediate, immediate64, value)
        , ks = (float64, bits32, bits64, word, immediate, immediate64, value)]
      [@@deriving compare ~localize, equal ~localize, sexp ~stackify, sexp_grammar]
    end
    with type ('f, 's) t := ('f, 's) t

  [%%rederive:
    type nonrec ('f, 's) t = ('f, 's) t =
      | First of 'f
      | Second of 's
    [@@deriving globalize, hash]]

  include Invariant.S2 with type ('a, 'b) t := ('a, 'b) t

  val swap : ('f, 's) t -> ('s, 'f) t
  val value : ('a, 'a) t -> 'a
  val iter : ('a, 'b) t -> first:local_ ('a -> unit) -> second:local_ ('b -> unit) -> unit
  val value_map : ('a, 'b) t -> first:local_ ('a -> 'c) -> second:local_ ('b -> 'c) -> 'c

  val map
    :  ('a, 'b) t
    -> first:local_ ('a -> 'c)
    -> second:local_ ('b -> 'd)
    -> ('c, 'd) t

  module First : Focused with type ('a, 'b) t = ('a, 'b) t
  module Second : Focused with type ('a, 'b) t = ('b, 'a) t

  val is_first : (_, _) t -> bool
  val is_second : (_, _) t -> bool

  (** [first] and [second] are [First.return] and [Second.return]. *)
  val first : 'f -> ('f, _) t

  val second : 's -> (_, 's) t

  (**/**)

  module Export : sig
    type ('f, 's) _either = ('f, 's) t =
      | First of 'f
      | Second of 's
  end
end
