(** A type that represents values with two possibilities.

    [Either] can be seen as a generic sum type, the dual of [Tuple]. [First] is neither
    more important nor less important than [Second].

    Many functions in [Either] focus on just one constructor. The [Focused] signature
    abstracts over which constructor is the focus. To use these functions, use the [First]
    or [Second] modules in [S]. *)

open! Import

module Definitions = struct
  module type Focused = sig
    type (+'focus, +'other) t

    include%template
      Monad.S2
      [@kind value_or_null mod maybe_null] [@mode local]
      with type ('a, 'b) t := ('a, 'b) t

    include%template
      Applicative.S2
      [@kind value_or_null mod maybe_null] [@mode local]
      with type ('a, 'b) t := ('a, 'b) t

    val value : 'a 'b. ('a, 'b) t -> default:'a -> 'a
    val to_option : 'a 'b. ('a, 'b) t -> 'a option
    val with_return : 'a 'b. ('a With_return.return -> 'b) -> ('a, 'b) t

    val combine
      : 'a 'b 'c 'd.
      ('a, 'd) t
      -> ('b, 'd) t
      -> f:('a -> 'b -> 'c)
      -> other:('d -> 'd -> 'd)
      -> ('c, 'd) t

    val combine_all : 'a 'b. ('a, 'b) t list -> f:('b -> 'b -> 'b) -> ('a list, 'b) t
    val combine_all_unit : 'b. (unit, 'b) t list -> f:('b -> 'b -> 'b) -> (unit, 'b) t
  end
end

module type Either = sig
  include module type of struct
    include Definitions
  end

  type ('f, 's) t = ('f, 's) Either0.t =
    | First of 'f
    | Second of 's

  include sig
      type%template ('f, 's) t = (('f, 's) Either0.t[@kind kf ks])
      [@@kind kf = base_or_null_with_imm, ks = base_or_null_with_imm]
      [@@deriving compare ~localize, equal ~localize, sexp ~stackify, sexp_grammar]
    end
    with type ('f, 's) t := ('f, 's) t

  [%%rederive:
    type nonrec ('f, 's) t = ('f, 's) t =
      | First of 'f
      | Second of 's
    [@@deriving globalize, hash]]

  include Invariant.S2 with type ('a, 'b) t := ('a, 'b) t

  val swap : 'f 's. ('f, 's) t -> ('s, 'f) t
  val value : 'a. ('a, 'a) t -> 'a
  val iter : 'a 'b. ('a, 'b) t -> first:('a -> unit) -> second:('b -> unit) -> unit
  val value_map : 'a 'b 'c. ('a, 'b) t -> first:('a -> 'c) -> second:('b -> 'c) -> 'c
  val map : 'a 'b 'c 'd. ('a, 'b) t -> first:('a -> 'c) -> second:('b -> 'd) -> ('c, 'd) t

  module First : Focused with type ('a, 'b) t = ('a, 'b) t
  module Second : Focused with type ('a, 'b) t = ('b, 'a) t

  val is_first : 'f 's. ('f, 's) t -> bool
  val is_second : 'f 's. ('f, 's) t -> bool

  (** [first] and [second] are [First.return] and [Second.return]. *)
  val first : 'f 's. 'f -> ('f, 's) t

  val second : 'f 's. 's -> ('f, 's) t

  (**/**)

  module Export : sig
    type ('f, 's) _either = ('f, 's) t =
      | First of 'f
      | Second of 's
  end
end
