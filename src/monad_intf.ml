open! Import

module Definitions = struct
  [%%template
  [@@@kind.default k = (value, value_or_null mod maybe_null)]
  [@@@mode.default m = (global, local)]

  (** Module types below provide both global and local versions. In OxCaml, the latter
      accept local [~f] closures. *)

  (** Monad operations. A monad abstracts the notion of composable, sequenced
      computations. An ['a t] represents a computation returning ['a].

      This module type subsumes the other [S*_kernel] module types below. It is extended
      with infix operators and support for [let%bind] and [let%map] in [S*] below. *)
  module type S3_indexed_kernel = sig
    (** Monadic type. ['a] is the contained value. Monadic operations compose the index
        types ['i] and ['j]. For example, ['i, 'j] composed with ['j, 'k] yields ['i, 'k].
        ['p] and ['q] are extra parameters unchanged by monadic operations. *)
    type ('a : k, 'i, 'j, 'p, 'q) t : k

    (** Convert a value to a [t]. *)
    val return : ('a : k) 'i 'p 'q. 'a -> ('a, 'i, 'i, 'p, 'q) t

    (** Transforms the contents of a [t]. *)
    val map
      : ('a : k) ('b : k) 'i 'j 'p 'q.
      ('a, 'i, 'j, 'p, 'q) t -> f:('a -> 'b) @ m -> ('b, 'i, 'j, 'p, 'q) t

    (** Sequences computations. [bind t ~f] computes [f v] for value(s) [v] in [t].
        Well-behaved monads satisfy these "laws" (where [( >>= )] is the infix [bind]
        operator):

        - [map t ~f] is equivalent to [bind t ~f:(fun x -> return (f x))]
        - [return x >>= f] is equivalent to [f x]
        - [t >>= return] is equivalent to [t]
        - [(t >>= f) >>= g] is equivalent to [t >>= fun x -> f x >>= g] *)
    val bind
      : ('a : k) ('b : k) 'i 'j 'k 'p 'q.
      ('a, 'i, 'j, 'p, 'q) t
      -> f:('a -> ('b, 'j, 'k, 'p, 'q) t) @ m
      -> ('b, 'i, 'k, 'p, 'q) t

    (** Combines nested [t] into just one layer. Equivalent to [bind t ~f:Fn.id]. *)
    val join
      : ('a : k) 'i 'j 'k 'p 'q.
      (('a, 'j, 'k, 'p, 'q) t, 'i, 'j, 'p, 'q) t -> ('a, 'i, 'k, 'p, 'q) t

    (** Ignores contained values of [t]. Equivalent to [map t ~f:ignore]. *)
    val ignore_m
      : ('a : k) 'i 'j 'p 'q.
      ('a, 'i, 'j, 'p, 'q) t -> (unit, 'i, 'j, 'p, 'q) t

    (** Combines a list of [t]. *)
    val all
      : ('a : k) 'i 'p 'q.
      ('a, 'i, 'i, 'p, 'q) t list -> ('a list, 'i, 'i, 'p, 'q) t

    (** Combines a list of [t] whose contents are unimportant. *)
    val all_unit : 'i 'p 'q. (unit, 'i, 'i, 'p, 'q) t list -> (unit, 'i, 'i, 'p, 'q) t
  end

  (** Monad operations for monads with one type parameter. *)
  module type S_kernel = sig
    type ('a : k) t : k

    include S3_indexed_kernel [@kind k] [@mode m] with type ('a : k, _, _, _, _) t := 'a t
  end

  (** Monad operations for monads with two type parameters. *)
  module type S2_kernel = sig
    type ('a : k, 'p) t : k

    include
      S3_indexed_kernel
      [@kind k] [@mode m]
      with type ('a : k, _, _, 'p, _) t := ('a, 'p) t
  end

  (** Monad operations for monads with three type parameters. *)
  module type S3_kernel = sig
    type ('a : k, 'p, 'q) t : k

    include
      S3_indexed_kernel
      [@kind k] [@mode m]
      with type ('a : k, _, _, 'p, 'q) t := ('a, 'p, 'q) t
  end

  (** Monad operations for monads with index type parameters. *)
  module type S_indexed_kernel = sig
    type ('a : k, 'i, 'j) t : k

    include
      S3_indexed_kernel
      [@kind k] [@mode m]
      with type ('a : k, 'i, 'j, _, _) t := ('a, 'i, 'j) t
  end

  (** Infix operators. This module type subsumes the other [Index*] types below. *)
  module type Infix3_indexed = sig
    type ('a : k, 'i, 'j, 'p, 'q) t : k

    (** Infix [bind]. *)
    val ( >>= )
      : ('a : k) ('b : k) 'i 'j 'k 'p 'q.
      ('a, 'i, 'j, 'p, 'q) t
      -> ('a -> ('b, 'j, 'k, 'p, 'q) t) @ m
      -> ('b, 'i, 'k, 'p, 'q) t

    (** Infix [map]. *)
    val ( >>| )
      : ('a : k) ('b : k) 'i 'j 'p 'q.
      ('a, 'i, 'j, 'p, 'q) t -> ('a -> 'b) @ m -> ('b, 'i, 'j, 'p, 'q) t
  end

  (** Infix operators for monads with one type parameter. *)
  module type Infix = sig
    type ('a : k) t : k

    include Infix3_indexed [@kind k] [@mode m] with type ('a : k, _, _, _, _) t := 'a t
  end

  (** Infix operators for monads with two type parameters. *)
  module type Infix2 = sig
    type ('a : k, 'p) t : k

    include
      Infix3_indexed [@kind k] [@mode m] with type ('a : k, _, _, 'p, _) t := ('a, 'p) t
  end

  (** Infix operators for monads with three type parameters. *)
  module type Infix3 = sig
    type ('a : k, 'p, 'q) t : k

    include
      Infix3_indexed
      [@kind k] [@mode m]
      with type ('a : k, _, _, 'p, 'q) t := ('a, 'p, 'q) t
  end

  (** Infix operators for monads with index type parameters. *)
  module type Infix_indexed = sig
    type ('a : k, 'i, 'j) t : k

    include
      Infix3_indexed
      [@kind k] [@mode m]
      with type ('a : k, 'i, 'j, _, _) t := ('a, 'i, 'j) t
  end

  (** Supports [let%bind] and [let%map] syntax. See [ppx_let] documentation.

      This module type subsumes the other [Syntax*] module types below. *)
  module type Syntax3_indexed = sig
    type ('a : k, 'i, 'j, 'p, 'q) t : k

    module Let_syntax : sig
      val return : ('a : k) 'i 'p 'q. 'a -> ('a, 'i, 'i, 'p, 'q) t

      include
        Infix3_indexed
        [@kind k] [@mode m]
        with type ('a : k, 'i, 'j, 'p, 'q) t := ('a, 'i, 'j, 'p, 'q) t

      module Let_syntax : sig
        val return : ('a : k) 'i 'p 'q. 'a -> ('a, 'i, 'i, 'p, 'q) t

        val bind
          : ('a : k) ('b : k) 'i 'j 'k 'p 'q.
          ('a, 'i, 'j, 'p, 'q) t
          -> f:('a -> ('b, 'j, 'k, 'p, 'q) t) @ m
          -> ('b, 'i, 'k, 'p, 'q) t

        val map
          : ('a : k) ('b : k) 'i 'j 'p 'q.
          ('a, 'i, 'j, 'p, 'q) t -> f:('a -> 'b) @ m -> ('b, 'i, 'j, 'p, 'q) t

        val both
          : ('a : k) ('b : k) 'i 'j 'k 'p 'q.
          ('a, 'i, 'j, 'p, 'q) t -> ('b, 'j, 'k, 'p, 'q) t -> ('a * 'b, 'i, 'k, 'p, 'q) t

        module Open_on_rhs : sig end
      end
    end
  end

  (** Supports [let%bind] and [let%map] syntax for monads with one type parameter. *)
  module type Syntax = sig
    type ('a : k) t : k

    include Syntax3_indexed [@kind k] [@mode m] with type ('a : k, _, _, _, _) t := 'a t
  end

  (** Supports [let%bind] and [let%map] syntax for monads with two type parameters. *)
  module type Syntax2 = sig
    type ('a : k, 'p) t : k

    include
      Syntax3_indexed [@kind k] [@mode m] with type ('a : k, _, _, 'p, _) t := ('a, 'p) t
  end

  (** Supports [let%bind] and [let%map] syntax for monads with three type parameters. *)
  module type Syntax3 = sig
    type ('a : k, 'p, 'q) t : k

    include
      Syntax3_indexed
      [@kind k] [@mode m]
      with type ('a : k, _, _, 'p, 'q) t := ('a, 'p, 'q) t
  end

  (** Supports [let%bind] and [let%map] syntax for monads with index type parameters. *)
  module type Syntax_indexed = sig
    type ('a : k, 'i, 'j) t : k

    include
      Syntax3_indexed
      [@kind k] [@mode m]
      with type ('a : k, 'i, 'j, _, _) t := ('a, 'i, 'j) t
  end

  (** Monad interface except for [let%bind] and [let%map] support.

      This module type subsumes the other [S*_without_syntax] types below. *)
  module type S3_indexed_without_syntax = sig
    include S3_indexed_kernel [@kind k] [@mode m]

    include
      Infix3_indexed
      [@kind k] [@mode m]
      with type ('a : k, 'i, 'j, 'p, 'q) t := ('a, 'i, 'j, 'p, 'q) t

    module Monad_infix :
      Infix3_indexed
      [@kind k] [@mode m]
      with type ('a : k, 'i, 'j, 'p, 'q) t := ('a, 'i, 'j, 'p, 'q) t
  end

  (** Monad interface except for [let%bind] and [let%map] support for monads with one type
      parameter. *)
  module type S_without_syntax = sig
    type ('a : k) t : k

    include
      S3_indexed_without_syntax
      [@kind k] [@mode m]
      with type ('a : k, _, _, _, _) t := 'a t
  end

  (** Monad interface except for [let%bind] and [let%map] support for monads with two type
      parameters. *)
  module type S2_without_syntax = sig
    type ('a : k, 'p) t : k

    include
      S3_indexed_without_syntax
      [@kind k] [@mode m]
      with type ('a : k, _, _, 'p, _) t := ('a, 'p) t
  end

  (** Monad interface except for [let%bind] and [let%map] support for monads with three
      type parameters. *)
  module type S3_without_syntax = sig
    type ('a : k, 'p, 'q) t : k

    include
      S3_indexed_without_syntax
      [@kind k] [@mode m]
      with type ('a : k, _, _, 'p, 'q) t := ('a, 'p, 'q) t
  end

  (** Monad interface except for [let%bind] and [let%map] support for monads with index
      type parameters. *)
  module type S_indexed_without_syntax = sig
    type ('a : k, 'i, 'j) t : k

    include
      S3_indexed_without_syntax
      [@kind k] [@mode m]
      with type ('a : k, 'i, 'j, _, _) t := ('a, 'i, 'j) t
  end

  (** Complete monad interface. Extends [_kernel] with infix operators and support for
      [let%bind] and [let%map].

      This module type subsumes the other [S*] types below. *)
  module type S3_indexed = sig
    include S3_indexed_without_syntax [@kind k] [@mode m]

    include
      Syntax3_indexed
      [@kind k] [@mode m]
      with type ('a : k, 'i, 'j, 'p, 'q) t := ('a, 'i, 'j, 'p, 'q) t
  end

  (** Complete monad interface with one type parameter. *)
  module type S = sig
    type ('a : k) t : k

    include S3_indexed [@kind k] [@mode m] with type ('a : k, _, _, _, _) t := 'a t
  end

  (** Complete monad interface with two type parameters. *)
  module type S2 = sig
    type ('a : k, 'p) t : k

    include S3_indexed [@kind k] [@mode m] with type ('a : k, _, _, 'p, _) t := ('a, 'p) t
  end

  (** Complete monad interface with three type parameters. *)
  module type S3 = sig
    type ('a : k, 'p, 'q) t : k

    include
      S3_indexed [@kind k] [@mode m] with type ('a : k, _, _, 'p, 'q) t := ('a, 'p, 'q) t
  end

  (** Complete monad interface with index type parameters. *)
  module type S_indexed = sig
    type ('a : k, 'i, 'j) t : k

    include
      S3_indexed [@kind k] [@mode m] with type ('a : k, 'i, 'j, _, _) t := ('a, 'i, 'j) t
  end

  (** Argument to [Make*]. This module type subsumes the other [Basic*] types below. *)
  module type Basic3_indexed = sig
    type ('a : k, 'i, 'j, 'p, 'q) t : k

    val return : ('a : k) 'i 'p 'q. 'a -> ('a, 'i, 'i, 'p, 'q) t

    (** [`Define_using_bind] defines [map] as [bind t ~f:(fun x -> return x)]. [`Custom]
        overrides the default, presumably with something more efficient.

        Some functions returned by [Make*] are defined in terms of [map]. Providing an
        efficient [map] improves them as well. *)
    val map
      : ('a : k) ('b : k) 'i 'j 'p 'q.
      [ `Define_using_bind
      | `Custom of ('a, 'i, 'j, 'p, 'q) t -> f:('a -> 'b) @ m -> ('b, 'i, 'j, 'p, 'q) t
      ]

    val bind
      : ('a : k) ('b : k) 'i 'j 'k 'p 'q.
      ('a, 'i, 'j, 'p, 'q) t
      -> f:('a -> ('b, 'j, 'k, 'p, 'q) t) @ m
      -> ('b, 'i, 'k, 'p, 'q) t
  end

  (** Argument to [Make] for monads with one type parameter. *)
  module type Basic = sig
    type ('a : k) t : k

    include Basic3_indexed [@kind k] [@mode m] with type ('a : k, _, _, _, _) t := 'a t
  end

  (** Argument to [Make2] for monads with two type parameters. *)
  module type Basic2 = sig
    type ('a : k, 'p) t : k

    include
      Basic3_indexed [@kind k] [@mode m] with type ('a : k, _, _, 'p, _) t := ('a, 'p) t
  end

  (** Argument to [Make3] for monads with three type parameters. *)
  module type Basic3 = sig
    type ('a : k, 'p, 'q) t : k

    include
      Basic3_indexed
      [@kind k] [@mode m]
      with type ('a : k, _, _, 'p, 'q) t := ('a, 'p, 'q) t
  end

  (** Argument to [Make_indexed] for monads with index type parameters. *)
  module type Basic_indexed = sig
    type ('a : k, 'i, 'j) t : k

    include
      Basic3_indexed
      [@kind k] [@mode m]
      with type ('a : k, 'i, 'j, _, _) t := ('a, 'i, 'j) t
  end]
end

module type Monad = sig @@ portable
  include module type of struct
    include Definitions
  end

  [%%template:
  (** The identity monad. Useful as an argument to functors that require a monad, to
      produce a non-monadic result. *)
  module Ident :
    S
    [@kind value_or_null mod maybe_null] [@mode local]
    with type ('a : value_or_null) t = 'a

  [@@@kind.default k = (value, value_or_null mod maybe_null)]
  [@@@mode.default m = (global, local)]

  (** Produces a monad with one type parameter. *)
  module%template.portable Make (X : Basic [@kind k] [@mode m]) :
    S [@kind k] [@mode m] with type ('a : k) t := 'a X.t

  (** Produces a monad with two type parameters. *)
  module%template.portable Make2 (X : Basic2 [@kind k] [@mode m]) :
    S2 [@kind k] [@mode m] with type ('a : k, 'p) t := ('a, 'p) X.t

  (** Produces a monad with three type parameters. *)
  module%template.portable Make3 (X : Basic3 [@kind k] [@mode m]) :
    S3 [@kind k] [@mode m] with type ('a : k, 'p, 'q) t := ('a, 'p, 'q) X.t

  (** Produces a monad with index type parameters. *)
  module%template.portable Make_indexed (X : Basic_indexed [@kind k] [@mode m]) :
    S_indexed [@kind k] [@mode m] with type ('a : k, 'i, 'j) t := ('a, 'i, 'j) X.t

  (** Converts between monadic types with one type parameter. *)
  module%template.portable Of_monad
      (Monad : S
    [@kind k] [@mode m])
      (M : sig
         type ('a : k) t : k

         val to_monad : ('a : k). 'a t -> 'a Monad.t
         val of_monad : ('a : k). 'a Monad.t -> 'a t
       end) : S [@kind k] [@mode m] with type ('a : k) t := 'a M.t

  (** Converts between monadic types with two type parameters. *)
  module%template.portable Of_monad2
      (Monad : S2
    [@kind k] [@mode m])
      (M : sig
         type ('a : k, 'p) t : k

         val to_monad : ('a : k) 'p. ('a, 'p) t -> ('a, 'p) Monad.t
         val of_monad : ('a : k) 'p. ('a, 'p) Monad.t -> ('a, 'p) t
       end) : S2 [@kind k] [@mode m] with type ('a : k, 'p) t := ('a, 'p) M.t

  (** Converts between monadic types with three type parameters. *)
  module%template.portable Of_monad3
      (Monad : S3
    [@kind k] [@mode m])
      (M : sig
         type ('a : k, 'p, 'q) t : k

         val to_monad : ('a : k) 'p 'q. ('a, 'p, 'q) t -> ('a, 'p, 'q) Monad.t
         val of_monad : ('a : k) 'p 'q. ('a, 'p, 'q) Monad.t -> ('a, 'p, 'q) t
       end) : S3 [@kind k] [@mode m] with type ('a : k, 'p, 'q) t := ('a, 'p, 'q) M.t

  (** Converts between monadic types with index type parameters. *)
  module%template.portable Of_monad_indexed
      (Monad : S_indexed
    [@kind k] [@mode m])
      (M : sig
         type ('a : k, 'i, 'j) t : k

         val to_monad : ('a : k) 'i 'j. ('a, 'i, 'j) t -> ('a, 'i, 'j) Monad.t
         val of_monad : ('a : k) 'i 'j. ('a, 'i, 'j) Monad.t -> ('a, 'i, 'j) t
       end) :
    S_indexed [@kind k] [@mode m] with type ('a : k, 'i, 'j) t := ('a, 'i, 'j) M.t]
end
