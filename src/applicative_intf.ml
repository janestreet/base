(** Applicatives model computations in which values computed by subcomputations cannot
    affect what subsequent computations will take place.

    Relative to monads, this restriction takes power away from the user of the interface
    and gives it to the implementation. In particular, because the structure of the entire
    computation is known, one can augment its definition with some description of that
    structure.

    For more information, see:

    {v
      Applicative Programming with Effects.
      Conor McBride and Ross Paterson.
      Journal of Functional Programming 18:1 (2008), pages 1-13.
      http://staff.city.ac.uk/~ross/papers/Applicative.pdf
    v} *)

open! Import

module Definitions = struct
  [%%template
  [@@@kind.default k = (value, value_or_null mod maybe_null)]
  [@@@mode.default m = (global, local), p = (nonportable, portable)]

  (** Module types below provide both global and local versions. In OxCaml, the latter
      accept local [~f] closures.

      Both [local] and [portable] versions are designed with a strict data structure in
      mind. The types won't make sense for Applicatives whose evaluation is delayed (e.g.
      most monads, Command.Param, or anything like a database query applicative). *)

  (** Applicative operations. An applicative abstracts the notion of computations whose
      results can be combined. An ['a t] represents a computation returning ['a].

      This module type subsumes the other [S*_kernel] module types below. It is extended
      with infix operators in [S*] below. *)
  module type S3_kernel = sig
    (** Applicative type. ['a] is the result type. ['p] and ['q] are extra parameters
        unchanged by applicative operations. *)
    type ('a : k, 'p, 'q) t : k

    (** Convert a value to a [t]. *)
    val return : ('a : k) 'p 'q. 'a @ p -> ('a, 'p, 'q) t @ p

    (** Applies the functions in one [t] to the values in another. Well-behaved
        applicatives satisfy these "laws", using [<*>] as infix [apply]:

        - [return Fn.id <*> t] is equivalent to [t]
        - [return Fn.compose <*> tf <*> tg <*> tx] is equivalent to [tf <*> (tg <*> tx)]
        - [return f <*> return x] is equivalent to [return (f x)]
        - [tf <*> return x] is equivalent to [return (fun f -> f x) <*> tf] *)
    val apply
      : ('a : k) ('b : k) 'p 'q.
      ('a @ p -> 'b @ p, 'p, 'q) t @ p -> ('a, 'p, 'q) t @ p -> ('b, 'p, 'q) t @ p

    (** Combines values in two [t]s as tuples. Using [<*>] as infix [apply], equivalent to
        [return (fun a b -> a, b) <*> ta <*> tb]. *)
    val both
      : ('a : k) ('b : k) 'p 'q.
      ('a, 'p, 'q) t @ p -> ('b, 'p, 'q) t @ p -> ('a * 'b, 'p, 'q) t @ p

    (** Transforms the contents of a [t]. Using [<*>] as infix [apply], equivalent to
        [return f <*> t]. *)
    val map
      : ('a : k) ('b : k) 'p 'q.
      ('a, 'p, 'q) t @ p -> f:('a @ p -> 'b @ p) @ m -> ('b, 'p, 'q) t @ p

    (** Combines the contents of two [t]s. Using [<*>] as infix [apply], equivalent to
        [return f <*> ta <*> tb]. *)
    val map2
      : ('a : k) ('b : k) ('c : k) 'p 'q.
      ('a, 'p, 'q) t @ p
      -> ('b, 'p, 'q) t @ p
      -> f:('a @ p -> 'b @ p -> 'c @ p) @ m
      -> ('c, 'p, 'q) t @ p

    (** Combines the contents of three [t]s. Using [<*>] as infix [apply], equivalent to
        [return f <*> ta <*> tb <*> tc]. *)
    val map3
      : ('a : k) ('b : k) ('c : k) ('d : k) 'p 'q.
      ('a, 'p, 'q) t @ p
      -> ('b, 'p, 'q) t @ p
      -> ('c, 'p, 'q) t @ p
      -> f:('a @ p -> 'b @ p -> 'c @ p -> 'd @ p) @ m
      -> ('d, 'p, 'q) t @ p

    (** Combines a list of [t]. *)
    val all : ('a : k) 'p 'q. ('a, 'p, 'q) t list @ p -> ('a list, 'p, 'q) t @ p

    (** Combines a list of [t] whose contents are unimportant. *)
    val all_unit : 'p 'q. (unit, 'p, 'q) t list @ p -> (unit, 'p, 'q) t @ p
  end

  (** Applicative operations for applicatives with two type parameters. *)
  module type S2_kernel = sig
    type ('a : k, 'p) t : k

    include S3_kernel [@kind k] [@mode m p] with type ('a : k, 'p, _) t := ('a, 'p) t
  end

  (** Applicative operations for applicatives with one type parameter. *)
  module type S_kernel = sig
    type ('a : k) t : k

    include S3_kernel [@kind k] [@mode m p] with type ('a : k, _, _) t := 'a t
  end

  (** Infix operators. This module type subsumes the other [Index*] types below. *)
  module type Applicative_infix3 = sig
    type ('a : k, 'p, 'q) t : k

    val ( <*> )
      : ('a : k) ('b : k) 'p 'q.
      ('a @ p -> 'b @ p, 'p, 'q) t @ p -> ('a, 'p, 'q) t @ p -> ('b, 'p, 'q) t @ p

    val ( <* )
      : ('a : k) 'p 'q.
      ('a, 'p, 'q) t @ p -> (unit, 'p, 'q) t @ p -> ('a, 'p, 'q) t @ p

    val ( *> )
      : ('a : k) 'p 'q.
      (unit, 'p, 'q) t @ p -> ('a, 'p, 'q) t @ p -> ('a, 'p, 'q) t @ p

    val ( >>| )
      : ('a : k) ('b : k) 'p 'q.
      ('a, 'p, 'q) t @ p -> ('a @ p -> 'b @ p) @ m -> ('b, 'p, 'q) t @ p
  end

  (** Infix operators for applicatives with two type parameters. *)
  module type Applicative_infix2 = sig
    type ('a : k, 'p) t : k

    include
      Applicative_infix3 [@kind k] [@mode m p] with type ('a : k, 'p, _) t := ('a, 'p) t
  end

  (** Infix operators for applicatives with one type parameter. *)
  module type Applicative_infix = sig
    type ('a : k) t : k

    include Applicative_infix3 [@kind k] [@mode m p] with type ('a : k, _, _) t := 'a t
  end

  (** Complete applicative interface. Extends [_kernel] with infix operators.

      This module type subsumes the other [S*] types below. *)
  module type S3 = sig
    type ('a : k, 'p, 'q) t : k

    include S3_kernel [@kind k] [@mode m p] with type ('a : k, 'p, 'q) t := ('a, 'p, 'q) t

    include
      Applicative_infix3
      [@kind k] [@mode m p]
      with type ('a : k, 'p, 'q) t := ('a, 'p, 'q) t

    module Applicative_infix :
      Applicative_infix3
      [@kind k] [@mode m p]
      with type ('a : k, 'p, 'q) t := ('a, 'p, 'q) t
  end

  (** Complete applicative interface with two type parameters. *)
  module type S2 = sig
    type ('a : k, 'p) t : k

    include S3 [@kind k] [@mode m p] with type ('a : k, 'p, _) t := ('a, 'p) t
  end

  (** Complete applicative interface with one type parameter. *)
  module type S = sig
    type ('a : k) t : k

    include S3 [@kind k] [@mode m p] with type ('a : k, _, _) t := 'a t
  end

  (** Supports [let%map] syntax. See [ppx_let] documentation.

      This module type subsumes the other [Let_syntax*] module types below. *)
  module type Let_syntax3 = sig
    type ('a : k, 'p, 'q) t : k

    module Open_on_rhs_intf : sig
      module type S
    end

    module Let_syntax : sig
      val return : ('a : k) 'p 'q. 'a -> ('a : k, 'p, 'q) t

      include
        Applicative_infix3
        [@kind k] [@mode m]
        with type ('a : k, 'p, 'q) t := ('a, 'p, 'q) t

      module Let_syntax : sig
        val return : ('a : k) 'p 'q. 'a -> ('a, 'p, 'q) t

        val map
          : ('a : k) ('b : k) 'p 'q.
          ('a, 'p, 'q) t -> f:('a -> 'b) @ m -> ('b, 'p, 'q) t

        val both
          : ('a : k) ('b : k) 'p 'q.
          ('a, 'p, 'q) t -> ('b, 'p, 'q) t -> ('a * 'b, 'p, 'q) t

        module Open_on_rhs : Open_on_rhs_intf.S
      end
    end
  end

  (** Supports [let%map] syntax for applicatives with two type parameters. *)
  module type Let_syntax2 = sig
    type ('a : k, 'p) t : k

    include Let_syntax3 [@kind k] [@mode m] with type ('a : k, 'p, _) t := ('a, 'p) t
  end

  (** Supports [let%map] syntax for applicatives with one type parameter. *)
  module type Let_syntax = sig
    type ('a : k) t : k

    include Let_syntax3 [@kind k] [@mode m] with type ('a : k, _, _) t := 'a t
  end

  (** [Lazy_applicative] is an applicative whose structure may be computed on-demand,
      instead of being constructed up-front. This is useful when implementing traversals
      over large data structures, where otherwise we have to pay O(n) up-front cost both
      in time and in memory. *)
  module type Lazy_applicative = sig
    include S [@kind k] [@mode m]

    val of_thunk : ('a : k). (unit -> 'a t) -> 'a t
  end

  (** Argument to [Make_let_syntax3].

      This module type subsumes the other [For_let_syntax*] types below. *)
  module type For_let_syntax3 = sig
    type ('a : k, 'p, 'q) t : k

    val return : ('a : k) 'p 'q. 'a -> ('a, 'p, 'q) t

    val map
      : ('a : k) ('b : k) 'p 'q.
      ('a, 'p, 'q) t -> f:('a -> 'b) @ m -> ('b, 'p, 'q) t

    val both
      : ('a : k) ('b : k) 'p 'q.
      ('a, 'p, 'q) t -> ('b, 'p, 'q) t -> ('a * 'b, 'p, 'q) t

    include
      Applicative_infix3
      [@kind k] [@mode m]
      with type ('a : k, 'p, 'q) t := ('a, 'p, 'q) t
  end

  (** Argument to [Make_let_syntax2]. *)
  module type For_let_syntax2 = sig
    type ('a : k, 'p) t : k

    include For_let_syntax3 [@kind k] [@mode m] with type ('a : k, 'p, _) t := ('a, 'p) t
  end

  (** Argument to [Make_let_syntax]. *)
  module type For_let_syntax = sig
    type ('a : k) t : k

    include For_let_syntax3 [@kind k] [@mode m] with type ('a : k, _, _) t := 'a t
  end

  (** Argument to [Make3_using_map2].

      This module type subsumes the other [Make*_using_map2] types below. *)
  module type Basic3_using_map2 = sig
    type ('a : k, 'p, 'q) t : k

    val return : ('a : k) 'p 'q. 'a -> ('a, 'p, 'q) t

    val map2
      : ('a : k) ('b : k) ('c : k) 'p 'q.
      ('a, 'p, 'q) t -> ('b, 'p, 'q) t -> f:('a -> 'b -> 'c) @ m -> ('c, 'p, 'q) t

    (** [`Define_using_map2] defines [map] as [map2 (return ()) t ~f:(fun () x -> x)].
        [`Custom] overrides the default.

        Some functions returned by [Make*_using_map2] are defined in terms of [map].
        Providing an efficient [map] improves them as well. *)
    val map
      : ('a : k) ('b : k) 'p 'q.
      [ `Define_using_map2
      | `Custom of ('a, 'p, 'q) t -> f:('a -> 'b) @ m -> ('b, 'p, 'q) t
      ]
  end

  (** Argument to [Make_using_map2]. *)
  module type Basic_using_map2 = sig
    type ('a : k) t : k

    include Basic3_using_map2 [@kind k] [@mode m] with type ('a : k, _, _) t := 'a t
  end

  (** Argument to [Make2_using_map2]. *)
  module type Basic2_using_map2 = sig
    type ('a : k, 'p) t : k

    include
      Basic3_using_map2 [@kind k] [@mode m] with type ('a : k, 'p, _) t := ('a, 'p) t
  end]

  [%%template
  [@@@kind.default k = (value, value_or_null mod maybe_null)]

  module type Basic3 = sig
    type ('a : k, 'p, 'q) t : k

    val return : ('a : k) 'p 'q. 'a -> ('a, 'p, 'q) t

    val apply
      : ('a : k) ('b : k) 'p 'q.
      ('a -> 'b, 'p, 'q) t -> ('a, 'p, 'q) t -> ('b, 'p, 'q) t

    (** [`Define_using_map2] defines [map] as [apply (return f) t]. [`Custom] overrides
        the default. Some functions returned by [Make*] are defined in terms of [map].
        Providing an efficient [map] improves them as well. *)
    val map
      : ('a : k) ('b : k) 'p 'q.
      [ `Define_using_apply
      | `Custom of ('a, 'p, 'q) t -> f:('a -> 'b) -> ('b, 'p, 'q) t
      ]
  end

  (** Argument to [Make]. *)
  module type Basic = sig
    type ('a : k) t : k

    include Basic3 [@kind k] with type ('a : k, _, _) t := 'a t
  end

  (** Argument to [Make2]. *)
  module type Basic2 = sig
    type ('a : k, 'p) t : k

    include Basic3 [@kind k] with type ('a : k, 'p, _) t := ('a, 'p) t
  end]
end

module type Applicative = sig @@ portable
  include module type of struct
    include Definitions
  end

  [%%template:
  (** The identity applicative. Useful as an argument to functors that require a monad, to
      produce a non-applicative result. *)
  module%template [@mode p = (portable, nonportable)] Ident :
    S
    [@kind value_or_null mod maybe_null] [@mode local p]
    with type ('a : value_or_null) t = 'a

  [@@@kind.default k = (value, value_or_null mod maybe_null)]

  (** Produces a monad with one type parameter. *)
  module%template.portable Make (X : Basic [@kind k]) :
    S [@kind k] with type ('a : k) t := 'a X.t

  (** Produces a monad with two type parameters. *)
  module%template.portable Make2 (X : Basic2 [@kind k]) :
    S2 [@kind k] with type ('a : k, 'p) t := ('a, 'p) X.t

  (** Produces a monad with three type parameters. *)
  module%template.portable Make3 (X : Basic3 [@kind k]) :
    S3 [@kind k] with type ('a : k, 'p, 'q) t := ('a, 'p, 'q) X.t

  [@@@mode.default m = (global, local)]

  (** Produces a monad with one type parameter. *)
  module%template.portable Make_using_map2 (X : Basic_using_map2 [@kind k] [@mode m]) :
    S [@kind k] [@mode m] with type ('a : k) t := 'a X.t

  (** Produces a monad with two type parameters. *)
  module%template.portable Make2_using_map2 (X : Basic2_using_map2 [@kind k] [@mode m]) :
    S2 [@kind k] [@mode m] with type ('a : k, 'p) t := ('a, 'p) X.t

  (** Produces a monad with three type parameters. *)
  module%template.portable Make3_using_map2 (X : Basic3_using_map2 [@kind k] [@mode m]) :
    S3 [@kind k] [@mode m] with type ('a : k, 'p, 'q) t := ('a, 'p, 'q) X.t

  (** Converts a monad with one type parameter to an applicative. *)
  module%template.portable Of_monad (M : Monad.S [@kind k] [@mode m]) :
    S [@kind k] [@mode m] with type ('a : k) t := 'a M.t

  (** Converts a monad with two type parameters to an applicative. *)
  module%template.portable Of_monad2 (M : Monad.S2 [@kind k] [@mode m]) :
    S2 [@kind k] [@mode m] with type ('a : k, 'p) t := ('a, 'p) M.t

  (** Converts a monad with three type parameters to an applicative. *)
  module%template.portable Of_monad3 (M : Monad.S3 [@kind k] [@mode m]) :
    S3 [@kind k] [@mode m] with type ('a : k, 'p, 'q) t := ('a, 'p, 'q) M.t

  (** Composes two applicatives, one nested inside the other. *)
  module%template.portable Compose
      (Inner : S
    [@kind k] [@mode m])
      (Outer : S
    [@kind k] [@mode m]) : S [@kind k] [@mode m] with type ('a : k) t = 'a Inner.t Outer.t

  (** Combines two applicatives as a pair. *)
  module%template.portable Pair
      (Fst : S
    [@kind k] [@mode m])
      (Snd : S
    [@kind k] [@mode m]) :
    S [@kind k] [@mode m] with type ('a : k) t = 'a Fst.t * 'a Snd.t

  [@@@modality.default p = (nonportable, portable)]

  (** Constructs a [Let_syntax] module for an applicative with one type parameter. *)
  module Make_let_syntax
      (X : sig
       @@ p
         include For_let_syntax [@kind k] [@mode m]
       end)
      (Intf : sig
         module type S
       end)
      (Impl : Intf.S) : sig @@ p
      include Let_syntax [@kind k] [@mode m]
    end
    with type ('a : k) t := 'a X.t
    with module Open_on_rhs_intf := Intf

  (** Constructs a [Let_syntax] module for an applicative with two type parameters. *)
  module Make_let_syntax2
      (X : sig
       @@ p
         include For_let_syntax2 [@kind k] [@mode m]
       end)
      (Intf : sig
         module type S
       end)
      (Impl : Intf.S) : sig @@ p
      include Let_syntax2 [@kind k] [@mode m]
    end
    with type ('a : k, 'p) t := ('a, 'p) X.t
    with module Open_on_rhs_intf := Intf

  (** Constructs a [Let_syntax] module for an applicative with three type parameters. *)
  module Make_let_syntax3
      (X : sig
       @@ p
         include For_let_syntax3 [@kind k] [@mode m]
       end)
      (Intf : sig
         module type S
       end)
      (Impl : Intf.S) : sig @@ p
      include Let_syntax3 [@kind k] [@mode m]
    end
    with type ('a : k, 'p, 'q) t := ('a, 'p, 'q) X.t
    with module Open_on_rhs_intf := Intf]
end
