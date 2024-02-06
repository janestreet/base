open! Import

module type Basic_gen = sig
  type 'a t
  type ('a, 'b) f_labeled_fn

  val bind : 'a t -> ('a -> 'b t, 'b t) f_labeled_fn
  val return : 'a -> 'a t

  (** The following identities ought to hold (for some value of =):

      - [return x >>= f = f x]
      - [t >>= fun x -> return x = t]
      - [(t >>= f) >>= g = t >>= fun x -> (f x >>= g)]

      Note: [>>=] is the infix notation for [bind]) *)

  (** The [map] argument to [Monad.Make] says how to implement the monad's [map] function.
      [`Define_using_bind] means to define [map t ~f = bind t ~f:(fun a -> return (f a))].
      [`Custom] overrides the default implementation, presumably with something more
      efficient.

      Some other functions returned by [Monad.Make] are defined in terms of [map], so
      passing in a more efficient [map] will improve their efficiency as well. *)
  val map : [ `Define_using_bind | `Custom of 'a t -> ('a -> 'b, 'b t) f_labeled_fn ]
end

module type Basic = Basic_gen with type ('a, 'b) f_labeled_fn := f:'a -> 'b
module type Basic_local = Basic_gen with type ('a, 'b) f_labeled_fn := f:'a -> 'b

module type Infix_gen = sig
  type 'a t
  type ('a, 'b) fn

  (** [t >>= f] returns a computation that sequences the computations represented by two
      monad elements.  The resulting computation first does [t] to yield a value [v], and
      then runs the computation returned by [f v]. *)
  val ( >>= ) : 'a t -> ('a -> 'b t, 'b t) fn

  (** [t >>| f] is [t >>= (fun a -> return (f a))]. *)
  val ( >>| ) : 'a t -> ('a -> 'b, 'b t) fn
end

module type Infix = Infix_gen with type ('a, 'b) fn := 'a -> 'b
module type Infix_local = Infix_gen with type ('a, 'b) fn := 'a -> 'b

module type Syntax_gen = sig
  (** Opening a module of this type allows one to use the [%bind] and [%map] syntax
      extensions defined by ppx_let, and brings [return] into scope. *)

  type 'a t
  type ('a, 'b) fn
  type ('a, 'b) f_labeled_fn

  module Let_syntax : sig
    (** These are convenient to have in scope when programming with a monad: *)

    val return : 'a -> 'a t

    include Infix_gen with type 'a t := 'a t and type ('a, 'b) fn := ('a, 'b) fn

    module Let_syntax : sig
      val return : 'a -> 'a t
      val bind : 'a t -> ('a -> 'b t, 'b t) f_labeled_fn
      val map : 'a t -> ('a -> 'b, 'b t) f_labeled_fn
      val both : 'a t -> 'b t -> ('a * 'b) t

      module Open_on_rhs : sig end
    end
  end
end

module type Syntax =
  Syntax_gen
    with type ('a, 'b) fn := 'a -> 'b
     and type ('a, 'b) f_labeled_fn := f:'a -> 'b

module type Syntax_local =
  Syntax_gen
    with type ('a, 'b) fn := 'a -> 'b
     and type ('a, 'b) f_labeled_fn := f:'a -> 'b

module type S_without_syntax_gen = sig
  type 'a t
  type ('a, 'b) fn
  type ('a, 'b) f_labeled_fn

  include Infix_gen with type 'a t := 'a t and type ('a, 'b) fn := ('a, 'b) fn

  module Monad_infix :
    Infix_gen with type 'a t := 'a t and type ('a, 'b) fn := ('a, 'b) fn

  (** [bind t ~f] = [t >>= f] *)
  val bind : 'a t -> ('a -> 'b t, 'b t) f_labeled_fn

  (** [return v] returns the (trivial) computation that returns v. *)
  val return : 'a -> 'a t

  (** [map t ~f] is t >>| f. *)
  val map : 'a t -> ('a -> 'b, 'b t) f_labeled_fn

  (** [join t] is [t >>= (fun t' -> t')]. *)
  val join : 'a t t -> 'a t

  (** [ignore_m t] is [map t ~f:(fun _ -> ())].  [ignore_m] used to be called [ignore],
      but we decided that was a bad name, because it shadowed the widely used
      [Stdlib.ignore].  Some monads still do [let ignore = ignore_m] for historical
      reasons. *)
  val ignore_m : 'a t -> unit t

  val all : 'a t list -> 'a list t

  (** Like [all], but ensures that every monadic value in the list produces a unit value,
      all of which are discarded rather than being collected into a list. *)
  val all_unit : unit t list -> unit t
end

module type S_without_syntax =
  S_without_syntax_gen
    with type ('a, 'b) f_labeled_fn := f:'a -> 'b
     and type ('a, 'b) fn := 'a -> 'b

module type S_without_syntax_local =
  S_without_syntax_gen
    with type ('a, 'b) f_labeled_fn := f:'a -> 'b
     and type ('a, 'b) fn := 'a -> 'b

module type S = sig
  type 'a t

  include S_without_syntax with type 'a t := 'a t
  include Syntax with type 'a t := 'a t
end

module type S_local = sig
  type 'a t

  include S_without_syntax_local with type 'a t := 'a t
  include Syntax_local with type 'a t := 'a t
end

module type Basic2_gen = sig
  (** Multi parameter monad. The second parameter gets unified across all the computation.
      This is used to encode monads working on a multi parameter data structure like
      ([('a,'b) result]). *)

  type ('a, 'e) t
  type ('a, 'b) f_labeled_fn

  val bind : ('a, 'e) t -> ('a -> ('b, 'e) t, ('b, 'e) t) f_labeled_fn

  val map
    : [ `Define_using_bind
      | `Custom of ('a, 'e) t -> ('a -> 'b, ('b, 'e) t) f_labeled_fn
      ]

  val return : 'a -> ('a, _) t
end

module type Basic2 = Basic2_gen with type ('a, 'b) f_labeled_fn := f:'a -> 'b
module type Basic2_local = Basic2_gen with type ('a, 'b) f_labeled_fn := f:'a -> 'b

module type Infix2_gen = sig
  (** Same as {!Infix}, except the monad type has two arguments. The second is always just
      passed through. *)

  type ('a, 'e) t
  type ('a, 'b) fn

  val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t, ('b, 'e) t) fn
  val ( >>| ) : ('a, 'e) t -> ('a -> 'b, ('b, 'e) t) fn
end

module type Infix2 = Infix2_gen with type ('a, 'b) fn := 'a -> 'b
module type Infix2_local = Infix2_gen with type ('a, 'b) fn := 'a -> 'b

module type Syntax2_gen = sig
  type ('a, 'e) t
  type ('a, 'b) fn
  type ('a, 'b) f_labeled_fn

  module Let_syntax : sig
    val return : 'a -> ('a, _) t

    include
      Infix2_gen with type ('a, 'e) t := ('a, 'e) t and type ('a, 'b) fn := ('a, 'b) fn

    module Let_syntax : sig
      val return : 'a -> ('a, _) t
      val bind : ('a, 'e) t -> ('a -> ('b, 'e) t, ('b, 'e) t) f_labeled_fn
      val map : ('a, 'e) t -> ('a -> 'b, ('b, 'e) t) f_labeled_fn
      val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t

      module Open_on_rhs : sig end
    end
  end
end

module type Syntax2 =
  Syntax2_gen
    with type ('a, 'b) fn := 'a -> 'b
     and type ('a, 'b) f_labeled_fn := f:'a -> 'b

module type Syntax2_local =
  Syntax2_gen
    with type ('a, 'b) fn := 'a -> 'b
     and type ('a, 'b) f_labeled_fn := f:'a -> 'b

module type S2_gen = sig
  (** The same as {!S} except the monad type has two arguments. The second is always just
      passed through. *)

  type ('a, 'e) t
  type ('a, 'b) fn
  type ('a, 'b) f_labeled_fn

  include
    Infix2_gen with type ('a, 'e) t := ('a, 'e) t and type ('a, 'b) fn := ('a, 'b) fn

  include
    Syntax2_gen
      with type ('a, 'e) t := ('a, 'e) t
       and type ('a, 'b) fn := ('a, 'b) fn
       and type ('a, 'b) f_labeled_fn := ('a, 'b) f_labeled_fn

  module Monad_infix :
    Infix2_gen with type ('a, 'e) t := ('a, 'e) t and type ('a, 'b) fn := ('a, 'b) fn

  val bind : ('a, 'e) t -> ('a -> ('b, 'e) t, ('b, 'e) t) f_labeled_fn
  val return : 'a -> ('a, _) t
  val map : ('a, 'e) t -> ('a -> 'b, ('b, 'e) t) f_labeled_fn
  val join : (('a, 'e) t, 'e) t -> ('a, 'e) t
  val ignore_m : (_, 'e) t -> (unit, 'e) t
  val all : ('a, 'e) t list -> ('a list, 'e) t
  val all_unit : (unit, 'e) t list -> (unit, 'e) t
end

module type S2 =
  S2_gen with type ('a, 'b) fn := 'a -> 'b and type ('a, 'b) f_labeled_fn := f:'a -> 'b

module type S2_local =
  S2_gen with type ('a, 'b) fn := 'a -> 'b and type ('a, 'b) f_labeled_fn := f:'a -> 'b

module type Basic3 = sig
  (** Multi parameter monad. The second and third parameters get unified across all the
      computation. *)

  type ('a, 'd, 'e) t

  val bind : ('a, 'd, 'e) t -> f:('a -> ('b, 'd, 'e) t) -> ('b, 'd, 'e) t

  val map
    : [ `Define_using_bind | `Custom of ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t ]

  val return : 'a -> ('a, _, _) t
end

module type Infix3 = sig
  (** Same as Infix, except the monad type has three arguments. The second and third are
      always just passed through. *)

  type ('a, 'd, 'e) t

  val ( >>= ) : ('a, 'd, 'e) t -> ('a -> ('b, 'd, 'e) t) -> ('b, 'd, 'e) t
  val ( >>| ) : ('a, 'd, 'e) t -> ('a -> 'b) -> ('b, 'd, 'e) t
end

module type Syntax3 = sig
  type ('a, 'd, 'e) t

  module Let_syntax : sig
    val return : 'a -> ('a, _, _) t

    include Infix3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) t

    module Let_syntax : sig
      val return : 'a -> ('a, _, _) t
      val bind : ('a, 'd, 'e) t -> f:('a -> ('b, 'd, 'e) t) -> ('b, 'd, 'e) t
      val map : ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t
      val both : ('a, 'd, 'e) t -> ('b, 'd, 'e) t -> ('a * 'b, 'd, 'e) t

      module Open_on_rhs : sig end
    end
  end
end

module type S3 = sig
  (** The same as {!S} except the monad type has three arguments. The second
      and third are always just passed through. *)

  type ('a, 'd, 'e) t

  include Infix3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) t
  include Syntax3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) t
  module Monad_infix : Infix3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) t

  val bind : ('a, 'd, 'e) t -> f:('a -> ('b, 'd, 'e) t) -> ('b, 'd, 'e) t
  val return : 'a -> ('a, _, _) t
  val map : ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t
  val join : (('a, 'd, 'e) t, 'd, 'e) t -> ('a, 'd, 'e) t
  val ignore_m : (_, 'd, 'e) t -> (unit, 'd, 'e) t
  val all : ('a, 'd, 'e) t list -> ('a list, 'd, 'e) t
  val all_unit : (unit, 'd, 'e) t list -> (unit, 'd, 'e) t
end

module type Basic_indexed = sig
  (** Indexed monad, in the style of Atkey. The second and third parameters are composed
      across all computation. To see this more clearly, you can look at the type of bind:

      {[
        val bind : ('a, 'i, 'j) t -> f:('a -> ('b, 'j, 'k) t) -> ('b, 'i, 'k) t
      ]}

      and isolate some of the type variables to see their individual behaviors:

      {[
        val bind : 'a             -> f:('a ->  'b           ) ->  'b
        val bind :      'i, 'j    ->               'j, 'k     ->     'i, 'k
      ]}

      For more information on Atkey-style indexed monads, see:

      {v
        Parameterised Notions of Computation
        Robert Atkey
        http://bentnib.org/paramnotions-jfp.pdf
      v} *)

  type ('a, 'i, 'j) t

  val bind : ('a, 'i, 'j) t -> f:('a -> ('b, 'j, 'k) t) -> ('b, 'i, 'k) t

  val map
    : [ `Define_using_bind | `Custom of ('a, 'i, 'j) t -> f:('a -> 'b) -> ('b, 'i, 'j) t ]

  val return : 'a -> ('a, 'i, 'i) t
end

module type Infix_indexed = sig
  (** Same as {!Infix}, except the monad type has three arguments. The second and
      third are composed across all computation. *)

  type ('a, 'i, 'j) t

  val ( >>= ) : ('a, 'i, 'j) t -> ('a -> ('b, 'j, 'k) t) -> ('b, 'i, 'k) t
  val ( >>| ) : ('a, 'i, 'j) t -> ('a -> 'b) -> ('b, 'i, 'j) t
end

module type Syntax_indexed = sig
  type ('a, 'i, 'j) t

  module Let_syntax : sig
    val return : 'a -> ('a, 'i, 'i) t

    include Infix_indexed with type ('a, 'i, 'j) t := ('a, 'i, 'j) t

    module Let_syntax : sig
      val return : 'a -> ('a, 'i, 'i) t
      val bind : ('a, 'i, 'j) t -> f:('a -> ('b, 'j, 'k) t) -> ('b, 'i, 'k) t
      val map : ('a, 'i, 'j) t -> f:('a -> 'b) -> ('b, 'i, 'j) t
      val both : ('a, 'i, 'j) t -> ('b, 'j, 'k) t -> ('a * 'b, 'i, 'k) t

      module Open_on_rhs : sig end
    end
  end
end

module type S_indexed = sig
  (** The same as {!S} except the monad type has three arguments. The second and
      third are composed across all computation. *)

  type ('a, 'i, 'j) t

  include Infix_indexed with type ('a, 'i, 'j) t := ('a, 'i, 'j) t
  include Syntax_indexed with type ('a, 'i, 'j) t := ('a, 'i, 'j) t
  module Monad_infix : Infix_indexed with type ('a, 'i, 'j) t := ('a, 'i, 'j) t

  val bind : ('a, 'i, 'j) t -> f:('a -> ('b, 'j, 'k) t) -> ('b, 'i, 'k) t
  val return : 'a -> ('a, 'i, 'i) t
  val map : ('a, 'i, 'j) t -> f:('a -> 'b) -> ('b, 'i, 'j) t
  val join : (('a, 'j, 'k) t, 'i, 'j) t -> ('a, 'i, 'k) t
  val ignore_m : (_, 'i, 'j) t -> (unit, 'i, 'j) t
  val all : ('a, 'i, 'i) t list -> ('a list, 'i, 'i) t
  val all_unit : (unit, 'i, 'i) t list -> (unit, 'i, 'i) t
end

module S_to_S2 (X : S) : S2 with type ('a, 'e) t = 'a X.t = struct
  include X

  type ('a, 'e) t = 'a X.t
end

module S2_to_S3 (X : S2) : S3 with type ('a, 'd, 'e) t = ('a, 'd) X.t = struct
  include X

  type ('a, 'd, 'e) t = ('a, 'd) X.t
end

module S_to_S_indexed (X : S) : S_indexed with type ('a, 'i, 'j) t = 'a X.t = struct
  include X

  type ('a, 'i, 'j) t = 'a X.t
end

module S2_to_S (X : S2) : S with type 'a t = ('a, unit) X.t = struct
  include X

  type 'a t = ('a, unit) X.t
end

module S3_to_S2 (X : S3) : S2 with type ('a, 'e) t = ('a, 'e, unit) X.t = struct
  include X

  type ('a, 'e) t = ('a, 'e, unit) X.t
end

module S_indexed_to_S2 (X : S_indexed) : S2 with type ('a, 'e) t = ('a, 'e, 'e) X.t =
struct
  include X

  type ('a, 'e) t = ('a, 'e, 'e) X.t
end

module type Monad = sig
  (** A monad is an abstraction of the concept of sequencing of computations.  A value of
      type ['a monad] represents a computation that returns a value of type ['a]. *)

  module type Basic = Basic
  module type Basic2 = Basic2
  module type Basic3 = Basic3
  module type Basic_indexed = Basic_indexed
  module type Basic_local = Basic_local
  module type Basic2_local = Basic2_local
  module type Infix = Infix
  module type Infix2 = Infix2
  module type Infix3 = Infix3
  module type Infix_indexed = Infix_indexed
  module type Infix_local = Infix_local
  module type Infix2_local = Infix2_local
  module type Syntax = Syntax
  module type Syntax2 = Syntax2
  module type Syntax3 = Syntax3
  module type Syntax_indexed = Syntax_indexed
  module type Syntax_local = Syntax_local
  module type Syntax2_local = Syntax2_local
  module type S_without_syntax = S_without_syntax
  module type S_without_syntax_local = S_without_syntax_local
  module type S = S
  module type S2 = S2
  module type S3 = S3
  module type S_indexed = S_indexed
  module type S_local = S_local
  module type S2_local = S2_local

  module Make (X : Basic) : S with type 'a t := 'a X.t
  module Make2 (X : Basic2) : S2 with type ('a, 'e) t := ('a, 'e) X.t
  module Make3 (X : Basic3) : S3 with type ('a, 'd, 'e) t := ('a, 'd, 'e) X.t

  module Make_indexed (X : Basic_indexed) :
    S_indexed with type ('a, 'd, 'e) t := ('a, 'd, 'e) X.t

  module Make_local (X : Basic_local) : S_local with type 'a t := 'a X.t
  module Make2_local (X : Basic2_local) : S2_local with type ('a, 'e) t := ('a, 'e) X.t

  (** Define a monad through an isomorphism with an existing monad. For example:

      {[
        type 'a t = { value : 'a }

        include Monad.Of_monad (Monad.Ident) (struct
            type nonrec 'a t = 'a t

            let to_monad { value } = value
            let of_monad value = { value }
          end)
      ]} *)
  module Of_monad
    (Monad : S) (M : sig
      type 'a t

      val to_monad : 'a t -> 'a Monad.t
      val of_monad : 'a Monad.t -> 'a t
    end) : S with type 'a t := 'a M.t

  module Of_monad2
    (Monad : S2) (M : sig
      type ('a, 'b) t

      val to_monad : ('a, 'b) t -> ('a, 'b) Monad.t
      val of_monad : ('a, 'b) Monad.t -> ('a, 'b) t
    end) : S2 with type ('a, 'b) t := ('a, 'b) M.t

  module Of_monad3
    (Monad : S3) (M : sig
      type ('a, 'b, 'c) t

      val to_monad : ('a, 'b, 'c) t -> ('a, 'b, 'c) Monad.t
      val of_monad : ('a, 'b, 'c) Monad.t -> ('a, 'b, 'c) t
    end) : S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) M.t

  module Of_monad_indexed
    (Monad : S_indexed) (M : sig
      type ('a, 'i, 'j) t

      val to_monad : ('a, 'i, 'j) t -> ('a, 'i, 'j) Monad.t
      val of_monad : ('a, 'i, 'j) Monad.t -> ('a, 'i, 'j) t
    end) : S_indexed with type ('a, 'i, 'j) t := ('a, 'i, 'j) M.t

  (** An eager identity monad with functions heavily annotated with
      [@inlined] or [@inline hint].

      The implementation is manually written, rather than being
      constructed by [Monad.Make]. This gives better inlining
      guarantees.
  *)
  module Ident : S_local with type 'a t = 'a
end
