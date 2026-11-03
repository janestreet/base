(** Provides generic signatures for container data structures.

    These signatures include functions ([iter], [fold], [exists], [for_all], ...) that you
    would expect to find in any container. Used by including [Container.S0] or
    [Container.S1] in the signature for every container-like data structure ([Array],
    [List], [String], ...) to ensure a consistent interface. *)

open! Import0

open struct
  (*_ If we just bind these aliases directly, without the [open struct ... end], then mdx
      sometimes gets confused by seeing these aliases to constructors and prints e.g.
      [Base.Container_intf.Option.Some] instead of [Some] *)
  module Either = Either0
  module List = List0
  module Option = Option0
  module Result = Result0
end

module Definitions = struct
  module Export = struct
    (** [Continue_or_stop.t] is used by the [f] argument to [fold_until] in order to
        indicate whether folding should continue, or stop early.

        @canonical Base.Container.Continue_or_stop *)
    module Continue_or_stop = struct
      type ('a, 'b) t =
        | Continue of 'a
        | Stop of 'b
    end
  end

  include Export

  (** @canonical Base.Container.Summable *)
  module type Summable = sig
    type t

    (** The result of summing no values. *)
    val zero : t

    (** An operation that combines two [t]'s and handles [zero + x] by just returning [x],
        as well as in the symmetric case. *)
    val ( + ) : t -> t -> t
  end

  module type Generic_types = sig
    (** A [('a, 'p1, 'p2) t] represents a container whose elements are ['a elt]s. ['p1]
        and ['p2] are available for extra phantom types a container may track; for
        example, the ['key] type of a [Core.Hash_queue]. *)
    type ('a, _, _) t

    (** ['a elt] abstracts over monomorphic and polymorphic containers. In polymorphic
        containers like ['a list], we substitute ['a elt := 'a] (see [S1] and friends
        below); In monomorphic containers like [string], we substitute ['a elt] with the
        element type (e.g. [char]; see [S0] and friends below). *)
    type 'a elt
  end

  include struct
    (*_ We template the following interfaces over the set of kinds that are allowed in the
        container [t]. Each such kind set has an associated set of kinds allowed in types
        that appear as function arguments/returns but are not put into [t]. For example, a
        container that only allows [value mod external64]s can easily still implement
        [fold] with a [value] accumulator.

        Because the functions with three type variables ([fold_until] and [partition_map])
        are rarely needed and are very expensive to generate for the entire cubic space of
        possible kinds, we also additionally restrict the [ks] and [ks_not_in_t] for the
        cubic functions specifically.
    *)

    include struct
      module type Generic_without_mem = sig
        include Generic_types

        type ('a, 'b, 'c) t := ('a, 'b, 'c) t
        type 'a elt := 'a elt

        include sig
          (*_ Here, we use [default_if_multiple] because this interface:
              (1) Sometimes uses [ks] to represent a single kind that we eventually would
                  make an abstract kind
              (2) Sometimes uses [ks] to represent a "universe" of kinds that the
                  interface broadly understands and supports all combinations of (which we
                  eventually will replace with layout polymorphism)

              In case (1), there is only one version of each function, so we don't
              actually want to mangle it over [k = ks] (because it's silly for users of
              e.g. [imm_array] to have to call [(length [@kind immediate64])] to compute
              the length of their array).

              In case (2), since there are many versions of each function, we mangle the
              functions. *)

          type ('a, 'b, 'c) t := ('a, 'b, 'c) t
          type 'a elt := 'a elt

          val length : 'a 'p1 'p2. ('a, 'p1, 'p2) t -> int
          val is_empty : 'a 'p1 'p2. ('a, 'p1, 'p2) t -> bool

          (** [iter] must allow exceptions raised in [f] to escape, terminating the
              iteration cleanly. The same holds for all functions below taking an [f]. *)
          val iter : 'a 'p1 'p2. ('a, 'p1, 'p2) t -> f:('a elt -> unit) -> unit

          (** Returns [true] if and only if there exists an element for which the provided
              function evaluates to [true]. This is a short-circuiting operation. *)
          val exists : 'a 'p1 'p2. ('a, 'p1, 'p2) t -> f:('a elt -> bool) -> bool

          (** Returns [true] if and only if the provided function evaluates to [true] for
              all elements. This is a short-circuiting operation. *)
          val for_all : 'a 'p1 'p2. ('a, 'p1, 'p2) t -> f:('a elt -> bool) -> bool

          (** Returns the number of elements for which the provided function evaluates to
              true. *)
          val count : 'a 'p1 'p2. ('a, 'p1, 'p2) t -> f:('a elt -> bool) -> int

          (** Returns as an [option] the first element for which [f] evaluates to true. *)
          val find : 'a 'p1 'p2. ('a, 'p1, 'p2) t -> f:('a elt -> bool) -> 'a elt Option.t

          val to_list : 'a 'p1 'p2. ('a, 'p1, 'p2) t -> 'a elt List.Constructors.t

          (** Returns a min (resp. max) element from the collection using the provided
              [compare] function. In case of a tie, the first element encountered while
              traversing the collection is returned. The implementation uses [fold] so it
              has the same complexity as [fold]. Returns [None] iff the collection is
              empty. *)
          val min_elt
            : 'a 'p1 'p2.
            ('a, 'p1, 'p2) t -> compare:('a elt -> 'a elt -> int) -> 'a elt Option.t

          val max_elt
            : 'a 'p1 'p2.
            ('a, 'p1, 'p2) t -> compare:('a elt -> 'a elt -> int) -> 'a elt Option.t

          (** Returns the sum of [f i] for all [i] in the container. The order in which
              the elements will be summed is unspecified. *)
          val sum
            : 'a 'sum 'p1 'p2.
            (module Summable with type t = 'sum)
            -> ('a, 'p1, 'p2) t
            -> f:('a elt -> 'sum)
            -> 'sum

          (** [iter_until t ~f ~finish] is a short-circuiting version of [iter]. If [f]
              returns [Stop x] the computation ceases and returns [x]. If [f] always
              returns [Continue ()] the final result is computed by [finish]. *)
          val iter_until
            : 'a 'p1 'p2 'final.
            ('a, 'p1, 'p2) t
            -> f:('a elt -> (unit, 'final) Continue_or_stop.t)
            -> finish:(unit -> 'final)
            -> 'final

          (** [fold t ~init ~f] returns [f (... f (f (f init e1) e2) e3 ...) en], where
              [e1..en] are the elements of [t]. *)
          val fold
            : 'a 'p1 'p2 'acc.
            ('a, 'p1, 'p2) t -> init:'acc -> f:('acc -> 'a elt -> 'acc) -> 'acc

          (** [fold_result t ~init ~f] is a short-circuiting version of [fold] that runs
              in the [Result] monad. If [f] returns an [Error _], that value is returned
              without any additional invocations of [f]. *)
          val fold_result
            : 'a 'p1 'p2 'acc 'e.
            ('a, 'p1, 'p2) t
            -> init:'acc
            -> f:('acc -> 'a elt -> ('acc, 'e) Result.t)
            -> ('acc, 'e) Result.t

          (** Returns the first evaluation of [f] that returns [Some], and returns [None]
              if there is no such element. *)
          val find_map
            : 'a 'p1 'p2 'b.
            ('a, 'p1, 'p2) t -> f:('a elt -> 'b Option.t) -> 'b Option.t
        end

        include sig
          type ('a, 'b, 'c) t := ('a, 'b, 'c) t
          type 'a elt := 'a elt

          (** [fold_until t ~init ~f ~finish] is a short-circuiting version of [fold]. If
              [f] returns [Stop _] the computation ceases and results in that value. If
              [f] returns [Continue _], the fold will proceed. If [f] never returns
              [Stop _], the final result is computed by [finish].

              Example:

              {[
                type maybe_negative =
                  | Found_negative of int
                  | All_nonnegative of { sum : int }

                (** [first_neg_or_sum list] returns the first negative number in [list], if any,
                    otherwise returns the sum of the list. *)
                let first_neg_or_sum =
                  List.fold_until ~init:0
                    ~f:(fun sum x ->
                      if x < 0
                      then Stop (Found_negative x)
                      else Continue (sum + x))
                    ~finish:(fun sum -> All_nonnegative { sum })
                ;;

                let x = first_neg_or_sum [1; 2; 3; 4; 5]
                val x : maybe_negative = All_nonnegative {sum = 15}

                let y = first_neg_or_sum [1; 2; -3; 4; 5]
                val y : maybe_negative = Found_negative -3
              ]} *)
          val fold_until
            : 'a 'p1 'p2 'acc 'final.
            ('a, 'p1, 'p2) t
            -> init:'acc
            -> f:('acc -> 'a elt -> ('acc, 'final) Continue_or_stop.t)
            -> finish:('acc -> 'final)
            -> 'final
        end
      end

      module type Generic = sig
        include Generic_without_mem

        (** Checks whether the provided element is there, using [equal]. *)
        val mem
          : 'a 'p1 'p2.
          ('a, 'p1, 'p2) t -> 'a elt -> equal:('a elt -> 'a elt -> bool) -> bool
      end

      (** Like [Generic], but [mem] does not accept an [equal] function, since [Make0]
          already takes [Elt.equal]. *)
      module type Generic_for_s0 = sig
        include Generic_without_mem

        (** Checks whether the provided element is there, using equality on [elt]s. *)
        val mem : ('a, _, _) t -> 'a elt -> bool
      end
    end
  end

  include struct
    module type Generic = sig
      include Generic

      (*_ This doesn't have a local version because arrays can't hold local values. *)
      val to_array : ('a, _, _) t -> 'a elt array
    end

    (** Signature for monomorphic container - a container for a specific element type,
        e.g., string, which is a container of characters ([type elt = char]) and never of
        anything else. *)
    module type S0 = sig
      include sig
        type t
        type elt
      end

      include Generic_for_s0 with type (_, _, _) t := t with type _ elt := elt

      (*_ This doesn't have a local version because arrays can't hold local values. *)
      val to_array : t -> elt array
    end

    module type S0_phantom = sig
      include sig
        type elt
        type 'phantom t
      end

      include
        Generic_for_s0 with type (_, 'phantom, _) t := 'phantom t with type _ elt := elt

      (*_ This doesn't have a local version because arrays can't hold local values. *)
      val to_array : _ t -> elt array
    end
  end

  include struct
    (*_ See the explanation of [ks_for_3] above *)

    include struct
      (** Signature for polymorphic container, e.g., ['a list] or ['a array]. *)

      module type S1 = sig
        type 'a t

        include Generic with type ('a, _, _) t := 'a t with type 'a elt := 'a

        (*_ This doesn't have a local version because arrays can't hold local values. *)
        val to_array : 'a. 'a t -> 'a array
      end

      module type S1_phantom = sig
        type ('a, 'phantom) t

        include
          Generic with type ('a, 'phantom, _) t := ('a, 'phantom) t with type 'a elt := 'a

        (*_ This doesn't have a local version because arrays can't hold local values. *)
        val to_array : 'a 'p. ('a, 'p) t -> 'a array
      end

      module type Creators = sig
        include Generic_types

        type ('a, 'b, 'c) t := ('a, 'b, 'c) t
        type 'a elt := 'a elt
        type (_, _, _) concat

        include sig
          type ('a, 'b, 'c) t := ('a, 'b, 'c) t
          type 'a elt := 'a elt

          val of_list : 'a 'p1 'p2. 'a elt List.Constructors.t -> ('a, 'p1, 'p2) t

          (** E.g., [append (of_list [a; b]) (of_list [c; d; e])] is
              [of_list [a; b; c; d; e]] *)
          val append
            : 'a 'p1 'p2.
            ('a, 'p1, 'p2) t -> ('a, 'p1, 'p2) t -> ('a, 'p1, 'p2) t

          (** Concatenates a nested container. The elements of the inner containers are
              concatenated together in order to give the result. *)
          val concat : 'a 'p1 'p2. (('a, 'p1, 'p2) t, 'p1, 'p2) concat -> ('a, 'p1, 'p2) t

          (** [filter t ~f] returns all the elements of [t] that satisfy the predicate
              [f]. *)
          val filter
            : 'a 'p1 'p2.
            ('a, 'p1, 'p2) t -> f:('a elt -> bool) -> ('a, 'p1, 'p2) t

          (** [partition_tf t ~f] returns a pair [t1, t2], where [t1] is all elements of
              [t] that satisfy [f], and [t2] is all elements of [t] that do not satisfy
              [f]. The "tf" suffix is mnemonic to remind readers that the result is
              (trues, falses). *)
          val partition_tf
            : 'a 'p1 'p2.
            ('a, 'p1, 'p2) t -> f:('a elt -> bool) -> ('a, 'p1, 'p2) t * ('a, 'p1, 'p2) t
        end

        include sig
          (** [map f (of_list [a1; ...; an])] applies [f] to [a1], [a2], ..., [an], in
              order, and builds a result equivalent to [of_list [f a1; ...; f an]]. *)
          val map
            : 'a 'p1 'p2 'b.
            ('a, 'p1, 'p2) t -> f:('a elt -> 'b elt) -> ('b, 'p1, 'p2) t

          (** [filter_map t ~f] applies [f] to every [x] in [t]. The result contains every
              [y] for which [f x] returns [Some y]. *)
          val filter_map
            : 'a 'p1 'p2 'b.
            ('a, 'p1, 'p2) t -> f:('a elt -> 'b elt Option.t) -> ('b, 'p1, 'p2) t

          (** [concat_map t ~f] is equivalent to [concat (map t ~f)]. *)
          val concat_map
            : 'a 'p1 'p2 'b.
            ('a, 'p1, 'p2) t -> f:('a elt -> ('b, 'p1, 'p2) t) -> ('b, 'p1, 'p2) t
        end

        include sig
          (** [partition_map t ~f] partitions [t] according to [f]. *)
          val partition_map
            : 'a 'p1 'p2 'b 'c.
            ('a, 'p1, 'p2) t
            -> f:('a elt -> ('b elt, 'c elt) Either.t)
            -> ('b, 'p1, 'p2) t * ('c, 'p1, 'p2) t
        end
      end

      module type Generic_with_creators = sig
        include Generic

        include
          Creators with type ('a, 'b, 'c) t := ('a, 'b, 'c) t with type 'a elt := 'a elt
      end

      (** Like [Generic_with_creators], but [mem] does not accept an [equal] function,
          since [Make0_with_creators] already takes [Elt.equal]. *)
      module type Generic_with_creators_for_s0 = sig
        include Generic_for_s0

        include
          Creators with type ('a, 'b, 'c) t := ('a, 'b, 'c) t with type 'a elt := 'a elt
      end
    end
  end

  include struct
    module type Generic_with_creators = sig
      include Generic_with_creators

      (*_ This doesn't have a local version because arrays can't hold local values. *)
      val to_array : ('a, _, _) t -> 'a elt array
      val of_array : 'a elt array -> ('a, _, _) t
    end

    include struct
      (*_ This is outside the template because an [immediate] container can't contain
          itself *)

      module type S0_with_creators = sig
        include sig
          type t
          type elt
        end

        include
          Generic_with_creators_for_s0
          with type (_, _, _) concat := t list
          with type ('a, _, _) t := t
          with type _ elt := elt

        (*_ This doesn't have a local version because arrays can't hold local values. *)
        val to_array : t -> elt array
        val of_array : elt array -> t
      end
    end

    include struct
      module type S1_with_creators = sig
        type 'a t

        include
          Generic_with_creators
          with type ('a, _, _) concat := 'a t
          with type ('a, _, _) t := 'a t
          with type 'a elt := 'a

        (*_ This doesn't have a local version because arrays can't hold local values. *)
        val to_array : 'a. 'a t -> 'a array
        val of_array : 'a. 'a array -> 'a t
      end
    end
  end

  type ('t, 'a, 'acc) fold = 't -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc

  type ('t, 'a, 'acc, 'final) fold_until =
    't
    -> init:'acc
    -> f:('acc -> 'a -> ('acc, 'final) Continue_or_stop.t)
    -> finish:('acc -> 'final)
    -> 'final

  type ('t, 'a) iter = 't -> f:('a -> unit) -> unit

  type ('t, 'a, 'final) iter_until =
    't -> f:('a -> (unit, 'final) Continue_or_stop.t) -> finish:(unit -> 'final) -> 'final

  type 't length = 't -> int

  module type Make_gen_arg = sig
    type ('a, 'phantom1, 'phantom2) t
    type 'a elt

    val fold_until : (('a, _, _) t, 'a elt, 'acc, 'final) fold_until
    val fold : [ `Define_using_fold_until | `Custom of (('a, _, _) t, 'a elt, 'acc) fold ]

    val iter_until
      : [ `Define_using_fold_until
        | `Custom of (('a, _, _) t, 'a elt, 'final) iter_until
        ]

    (** The [iter] argument to [Container.Make] specifies how to implement the container's
        [iter] function. [`Define_using_fold] means to define [iter] via:

        {[
          iter t ~f = Container.iter ~fold t ~f
        ]}

        [`Custom] overrides the default implementation, presumably with something more
        efficient. Several other functions returned by [Container.Make] are defined in
        terms of [iter], so passing in a more efficient [iter] will improve their
        efficiency as well. *)
    val iter
      : [ `Define_using_fold
        | `Define_using_iter_until
        | `Custom of (('a, _, _) t, 'a elt) iter
        ]

    (** The [length] argument to [Container.Make] specifies how to implement the
        container's [length] function. [`Define_using_fold] means to define [length] via:

        {[
          length t ~f = Container.length ~fold t ~f
        ]}

        [`Custom] overrides the default implementation, presumably with something more
        efficient. Several other functions returned by [Container.Make] are defined in
        terms of [length], so passing in a more efficient [length] will improve their
        efficiency as well. *)
    val length : [ `Define_using_fold | `Custom of ('a, _, _) t length ]
  end

  module type Make_arg = sig
    type 'a t

    include Make_gen_arg with type ('a, _, _) t := 'a t and type 'a elt := 'a
  end

  module type Make0_arg = sig
    module Elt : sig
      type t

      val equal : t -> t -> bool
    end

    type t

    include Make_gen_arg with type ('a, _, _) t := t and type 'a elt := Elt.t
  end

  module type Make_common_with_creators_arg = sig
    include Make_gen_arg

    type (_, _, _) concat

    val of_list : 'a elt list -> ('a, _, _) t
    val of_array : 'a elt array -> ('a, _, _) t
    val concat : (('a, _, _) t, _, _) concat -> ('a, _, _) t
  end

  module type Make_gen_with_creators_arg = sig
    include Make_common_with_creators_arg

    val concat_of_array : 'a array -> ('a, _, _) concat
  end

  module type Make_with_creators_arg = sig
    type 'a t

    include
      Make_common_with_creators_arg
      with type ('a, _, _) t := 'a t
       and type 'a elt := 'a
       and type ('a, _, _) concat := 'a t
  end

  module type Make0_with_creators_arg = sig
    module Elt : sig
      type t

      val equal : t -> t -> bool
    end

    type t

    include
      Make_common_with_creators_arg
      with type ('a, _, _) t := t
       and type 'a elt := Elt.t
       and type ('a, _, _) concat := 'a list
  end

  module type Derived = sig
    include sig
      (** Generic defintions that rely on [With_return] and therefore don't support local
          return values. *)

      val fold_until
        : 't 'a 'acc 'final.
        fold:('t, 'a, 'acc) fold -> ('t, 'a, 'acc, 'final) fold_until

      (** Generic definitions of container operations in terms of [fold].

          E.g.: [iter ~fold t ~f = fold t ~init:() ~f:(fun () a -> f a)]. *)

      val iter_via_fold : 't 'a. fold:('t, 'a, unit) fold -> ('t, 'a) iter
      val count : 't 'a. fold:('t, 'a, int) fold -> 't -> f:('a -> bool) -> int

      val min_elt
        : 't 'a.
        fold:('t, 'a, 'a option) fold -> 't -> compare:('a -> 'a -> int) -> 'a option

      val max_elt
        : 't 'a.
        fold:('t, 'a, 'a option) fold -> 't -> compare:('a -> 'a -> int) -> 'a option

      val length : 't 'a. fold:('t, 'a, int) fold -> 't length
    end

    val to_list : 't 'a. fold:('t, 'a, 'a list) fold -> 't -> 'a list

    val sum
      : 't 'a 'sum.
      fold:('t, 'a, 'sum) fold
      -> (module Summable with type t = 'sum)
      -> 't
      -> f:('a -> 'sum)
      -> 'sum

    (** Generic definitions of container operations in terms of [fold_until]. *)

    val fold
      : 't 'a 'acc.
      fold_until:('t, 'a, 'acc, 'acc) fold_until -> ('t, 'a, 'acc) fold

    val fold_result
      : 't 'a 'acc 'e.
      fold_until:('t, 'a, 'acc, ('acc, 'e) Result.t) fold_until
      -> 't
      -> init:'acc
      -> f:('acc -> 'a -> ('acc, 'e) Result.t)
      -> ('acc, 'e) Result.t

    val iter_until
      : 't 'a 'final.
      fold_until:('t, 'a, unit, 'final) fold_until -> ('t, 'a, 'final) iter_until

    (** Generic definitions of container operations in terms of [iter_until]. *)

    include sig
      val iter_via_iter_until
        : 't 'a.
        iter_until:('t, 'a, unit) iter_until -> ('t, 'a) iter

      val is_empty : 't 'a. iter_until:('t, 'a, bool) iter_until -> 't -> bool

      val mem
        : 't 'a.
        iter_until:('t, 'a, bool) iter_until
        -> 't
        -> 'a
        -> equal:('a -> 'a -> bool)
        -> bool

      val exists
        : 't 'a.
        iter_until:('t, 'a, bool) iter_until -> 't -> f:('a -> bool) -> bool

      val for_all
        : 't 'a.
        iter_until:('t, 'a, bool) iter_until -> 't -> f:('a -> bool) -> bool

      val find
        : 't 'a.
        iter_until:('t, 'a, 'a option) iter_until -> 't -> f:('a -> bool) -> 'a option
    end

    val find_map
      : 't 'a 'b.
      iter_until:('t, 'a, 'b option) iter_until -> 't -> f:('a -> 'b option) -> 'b option

    (** Generic definitons of container operations in terms of [iter] and [length]. *)

    val to_array : 't 'a. length:'t length -> iter:('t, 'a) iter -> 't -> 'a array
  end
end

module type Container = sig
  include module type of struct
    include Definitions
  end

  module Derived : Derived
  include module type of Derived

  (** The idiom for using [Container.Make] is to bind the resulting module and to
      explicitly import each of the functions that one wants:

      {[
        module C = Container.Make (struct ... end)
        let count    = C.count
        let exists   = C.exists
        let find     = C.find
        (* ... *)
      ]}

      This is preferable to:

      {[
        include Container.Make (struct ... end)
      ]}

      because the [include] makes it too easy to shadow specialized implementations of
      container functions ([length] being a common one).

      [Container.Make0] is like [Container.Make], but for monomorphic containers like
      [string]. *)
  module Make (T : Make_arg) : S1 with type 'a t := 'a T.t

  module Make0 (T : Make0_arg) : S0 with type t := T.t and type elt := T.Elt.t

  module Make_gen (T : Make_gen_arg) :
    Generic
    with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) T.t
     and type 'a elt := 'a T.elt

  module Make_with_creators (T : Make_with_creators_arg) :
    S1_with_creators with type 'a t := 'a T.t

  module Make0_with_creators (T : Make0_with_creators_arg) :
    S0_with_creators with type t := T.t and type elt := T.Elt.t

  module Make_gen_with_creators (T : Make_gen_with_creators_arg) :
    Generic_with_creators
    with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) T.t
     and type 'a elt := 'a T.elt
     and type ('a, 'phantom1, 'phantom2) concat := ('a, 'phantom1, 'phantom2) T.concat
end
