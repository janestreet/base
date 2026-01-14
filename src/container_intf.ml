(** Provides generic signatures for container data structures.

    These signatures include functions ([iter], [fold], [exists], [for_all], ...) that you
    would expect to find in any container. Used by including [Container.S0] or
    [Container.S1] in the signature for every container-like data structure ([Array],
    [List], [String], ...) to ensure a consistent interface. *)

open! Import

open struct
  (*_ If we just bind these aliases directly, without the [open struct ... end], then mdx
      sometimes gets confused by seeing these aliases to constructors and prints e.g.
      [Base.Container_intf.Option.Some] instead of [Some] *)
  module Either = Either0
  module Option = Option0
  module Result = Result0
end

[@@@warning "-incompatible-with-upstream"]

[%%template
[@@@kind_set.define base_with_ext = (base, value mod external64)]

module Definitions = struct
  module Export = struct
    (** [Continue_or_stop.t] is used by the [f] argument to [fold_until] in order to
        indicate whether folding should continue, or stop early.

        @canonical Base.Container.Continue_or_stop *)
    module Continue_or_stop = struct
      type ('a : ka, 'b : kb) t =
        | Continue of 'a
        | Stop of 'b
      [@@kind ka = base_or_null, kb = base_or_null]
    end
  end

  include Export

  (** @canonical Base.Container.Summable *)
  module type Summable = sig
    type t : k

    (** The result of summing no values. *)
    val zero : (t Toplevel_value.t[@kind k])

    (** An operation that combines two [t]'s and handles [zero + x] by just returning [x],
        as well as in the symmetric case. *)
    val ( + ) : t @ m -> t @ m -> t @ m
  end
  [@@kind.explicit
    k = (base_or_null, value & value, value & value & value, value & value & value & value)]
  [@@mode m = (global, local)]

  module type Summable = Summable [@kind.explicit value_or_null] [@mode m]
  [@@kind.explicit __ = (value, value mod external64)] [@@mode m = (global, local)]

  module type Summable = Summable [@kind.explicit value] [@mode m]
  [@@mode m = (global, local)]

  module type Generic_types = sig
    [@@@kind.default k = ks]

    type ('a : k, _, _) t
    type ('a : k) elt : k
  end
  [@@kind_set.explicit
    ks = (value, value_or_null, value mod external64, base, base_with_ext)]

  module type Generic_types = Generic_types [@kind_set.explicit value]

  include struct
    [@@@alloc.default a @ m = (heap_global, stack_local)]

    (*_ We template the following interfaces over the set of kinds that are allowed in the
        container [t]. Each such kind set has an associated set of kinds allowed in types
        that appear as function arguments/returns but are not put into [t]. For example, a
        container that only allows [value mod external64]s can easily still implement
        [fold] with a [value] accumulator. That kind set is currently [ks or value]. *)

    [@@@kind_set.default.explicit
      ks = (value, value_or_null, value mod external64, base_with_ext)]

    module type Generic_without_mem = sig
      include Generic_types [@kind_set.explicit ks]

      include sig
        (*_ Here, we use [default_if_multiple] because this interface:
            (1) Sometimes uses [ks] to represent a single kind that we eventually would
                make an abstract kind
            (2) Sometimes uses [ks] to represent a "universe" of kinds that the interface
                broadly understands and supports all combinations of (which we eventually
                will replace with layout polymorphism)

            In case (1), there is only one version of each function, so we don't actually
            want to mangle it over [k = ks] (because it's silly for users of e.g.
            [imm_array] to have to call [(length [@kind immediate64])] to compute the
            length of their array).

            In case (2), since there are many versions of each function, we mangle the
            functions. *)

        [@@@kind.default_if_multiple k1 = ks]

        type ('a : k1, 'b, 'c) t := (('a, 'b, 'c) t[@kind k1]) [@@kind value]
        type ('a : k1) elt : k1 := ('a elt[@kind k1]) [@@kind value]

        val length : ('a : k1) 'p1 'p2. ('a, 'p1, 'p2) t @ m -> int
        val is_empty : ('a : k1) 'p1 'p2. ('a, 'p1, 'p2) t @ m -> bool

        (** [iter] must allow exceptions raised in [f] to escape, terminating the
            iteration cleanly. The same holds for all functions below taking an [f]. *)
        val iter
          : ('a : k1) 'p1 'p2.
          ('a, 'p1, 'p2) t @ m -> f:('a elt @ m -> unit) @ local -> unit
        [@@mode m = (global, m)]

        (** Returns [true] if and only if there exists an element for which the provided
            function evaluates to [true]. This is a short-circuiting operation. *)
        val exists
          : ('a : k1) 'p1 'p2.
          ('a, 'p1, 'p2) t @ m -> f:('a elt @ m -> bool) @ local -> bool
        [@@mode m = (global, m)]

        (** Returns [true] if and only if the provided function evaluates to [true] for
            all elements. This is a short-circuiting operation. *)
        val for_all
          : ('a : k1) 'p1 'p2.
          ('a, 'p1, 'p2) t @ m -> f:('a elt @ m -> bool) @ local -> bool
        [@@mode m = (global, m)]

        (** Returns the number of elements for which the provided function evaluates to
            true. *)
        val count
          : ('a : k1) 'p1 'p2.
          ('a, 'p1, 'p2) t @ m -> f:('a elt @ m -> bool) @ local -> int
        [@@mode m = (global, m)]

        (** Returns as an [option] the first element for which [f] evaluates to true. *)
        val find
          : ('a : k1) 'p1 'p2.
          ('a, 'p1, 'p2) t @ m
          -> f:('a elt @ m -> bool) @ local
          -> ('a elt Option.t[@kind k1 or value_or_null]) @ m
        [@@mode m = (global, m)]

        val to_list
          : ('a : k1) 'p1 'p2.
          ('a, 'p1, 'p2) t @ m
          -> ('a elt List0.Constructors.t[@kind k1 or value_or_null]) @ m
        [@@alloc __ @ m = (heap_global, a @ m)]

        (** Returns a min (resp. max) element from the collection using the provided
            [compare] function. In case of a tie, the first element encountered while
            traversing the collection is returned. The implementation uses [fold] so it
            has the same complexity as [fold]. Returns [None] iff the collection is empty. *)
        val min_elt
          : ('a : k1) 'p1 'p2.
          ('a, 'p1, 'p2) t @ m
          -> compare:('a elt @ m -> 'a elt @ m -> int) @ local
          -> ('a elt Option.t[@kind k1 or value_or_null]) @ m
        [@@mode m = (global, m)]

        val max_elt
          : ('a : k1) 'p1 'p2.
          ('a, 'p1, 'p2) t @ m
          -> compare:('a elt @ m -> 'a elt @ m -> int) @ local
          -> ('a elt Option.t[@kind k1 or value_or_null]) @ m
        [@@mode m = (global, m)]

        [@@@kind.default_if_multiple k2 = (ks or value)]

        (** Returns the sum of [f i] for all [i] in the container. The order in which the
            elements will be summed is unspecified. *)
        val sum
          : ('a : k1) ('sum : k2) 'p1 'p2.
          ((module Summable with type t = 'sum)[@mode mo] [@kind.explicit k2])
          -> ('a, 'p1, 'p2) t @ mi
          -> f:('a elt @ mi -> 'sum @ mo) @ local
          -> 'sum @ mo
        [@@mode mi = (global, m), mo = (global, m)]

        (** [iter_until t ~f ~finish] is a short-circuiting version of [iter]. If [f]
            returns [Stop x] the computation ceases and returns [x]. If [f] always returns
            [Continue ()] the final result is computed by [finish]. *)
        val iter_until
          : ('a : k1) 'p1 'p2 ('final : k2).
          ('a, 'p1, 'p2) t @ mi
          -> f:
               ('a elt @ mi
                -> ((unit, 'final) Continue_or_stop.t
                   [@kind value_or_null (k2 or value_or_null)])
                   @ mo)
             @ local
          -> finish:(unit -> 'final @ mo) @ local
          -> 'final @ mo
        [@@mode mi = (global, m), mo = (global, m)]

        (** [fold t ~init ~f] returns [f (... f (f (f init e1) e2) e3 ...) en], where
            [e1..en] are the elements of [t]. *)
        val fold
          : ('a : k1) 'p1 'p2 ('acc : k2).
          ('a, 'p1, 'p2) t @ mi
          -> init:'acc @ mo
          -> f:('acc @ mo -> 'a elt @ mi -> 'acc @ mo) @ local
          -> 'acc @ mo
        [@@mode mi = (global, m), mo = (global, m)]

        (** [fold_result t ~init ~f] is a short-circuiting version of [fold] that runs in
            the [Result] monad. If [f] returns an [Error _], that value is returned
            without any additional invocations of [f]. *)
        val fold_result
          : ('a : k1) 'p1 'p2 ('acc : k2) 'e.
          ('a, 'p1, 'p2) t @ mi
          -> init:'acc @ mo
          -> f:('acc @ mo -> 'a elt @ mi -> (('acc, 'e) Result.t[@kind k2]) @ mo) @ local
          -> (('acc, 'e) Result.t[@kind k2]) @ mo
        [@@mode mi = (global, m), mo = (global, m)]

        (** Returns the first evaluation of [f] that returns [Some], and returns [None] if
            there is no such element. *)
        val find_map
          : ('a : k1) 'p1 'p2 ('b : k2).
          ('a, 'p1, 'p2) t @ mi
          -> f:('a elt @ mi -> ('b Option.t[@kind k2]) @ mo) @ local
          -> ('b Option.t[@kind k2]) @ mo
        [@@mode mi = (global, m), mo = (global, m)]

        [@@@kind.default_if_multiple k3 = (ks or value)]

        (** [fold_until t ~init ~f ~finish] is a short-circuiting version of [fold]. If
            [f] returns [Stop _] the computation ceases and results in that value. If [f]
            returns [Continue _], the fold will proceed. If [f] never returns [Stop _],
            the final result is computed by [finish].

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
          : ('a : k1) 'p1 'p2 ('acc : k2) ('final : k3).
          ('a, 'p1, 'p2) t @ mi
          -> init:'acc @ mo
          -> f:
               ('acc @ mo
                -> 'a elt @ mi
                -> (('acc, 'final) Continue_or_stop.t
                   [@kind (k2 or value_or_null) (k3 or value_or_null)])
                   @ mo)
             @ local
          -> finish:('acc @ mo -> 'final @ mo) @ local
          -> 'final @ mo
        [@@mode mi = (global, m), mo = (global, m)]
      end
    end

    module type Generic_without_mem = Generic_without_mem
    [@kind_set.explicit ks] [@alloc a]
    [@@kind_set ks = value]

    module type Generic = sig
      include Generic_without_mem [@kind_set.explicit ks] [@alloc a]

      [@@@kind.default_if_multiple k = ks]

      (** Checks whether the provided element is there, using [equal]. *)
      val mem
        : ('a : k) 'p1 'p2.
        (('a, 'p1, 'p2) t[@kind k]) @ m
        -> ('a elt[@kind k]) @ m
        -> equal:(('a elt[@kind k]) @ m -> ('a elt[@kind k]) @ m -> bool) @ local
        -> bool
      [@@mode m = (global, m)]
    end

    (** Like [Generic], but [mem] does not accept an [equal] function, since [Make0]
        already takes [Elt.equal]. *)
    module type Generic_for_s0 = sig
      include Generic_without_mem [@kind_set.explicit ks] [@alloc a]

      [@@@kind.default_if_multiple k = ks]

      (** Checks whether the provided element is there, using equality on [elt]s. *)
      val mem : (('a, _, _) t[@kind k]) @ m -> ('a elt[@kind k]) @ m -> bool
      [@@mode m = (global, m)]
    end

    module type Generic_for_s0 = Generic_for_s0 [@kind_set.explicit ks] [@alloc a]
    [@@kind_set ks = value]
  end

  include struct
    [@@@alloc.default a @ m = (heap_global, stack_local)]

    module type Generic = sig
      include Generic [@kind_set.explicit ks] [@alloc a]

      (*_ This doesn't have a local version because arrays can't hold local values. *)
      val to_array : ('a, _, _) t -> 'a elt array
    end
    [@@kind_set ks = value]

    (** Signature for monomorphic container - a container for a specific element type,
        e.g., string, which is a container of characters ([type elt = char]) and never of
        anything else. *)
    module type
      [@kind_set.explicit
        ks
        = (*_ We can't implement this module type for [value_or_null] at the moment, since
              [value_or_null] elements cannot reside in arrays. We could
              template-specialize a separate [S0] for [value_or_null] that does not
              contain [to_array], but do not currently have a need for it. *)
        (value, value mod external64, base_with_ext)] S0 = sig
      include sig
        [@@@kind.default k = ks]

        type t
        type elt : k
      end

      include
        Generic_for_s0
      [@kind_set.explicit ks]
      [@alloc a]
      [@with:
        [@@@kind.default k = ks]

        type (_ : k, _, _) t := (t[@kind k])
        type (_ : k) elt := (elt[@kind k])]

      [@@@kind.default_if_multiple k = ks]

      (*_ This doesn't have a local version because arrays can't hold local values. *)
      val to_array : (t[@kind k]) -> (elt[@kind k]) array
    end

    module type S0 = S0 [@kind_set.explicit ks] [@alloc a] [@@kind_set ks = value]

    module type
      [@kind_set.explicit ks = (value, value mod external64, base_with_ext)] S0_phantom = sig
      include sig
        [@@@kind.default k = ks]

        type elt : k
        type 'phantom t
      end

      include
        Generic_for_s0
      [@kind_set.explicit ks]
      [@alloc a]
      [@with:
        [@@@kind.default k = ks]

        type (_ : k, 'phantom, _) t := ('phantom t[@kind k])
        type (_ : k) elt := (elt[@kind k])]

      [@@@kind.default_if_multiple k = ks]

      (*_ This doesn't have a local version because arrays can't hold local values. *)
      val to_array : (_ t[@kind k]) -> (elt[@kind k]) array
    end

    module type S0_phantom = S0_phantom [@kind_set.explicit ks] [@alloc a]
    [@@kind_set ks = value]
  end

  include struct
    [@@@alloc.default a @ m = (heap_global, stack_local)]

    [@@@kind_set.default.explicit
      ks = (value, value_or_null, value mod external64, base_with_ext)]

    (** Signature for polymorphic container, e.g., ['a list] or ['a array]. *)

    module type S1 = sig
      type ('a : k) t [@@kind k = ks]

      include
        Generic
      [@kind_set.explicit ks]
      [@alloc a]
      [@with:
        [@@@kind.default k = ks]

        type ('a : k, _, _) t := ('a t[@kind k])
        type ('a : k) elt := 'a]

      [@@@kind.default_if_multiple k = ks]

      (*_ This doesn't have a local version because arrays can't hold local values. *)
      val to_array : ('a : k mod separable). ('a t[@kind k]) -> 'a array
    end

    module type S1 = S1 [@kind_set.explicit ks] [@alloc a] [@@kind_set ks = value]

    module type S1_phantom = sig
      type ('a : k, 'phantom) t [@@kind k = ks]

      include
        Generic
      [@kind_set.explicit ks]
      [@alloc a]
      [@with:
        [@@@kind.default k = ks]

        type ('a : k, 'phantom, _) t := (('a, 'phantom) t[@kind k])
        type ('a : k) elt := 'a]

      [@@@kind.default_if_multiple k = ks]

      (*_ This doesn't have a local version because arrays can't hold local values. *)
      val to_array : ('a : k mod separable) 'p. (('a, 'p) t[@kind k]) -> 'a array
    end

    module type S1_phantom = S1_phantom [@kind_set.explicit ks] [@alloc a]
    [@@kind_set ks = value]

    module type Creators = sig
      include Generic_types [@kind_set.explicit ks]

      type (_, _, _) concat

      include sig
        [@@@kind.default_if_multiple k1 = ks]

        type ('a : k1, 'b, 'c) t := (('a, 'b, 'c) t[@kind k1]) [@@kind value]
        type ('a : k1) elt : k1 := ('a elt[@kind k1]) [@@kind value]

        val of_list
          : ('a : k1) 'p1 'p2.
          ('a elt List0.Constructors.t[@kind k1 or value_or_null]) @ m
          -> ('a, 'p1, 'p2) t @ m
        [@@alloc __ @ m = (heap_global, a @ m)]

        (** E.g., [append (of_list [a; b]) (of_list [c; d; e])] is
            [of_list [a; b; c; d; e]] *)
        val append
          : ('a : k1) 'p1 'p2.
          ('a, 'p1, 'p2) t @ m -> ('a, 'p1, 'p2) t @ m -> ('a, 'p1, 'p2) t @ m
        [@@alloc __ @ m = (heap_global, a @ m)]

        (** Concatenates a nested container. The elements of the inner containers are
            concatenated together in order to give the result. *)
        val concat
          : ('a : k1) 'p1 'p2.
          (('a, 'p1, 'p2) t, 'p1, 'p2) concat @ m -> ('a, 'p1, 'p2) t @ m
        [@@alloc __ @ m = (heap_global, a @ m)]

        (** [filter t ~f] returns all the elements of [t] that satisfy the predicate [f]. *)
        val filter
          : ('a : k1) 'p1 'p2.
          ('a, 'p1, 'p2) t @ m -> f:('a elt @ m -> bool) @ local -> ('a, 'p1, 'p2) t @ m
        [@@alloc __ @ m = (heap_global, a @ m)]

        (** [partition_tf t ~f] returns a pair [t1, t2], where [t1] is all elements of [t]
            that satisfy [f], and [t2] is all elements of [t] that do not satisfy [f]. The
            "tf" suffix is mnemonic to remind readers that the result is (trues, falses). *)
        val partition_tf
          : ('a : k1) 'p1 'p2.
          ('a, 'p1, 'p2) t @ m
          -> f:('a elt @ m -> bool) @ local
          -> ('a, 'p1, 'p2) t * ('a, 'p1, 'p2) t @ m
        [@@alloc __ @ m = (heap_global, a @ m)]
      end

      [@@@kind.default_if_multiple k1 = ks]
      [@@@kind.default_if_multiple k2 = ks]

      (** [map f (of_list [a1; ...; an])] applies [f] to [a1], [a2], ..., [an], in order,
          and builds a result equivalent to [of_list [f a1; ...; f an]]. *)
      val map
        : ('a : k1) 'p1 'p2 ('b : k2).
        (('a, 'p1, 'p2) t[@kind k1]) @ mi
        -> f:(('a elt[@kind k1]) @ mi -> ('b elt[@kind k2]) @ mo) @ local
        -> (('b, 'p1, 'p2) t[@kind k2]) @ mo
      [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]

      (** [filter_map t ~f] applies [f] to every [x] in [t]. The result contains every [y]
          for which [f x] returns [Some y]. *)
      val filter_map
        : ('a : k1) 'p1 'p2 ('b : k2).
        (('a, 'p1, 'p2) t[@kind k1]) @ mi
        -> f:
             (('a elt[@kind k1]) @ mi
              -> (('b elt[@kind k2]) Option.t[@kind k2 or value_or_null]) @ mo)
           @ local
        -> (('b, 'p1, 'p2) t[@kind k2]) @ mo
      [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]

      (** [concat_map t ~f] is equivalent to [concat (map t ~f)]. *)
      val concat_map
        : ('a : k1) 'p1 'p2 ('b : k2).
        (('a, 'p1, 'p2) t[@kind k1]) @ mi
        -> f:(('a elt[@kind k1]) @ mi -> (('b, 'p1, 'p2) t[@kind k2]) @ mo) @ local
        -> (('b, 'p1, 'p2) t[@kind k2]) @ mo
      [@@mode mi = (global, m)] [@@alloc a @ mo = (heap_global, a @ m)]

      [@@@kind.default_if_multiple k3 = ks]

      (** [partition_map t ~f] partitions [t] according to [f]. *)
      val partition_map
        : ('a : k1) 'p1 'p2 ('b : k2) ('c : k3).
        (('a, 'p1, 'p2) t[@kind k1]) @ mi
        -> f:
             (('a elt[@kind k1]) @ mi
              -> ((('b elt[@kind k2]), ('c elt[@kind k3])) Either.t
                 [@kind (k2 or value_or_null) (k3 or value_or_null)])
                 @ mo)
           @ local
        -> (('b, 'p1, 'p2) t[@kind k2]) * (('c, 'p1, 'p2) t[@kind k3]) @ mo
      [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]
    end

    module type Creators = Creators [@kind_set.explicit ks] [@alloc a]
    [@@kind_set ks = value]

    module type Generic_with_creators = sig
      include Generic [@kind_set.explicit ks] [@alloc a]

      include
        Creators
      [@kind_set.explicit ks]
      [@alloc a]
      [@with:
        [@@@kind.default k = ks]

        type ('a : k, 'b, 'c) t := (('a, 'b, 'c) t[@kind k])
        type ('a : k) elt := ('a elt[@kind k])]
    end

    (** Like [Generic_with_creators], but [mem] does not accept an [equal] function, since
        [Make0_with_creators] already takes [Elt.equal]. *)
    module type Generic_with_creators_for_s0 = sig
      include Generic_for_s0 [@kind_set.explicit ks] [@alloc a]

      include
        Creators
      [@kind_set.explicit ks]
      [@alloc a]
      [@with:
        [@@@kind.default k = ks]

        type ('a : k, 'b, 'c) t := (('a, 'b, 'c) t[@kind k])
        type ('a : k) elt := ('a elt[@kind k])]
    end

    module type Generic_with_creators_for_s0 = Generic_with_creators_for_s0
    [@kind_set.explicit ks]
    [@@kind_set ks = value]
  end

  include struct
    [@@@alloc.default a @ m = (heap_global, stack_local)]

    module type Generic_with_creators = sig
      include Generic_with_creators [@kind_set.explicit ks] [@alloc a]

      (*_ This doesn't have a local version because arrays can't hold local values. *)
      val to_array : ('a, _, _) t -> 'a elt array

      val of_array : 'a elt array @ m -> ('a, _, _) t @ m
      [@@alloc __ @ m = (heap_global, a @ m)]
    end
    [@@kind_set ks = value]

    (*_ This is outside the template because an [immediate] container can't contain itself *)

    module type [@kind_set.explicit ks = (value, base_with_ext)] S0_with_creators = sig
      include sig
        [@@@kind.default k = ks]

        type t
        type elt : k
      end

      include
        Generic_with_creators_for_s0
      [@kind_set.explicit ks]
      [@alloc a]
      [@with:
        type (_, _, _) concat := t list

        [@@@kind.default k = ks]

        type ('a, _, _) t := (t[@kind k])
        type _ elt := (elt[@kind k])]

      [@@@kind.default_if_multiple k = ks]

      (*_ This doesn't have a local version because arrays can't hold local values. *)
      val to_array : (t[@kind k]) -> (elt[@kind k]) array

      val of_array : (elt[@kind k]) array @ m -> (t[@kind k]) @ m
      [@@alloc __ @ m = (heap_global, a @ m)]
    end

    module type S0_with_creators = S0_with_creators [@kind_set.explicit ks] [@alloc a]
    [@@kind_set ks = value]

    module type
      [@kind_set.explicit ks = (value, value_or_null, base_with_ext)] S1_with_creators = sig
      type ('a : k) t [@@kind k = ks]

      include
        Generic_with_creators
      [@kind_set.explicit ks]
      [@alloc a]
      [@with:
        type ('a, _, _) concat := 'a t

        [@@@kind.default k = ks]

        type ('a : k, _, _) t := ('a t[@kind k])
        type ('a : k) elt := 'a]

      [@@@kind.default_if_multiple k = ks]

      (*_ This doesn't have a local version because arrays can't hold local values. *)
      val to_array : ('a : k mod separable). ('a t[@kind k]) -> 'a array

      val of_array : ('a : k mod separable). 'a array @ m -> ('a t[@kind k]) @ m
      [@@alloc __ @ m = (heap_global, a @ m)]
    end

    module type S1_with_creators = S1_with_creators [@kind_set.explicit ks] [@alloc a]
    [@@kind_set ks = value]
  end

  type ('t, 'a : any, 'acc : any) fold =
    't @ mi
    -> init:'acc @ mo
    -> f:('acc @ mo -> 'a @ mi -> 'acc @ mo) @ local
    -> 'acc @ mo
  [@@mode mi = (global, local), mo = (global, local)]

  type ('t, 'a : any, 'acc : any, 'final : any) fold_until =
    't @ mi
    -> init:'acc @ mo
    -> f:('acc @ mo -> 'a @ mi -> ('acc, 'final) Continue_or_stop.t @ mo) @ local
    -> finish:('acc @ mo -> 'final @ mo) @ local
    -> 'final @ mo
  [@@mode mi = (global, local), mo = (global, local)]

  type ('t, 'a : any) iter = 't @ m -> f:('a @ m -> unit) @ local -> unit
  [@@mode m = (global, local)]

  type ('t, 'a : any, 'final : any) iter_until =
    't @ mi
    -> f:('a @ mi -> (unit, 'final) Continue_or_stop.t @ mo) @ local
    -> finish:(unit -> 'final @ mo) @ local
    -> 'final @ mo
  [@@mode mi = (global, local), mo = (global, local)]

  type 't length = 't @ m -> int [@@mode m = (global, local)]

  module type Make_gen_arg = sig
    type ('a, 'phantom1, 'phantom2) t
    type 'a elt

    val fold_until : ((('a, _, _) t, 'a elt, 'acc, 'final) fold_until[@mode mi mo])
    [@@mode mi = (global, m), mo = (global, m)]

    val fold
      : [ `Define_using_fold_until
        | `Custom of ((('a, _, _) t, 'a elt, 'acc) fold[@mode mi mo])
        ]
    [@@mode mi = (global, m), mo = (global, m)]

    val iter_until
      : [ `Define_using_fold_until
        | `Custom of ((('a, _, _) t, 'a elt, 'final) iter_until[@mode mi mo])
        ]
    [@@mode mi = (global, m), mo = (global, m)]

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
        | `Custom of ((('a, _, _) t, 'a elt) iter[@mode m])
        ]
    [@@mode m = (global, m)]

    (** The [length] argument to [Container.Make] specifies how to implement the
        container's [length] function. [`Define_using_fold] means to define [length] via:

        {[
          length t ~f = Container.length ~fold t ~f
        ]}

        [`Custom] overrides the default implementation, presumably with something more
        efficient. Several other functions returned by [Container.Make] are defined in
        terms of [length], so passing in a more efficient [length] will improve their
        efficiency as well. *)
    val length : [ `Define_using_fold | `Custom of (('a, _, _) t length[@mode m]) ]
  end
  [@@mode m = (global, local)]

  module type Make_arg = sig
    type 'a t

    include Make_gen_arg [@mode m] with type ('a, _, _) t := 'a t and type 'a elt := 'a
  end
  [@@mode m = (global, local)]

  module type Make0_arg = sig
    module Elt : sig
      type t

      val equal : t @ m -> t @ m -> bool [@@mode m = (global, m)]
    end

    type t

    include Make_gen_arg [@mode m] with type ('a, _, _) t := t and type 'a elt := Elt.t
  end
  [@@mode m = (global, local)]

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

  module type [@kind.explicit k = (value, value_or_null)] Derived = sig
    include sig
      [@@@mode.default m = (global, local)]

      (** Generic defintions that rely on [With_return] and therefore don't support local
          return values. *)

      val fold_until
        : 't ('a : k) ('acc : k) ('final : k).
        fold:(('t, 'a, 'acc) fold[@mode m global])
        -> (('t, 'a, 'acc, 'final) fold_until[@mode m global])

      (** Generic definitions of container operations in terms of [fold].

          E.g.: [iter ~fold t ~f = fold t ~init:() ~f:(fun () a -> f a)]. *)

      val iter_via_fold
        : 't ('a : k).
        fold:(('t, 'a, unit) fold[@mode m global]) -> (('t, 'a) iter[@mode m])

      val count
        : 't ('a : k).
        fold:(('t, 'a, int) fold[@mode m global])
        -> 't @ m
        -> f:('a @ m -> bool) @ local
        -> int

      val min_elt
        : 't ('a : k).
        fold:(('t, 'a, 'a option) fold[@mode m m])
        -> 't @ m
        -> compare:('a @ m -> 'a @ m -> int) @ local
        -> 'a option @ m

      val max_elt
        : 't ('a : k).
        fold:(('t, 'a, 'a option) fold[@mode m m])
        -> 't @ m
        -> compare:('a @ m -> 'a @ m -> int) @ local
        -> 'a option @ m

      val length
        : 't ('a : k).
        fold:(('t, 'a, int) fold[@mode m global]) -> ('t length[@mode m])
    end

    val to_list
      : 't ('a : k).
      fold:(('t, 'a, 'a list) fold[@mode m m]) -> 't @ m -> 'a list @ m
    [@@alloc __ @ m = (heap_global, stack_local)]

    val sum
      : 't ('a : k) ('sum : k).
      fold:(('t, 'a, 'sum) fold[@mode mi mo])
      -> ((module Summable with type t = 'sum)[@kind.explicit k] [@mode mo])
      -> 't @ mi
      -> f:('a @ mi -> 'sum @ mo) @ local
      -> 'sum @ mo
    [@@mode mi = (global, local), mo = (global, local)]

    (** Generic definitions of container operations in terms of [fold_until]. *)

    val fold
      : 't ('a : k) ('acc : k).
      fold_until:(('t, 'a, 'acc, 'acc) fold_until[@mode mi mo])
      -> (('t, 'a, 'acc) fold[@mode mi mo])
    [@@mode mi = (global, local), mo = (global, local)]

    val fold_result
      : 't ('a : k) ('acc : k) 'e.
      fold_until:(('t, 'a, 'acc, ('acc, 'e) Result.t) fold_until[@mode mi mo])
      -> 't @ mi
      -> init:'acc @ mo
      -> f:('acc @ mo -> 'a @ mi -> ('acc, 'e) Result.t @ mo) @ local
      -> ('acc, 'e) Result.t @ mo
    [@@mode mi = (global, local), mo = (global, local)]

    val iter_until
      : 't ('a : k) ('final : k).
      fold_until:(('t, 'a, unit, 'final) fold_until[@mode mi mo])
      -> (('t, 'a, 'final) iter_until[@mode mi mo])
    [@@mode mi = (global, local), mo = (global, local)]

    (** Generic definitions of container operations in terms of [iter_until]. *)

    include sig
      [@@@mode.default m = (global, local)]

      val iter_via_iter_until
        : 't ('a : k).
        iter_until:(('t, 'a, unit) iter_until[@mode m global]) -> (('t, 'a) iter[@mode m])

      val is_empty
        : 't ('a : k).
        iter_until:(('t, 'a, bool) iter_until[@mode m global]) -> 't @ m -> bool

      val mem
        : 't ('a : k).
        iter_until:(('t, 'a, bool) iter_until[@mode m global])
        -> 't @ m
        -> 'a @ m
        -> equal:('a @ m -> 'a @ m -> bool) @ local
        -> bool

      val exists
        : 't ('a : k).
        iter_until:(('t, 'a, bool) iter_until[@mode m global])
        -> 't @ m
        -> f:('a @ m -> bool) @ local
        -> bool

      val for_all
        : 't ('a : k).
        iter_until:(('t, 'a, bool) iter_until[@mode m global])
        -> 't @ m
        -> f:('a @ m -> bool) @ local
        -> bool

      val find
        : 't ('a : k).
        iter_until:(('t, 'a, 'a option) iter_until[@mode m m])
        -> 't @ m
        -> f:('a @ m -> bool) @ local
        -> 'a option @ m
    end

    val find_map
      : 't ('a : k) ('b : k).
      iter_until:(('t, 'a, 'b option) iter_until[@mode mi mo])
      -> 't @ mi
      -> f:('a @ mi -> 'b option @ mo) @ local
      -> 'b option @ mo
    [@@mode mi = (global, local), mo = (global, local)]

    (** Generic definitons of container operations in terms of [iter] and [length]. *)

    val to_array
      : 't ('a : k mod separable).
      length:'t length -> iter:('t, 'a) iter -> 't -> 'a array
  end
end

module type Container = sig @@ portable
  include module type of struct
    include Definitions
  end

  module%template Derived : Derived [@kind.explicit k]
  [@@kind.explicit k = (value, value_or_null)]

  include module type of Derived [@kind.explicit value]

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
  module%template.portable Make (T : Make_arg [@mode m]) :
    S1 [@alloc a] with type 'a t := 'a T.t
  [@@alloc a @ m = (heap_global, stack_local)]

  module%template.portable Make0 (T : Make0_arg [@mode m]) :
    S0 [@alloc a] with type t := T.t and type elt := T.Elt.t
  [@@alloc a @ m = (heap_global, stack_local)]

  module%template.portable Make_gen (T : Make_gen_arg [@mode m]) :
    Generic
    [@alloc a]
    with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) T.t
     and type 'a elt := 'a T.elt
  [@@alloc a @ m = (heap_global, stack_local)]

  module%template.portable Make_with_creators (T : Make_with_creators_arg) :
    S1_with_creators [@alloc heap] with type 'a t := 'a T.t

  module%template.portable Make0_with_creators (T : Make0_with_creators_arg) :
    S0_with_creators [@alloc heap] with type t := T.t and type elt := T.Elt.t

  module%template.portable Make_gen_with_creators (T : Make_gen_with_creators_arg) :
    Generic_with_creators
    [@alloc heap]
    with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) T.t
     and type 'a elt := 'a T.elt
     and type ('a, 'phantom1, 'phantom2) concat := ('a, 'phantom1, 'phantom2) T.concat
end]
