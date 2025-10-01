(** Provides generic signatures for container data structures.

    These signatures include functions ([iter], [fold], [exists], [for_all], ...) that you
    would expect to find in any container. Used by including [Container.S0] or
    [Container.S1] in the signature for every container-like data structure ([Array],
    [List], [String], ...) to ensure a consistent interface. *)

open! Import
module Either = Either0
module Option = Option0
module Result = Result0

[@@@warning "-incompatible-with-upstream"]

[%%template
module Definitions = struct
  module Export = struct
    (** [Continue_or_stop.t] is used by the [f] argument to [fold_until] in order to
        indicate whether folding should continue, or stop early.

        @canonical Base.Container.Continue_or_stop *)
    module Continue_or_stop = struct
      type ('a : ka, 'b : kb) t =
        | Continue of 'a
        | Stop of 'b
      [@@kind
        ka = (float64, bits32, bits64, word, value)
        , kb = (float64, bits32, bits64, word, value)]

      type ('a : ka, 'b : kb) t = (('a, 'b) t[@kind value kb])
      [@@kind ka = (immediate, immediate64), kb = (float64, bits32, bits64, word, value)]

      type ('a : ka, 'b : kb) t = (('a, 'b) t[@kind ka value])
      [@@kind
        ka = (float64, bits32, bits64, word, value, immediate, immediate64)
        , kb = (immediate, immediate64)]
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
  [@@kind
    k
    = ( value
      , immediate
      , immediate64
      , float64
      , bits32
      , bits64
      , word
      , value & value
      , value & value & value
      , value & value & value & value )]
  [@@mode m = (global, local)]

  module type Generic_types = sig
    type ('a : k, _, _) t
    type ('a : k) elt
  end
  [@@kind k = (value, immediate, immediate64, value mod external_, value mod external64)]

  module type Generic_types__base = sig
    [@@@kind.default k = (value, float64, bits32, bits64, word, immediate, immediate64)]

    type ('a : k, _, _) t
    type ('a : k) elt : k
  end

  [%%template
  [@@@alloc.default a @ m = (heap_global, stack_local)]

  module type Generic_without_mem = sig
    include Generic_types [@kind k]

    val length : (_, _, _) t @ m -> int
    val is_empty : (_, _, _) t @ m -> bool

    (** [iter] must allow exceptions raised in [f] to escape, terminating the iteration
        cleanly. The same holds for all functions below taking an [f]. *)
    val iter : ('a, _, _) t @ m -> f:('a elt @ m -> unit) @ local -> unit
    [@@mode m = (global, m)]

    (** [iter_until t ~f ~finish] is a short-circuiting version of [iter]. If [f] returns
        [Stop x] the computation ceases and returns [x]. If [f] always returns
        [Continue ()] the final result is computed by [finish]. *)
    val iter_until
      :  ('a, _, _) t @ mi
      -> f:('a elt @ mi -> (unit, 'final) Continue_or_stop.t @ mo) @ local
      -> finish:(unit -> 'final @ mo) @ local
      -> 'final @ mo
    [@@mode mi = (global, m), mo = (global, m)]

    (** [fold t ~init ~f] returns [f (... f (f (f init e1) e2) e3 ...) en], where [e1..en]
        are the elements of [t]. *)
    val fold
      :  ('a, _, _) t @ mi
      -> init:'acc @ mo
      -> f:('acc @ mo -> 'a elt @ mi -> 'acc @ mo) @ local
      -> 'acc @ mo
    [@@mode mi = (global, m), mo = (global, m)]

    (** [fold_result t ~init ~f] is a short-circuiting version of [fold] that runs in the
        [Result] monad. If [f] returns an [Error _], that value is returned without any
        additional invocations of [f]. *)
    val fold_result
      :  ('a, _, _) t @ mi
      -> init:'acc @ mo
      -> f:('acc @ mo -> 'a elt @ mi -> ('acc, 'e) Result.t @ mo) @ local
      -> ('acc, 'e) Result.t @ mo
    [@@mode mi = (global, m), mo = (global, m)]

    (** [fold_until t ~init ~f ~finish] is a short-circuiting version of [fold]. If [f]
        returns [Stop _] the computation ceases and results in that value. If [f] returns
        [Continue _], the fold will proceed. If [f] never returns [Stop _], the final
        result is computed by [finish].

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
      :  ('a, _, _) t @ mi
      -> init:'acc @ mo
      -> f:('acc @ mo -> 'a elt @ mi -> ('acc, 'final) Continue_or_stop.t @ mo) @ local
      -> finish:('acc @ mo -> 'final @ mo) @ local
      -> 'final @ mo
    [@@mode mi = (global, m), mo = (global, m)]

    (** Returns [true] if and only if there exists an element for which the provided
        function evaluates to [true]. This is a short-circuiting operation. *)
    val exists : ('a, _, _) t @ m -> f:('a elt @ m -> bool) @ local -> bool
    [@@mode m = (global, m)]

    (** Returns [true] if and only if the provided function evaluates to [true] for all
        elements. This is a short-circuiting operation. *)
    val for_all : ('a, _, _) t @ m -> f:('a elt @ m -> bool) @ local -> bool
    [@@mode m = (global, m)]

    (** Returns the number of elements for which the provided function evaluates to true. *)
    val count : ('a, _, _) t @ m -> f:('a elt @ m -> bool) @ local -> int
    [@@mode m = (global, m)]

    (** Returns the sum of [f i] for all [i] in the container. The order in which the
        elements will be summed is unspecified. *)
    val sum
      :  ((module Summable with type t = 'sum)[@mode mo])
      -> ('a, _, _) t @ mi
      -> f:('a elt @ mi -> 'sum @ mo) @ local
      -> 'sum @ mo
    [@@mode mi = (global, m), mo = (global, m)]

    (** Returns as an [option] the first element for which [f] evaluates to true. *)
    val find : ('a, _, _) t @ m -> f:('a elt @ m -> bool) @ local -> 'a elt option @ m
    [@@mode m = (global, m)]

    (** Returns the first evaluation of [f] that returns [Some], and returns [None] if
        there is no such element. *)
    val find_map
      :  ('a, _, _) t @ mi
      -> f:('a elt @ mi -> 'b option @ mo) @ local
      -> 'b option @ mo
    [@@mode mi = (global, m), mo = (global, m)]

    val to_list : ('a, _, _) t @ m -> 'a elt list @ m
    [@@alloc __ @ m = (heap_global, a @ m)]

    (*_ This doesn't have a local version because arrays can't hold local values. *)
    val to_array : ('a, _, _) t -> 'a elt array

    (** Returns a min (resp. max) element from the collection using the provided [compare]
        function. In case of a tie, the first element encountered while traversing the
        collection is returned. The implementation uses [fold] so it has the same
        complexity as [fold]. Returns [None] iff the collection is empty. *)
    val min_elt
      :  ('a, _, _) t @ m
      -> compare:('a elt @ m -> 'a elt @ m -> int) @ local
      -> 'a elt option @ m
    [@@mode m = (global, m)]

    val max_elt
      :  ('a, _, _) t @ m
      -> compare:('a elt @ m -> 'a elt @ m -> int) @ local
      -> 'a elt option @ m
    [@@mode m = (global, m)]
  end
  [@@kind k = (value, immediate, immediate64, value mod external_, value mod external64)]

  module type Generic_without_mem__base = sig
    include Generic_types__base

    [@@@kind k1 = (value, float64, bits32, bits64, word, immediate, immediate64)]

    include sig
        type ('a : k1, 'b, 'c) t
        type ('a : k1) elt : k1

        [@@@kind.default k1]

        val length : (_, _, _) t @ m -> int
        val is_empty : (_, _, _) t @ m -> bool

        (** [iter] must allow exceptions raised in [f] to escape, terminating the
            iteration cleanly. The same holds for all functions below taking an [f]. *)
        val iter : ('a, _, _) t @ m -> f:('a elt @ m -> unit) @ local -> unit
        [@@mode m = (global, m)]

        (** Returns [true] if and only if there exists an element for which the provided
            function evaluates to [true]. This is a short-circuiting operation. *)
        val exists : ('a, _, _) t @ m -> f:('a elt @ m -> bool) @ local -> bool
        [@@mode m = (global, m)]

        (** Returns [true] if and only if the provided function evaluates to [true] for
            all elements. This is a short-circuiting operation. *)
        val for_all : ('a, _, _) t @ m -> f:('a elt @ m -> bool) @ local -> bool
        [@@mode m = (global, m)]

        (** Returns the number of elements for which the provided function evaluates to
            true. *)
        val count : ('a, _, _) t @ m -> f:('a elt @ m -> bool) @ local -> int
        [@@mode m = (global, m)]

        (** Returns the sum of [f i] for all [i] in the container. The order in which the
            elements will be summed is unspecified. *)
        val sum
          :  ((module Summable with type t = 'sum)[@mode m] [@kind k1])
          -> ('a, _, _) t @ m
          -> f:('a elt @ m -> 'sum @ m) @ local
          -> 'sum @ m
        [@@mode m = (global, m)]

        (** Returns as an [option] the first element for which [f] evaluates to true. *)
        val find
          :  ('a, _, _) t @ m
          -> f:('a elt @ m -> bool) @ local
          -> ('a elt Option.t[@kind k1]) @ m
        [@@mode m = (global, m)]

        val to_list : ('a, _, _) t @ m -> ('a elt List0.Constructors.t[@kind k1]) @ m
        [@@alloc __ @ m = (heap_global, a @ m)]

        val to_array : ('a, _, _) t -> 'a elt array @ m
        [@@alloc __ @ m = (heap_global, a @ m)]

        (** Returns a min (resp. max) element from the collection using the provided
            [compare] function. In case of a tie, the first element encountered while
            traversing the collection is returned. The implementation uses [fold] so it
            has the same complexity as [fold]. Returns [None] iff the collection is empty. *)
        val min_elt
          :  ('a, _, _) t @ m
          -> compare:('a elt @ m -> 'a elt @ m -> int) @ local
          -> ('a elt Option.t[@kind k1]) @ m
        [@@mode m = (global, m)]

        val max_elt
          :  ('a, _, _) t @ m
          -> compare:('a elt @ m -> 'a elt @ m -> int) @ local
          -> ('a elt Option.t[@kind k1]) @ m
        [@@mode m = (global, m)]

        [@@@kind.default
          k2 = (value, float64, bits32, bits64, word, immediate, immediate64)]

        (** [iter_until t ~f ~finish] is a short-circuiting version of [iter]. If [f]
            returns [Stop x] the computation ceases and returns [x]. If [f] always returns
            [Continue ()] the final result is computed by [finish]. *)
        val iter_until
          : ('a : k1) 'b 'c ('final : k2).
          ('a, 'b, 'c) t @ mi
          -> f:('a elt @ mi -> ((unit, 'final) Continue_or_stop.t[@kind value k2]) @ mo)
             @ local
          -> finish:(unit -> 'final @ mo) @ local
          -> 'final @ mo
        [@@mode mi = (global, m), mo = (global, m)]

        (** [fold t ~init ~f] returns [f (... f (f (f init e1) e2) e3 ...) en], where
            [e1..en] are the elements of [t]. *)
        val fold
          : ('a : k1) 'b 'c ('acc : k2).
          ('a, 'b, 'c) t @ mi
          -> init:'acc @ mo
          -> f:('acc @ mo -> 'a elt @ mi -> 'acc @ mo) @ local
          -> 'acc @ mo
        [@@mode mi = (global, m), mo = (global, m)]

        (** [fold_result t ~init ~f] is a short-circuiting version of [fold] that runs in
            the [Result] monad. If [f] returns an [Error _], that value is returned
            without any additional invocations of [f]. *)
        val fold_result
          : ('a : k1) 'b 'c ('acc : k2) 'e.
          ('a, 'b, 'c) t @ mi
          -> init:'acc @ mo
          -> f:('acc @ mo -> 'a elt @ mi -> (('acc, 'e) Result.t[@kind k2]) @ mo) @ local
          -> (('acc, 'e) Result.t[@kind k2]) @ mo
        [@@mode mi = (global, m), mo = (global, m)]

        (** Returns the first evaluation of [f] that returns [Some], and returns [None] if
            there is no such element. *)
        val find_map
          :  ('a, _, _) t @ mi
          -> f:('a elt @ mi -> ('b Option.t[@kind k2]) @ mo) @ local
          -> ('b Option.t[@kind k2]) @ mo
        [@@mode mi = (global, m), mo = (global, m)]

        [@@@kind.default
          k3 = (value, float64, bits32, bits64, word, immediate, immediate64)]

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
          :  ('a, _, _) t @ mi
          -> init:'acc @ mo
          -> f:
               ('acc @ mo
                -> 'a elt @ mi
                -> (('acc, 'final) Continue_or_stop.t[@kind k2 k3]) @ mo)
             @ local
          -> finish:('acc @ mo -> 'final @ mo) @ local
          -> 'final @ mo
        [@@mode mi = (global, m), mo = (global, m)]
      end
      with type ('a : k1, 'b, 'c) t := (('a, 'b, 'c) t[@kind k1])
       and type ('a : k1) elt := ('a elt[@kind k1])
  end

  module type Generic = sig
    include Generic_without_mem [@kind k] [@alloc a]

    (** Checks whether the provided element is there, using [equal]. *)
    val mem
      :  ('a, _, _) t @ m
      -> 'a elt @ m
      -> equal:('a elt @ m -> 'a elt @ m -> bool) @ local
      -> bool
    [@@mode m = (global, m)]
  end
  [@@kind k = (value, immediate, immediate64, value mod external_, value mod external64)]

  module type Generic__base = sig
    include Generic_without_mem__base [@alloc a]

    [@@@kind.default k = (value, float64, bits32, bits64, word, immediate, immediate64)]

    (** Checks whether the provided element is there, using [equal]. *)
    val mem
      :  (('a, _, _) t[@kind k]) @ m
      -> ('a elt[@kind k]) @ m
      -> equal:(('a elt[@kind k]) @ m -> ('a elt[@kind k]) @ m -> bool) @ local
      -> bool
  end

  (** Like [Generic], but [mem] does not accept an [equal] function, since [Make0] already
      takes [Elt.equal]. *)
  module type Generic_for_s0 = sig
    include Generic_without_mem [@kind k] [@alloc a]

    (** Checks whether the provided element is there, using equality on [elt]s. *)
    val mem : ('a, _, _) t @ m -> 'a elt @ m -> bool
    [@@mode m = (global, m)]
  end
  [@@kind k = (value, immediate, immediate64)]

  module type Generic_for_s0__base = sig
    include Generic_without_mem__base [@alloc a]

    [@@@kind.default k = (value, float64, bits32, bits64, word, immediate, immediate64)]

    (** Checks whether the provided element is there, using equality on [elt]s. *)
    val mem : (('a, _, _) t[@kind k]) @ m -> ('a elt[@kind k]) @ m -> bool
    [@@mode m = (global, m)]
  end

  (** Signature for monomorphic container - a container for a specific element type, e.g.,
      string, which is a container of characters ([type elt = char]) and never of anything
      else. *)
  module type S0 = sig
    type t
    type elt

    include Generic_for_s0 [@alloc a] with type (_, _, _) t := t and type _ elt := elt
  end

  module type S0__base = sig
    [%%template:
    [@@@kind.default k = (value, float64, bits32, bits64, word, immediate, immediate64)]

    type t
    type elt : k]

    include
      Generic_for_s0__base
      [@alloc a]
      with type ('a, _, _) t := t
       and type ('a, _, _) t__float64 := t__float64
       and type ('a, _, _) t__bits32 := t__bits32
       and type ('a, _, _) t__bits64 := t__bits64
       and type ('a, _, _) t__word := t__word
       and type ('a, _, _) t__immediate := t__immediate
       and type ('a, _, _) t__immediate64 := t__immediate64
       and type _ elt := elt
       and type _ elt__float64 := elt__float64
       and type _ elt__bits32 := elt__bits32
       and type _ elt__bits64 := elt__bits64
       and type _ elt__word := elt__word
       and type _ elt__immediate := elt__immediate
       and type _ elt__immediate64 := elt__immediate64
  end

  module type S0_phantom = sig
    type elt
    type 'phantom t

    include
      Generic_for_s0
      [@alloc a]
      with type (_, 'phantom, _) t := 'phantom t
       and type _ elt := elt
  end

  module type S0_phantom__base = sig
    [%%template:
    [@@@kind.default k = (value, float64, bits32, bits64, word, immediate, immediate64)]

    type elt : k
    type 'phantom t]

    include
      Generic_for_s0__base
      [@alloc a]
      with type (_, 'phantom, _) t := 'phantom t
       and type (_, 'phantom, _) t__float64 := 'phantom t__float64
       and type (_, 'phantom, _) t__bits32 := 'phantom t__bits32
       and type (_, 'phantom, _) t__bits64 := 'phantom t__bits64
       and type (_, 'phantom, _) t__word := 'phantom t__word
       and type (_, 'phantom, _) t__immediate := 'phantom t__immediate
       and type (_, 'phantom, _) t__immediate64 := 'phantom t__immediate64
       and type _ elt := elt
       and type _ elt__float64 := elt__float64
       and type _ elt__bits32 := elt__bits32
       and type _ elt__bits64 := elt__bits64
       and type _ elt__word := elt__word
       and type _ elt__immediate := elt__immediate
       and type _ elt__immediate64 := elt__immediate64
  end

  (** Signature for polymorphic container, e.g., ['a list] or ['a array]. *)
  module type S1 = sig
    type ('a : k) t

    include
      Generic [@kind k] [@alloc a] with type ('a, _, _) t := 'a t and type 'a elt := 'a
  end
  [@@kind k = (value, immediate, immediate64, value mod external_, value mod external64)]

  module type S1__base = sig
    type ('a : k) t
    [@@kind k = (value, float64, bits32, bits64, word, immediate, immediate64)]

    include
      Generic__base
      [@alloc a]
      with type ('a, _, _) t := 'a t
       and type ('a, _, _) t__float64 := 'a t__float64
       and type ('a, _, _) t__bits32 := 'a t__bits32
       and type ('a, _, _) t__bits64 := 'a t__bits64
       and type ('a, _, _) t__word := 'a t__word
       and type ('a, _, _) t__immediate := 'a t__immediate
       and type ('a, _, _) t__immediate64 := 'a t__immediate64
       and type 'a elt := 'a
       and type 'a elt__float64 := 'a
       and type 'a elt__bits32 := 'a
       and type 'a elt__bits64 := 'a
       and type 'a elt__word := 'a
       and type 'a elt__immediate := 'a
       and type 'a elt__immediate64 := 'a
  end

  module type S1_phantom = sig
    type ('a : k, 'phantom) t

    include
      Generic
      [@kind k] [@alloc a]
      with type ('a, 'phantom, _) t := ('a, 'phantom) t
       and type 'a elt := 'a
  end
  [@@kind k = (value, immediate, immediate64)]

  module type S1_phantom__base = sig
    type ('a : k, 'phantom) t
    [@@kind k = (value, float64, bits32, bits64, word, immediate, immediate64)]

    include
      Generic__base
      [@alloc a]
      with type ('a, 'phantom, _) t := ('a, 'phantom) t
       and type ('a, 'phantom, _) t__float64 := ('a, 'phantom) t__float64
       and type ('a, 'phantom, _) t__bits32 := ('a, 'phantom) t__bits32
       and type ('a, 'phantom, _) t__bits64 := ('a, 'phantom) t__bits64
       and type ('a, 'phantom, _) t__word := ('a, 'phantom) t__word
       and type ('a, 'phantom, _) t__immediate := ('a, 'phantom) t__immediate
       and type ('a, 'phantom, _) t__immediate64 := ('a, 'phantom) t__immediate64
       and type 'a elt := 'a
       and type 'a elt__float64 := 'a
       and type 'a elt__bits32 := 'a
       and type 'a elt__bits64 := 'a
       and type 'a elt__word := 'a
       and type 'a elt__immediate := 'a
       and type 'a elt__immediate64 := 'a
  end

  module type Creators = sig
    include Generic_types

    type (_, _, _) concat

    val of_list : 'a elt list @ m -> ('a, _, _) t @ m
    [@@alloc __ @ m = (heap_global, a @ m)]

    val of_array : 'a elt array @ m -> ('a, _, _) t @ m
    [@@alloc __ @ m = (heap_global, a @ m)]

    (** E.g., [append (of_list [a; b]) (of_list [c; d; e])] is [of_list [a; b; c; d; e]] *)
    val append : ('a, 'p1, 'p2) t @ m -> ('a, 'p1, 'p2) t @ m -> ('a, 'p1, 'p2) t @ m
    [@@alloc __ @ m = (heap_global, a @ m)]

    (** Concatenates a nested container. The elements of the inner containers are
        concatenated together in order to give the result. *)
    val concat : (('a, 'p1, 'p2) t, 'p1, 'p2) concat @ m -> ('a, 'p1, 'p2) t @ m
    [@@alloc __ @ m = (heap_global, a @ m)]

    (** [map f (of_list [a1; ...; an])] applies [f] to [a1], [a2], ..., [an], in order,
        and builds a result equivalent to [of_list [f a1; ...; f an]]. *)
    val map
      :  ('a, 'p1, 'p2) t @ mi
      -> f:('a elt @ mi -> 'b elt @ mo) @ local
      -> ('b, 'p1, 'p2) t @ mo
    [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]

    (** [filter t ~f] returns all the elements of [t] that satisfy the predicate [f]. *)
    val filter
      :  ('a, 'p1, 'p2) t @ m
      -> f:('a elt @ m -> bool) @ local
      -> ('a, 'p1, 'p2) t @ m
    [@@alloc __ @ m = (heap_global, a @ m)]

    (** [filter_map t ~f] applies [f] to every [x] in [t]. The result contains every [y]
        for which [f x] returns [Some y]. *)
    val filter_map
      :  ('a, 'p1, 'p2) t @ mi
      -> f:('a elt @ mi -> 'b elt option @ mo) @ local
      -> ('b, 'p1, 'p2) t @ mo
    [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]

    (** [concat_map t ~f] is equivalent to [concat (map t ~f)]. *)
    val concat_map
      :  ('a, 'p1, 'p2) t @ mi
      -> f:('a elt @ mi -> ('b, 'p1, 'p2) t @ mo) @ local
      -> ('b, 'p1, 'p2) t @ mo
    [@@mode mi = (global, m)] [@@alloc a @ mo = (heap_global, a @ m)]

    (** [partition_tf t ~f] returns a pair [t1, t2], where [t1] is all elements of [t]
        that satisfy [f], and [t2] is all elements of [t] that do not satisfy [f]. The
        "tf" suffix is mnemonic to remind readers that the result is (trues, falses). *)
    val partition_tf
      :  ('a, 'p1, 'p2) t @ m
      -> f:('a elt @ m -> bool) @ local
      -> ('a, 'p1, 'p2) t * ('a, 'p1, 'p2) t @ m
    [@@alloc __ @ m = (heap_global, a @ m)]

    (** [partition_map t ~f] partitions [t] according to [f]. *)
    val partition_map
      :  ('a, 'p1, 'p2) t @ mi
      -> f:('a elt @ mi -> ('b elt, 'c elt) Either.t @ mo) @ local
      -> ('b, 'p1, 'p2) t * ('c, 'p1, 'p2) t @ mo
    [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]
  end

  module type Creators__base = sig
    include Generic_types__base

    type (_, _, _) concat

    [@@@kind.default k1 = (value, float64, bits32, bits64, word, immediate, immediate64)]

    include sig
      type ('a : k1, 'b, 'c) t := (('a, 'b, 'c) t[@kind k1])
      type ('a : k1) elt : k1 := ('a elt[@kind k1])

      [@@@kind.default k1]

      val of_list : ('a elt List0.Constructors.t[@kind k1]) @ m -> ('a, _, _) t @ m
      [@@alloc __ @ m = (heap_global, a @ m)]

      val of_array : 'a elt array @ m -> ('a, _, _) t @ m
      [@@alloc __ @ m = (heap_global, a @ m)]

      (** E.g., [append (of_list [a; b]) (of_list [c; d; e])] is [of_list [a; b; c; d; e]] *)
      val append : ('a, 'p1, 'p2) t @ m -> ('a, 'p1, 'p2) t @ m -> ('a, 'p1, 'p2) t @ m
      [@@alloc __ @ m = (heap_global, a @ m)]

      (** Concatenates a nested container. The elements of the inner containers are
          concatenated together in order to give the result. *)
      val concat : (('a, 'p1, 'p2) t, 'p1, 'p2) concat @ m -> ('a, 'p1, 'p2) t @ m
      [@@alloc __ @ m = (heap_global, a @ m)]

      (** [filter t ~f] returns all the elements of [t] that satisfy the predicate [f]. *)
      val filter
        :  ('a, 'p1, 'p2) t @ m
        -> f:('a elt @ m -> bool) @ local
        -> ('a, 'p1, 'p2) t @ m
      [@@alloc __ @ m = (heap_global, a @ m)]

      (** [partition_tf t ~f] returns a pair [t1, t2], where [t1] is all elements of [t]
          that satisfy [f], and [t2] is all elements of [t] that do not satisfy [f]. The
          "tf" suffix is mnemonic to remind readers that the result is (trues, falses). *)
      val partition_tf
        :  ('a, 'p1, 'p2) t @ m
        -> f:('a elt @ m -> bool) @ local
        -> ('a, 'p1, 'p2) t * ('a, 'p1, 'p2) t @ m
      [@@alloc __ @ m = (heap_global, a @ m)]
    end

    [@@@kind.default k2 = (value, float64, bits32, bits64, word, immediate, immediate64)]

    (** [map f (of_list [a1; ...; an])] applies [f] to [a1], [a2], ..., [an], in order,
        and builds a result equivalent to [of_list [f a1; ...; f an]]. *)
    val map
      :  (('a, 'p1, 'p2) t[@kind k1]) @ mi
      -> f:(('a elt[@kind k1]) @ mi -> ('b elt[@kind k2]) @ mo) @ local
      -> (('b, 'p1, 'p2) t[@kind k2]) @ mo
    [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]

    (** [filter_map t ~f] applies [f] to every [x] in [t]. The result contains every [y]
        for which [f x] returns [Some y]. *)
    val filter_map
      :  (('a, 'p1, 'p2) t[@kind k1]) @ mi
      -> f:(('a elt[@kind k1]) @ mi -> (('b elt[@kind k2]) Option.t[@kind k2]) @ mo)
         @ local
      -> (('b, 'p1, 'p2) t[@kind k2]) @ mo
    [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]

    (** [concat_map t ~f] is equivalent to [concat (map t ~f)]. *)
    val concat_map
      :  (('a, 'p1, 'p2) t[@kind k1]) @ mi
      -> f:(('a elt[@kind k1]) @ mi -> (('b, 'p1, 'p2) t[@kind k2]) @ mo) @ local
      -> (('b, 'p1, 'p2) t[@kind k2]) @ mo
    [@@mode mi = (global, m)] [@@alloc a @ mo = (heap_global, a @ m)]

    [@@@kind.default k3 = (value, float64, bits32, bits64, word, immediate, immediate64)]

    (** [partition_map t ~f] partitions [t] according to [f]. *)
    val partition_map
      :  (('a, 'p1, 'p2) t[@kind k1]) @ mi
      -> f:
           (('a elt[@kind k1]) @ mi
            -> ((('b elt[@kind k2]), ('c elt[@kind k3])) Either.t[@kind k2 k3]) @ mo)
         @ local
      -> (('b, 'p1, 'p2) t[@kind k2]) * (('c, 'p1, 'p2) t[@kind k3]) @ mo
    [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]
  end

  module type Generic_with_creators = sig
    include Generic [@alloc a]

    include
      Creators
      [@alloc a]
      with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) t
       and type 'a elt := 'a elt
  end

  module type Generic_with_creators__base = sig
    include Generic__base [@alloc a]

    include
      Creators__base
      [@alloc a]
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
       and type ('a, 'b, 'c) t__float64 := ('a, 'b, 'c) t__float64
       and type ('a, 'b, 'c) t__bits32 := ('a, 'b, 'c) t__bits32
       and type ('a, 'b, 'c) t__bits64 := ('a, 'b, 'c) t__bits64
       and type ('a, 'b, 'c) t__word := ('a, 'b, 'c) t__word
       and type ('a, 'b, 'c) t__immediate := ('a, 'b, 'c) t__immediate
       and type ('a, 'b, 'c) t__immediate64 := ('a, 'b, 'c) t__immediate64
       and type 'a elt := 'a elt
       and type 'a elt__float64 := 'a elt__float64
       and type 'a elt__bits32 := 'a elt__bits32
       and type 'a elt__bits64 := 'a elt__bits64
       and type 'a elt__word := 'a elt__word
       and type 'a elt__immediate := 'a elt__immediate
       and type 'a elt__immediate64 := 'a elt__immediate64
  end

  (** Like [Generic_with_creators], but [mem] does not accept an [equal] function, since
      [Make0_with_creators] already takes [Elt.equal]. *)
  module type Generic_with_creators_for_s0 = sig
    include Generic_for_s0 [@alloc a]

    include
      Creators
      [@alloc a]
      with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) t
       and type 'a elt := 'a elt
  end

  module type Generic_with_creators_for_s0__base = sig
    include Generic_for_s0__base [@alloc a]

    include
      Creators__base
      [@alloc a]
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
       and type ('a, 'b, 'c) t__float64 := ('a, 'b, 'c) t__float64
       and type ('a, 'b, 'c) t__bits32 := ('a, 'b, 'c) t__bits32
       and type ('a, 'b, 'c) t__bits64 := ('a, 'b, 'c) t__bits64
       and type ('a, 'b, 'c) t__word := ('a, 'b, 'c) t__word
       and type ('a, 'b, 'c) t__immediate := ('a, 'b, 'c) t__immediate
       and type ('a, 'b, 'c) t__immediate64 := ('a, 'b, 'c) t__immediate64
       and type 'a elt := 'a elt
       and type 'a elt__float64 := 'a elt__float64
       and type 'a elt__bits32 := 'a elt__bits32
       and type 'a elt__bits64 := 'a elt__bits64
       and type 'a elt__word := 'a elt__word
       and type 'a elt__immediate := 'a elt__immediate
       and type 'a elt__immediate64 := 'a elt__immediate64
  end

  module type S0_with_creators = sig
    type t
    type elt

    include
      Generic_with_creators_for_s0
      [@alloc a]
      with type (_, _, _) t := t
       and type _ elt := elt
       and type (_, _, _) concat := t list
  end

  module type S0_with_creators__base = sig
    [%%template:
    [@@@kind.default k = (value, float64, bits32, bits64, word, immediate, immediate64)]

    type t
    type elt : k]

    include
      Generic_with_creators_for_s0__base
      [@alloc a]
      with type ('a, _, _) t := t
       and type ('a, _, _) t__float64 := t__float64
       and type ('a, _, _) t__bits32 := t__bits32
       and type ('a, _, _) t__bits64 := t__bits64
       and type ('a, _, _) t__word := t__word
       and type ('a, _, _) t__immediate := t__immediate
       and type ('a, _, _) t__immediate64 := t__immediate64
       and type _ elt := elt
       and type _ elt__float64 := elt__float64
       and type _ elt__bits32 := elt__bits32
       and type _ elt__bits64 := elt__bits64
       and type _ elt__word := elt__word
       and type _ elt__immediate := elt__immediate
       and type _ elt__immediate64 := elt__immediate64
       and type ('a, _, _) concat := t list
  end

  module type S1_with_creators = sig
    type 'a t

    include
      Generic_with_creators
      [@alloc a]
      with type ('a, _, _) t := 'a t
       and type 'a elt := 'a
       and type ('a, _, _) concat := 'a t
  end

  module type S1_with_creators__base = sig
    type ('a : k) t
    [@@kind k = (value, float64, bits32, bits64, word, immediate, immediate64)]

    include
      Generic_with_creators__base
      [@alloc a]
      with type ('a, _, _) t := 'a t
       and type ('a, _, _) t__float64 := 'a t__float64
       and type ('a, _, _) t__bits32 := 'a t__bits32
       and type ('a, _, _) t__bits64 := 'a t__bits64
       and type ('a, _, _) t__word := 'a t__word
       and type ('a, _, _) t__immediate := 'a t__immediate
       and type ('a, _, _) t__immediate64 := 'a t__immediate64
       and type 'a elt := 'a
       and type 'a elt__float64 := 'a
       and type 'a elt__bits32 := 'a
       and type 'a elt__bits64 := 'a
       and type 'a elt__word := 'a
       and type 'a elt__immediate := 'a
       and type 'a elt__immediate64 := 'a
       and type ('a, _, _) concat := 'a t
  end]

  type%template ('t, 'a, 'acc) fold =
    't @ mi
    -> init:'acc @ mo
    -> f:('acc @ mo -> 'a @ mi -> 'acc @ mo) @ local
    -> 'acc @ mo
  [@@mode mi = (global, local), mo = (global, local)]

  type%template ('t, 'a, 'acc, 'final) fold_until =
    't @ mi
    -> init:'acc @ mo
    -> f:('acc @ mo -> 'a @ mi -> ('acc, 'final) Continue_or_stop.t @ mo) @ local
    -> finish:('acc @ mo -> 'final @ mo) @ local
    -> 'final @ mo
  [@@mode mi = (global, local), mo = (global, local)]

  type%template ('t, 'a) iter = 't @ m -> f:('a @ m -> unit) @ local -> unit
  [@@mode m = (global, local)]

  type%template ('t, 'a, 'final) iter_until =
    't @ mi
    -> f:('a @ mi -> (unit, 'final) Continue_or_stop.t @ mo) @ local
    -> finish:(unit -> 'final @ mo) @ local
    -> 'final @ mo
  [@@mode mi = (global, local), mo = (global, local)]

  type%template 't length = 't @ m -> int [@@mode m = (global, local)]

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

  module type%template Derived = sig
    [%%template:
    [@@@mode.default m = (global, local)]

    (** Generic defintions that rely on [With_return] and therefore don't support local
        return values. *)

    val fold_until
      :  fold:(('t, 'a, 'acc) fold[@mode m global])
      -> (('t, 'a, 'acc, 'final) fold_until[@mode m global])

    (** Generic definitions of container operations in terms of [fold].

        E.g.: [iter ~fold t ~f = fold t ~init:() ~f:(fun () a -> f a)]. *)

    val iter_via_fold
      :  fold:(('t, 'a, unit) fold[@mode m global])
      -> (('t, 'a) iter[@mode m])

    val count
      :  fold:(('t, 'a, int) fold[@mode m global])
      -> 't @ m
      -> f:('a @ m -> bool) @ local
      -> int

    val min_elt
      :  fold:(('t, 'a, 'a option) fold[@mode m m])
      -> 't @ m
      -> compare:('a @ m -> 'a @ m -> int) @ local
      -> 'a option @ m

    val max_elt
      :  fold:(('t, 'a, 'a option) fold[@mode m m])
      -> 't @ m
      -> compare:('a @ m -> 'a @ m -> int) @ local
      -> 'a option @ m

    val length : fold:(('t, _, int) fold[@mode m global]) -> ('t length[@mode m])]

    val to_list : fold:(('t, 'a, 'a list) fold[@mode m m]) -> 't @ m -> 'a list @ m
    [@@alloc __ @ m = (heap_global, stack_local)]

    val sum
      :  fold:(('t, 'a, 'sum) fold[@mode mi mo])
      -> ((module Summable with type t = 'sum)[@mode mo])
      -> 't @ mi
      -> f:('a @ mi -> 'sum @ mo) @ local
      -> 'sum @ mo
    [@@mode mi = (global, local), mo = (global, local)]

    (** Generic definitions of container operations in terms of [fold_until]. *)

    val fold_result
      :  fold_until:(('t, 'a, 'acc, ('acc, 'e) Result.t) fold_until[@mode mi mo])
      -> 't @ mi
      -> init:'acc @ mo
      -> f:('acc @ mo -> 'a @ mi -> ('acc, 'e) Result.t @ mo) @ local
      -> ('acc, 'e) Result.t @ mo
    [@@mode mi = (global, local), mo = (global, local)]

    val iter_until
      :  fold_until:(('t, 'a, unit, 'final) fold_until[@mode mi mo])
      -> (('t, 'a, 'final) iter_until[@mode mi mo])
    [@@mode mi = (global, local), mo = (global, local)]

    (** Generic definitions of container operations in terms of [iter_until]. *)

    [%%template:
    [@@@mode.default m = (global, local)]

    val iter_via_iter_until
      :  iter_until:(('t, 'a, unit) iter_until[@mode m global])
      -> (('t, 'a) iter[@mode m])

    val is_empty
      :  iter_until:(('t, 'a, bool) iter_until[@mode m global])
      -> 't @ m
      -> bool

    val mem
      :  iter_until:(('t, 'a, bool) iter_until[@mode m global])
      -> 't @ m
      -> 'a @ m
      -> equal:('a @ m -> 'a @ m -> bool) @ local
      -> bool

    val exists
      :  iter_until:(('t, 'a, bool) iter_until[@mode m global])
      -> 't @ m
      -> f:('a @ m -> bool) @ local
      -> bool

    val for_all
      :  iter_until:(('t, 'a, bool) iter_until[@mode m global])
      -> 't @ m
      -> f:('a @ m -> bool) @ local
      -> bool

    val find
      :  iter_until:(('t, 'a, 'a option) iter_until[@mode m m])
      -> 't @ m
      -> f:('a @ m -> bool) @ local
      -> 'a option @ m]

    val find_map
      :  iter_until:(('t, 'a, 'b option) iter_until[@mode mi mo])
      -> 't @ mi
      -> f:('a @ mi -> 'b option @ mo) @ local
      -> 'b option @ mo
    [@@mode mi = (global, local), mo = (global, local)]

    (** Generic definitons of container operations in terms of [iter] and [length]. *)

    val to_array : length:'t length -> iter:('t, 'a) iter -> 't -> 'a array
  end
end

module type Container = sig @@ portable
  include module type of struct
    include Definitions
  end

  include Derived

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
