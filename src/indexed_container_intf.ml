open! Import

[@@@warning "-incompatible-with-upstream"]

[%%template
module Definitions = struct
  type ('t, 'a : any, 'accum : any) fold =
    't @ mi
    -> init:'accum @ mo
    -> f:('accum @ mo -> 'a @ mi -> 'accum @ mo) @ local
    -> 'accum @ mo
  [@@mode mi = (global, local), mo = (global, local)]

  type ('t, 'a : any, 'accum : any, 'finish : any) fold_until =
    't @ mi
    -> init:'accum @ mo
    -> f:('accum @ mo -> 'a @ mi -> ('accum, 'finish) Container.Continue_or_stop.t @ mo)
       @ local
    -> finish:('accum @ mo -> 'finish @ mo) @ local
    -> 'finish @ mo
  [@@mode mi = (global, local), mo = (global, local)]

  type ('t, 'a : any, 'accum : any) foldi =
    't @ mi
    -> init:'accum @ mo
    -> f:(int -> 'accum @ mo -> 'a @ mi -> 'accum @ mo) @ local
    -> 'accum @ mo
  [@@mode mi = (global, local), mo = (global, local)]

  type ('t, 'a : any) iteri = 't @ m -> f:(int -> 'a @ m -> unit) @ local -> unit
  [@@mode m = (global, local)]

  [%%template
  [@@@kind.default
    k1 = (value, float64, bits32, bits64, word, immediate, immediate64)
    , k2 = (value, float64, bits32, bits64, word, immediate, immediate64)]

  type ('t, 'a : k1, 'finish : k2) iteri_until =
    't @ mi
    -> f:
         (int
          -> 'a @ mi
          -> ((unit, 'finish) Container.Continue_or_stop.t[@kind value k2]) @ mo)
       @ local
    -> finish:(int -> 'finish @ mo) @ local
    -> 'finish @ mo
  [@@mode mi = (global, local), mo = (global, local)]

  [@@@kind.default k3 = (value, float64, bits32, bits64, word, immediate, immediate64)]

  type ('t, 'a : k1, 'accum : k2, 'finish : k3) foldi_until =
    't @ mi
    -> init:'accum @ mo
    -> f:
         (int
          -> 'accum @ mo
          -> 'a @ mi
          -> (('accum, 'finish) Container.Continue_or_stop.t[@kind k2 k3]) @ mo)
       @ local
    -> finish:(int -> 'accum @ mo -> 'finish @ mo) @ local
    -> 'finish @ mo
  [@@mode mi = (global, local), mo = (global, local)]]

  [%%template
  [@@@mode.default m = (global, local)]

  module type Iterators_with_index = sig
    include Container.Generic_types [@kind k]

    (** These are all like their equivalents in [Container] except that an index starting
        at 0 is added as the first argument to [f]. *)

    val foldi : ((('a, _, _) t, 'a elt, _) foldi[@mode mi mo])
    [@@mode mi = (global, m), mo = (global, m)]

    val foldi_until : ((('a, _, _) t, 'a elt, _, _) foldi_until[@mode mi mo])
    [@@mode mi = (global, m), mo = (global, m)]

    val iteri : ((('a, _, _) t, 'a elt) iteri[@mode m]) [@@mode m = (global, m)]

    val iteri_until : ((('a, _, _) t, 'a elt, 'final) iteri_until[@mode mi mo])
    [@@mode mi = (global, m), mo = (global, m)]

    val existsi : ('a, _, _) t @ m -> f:(int -> 'a elt @ m -> bool) @ local -> bool
    [@@mode m = (global, m)]

    val for_alli : ('a, _, _) t @ m -> f:(int -> 'a elt @ m -> bool) @ local -> bool
    [@@mode m = (global, m)]

    val counti : ('a, _, _) t @ m -> f:(int -> 'a elt @ m -> bool) @ local -> int
    [@@mode m = (global, m)]

    val findi
      :  ('a, _, _) t @ m
      -> f:(int -> 'a elt @ m -> bool) @ local
      -> (int * 'a elt) option @ m
    [@@mode m = (global, m)]

    val find_mapi
      :  ('a, _, _) t @ mi
      -> f:(int -> 'a elt @ mi -> 'b option @ mo) @ local
      -> 'b option @ mo
    [@@mode mi = (global, m), mo = (global, m)]
  end
  [@@kind k = (value, immediate, immediate64)]

  module type Iterators_with_index__base = sig
    include Container.Generic_types__base

    (** These are all like their equivalents in [Container] except that an index starting
        at 0 is added as the first argument to [f]. *)

    [@@@kind k1 = (value, float64, bits32, bits64, word, immediate, immediate64)]

    include sig
        type ('a : k1, 'b, 'c) t
        type ('a : k1) elt : k1

        [@@@kind.default k1]
        [@@@mode.default mi = (global, m)]

        val iteri : ((('a, _, _) t, 'a elt) iteri[@mode m])
        val existsi : ('a, _, _) t @ m -> f:(int -> 'a elt @ m -> bool) @ local -> bool
        val for_alli : ('a, _, _) t @ m -> f:(int -> 'a elt @ m -> bool) @ local -> bool
        val counti : ('a, _, _) t @ m -> f:(int -> 'a elt @ m -> bool) @ local -> int

        val findi
          :  ('a, _, _) t @ m
          -> f:(int -> 'a elt @ m -> bool) @ local
          -> (#(int * 'a elt) Option0.t[@kind value & k1]) @ m

        [@@@kind.default
          k2 = (value, float64, bits32, bits64, word, immediate, immediate64)]

        [@@@mode.default mo = (global, m)]

        val foldi : ((('a, _, _) t, 'a elt, _) foldi[@mode mi mo])

        val iteri_until
          : ((('a, _, _) t, 'a elt, 'final) iteri_until[@mode mi mo] [@kind k1 k2])

        val find_mapi
          :  ('a, _, _) t @ mi
          -> f:(int -> 'a elt @ mi -> ('b Option0.t[@kind k2]) @ mo) @ local
          -> ('b Option0.t[@kind k2]) @ mo

        [@@@kind.default
          k3 = (value, float64, bits32, bits64, word, immediate, immediate64)]

        val foldi_until
          : ((('a, _, _) t, 'a elt, _, _) foldi_until[@mode mi mo] [@kind k1 k2 k3])
      end
      with type ('a : k1, 'b, 'c) t := (('a, 'b, 'c) t[@kind k1])
       and type ('a : k1) elt := ('a elt[@kind k1])
  end]

  [%%template
  [@@@alloc.default a @ m = (heap_global, stack_local)]

  module type Generic = sig
    include Container.Generic [@alloc a]

    include
      Iterators_with_index
      [@mode m]
      with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) t
       and type 'a elt := 'a elt
  end

  module type Generic__base = sig
    include Container.Generic__base [@alloc a]

    include
      Iterators_with_index__base
      [@mode m]
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

  (** Like [Generic], but [mem] does not accept an [equal] function, since [Make0] already
      takes [Elt.equal]. *)
  module type S0 = sig
    include Container.S0 [@alloc a]

    include
      Iterators_with_index [@mode m] with type (_, _, _) t := t and type _ elt := elt
  end

  module type S0__base = sig
    include Container.S0__base [@alloc a]

    include
      Iterators_with_index__base
      [@mode m]
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

  module type S1 = sig
    include Container.S1 [@kind k] [@alloc a]

    include
      Iterators_with_index
      [@kind k] [@mode m]
      with type ('a, _, _) t := 'a t
       and type 'a elt := 'a
  end
  [@@kind k = (value, immediate, immediate64)]

  module type S1__base = sig
    include Container.S1__base [@alloc a]

    include
      Iterators_with_index__base
      [@mode m]
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

  module type Creators_with_index = sig
    include Container.Generic_types

    (** [init n ~f] is equivalent to [of_list [f 0; f 1; ...; f (n-1)]]. It raises an
        exception if [n < 0]. *)
    val init : int -> f:(int -> 'a elt @ m) @ local -> ('a, _, _) t @ m
    [@@alloc __ @ m = (heap_global, a @ m)]

    (** [mapi] is like map. Additionally, it passes in the index of each element as the
        first argument to the mapped function. *)
    val mapi
      :  ('a, 'p1, 'p2) t @ mi
      -> f:(int -> 'a elt @ mi -> 'b elt @ mo) @ local
      -> ('b, 'p1, 'p2) t @ mo
    [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]

    val filteri
      :  ('a, 'p1, 'p2) t @ m
      -> f:(int -> 'a elt @ m -> bool) @ local
      -> ('a, 'p1, 'p2) t @ m
    [@@alloc __ @ m = (heap_global, a @ m)]

    (** filter_mapi is like [filter_map]. Additionally, it passes in the index of each
        element as the first argument to the mapped function. *)
    val filter_mapi
      :  ('a, 'p1, 'p2) t @ mi
      -> f:(int -> 'a elt @ mi -> 'b elt option @ mo) @ local
      -> ('b, 'p1, 'p2) t @ mo
    [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]

    (** [concat_mapi t ~f] is like concat_map. Additionally, it passes the index as an
        argument. *)
    val concat_mapi
      :  ('a, 'p1, 'p2) t @ mi
      -> f:(int -> 'a elt @ mi -> ('b, 'p1, 'p2) t @ mo) @ local
      -> ('b, 'p1, 'p2) t @ mo
    [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]

    (** [partitioni_tf t ~f] is like partition_tf. Additionally, it passes the index as an
        argument. *)
    val partitioni_tf
      :  ('a, 'p1, 'p2) t @ m
      -> f:(int -> 'a elt @ m -> bool) @ local
      -> ('a, 'p1, 'p2) t * ('a, 'p1, 'p2) t @ m
    [@@alloc __ @ m = (heap_global, a @ m)]

    (** [partition_mapi t ~f] is like partition_map. Additionally, it passes the index as
        an argument. *)
    val partition_mapi
      :  ('a, 'p1, 'p2) t @ mi
      -> f:(int -> 'a elt @ mi -> ('b elt, 'c elt) Either0.t @ mo) @ local
      -> ('b, 'p1, 'p2) t * ('c, 'p1, 'p2) t @ mo
    [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]
  end

  module type Creators_with_index__base = sig
    include Container.Generic_types__base

    [@@@kind.default k1 = (value, float64, bits32, bits64, word, immediate, immediate64)]

    include sig
        type ('a : k1, 'b, 'c) t
        type ('a : k1) elt : k1

        [@@@kind.default k1]

        (** [init n ~f] is equivalent to [of_list [f 0; f 1; ...; f (n-1)]]. It raises an
            exception if [n < 0]. *)
        val init : int -> f:(int -> 'a elt @ m) @ local -> ('a, _, _) t @ m
        [@@alloc __ @ m = (heap_global, a @ m)]

        val filteri
          :  ('a, 'p1, 'p2) t @ m
          -> f:(int -> 'a elt @ m -> bool) @ local
          -> ('a, 'p1, 'p2) t @ m
        [@@alloc __ @ m = (heap_global, a @ m)]

        (** [partitioni_tf t ~f] is like partition_tf. Additionally, it passes the index
            as an argument. *)
        val partitioni_tf
          :  ('a, 'p1, 'p2) t @ m
          -> f:(int -> 'a elt @ m -> bool) @ local
          -> ('a, 'p1, 'p2) t * ('a, 'p1, 'p2) t @ m
        [@@alloc __ @ m = (heap_global, a @ m)]
      end
      with type ('a : k1, 'b, 'c) t := (('a, 'b, 'c) t[@kind k1])
       and type ('a : k1) elt := ('a elt[@kind k1])

    [@@@kind.default k2 = (value, float64, bits32, bits64, word, immediate, immediate64)]

    (** [mapi] is like map. Additionally, it passes in the index of each element as the
        first argument to the mapped function. *)
    val mapi
      :  (('a, 'p1, 'p2) t[@kind k1]) @ m
      -> f:(int -> ('a elt[@kind k1]) @ m -> ('b elt[@kind k2]) @ n) @ local
      -> (('b, 'p1, 'p2) t[@kind k2]) @ n
    [@@mode m = (global, m)] [@@alloc __ @ n = (heap_global, a @ m)]

    (** filter_mapi is like [filter_map]. Additionally, it passes in the index of each
        element as the first argument to the mapped function. *)
    val filter_mapi
      :  (('a, 'p1, 'p2) t[@kind k1]) @ m
      -> f:(int -> ('a elt[@kind k1]) @ m -> (('b elt[@kind k2]) Option0.t[@kind k2]) @ n)
         @ local
      -> (('b, 'p1, 'p2) t[@kind k2]) @ n
    [@@mode m = (global, m)] [@@alloc __ @ n = (heap_global, a @ m)]

    (** [concat_mapi t ~f] is like concat_map. Additionally, it passes the index as an
        argument. *)
    val concat_mapi
      :  (('a, 'p1, 'p2) t[@kind k1]) @ m
      -> f:(int -> ('a elt[@kind k1]) @ m -> (('b, 'p1, 'p2) t[@kind k2]) @ n) @ local
      -> (('b, 'p1, 'p2) t[@kind k2]) @ n
    [@@mode m = (global, m)] [@@alloc __ @ n = (heap_global, a @ m)]

    [@@@kind.default k3 = (value, float64, bits32, bits64, word, immediate, immediate64)]

    (** [partition_mapi t ~f] is like partition_map. Additionally, it passes the index as
        an argument. *)
    val partition_mapi
      :  (('a, 'p1, 'p2) t[@kind k1]) @ m
      -> f:
           (int
            -> ('a elt[@kind k1]) @ m
            -> ((('b elt[@kind k2]), ('c elt[@kind k3])) Either0.t[@kind k2 k3]) @ n)
         @ local
      -> (('b, 'p1, 'p2) t[@kind k2]) * (('c, 'p1, 'p2) t[@kind k3]) @ n
    [@@mode m = (global, m)] [@@alloc __ @ n = (heap_global, a @ m)]
  end

  module type Generic_with_creators = sig
    include Generic [@alloc a]

    include
      Container.Creators
      [@alloc a]
      with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) t
       and type 'a elt := 'a elt

    include
      Creators_with_index
      [@alloc a]
      with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) t
       and type 'a elt := 'a elt
  end

  module type Generic_with_creators__base = sig
    include Generic__base [@alloc a]

    include
      Container.Creators__base
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

    include
      Creators_with_index__base
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
  module type S0_with_creators = sig
    include S0 [@alloc a]

    include
      Container.Creators
      [@alloc a]
      with type (_, _, _) t := t
       and type _ elt := elt
       and type (_, _, _) concat := t list

    include
      Creators_with_index [@alloc a] with type (_, _, _) t := t and type _ elt := elt
  end

  module type S0_with_creators__base = sig
    include S0__base [@alloc a]

    include
      Container.Creators__base
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

    include
      Creators_with_index__base
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

  module type S1_with_creators = sig
    include S1 [@alloc a]

    include
      Container.Creators
      [@alloc a]
      with type ('a, _, _) t := 'a t
       and type 'a elt := 'a
       and type ('a, _, _) concat := 'a t

    include
      Creators_with_index [@alloc a] with type ('a, _, _) t := 'a t and type 'a elt := 'a
  end

  module type S1_with_creators__base = sig
    include S1__base [@alloc a]

    include
      Container.Creators__base
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

    include
      Creators_with_index__base
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
  end]

  module type Make_gen_arg = sig
    include Container.Make_gen_arg [@mode m]

    val iteri
      : [ `Define_using_fold | `Custom of ((('a, _, _) t, 'a elt) iteri[@mode m]) ]
    [@@mode m = (global, m)]

    val foldi
      : [ `Define_using_fold | `Custom of ((('a, _, _) t, 'a elt, _) foldi[@mode mi mo]) ]
    [@@mode mi = (global, m), mo = (global, m)]

    val foldi_until
      : [ `Define_using_fold_until
        | `Custom of ((('a, _, _) t, 'a elt, _, _) foldi_until[@mode mi mo])
        ]
    [@@mode mi = (global, m), mo = (global, m)]
  end
  [@@mode m = (global, local)]

  module type Make_arg = sig
    include Container.Make_arg [@mode m]
    include Make_gen_arg [@mode m] with type ('a, _, _) t := 'a t and type 'a elt := 'a
  end
  [@@mode m = (global, local)]

  module type Make0_arg = sig
    include Container.Make0_arg [@mode m]
    include Make_gen_arg [@mode m] with type ('a, _, _) t := t and type 'a elt := Elt.t
  end
  [@@mode m = (global, local)]

  module type Make_common_with_creators_arg = sig
    include Container.Make_common_with_creators_arg

    include
      Make_gen_arg
      with type ('a, 'p1, 'p2) t := ('a, 'p1, 'p2) t
       and type 'a elt := 'a elt

    val init
      : [ `Define_using_of_array
        | `Custom of int -> f:(int -> 'a elt) @ local -> ('a, _, _) t
        ]

    val concat_mapi
      : [ `Define_using_concat
        | `Custom of
          ('a, _, _) t -> f:(int -> 'a elt -> ('b, _, _) t) @ local -> ('b, _, _) t
        ]
  end

  module type Make_gen_with_creators_arg = sig
    include Container.Make_gen_with_creators_arg

    include
      Make_common_with_creators_arg
      with type ('a, 'p1, 'p2) t := ('a, 'p1, 'p2) t
       and type 'a elt := 'a elt
       and type ('a, 'p1, 'p2) concat := ('a, 'p1, 'p2) concat
  end

  module type Make_with_creators_arg = sig
    include Container.Make_with_creators_arg

    include
      Make_common_with_creators_arg
      with type ('a, _, _) t := 'a t
       and type 'a elt := 'a
       and type ('a, _, _) concat := 'a t
  end

  module type Make0_with_creators_arg = sig
    include Container.Make0_with_creators_arg

    include
      Make_common_with_creators_arg
      with type ('a, _, _) t := t
       and type 'a elt := Elt.t
       and type ('a, _, _) concat := 'a list
  end

  module type%template Derived = sig
    [@@@mode.default m = (global, local)]

    (** Generic definition of [foldi_until] in terms of [fold_until]. *)

    val foldi_until
      :  fold_until:(('t, 'a, 'acc, 'final) fold_until[@mode mi mo])
      -> (('t, 'a, 'acc, 'final) foldi_until[@mode mi mo])
    [@@mode mi = m, mo = (global, local)]

    (** Generic definitions of [foldi] and [iteri] in terms of [fold].

        E.g., [iteri ~fold t ~f = ignore (fold t ~init:0 ~f:(fun i x -> f i x; i + 1))]. *)

    val foldi
      :  fold:(('t, 'a, 'acc) fold[@mode mi mo])
      -> (('t, 'a, 'acc) foldi[@mode mi mo])
    [@@mode mi = m, mo = (global, local)]

    val iteri : fold:(('t, 'a, int) fold[@mode m global]) -> (('t, 'a) iteri[@mode m])

    (** Generic definition of [iteri_until] in terms of [foldi_until]. *)

    val iteri_until
      :  foldi_until:(('t, 'a, unit, 'final) foldi_until[@mode mi mo])
      -> (('t, 'a, 'final) iteri_until[@mode mi mo])
    [@@mode mi = m, mo = (global, local)]

    (** Generic definitions of indexed container operations in terms of [foldi]. *)

    val counti
      :  foldi:(('t, 'a, int) foldi[@mode m global])
      -> 't @ m
      -> f:(int -> 'a @ m -> bool) @ local
      -> int

    (** Generic definitions of indexed container operations in terms of [iteri]. *)

    val existsi
      :  iteri_until:(('t, 'a, bool) iteri_until[@mode m global])
      -> 't @ m
      -> f:(int -> 'a @ m -> bool) @ local
      -> bool

    val for_alli
      :  iteri_until:(('t, 'a, bool) iteri_until[@mode m global])
      -> 't @ m
      -> f:(int -> 'a @ m -> bool) @ local
      -> bool

    val findi
      :  iteri_until:(('t, 'a, (int * 'a) option) iteri_until[@mode m m])
      -> 't @ m
      -> f:(int -> 'a @ m -> bool) @ local
      -> (int * 'a) option @ m

    val find_mapi
      :  iteri_until:(('t, 'a, 'b option) iteri_until[@mode mi mo])
      -> 't @ mi
      -> f:(int -> 'a @ mi -> 'b option @ mo) @ local
      -> 'b option @ mo
    [@@mode mi = m, mo = (global, local)]
  end
end

module type Indexed_container = sig @@ portable
  include module type of struct
    include Definitions
  end

  (** Provides generic signatures for containers that support indexed iteration ([iteri],
      [foldi], ...). In principle, any container that has [iter] can also implement
      [iteri], but the idea is that [Indexed_container_intf] should be included only for
      containers that have a meaningful underlying ordering. *)

  include Derived

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
