open! Import

[@@@warning "-incompatible-with-upstream"]

[%%template
[@@@kind_set.define base_with_ext = (base, value mod external64)]

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
  [@@@kind k1 = base_or_null, k2 = base_or_null]

  include struct
    type ('t, 'a : k1, 'finish : k2) iteri_until =
      't @ mi
      -> f:
           (int
            -> 'a @ mi
            -> ((unit, 'finish) Container.Continue_or_stop.t
               [@kind value_or_null (k2 or value_or_null)])
               @ mo)
         @ local
      -> finish:(int -> 'finish @ mo) @ local
      -> 'finish @ mo
    [@@mode mi = (global, local), mo = (global, local)] [@@kind k1 = k1, k2 = k2]
  end

  [@@@kind k3 = base_or_null]

  include struct
    type ('t, 'a : k1, 'accum : k2, 'finish : k3) foldi_until =
      't @ mi
      -> init:'accum @ mo
      -> f:
           (int
            -> 'accum @ mo
            -> 'a @ mi
            -> (('accum, 'finish) Container.Continue_or_stop.t
               [@kind (k2 or value_or_null) (k3 or value_or_null)])
               @ mo)
         @ local
      -> finish:(int -> 'accum @ mo -> 'finish @ mo) @ local
      -> 'finish @ mo
    [@@kind k1 = k1, k2 = k2, k3 = k3] [@@mode mi = (global, local), mo = (global, local)]
  end]

  [%%template
  [@@@mode.default m = (global, local)]

  module type
    [@kind_set.explicit ks = (value, value_or_null, value mod external64, base_with_ext)] Iterators_with_index_without_findi = sig
    include Container.Generic_types [@kind_set.explicit ks]

    (** These are all like their equivalents in [Container] except that an index starting
        at 0 is added as the first argument to [f]. *)

    [%%template:
    [@@@kind.default_if_multiple k1 = ks]

    type ('a : k1, 'b, 'c) t := (('a, 'b, 'c) t[@kind k1]) [@@kind value]
    type ('a : k1) elt : k1 := ('a elt[@kind k1]) [@@kind value]

    [@@@mode.default mi = (global, m)]

    val iteri : ('a : k1) 'p1 'p2. ((('a, 'p1, 'p2) t, 'a elt) iteri[@mode mi])

    val existsi
      : ('a : k1) 'p1 'p2.
      ('a, 'p1, 'p2) t @ mi -> f:(int -> 'a elt @ mi -> bool) @ local -> bool

    val for_alli
      : ('a : k1) 'p1 'p2.
      ('a, 'p1, 'p2) t @ mi -> f:(int -> 'a elt @ mi -> bool) @ local -> bool

    val counti
      : ('a : k1) 'p1 'p2.
      ('a, 'p1, 'p2) t @ mi -> f:(int -> 'a elt @ mi -> bool) @ local -> int

    [@@@kind.default_if_multiple k2 = (ks or value)]
    [@@@mode.default mo = (global, m)]

    val foldi
      : ('a : k1) 'p1 'p2 ('acc : k2).
      ((('a, 'p1, 'p2) t, 'a elt, 'acc) foldi[@mode mi mo])

    val iteri_until
      : ('a : k1) 'p1 'p2 ('final : k2).
      ((('a, 'p1, 'p2) t, 'a elt, 'final) iteri_until
      [@mode mi mo] [@kind (k1 or value_or_null) (k2 or value_or_null)])

    val find_mapi
      : ('a : k1) 'p1 'p2 ('b : k2).
      ('a, 'p1, 'p2) t @ mi
      -> f:(int -> 'a elt @ mi -> ('b Option0.t[@kind k2]) @ mo) @ local
      -> ('b Option0.t[@kind k2]) @ mo

    [@@@kind.default_if_multiple k3 = (ks or value)]

    val foldi_until
      : ('a : k1) 'p1 'p2 ('acc : k2) ('final : k3).
      ((('a, 'p1, 'p2) t, 'a elt, 'acc, 'final) foldi_until
      [@mode mi mo]
      [@kind (k1 or value_or_null) (k2 or value_or_null) (k3 or value_or_null)])]
  end

  module type
    [@kind_set.explicit ks = (value, value_or_null, value mod external64)] Iterators_with_index = sig
    (** Use a boxed product just for the [value] versions for backwards-compatibility *)

    include Iterators_with_index_without_findi [@kind_set.explicit ks] [@mode m]

    [@@@kind.default_if_multiple k = ks]

    val findi
      : ('a : k) 'p1 'p2.
      (('a, 'p1, 'p2) t[@kind k]) @ m
      -> f:(int -> ('a elt[@kind k]) @ m -> bool) @ local
      -> (int * ('a elt[@kind k])) option @ m
    [@@mode m = (global, m)]
  end

  module type Iterators_with_index = Iterators_with_index [@kind_set.explicit ks]
  [@@kind_set ks = value]

  module type [@kind_set.explicit ks = base_with_ext] Iterators_with_index = sig
    include Iterators_with_index_without_findi [@kind_set.explicit ks] [@mode m]

    [@@@kind.default_if_multiple k = ks]

    val findi
      : ('a : k) 'p1 'p2.
      (('a, 'p1, 'p2) t[@kind k]) @ m
      -> f:(int -> ('a elt[@kind k]) @ m -> bool) @ local
      -> (#(int * ('a elt[@kind k])) Option0.t[@kind value & (k or value)]) @ m
    [@@mode m = (global, m)]
  end]

  [%%template
  [@@@alloc.default a @ m = (heap_global, stack_local)]

  module type
    [@kind_set.explicit ks = (value, value_or_null, value mod external64, base_with_ext)] Generic = sig
    include Container.Generic [@kind_set.explicit ks] [@alloc a]

    include
      Iterators_with_index
    [@kind_set.explicit ks]
    [@mode m]
    [@with:
      [@@@kind.default k = ks]

      type ('a : k, 'b, 'c) t := (('a, 'b, 'c) t[@kind k])
      type ('a : k) elt := ('a elt[@kind k])]
  end

  module type Generic = sig
    include Generic [@kind_set.explicit ks] [@alloc a]

    include
      Container.Generic
      [@alloc a]
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
       and type 'a elt := 'a elt
  end
  [@@kind_set ks = value]

  (** Like [Generic], but [mem] does not accept an [equal] function, since [Make0] already
      takes [Elt.equal]. *)

  module type
    [@kind_set.explicit ks = (value, value mod external64, base_with_ext)] S0 = sig
    include Container.S0 [@kind_set.explicit ks] [@alloc a]

    include
      Iterators_with_index
    [@kind_set.explicit ks]
    [@mode m]
    [@with:
      [@@@kind.default k = ks]

      type ('a, _, _) t := (t[@kind k])
      type _ elt := (elt[@kind k])]
  end

  module type S0 = S0 [@kind_set.explicit ks] [@alloc a] [@@kind_set ks = value]

  [@@@kind_set.default.explicit
    ks = (value, value_or_null, value mod external64, base_with_ext)]

  module type S1 = sig
    include Container.S1 [@kind_set.explicit ks] [@alloc a]

    include
      Iterators_with_index
    [@kind_set.explicit ks]
    [@mode m]
    [@with:
      [@@@kind.default k = ks]

      type ('a : k, _, _) t := ('a t[@kind k])
      type ('a : k) elt := 'a]
  end

  module type S1 = S1 [@kind_set.explicit ks] [@alloc a] [@@kind_set ks = value]

  module type Creators_with_index = sig
    include Container.Generic_types [@kind_set.explicit ks]

    [%%template:
    [@@@kind.default_if_multiple k1 = ks]

    type ('a : k1, 'b, 'c) t := (('a, 'b, 'c) t[@kind k1]) [@@kind value]
    type ('a : k1) elt := ('a elt[@kind k1]) [@@kind value]

    (** [init n ~f] is equivalent to [of_list [f 0; f 1; ...; f (n-1)]]. It raises an
        exception if [n < 0]. *)
    val init
      : ('a : k1) 'p1 'p2.
      int -> f:(int -> 'a elt @ m) @ local -> ('a, 'p1, 'p2) t @ m
    [@@alloc __ @ m = (heap_global, a @ m)]

    val filteri
      : ('a : k1) 'p1 'p2.
      ('a, 'p1, 'p2) t @ m
      -> f:(int -> 'a elt @ m -> bool) @ local
      -> ('a, 'p1, 'p2) t @ m
    [@@alloc __ @ m = (heap_global, a @ m)]

    (** [partitioni_tf t ~f] is like partition_tf. Additionally, it passes the index as an
        argument. *)
    val partitioni_tf
      : ('a : k1) 'p1 'p2.
      ('a, 'p1, 'p2) t @ m
      -> f:(int -> 'a elt @ m -> bool) @ local
      -> ('a, 'p1, 'p2) t * ('a, 'p1, 'p2) t @ m
    [@@alloc __ @ m = (heap_global, a @ m)]]

    [@@@kind.default_if_multiple k1 = ks]
    [@@@kind.default_if_multiple k2 = ks]

    (** [mapi] is like map. Additionally, it passes in the index of each element as the
        first argument to the mapped function. *)
    val mapi
      : ('a : k1) 'p1 'p2 ('b : k2).
      (('a, 'p1, 'p2) t[@kind k1]) @ m
      -> f:(int -> ('a elt[@kind k1]) @ m -> ('b elt[@kind k2]) @ n) @ local
      -> (('b, 'p1, 'p2) t[@kind k2]) @ n
    [@@mode m = (global, m)] [@@alloc __ @ n = (heap_global, a @ m)]

    (** filter_mapi is like [filter_map]. Additionally, it passes in the index of each
        element as the first argument to the mapped function. *)
    val filter_mapi
      : ('a : k1) 'p1 'p2 ('b : k2).
      (('a, 'p1, 'p2) t[@kind k1]) @ m
      -> f:
           (int
            -> ('a elt[@kind k1]) @ m
            -> (('b elt[@kind k2]) Option0.t[@kind k2 or value_or_null]) @ n)
         @ local
      -> (('b, 'p1, 'p2) t[@kind k2]) @ n
    [@@mode m = (global, m)] [@@alloc __ @ n = (heap_global, a @ m)]

    (** [concat_mapi t ~f] is like concat_map. Additionally, it passes the index as an
        argument. *)
    val concat_mapi
      : ('a : k1) 'p1 'p2 ('b : k2).
      (('a, 'p1, 'p2) t[@kind k1]) @ m
      -> f:(int -> ('a elt[@kind k1]) @ m -> (('b, 'p1, 'p2) t[@kind k2]) @ n) @ local
      -> (('b, 'p1, 'p2) t[@kind k2]) @ n
    [@@mode m = (global, m)] [@@alloc __ @ n = (heap_global, a @ m)]

    [@@@kind.default_if_multiple k3 = ks]

    (** [partition_mapi t ~f] is like partition_map. Additionally, it passes the index as
        an argument. *)
    val partition_mapi
      : ('a : k1) 'p1 'p2 ('b : k2) ('c : k3).
      (('a, 'p1, 'p2) t[@kind k1]) @ m
      -> f:
           (int
            -> ('a elt[@kind k1]) @ m
            -> ((('b elt[@kind k2]), ('c elt[@kind k3])) Either0.t
               [@kind (k2 or value_or_null) (k3 or value_or_null)])
               @ n)
         @ local
      -> (('b, 'p1, 'p2) t[@kind k2]) * (('c, 'p1, 'p2) t[@kind k3]) @ n
    [@@mode m = (global, m)] [@@alloc __ @ n = (heap_global, a @ m)]
  end

  module type Creators_with_index = Creators_with_index [@kind_set.explicit ks] [@alloc a]
  [@@kind_set ks = value]

  module type Generic_with_creators = sig
    include Generic [@kind_set.explicit ks] [@alloc a]

    include
      Container.Creators
    [@kind_set.explicit ks]
    [@alloc a]
    [@with:
      [@@@kind.default k = ks]

      type ('a : k, 'b, 'c) t := (('a, 'b, 'c) t[@kind k])
      type ('a : k) elt := ('a elt[@kind k])]

    include
      Creators_with_index
    [@kind_set.explicit ks]
    [@alloc a]
    [@with:
      [@@@kind.default k = ks]

      type ('a : k, 'b, 'c) t := (('a, 'b, 'c) t[@kind k])
      type ('a : k) elt := ('a elt[@kind k])]
  end

  module type Generic_with_creators = sig
    include Generic_with_creators [@kind_set.explicit ks] [@alloc a]

    include
      Container.Generic_with_creators
      [@alloc a]
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
       and type 'a elt := 'a elt
  end
  [@@kind_set ks = value]]

  [%%template
  [@@@alloc.default a @ m = (heap_global, stack_local)]

  (** Like [Generic_with_creators], but [mem] does not accept an [equal] function, since
      [Make0_with_creators] already takes [Elt.equal]. *)

  module type [@kind_set.explicit ks = (value, base_with_ext)] S0_with_creators = sig
    include S0 [@kind_set.explicit ks] [@alloc a]

    include
      Container.S0_with_creators
    [@kind_set.explicit ks]
    [@alloc a]
    [@with:
      [@@@kind.default k = ks]

      type t := (t[@kind k])
      type elt := (elt[@kind k])]

    include
      Creators_with_index
    [@kind_set.explicit ks]
    [@alloc a]
    [@with:
      [@@@kind.default k = ks]

      type ('a, _, _) t := (t[@kind k])
      type _ elt := (elt[@kind k])]
  end

  module type S0_with_creators = S0_with_creators [@kind_set.explicit ks] [@alloc a]
  [@@kind_set ks = value]

  [@@@kind_set.default.explicit ks = (value, value_or_null, base_with_ext)]

  module type S1_with_creators = sig
    include S1 [@kind_set.explicit ks] [@alloc a]

    include
      Container.S1_with_creators
    [@kind_set.explicit ks]
    [@alloc a]
    [@with: type ('a : k) t := ('a t[@kind k]) [@@kind k = ks]]

    include
      Creators_with_index
    [@kind_set.explicit ks]
    [@alloc a]
    [@with:
      [@@@kind.default k = ks]

      type ('a : k, _, _) t := ('a t[@kind k])
      type ('a : k) elt := 'a]
  end

  module type S1_with_creators = S1_with_creators [@kind_set.explicit ks] [@alloc a]
  [@@kind_set ks = value]]

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

  module type [@kind.explicit k = (value, value_or_null)] Derived = sig
    [@@@mode.default m = (global, local)]

    (** Generic definition of [foldi_until] in terms of [fold_until]. *)

    val foldi_until
      : ('a : k) 't ('acc : k) ('final : k).
      fold_until:(('t, 'a, 'acc, 'final) fold_until[@mode mi mo])
      -> (('t, 'a, 'acc, 'final) foldi_until[@mode mi mo])
    [@@mode mi = m, mo = (global, local)]

    (** Generic definitions of [foldi] and [iteri] in terms of [fold].

        E.g., [iteri ~fold t ~f = ignore (fold t ~init:0 ~f:(fun i x -> f i x; i + 1))]. *)

    val foldi
      : ('a : k) 't ('acc : k).
      fold:(('t, 'a, 'acc) fold[@mode mi mo]) -> (('t, 'a, 'acc) foldi[@mode mi mo])
    [@@mode mi = m, mo = (global, local)]

    val iteri
      : ('a : k) 't.
      fold:(('t, 'a, int) fold[@mode m global]) -> (('t, 'a) iteri[@mode m])

    (** Generic definition of [iteri_until] in terms of [foldi_until]. *)

    val iteri_until
      : ('a : k) 't ('final : k).
      foldi_until:(('t, 'a, unit, 'final) foldi_until[@mode mi mo])
      -> (('t, 'a, 'final) iteri_until[@mode mi mo])
    [@@mode mi = m, mo = (global, local)]

    (** Generic definitions of indexed container operations in terms of [foldi]. *)

    val counti
      : ('a : k) 't.
      foldi:(('t, 'a, int) foldi[@mode m global])
      -> 't @ m
      -> f:(int -> 'a @ m -> bool) @ local
      -> int

    (** Generic definitions of indexed container operations in terms of [iteri]. *)

    val existsi
      : ('a : k) 't.
      iteri_until:(('t, 'a, bool) iteri_until[@mode m global])
      -> 't @ m
      -> f:(int -> 'a @ m -> bool) @ local
      -> bool

    val for_alli
      : ('a : k) 't.
      iteri_until:(('t, 'a, bool) iteri_until[@mode m global])
      -> 't @ m
      -> f:(int -> 'a @ m -> bool) @ local
      -> bool

    val findi
      : ('a : k) 't.
      iteri_until:(('t, 'a, (int * 'a) option) iteri_until[@mode m m])
      -> 't @ m
      -> f:(int -> 'a @ m -> bool) @ local
      -> (int * 'a) option @ m

    val find_mapi
      : ('a : k) 't ('b : k).
      iteri_until:(('t, 'a, 'b option) iteri_until[@mode mi mo])
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

  module%template Derived : Derived [@kind.explicit k]
  [@@kind.explicit k = (value, value_or_null)]

  include module type of Derived [@kind.explicit value]

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
