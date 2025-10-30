open! Import

[%%template
module Definitions = struct
  type ('t, 'a, 'accum) fold = 't -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum
  [@@mode mi = (global, local), mo = (global, local)]

  type ('t, 'a, 'accum, 'finish) fold_until =
    't
    -> init:'accum
    -> f:('accum -> 'a -> ('accum, 'finish) Container.Continue_or_stop.t)
    -> finish:('accum -> 'finish)
    -> 'finish
  [@@mode mi = (global, local), mo = (global, local)]

  type ('t, 'a, 'accum) foldi =
    't -> init:'accum -> f:(int -> 'accum -> 'a -> 'accum) -> 'accum
  [@@mode mi = (global, local), mo = (global, local)]

  type ('t, 'a) iteri = 't -> f:(int -> 'a -> unit) -> unit [@@mode m = (global, local)]

  [%%template
  [@@@kind k1 = (value, base_or_null_with_imm), k2 = (value, base_or_null_with_imm)]

  include struct
    type ('t, 'a, 'finish) iteri_until =
      't
      -> f:(int -> 'a -> ((unit, 'finish) Container.Continue_or_stop.t[@kind value k2]))
      -> finish:(int -> 'finish)
      -> 'finish
    [@@mode mi = (global, local), mo = (global, local)] [@@kind k1 = k1, k2 = k2]

    type ('a, 'b, 'c) _supress_unused = ('a, 'b, 'c) iteri_until
  end

  [@@@kind k3 = (value, base_or_null_with_imm)]

  include struct
    type ('t, 'a, 'accum, 'finish) foldi_until =
      't
      -> init:'accum
      -> f:
           (int
            -> 'accum
            -> 'a
            -> (('accum, 'finish) Container.Continue_or_stop.t[@kind k2 k3]))
      -> finish:(int -> 'accum -> 'finish)
      -> 'finish
    [@@kind k1 = k1, k2 = k2, k3 = k3] [@@mode mi = (global, local), mo = (global, local)]

    type ('a, 'b, 'c, 'd) _supress_unused = ('a, 'b, 'c, 'd) foldi_until
  end]

  [%%template
  [@@@mode.default m = (global, local)]
  [@@@kind_set.define base_with_imm = (value, base_with_imm)]

  module type
    [@kind_set.explicit
      (ks, ks_not_in_t)
      = ( (value, value)
        , (value_or_null, value_or_null)
        , (immediate, value)
        , (immediate64, value)
        , (base_with_imm, base) )] Iterators_with_index_without_findi = sig
    include Container.Generic_types [@kind_set.explicit ks]

    (** These are all like their equivalents in [Container] except that an index starting
        at 0 is added as the first argument to [f]. *)

    [%%template:
    [@@@kind.default_if_multiple k1 = ks]

    type ('a, 'b, 'c) t := (('a, 'b, 'c) t[@kind k1]) [@@kind value]
    type 'a elt := ('a elt[@kind k1]) [@@kind value]

    [@@@mode.default mi = (global, m)]

    val iteri : 'a 'p1 'p2. ((('a, 'p1, 'p2) t, 'a elt) iteri[@mode mi])
    val existsi : 'a 'p1 'p2. ('a, 'p1, 'p2) t -> f:(int -> 'a elt -> bool) -> bool
    val for_alli : 'a 'p1 'p2. ('a, 'p1, 'p2) t -> f:(int -> 'a elt -> bool) -> bool
    val counti : 'a 'p1 'p2. ('a, 'p1, 'p2) t -> f:(int -> 'a elt -> bool) -> int

    [@@@kind.default_if_multiple k2 = ks_not_in_t]
    [@@@mode.default mo = (global, m)]

    val foldi : 'a 'p1 'p2 'acc. ((('a, 'p1, 'p2) t, 'a elt, 'acc) foldi[@mode mi mo])

    val iteri_until
      : 'a 'p1 'p2 'final.
      ((('a, 'p1, 'p2) t, 'a elt, 'final) iteri_until[@mode mi mo] [@kind k1 k2])

    val find_mapi
      : 'a 'p1 'p2 'b.
      ('a, 'p1, 'p2) t
      -> f:(int -> 'a elt -> ('b Option0.t[@kind k2]))
      -> ('b Option0.t[@kind k2])

    [@@@kind.default_if_multiple k3 = ks_not_in_t]

    val foldi_until
      : 'a 'p1 'p2 'acc 'final.
      ((('a, 'p1, 'p2) t, 'a elt, 'acc, 'final) foldi_until[@mode mi mo] [@kind k1 k2 k3])]
  end

  module type
    [@kind_set.explicit ks = (value, value_or_null, immediate, immediate64)] Iterators_with_index = sig
    (** Use a boxed product just for the [value] versions for backwards-compatibility *)

    include Iterators_with_index_without_findi [@kind_set.explicit ks] [@mode m]

    [@@@kind.default_if_multiple k = ks]

    val findi
      : 'a 'p1 'p2.
      (('a, 'p1, 'p2) t[@kind k])
      -> f:(int -> ('a elt[@kind k]) -> bool)
      -> (int * ('a elt[@kind k])) option
    [@@mode m = (global, m)]
  end

  module type Iterators_with_index = Iterators_with_index [@kind_set.explicit ks]
  [@@kind_set ks = value]

  module type [@kind_set.explicit ks = base_with_imm] Iterators_with_index = sig
    include Iterators_with_index_without_findi [@kind_set.explicit ks] [@mode m]

    [@@@kind.default_if_multiple k = ks]

    val findi
      : 'a 'p1 'p2.
      (('a, 'p1, 'p2) t[@kind k])
      -> f:(int -> ('a elt[@kind k]) -> bool)
      -> ((int * ('a elt[@kind k])) Option0.t[@kind value & k])
    [@@mode m = (global, m)]
  end]

  [%%template
  [@@@alloc.default a @ m = (heap_global, stack_local)]

  module type
    [@kind_set.explicit
      ks = (value, value_or_null, immediate, immediate64, base_with_imm)] Generic = sig
    include Container.Generic [@kind_set.explicit ks] [@alloc a]

    include
      Iterators_with_index
    [@kind_set.explicit ks]
    [@mode m]
    [@with:
      [@@@kind.default k = ks]

      type ('a, 'b, 'c) t := (('a, 'b, 'c) t[@kind k])
      type 'a elt := ('a elt[@kind k])]
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
    [@kind_set.explicit ks = (value, immediate, immediate64, base_with_imm)] S0 = sig
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
    ks = (value, value_or_null, immediate, immediate64, base_with_imm)]

  module type S1 = sig
    include Container.S1 [@kind_set.explicit ks] [@alloc a]

    include
      Iterators_with_index
    [@kind_set.explicit ks]
    [@mode m]
    [@with:
      [@@@kind.default k = ks]

      type ('a, _, _) t := ('a t[@kind k])
      type 'a elt := 'a]
  end

  module type S1 = S1 [@kind_set.explicit ks] [@alloc a] [@@kind_set ks = value]

  module type Creators_with_index = sig
    include Container.Generic_types [@kind_set.explicit ks]

    [%%template:
    [@@@kind.default_if_multiple k1 = ks]

    type ('a, 'b, 'c) t := (('a, 'b, 'c) t[@kind k1]) [@@kind value]
    type 'a elt := ('a elt[@kind k1]) [@@kind value]

    (** [init n ~f] is equivalent to [of_list [f 0; f 1; ...; f (n-1)]]. It raises an
        exception if [n < 0]. *)
    val init : 'a 'p1 'p2. int -> f:(int -> 'a elt) -> ('a, 'p1, 'p2) t
    [@@alloc __ @ m = (heap_global, a @ m)]

    val filteri
      : 'a 'p1 'p2.
      ('a, 'p1, 'p2) t -> f:(int -> 'a elt -> bool) -> ('a, 'p1, 'p2) t
    [@@alloc __ @ m = (heap_global, a @ m)]

    (** [partitioni_tf t ~f] is like partition_tf. Additionally, it passes the index as an
        argument. *)
    val partitioni_tf
      : 'a 'p1 'p2.
      ('a, 'p1, 'p2) t -> f:(int -> 'a elt -> bool) -> ('a, 'p1, 'p2) t * ('a, 'p1, 'p2) t
    [@@alloc __ @ m = (heap_global, a @ m)]]

    [@@@kind.default_if_multiple k1 = ks]
    [@@@kind.default_if_multiple k2 = ks]

    (** [mapi] is like map. Additionally, it passes in the index of each element as the
        first argument to the mapped function. *)
    val mapi
      : 'a 'p1 'p2 'b.
      (('a, 'p1, 'p2) t[@kind k1])
      -> f:(int -> ('a elt[@kind k1]) -> ('b elt[@kind k2]))
      -> (('b, 'p1, 'p2) t[@kind k2])
    [@@mode m = (global, m)] [@@alloc __ @ n = (heap_global, a @ m)]

    (** filter_mapi is like [filter_map]. Additionally, it passes in the index of each
        element as the first argument to the mapped function. *)
    val filter_mapi
      : 'a 'p1 'p2 'b.
      (('a, 'p1, 'p2) t[@kind k1])
      -> f:(int -> ('a elt[@kind k1]) -> (('b elt[@kind k2]) Option0.t[@kind k2]))
      -> (('b, 'p1, 'p2) t[@kind k2])
    [@@mode m = (global, m)] [@@alloc __ @ n = (heap_global, a @ m)]

    (** [concat_mapi t ~f] is like concat_map. Additionally, it passes the index as an
        argument. *)
    val concat_mapi
      : 'a 'p1 'p2 'b.
      (('a, 'p1, 'p2) t[@kind k1])
      -> f:(int -> ('a elt[@kind k1]) -> (('b, 'p1, 'p2) t[@kind k2]))
      -> (('b, 'p1, 'p2) t[@kind k2])
    [@@mode m = (global, m)] [@@alloc __ @ n = (heap_global, a @ m)]

    [@@@kind.default_if_multiple k3 = ks]

    (** [partition_mapi t ~f] is like partition_map. Additionally, it passes the index as
        an argument. *)
    val partition_mapi
      : 'a 'p1 'p2 'b 'c.
      (('a, 'p1, 'p2) t[@kind k1])
      -> f:
           (int
            -> ('a elt[@kind k1])
            -> ((('b elt[@kind k2]), ('c elt[@kind k3])) Either0.t[@kind k2 k3]))
      -> (('b, 'p1, 'p2) t[@kind k2]) * (('c, 'p1, 'p2) t[@kind k3])
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

      type ('a, 'b, 'c) t := (('a, 'b, 'c) t[@kind k])
      type 'a elt := ('a elt[@kind k])]

    include
      Creators_with_index
    [@kind_set.explicit ks]
    [@alloc a]
    [@with:
      [@@@kind.default k = ks]

      type ('a, 'b, 'c) t := (('a, 'b, 'c) t[@kind k])
      type 'a elt := ('a elt[@kind k])]
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

  module type [@kind_set.explicit ks = (value, base_with_imm)] S0_with_creators = sig
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

  [@@@kind_set.default.explicit ks = (value, value_or_null, base_with_imm)]

  module type S1_with_creators = sig
    include S1 [@kind_set.explicit ks] [@alloc a]

    include
      Container.S1_with_creators
    [@kind_set.explicit ks]
    [@alloc a]
    [@with: type 'a t := ('a t[@kind k]) [@@kind k = ks]]

    include
      Creators_with_index
    [@kind_set.explicit ks]
    [@alloc a]
    [@with:
      [@@@kind.default k = ks]

      type ('a, _, _) t := ('a t[@kind k])
      type 'a elt := 'a]
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
      : [ `Define_using_of_array | `Custom of int -> f:(int -> 'a elt) -> ('a, _, _) t ]

    val concat_mapi
      : [ `Define_using_concat
        | `Custom of ('a, _, _) t -> f:(int -> 'a elt -> ('b, _, _) t) -> ('b, _, _) t
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
      : 'a 't 'acc 'final.
      fold_until:(('t, 'a, 'acc, 'final) fold_until[@mode mi mo])
      -> (('t, 'a, 'acc, 'final) foldi_until[@mode mi mo])
    [@@mode mi = m, mo = (global, local)]

    (** Generic definitions of [foldi] and [iteri] in terms of [fold].

        E.g., [iteri ~fold t ~f = ignore (fold t ~init:0 ~f:(fun i x -> f i x; i + 1))]. *)

    val foldi
      : 'a 't 'acc.
      fold:(('t, 'a, 'acc) fold[@mode mi mo]) -> (('t, 'a, 'acc) foldi[@mode mi mo])
    [@@mode mi = m, mo = (global, local)]

    val iteri
      : 'a 't.
      fold:(('t, 'a, int) fold[@mode m global]) -> (('t, 'a) iteri[@mode m])

    (** Generic definition of [iteri_until] in terms of [foldi_until]. *)

    val iteri_until
      : 'a 't 'final.
      foldi_until:(('t, 'a, unit, 'final) foldi_until[@mode mi mo])
      -> (('t, 'a, 'final) iteri_until[@mode mi mo])
    [@@mode mi = m, mo = (global, local)]

    (** Generic definitions of indexed container operations in terms of [foldi]. *)

    val counti
      : 'a 't.
      foldi:(('t, 'a, int) foldi[@mode m global]) -> 't -> f:(int -> 'a -> bool) -> int

    (** Generic definitions of indexed container operations in terms of [iteri]. *)

    val existsi
      : 'a 't.
      iteri_until:(('t, 'a, bool) iteri_until[@mode m global])
      -> 't
      -> f:(int -> 'a -> bool)
      -> bool

    val for_alli
      : 'a 't.
      iteri_until:(('t, 'a, bool) iteri_until[@mode m global])
      -> 't
      -> f:(int -> 'a -> bool)
      -> bool

    val findi
      : 'a 't.
      iteri_until:(('t, 'a, (int * 'a) option) iteri_until[@mode m m])
      -> 't
      -> f:(int -> 'a -> bool)
      -> (int * 'a) option

    val find_mapi
      : 'a 't 'b.
      iteri_until:(('t, 'a, 'b option) iteri_until[@mode mi mo])
      -> 't
      -> f:(int -> 'a -> 'b option)
      -> 'b option
    [@@mode mi = m, mo = (global, local)]
  end
end

module type Indexed_container = sig
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
