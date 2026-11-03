open! Import0
module Either = Either0
module Option = Option0

module Definitions = struct
  type ('t, 'a, 'accum) fold = 't -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum

  type ('t, 'a, 'accum, 'finish) fold_until =
    't
    -> init:'accum
    -> f:('accum -> 'a -> ('accum, 'finish) Container.Continue_or_stop.t)
    -> finish:('accum -> 'finish)
    -> 'finish

  type ('t, 'a, 'accum) foldi =
    't -> init:'accum -> f:(int -> 'accum -> 'a -> 'accum) -> 'accum

  type ('t, 'a) iteri = 't -> f:(int -> 'a -> unit) -> unit

  include struct
    type ('t, 'a, 'finish) iteri_until =
      't
      -> f:(int -> 'a -> (unit, 'finish) Container.Continue_or_stop.t)
      -> finish:(int -> 'finish)
      -> 'finish
  end

  include struct
    type ('t, 'a, 'accum, 'finish) foldi_until =
      't
      -> init:'accum
      -> f:(int -> 'accum -> 'a -> ('accum, 'finish) Container.Continue_or_stop.t)
      -> finish:(int -> 'accum -> 'finish)
      -> 'finish
  end

  module type Iterators_with_index_without_findi = sig
    include Container.Generic_types

    type ('a, 'b, 'c) t := ('a, 'b, 'c) t
    type 'a elt := 'a elt

    (** These are all like their equivalents in [Container] except that an index starting
        at 0 is added as the first argument to [f]. *)

    type ('a, 'b, 'c) t := ('a, 'b, 'c) t
    type 'a elt := 'a elt

    val iteri : 'a 'p1 'p2. (('a, 'p1, 'p2) t, 'a elt) iteri
    val existsi : 'a 'p1 'p2. ('a, 'p1, 'p2) t -> f:(int -> 'a elt -> bool) -> bool
    val for_alli : 'a 'p1 'p2. ('a, 'p1, 'p2) t -> f:(int -> 'a elt -> bool) -> bool
    val counti : 'a 'p1 'p2. ('a, 'p1, 'p2) t -> f:(int -> 'a elt -> bool) -> int
    val foldi : 'a 'p1 'p2 'acc. (('a, 'p1, 'p2) t, 'a elt, 'acc) foldi
    val iteri_until : 'a 'p1 'p2 'final. (('a, 'p1, 'p2) t, 'a elt, 'final) iteri_until

    val find_mapi
      : 'a 'p1 'p2 'b.
      ('a, 'p1, 'p2) t -> f:(int -> 'a elt -> 'b Option.t) -> 'b Option.t

    type ('a, 'b, 'c) t := ('a, 'b, 'c) t
    type 'a elt := 'a elt

    val foldi_until
      : 'a 'p1 'p2 'acc 'final.
      (('a, 'p1, 'p2) t, 'a elt, 'acc, 'final) foldi_until
  end

  module type Iterators_with_index = sig
    (** Use a boxed product just for the [value] versions for backwards-compatibility *)

    include Iterators_with_index_without_findi

    val findi
      : 'a 'p1 'p2.
      ('a, 'p1, 'p2) t -> f:(int -> 'a elt -> bool) -> (int * 'a elt) option
  end

  include struct
    module type Generic = sig
      include Container.Generic

      include
        Iterators_with_index
        with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
        with type 'a elt := 'a elt
    end
  end

  include struct
    module type Generic = sig
      include Generic

      include
        Container.Generic
        with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
         and type 'a elt := 'a elt
    end

    (** Like [Generic], but [mem] does not accept an [equal] function, since [Make0]
        already takes [Elt.equal]. *)

    include struct
      module type S0 = sig
        include Container.S0
        include Iterators_with_index with type ('a, _, _) t := t with type _ elt := elt
      end
    end

    include struct
      module type S1 = sig
        include Container.S1
        include Iterators_with_index with type ('a, _, _) t := 'a t with type 'a elt := 'a
      end

      module type Creators_with_index = sig
        include Container.Generic_types

        type ('a, 'b, 'c) t := ('a, 'b, 'c) t
        type 'a elt := 'a elt
        type ('a, 'b, 'c) t := ('a, 'b, 'c) t
        type 'a elt := 'a elt

        (** [init n ~f] is equivalent to [of_list [f 0; f 1; ...; f (n-1)]]. It raises an
            exception if [n < 0]. *)
        val init : 'a 'p1 'p2. int -> f:(int -> 'a elt) -> ('a, 'p1, 'p2) t

        val filteri
          : 'a 'p1 'p2.
          ('a, 'p1, 'p2) t -> f:(int -> 'a elt -> bool) -> ('a, 'p1, 'p2) t

        (** [partitioni_tf t ~f] is like partition_tf. Additionally, it passes the index
            as an argument. *)
        val partitioni_tf
          : 'a 'p1 'p2.
          ('a, 'p1, 'p2) t
          -> f:(int -> 'a elt -> bool)
          -> ('a, 'p1, 'p2) t * ('a, 'p1, 'p2) t

        (** [mapi] is like map. Additionally, it passes in the index of each element as
            the first argument to the mapped function. *)
        val mapi
          : 'a 'p1 'p2 'b.
          ('a, 'p1, 'p2) t -> f:(int -> 'a elt -> 'b elt) -> ('b, 'p1, 'p2) t

        (** filter_mapi is like [filter_map]. Additionally, it passes in the index of each
            element as the first argument to the mapped function. *)
        val filter_mapi
          : 'a 'p1 'p2 'b.
          ('a, 'p1, 'p2) t -> f:(int -> 'a elt -> 'b elt Option.t) -> ('b, 'p1, 'p2) t

        (** [concat_mapi t ~f] is like concat_map. Additionally, it passes the index as an
            argument. *)
        val concat_mapi
          : 'a 'p1 'p2 'b.
          ('a, 'p1, 'p2) t -> f:(int -> 'a elt -> ('b, 'p1, 'p2) t) -> ('b, 'p1, 'p2) t

        (** [partition_mapi t ~f] is like partition_map. Additionally, it passes the index
            as an argument. *)
        val partition_mapi
          : 'a 'p1 'p2 'b 'c.
          ('a, 'p1, 'p2) t
          -> f:(int -> 'a elt -> ('b elt, 'c elt) Either.t)
          -> ('b, 'p1, 'p2) t * ('c, 'p1, 'p2) t
      end

      module type Generic_with_creators = sig
        include Generic

        include
          Container.Creators
          with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
          with type 'a elt := 'a elt

        include
          Creators_with_index
          with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
          with type 'a elt := 'a elt
      end
    end

    module type Generic_with_creators = sig
      include Generic_with_creators

      include
        Container.Generic_with_creators
        with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
         and type 'a elt := 'a elt
    end
  end

  include struct
    (** Like [Generic_with_creators], but [mem] does not accept an [equal] function, since
        [Make0_with_creators] already takes [Elt.equal]. *)

    module type S0_with_creators = sig
      include S0
      include Container.S0_with_creators with type t := t with type elt := elt
      include Creators_with_index with type ('a, _, _) t := t with type _ elt := elt
    end
  end

  include struct
    module type S1_with_creators = sig
      include S1
      include Container.S1_with_creators with type 'a t := 'a t
      include Creators_with_index with type ('a, _, _) t := 'a t with type 'a elt := 'a
    end
  end

  module type Make_gen_arg = sig
    include Container.Make_gen_arg

    val iteri : [ `Define_using_fold | `Custom of (('a, _, _) t, 'a elt) iteri ]
    val foldi : [ `Define_using_fold | `Custom of (('a, _, _) t, 'a elt, _) foldi ]

    val foldi_until
      : [ `Define_using_fold_until | `Custom of (('a, _, _) t, 'a elt, _, _) foldi_until ]
  end

  module type Make_arg = sig
    include Container.Make_arg
    include Make_gen_arg with type ('a, _, _) t := 'a t and type 'a elt := 'a
  end

  module type Make0_arg = sig
    include Container.Make0_arg
    include Make_gen_arg with type ('a, _, _) t := t and type 'a elt := Elt.t
  end

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

  module type Derived = sig
    (** Generic definition of [foldi_until] in terms of [fold_until]. *)

    val foldi_until
      : 'a 't 'acc 'final.
      fold_until:('t, 'a, 'acc, 'final) fold_until -> ('t, 'a, 'acc, 'final) foldi_until

    (** Generic definitions of [foldi] and [iteri] in terms of [fold].

        E.g., [iteri ~fold t ~f = ignore (fold t ~init:0 ~f:(fun i x -> f i x; i + 1))]. *)

    val foldi : 'a 't 'acc. fold:('t, 'a, 'acc) fold -> ('t, 'a, 'acc) foldi
    val iteri : 'a 't. fold:('t, 'a, int) fold -> ('t, 'a) iteri

    (** Generic definition of [iteri_until] in terms of [foldi_until]. *)

    val iteri_until
      : 'a 't 'final.
      foldi_until:('t, 'a, unit, 'final) foldi_until -> ('t, 'a, 'final) iteri_until

    (** Generic definitions of indexed container operations in terms of [foldi]. *)

    val counti : 'a 't. foldi:('t, 'a, int) foldi -> 't -> f:(int -> 'a -> bool) -> int

    (** Generic definitions of indexed container operations in terms of [iteri]. *)

    val existsi
      : 'a 't.
      iteri_until:('t, 'a, bool) iteri_until -> 't -> f:(int -> 'a -> bool) -> bool

    val for_alli
      : 'a 't.
      iteri_until:('t, 'a, bool) iteri_until -> 't -> f:(int -> 'a -> bool) -> bool

    val findi
      : 'a 't.
      iteri_until:('t, 'a, (int * 'a) option) iteri_until
      -> 't
      -> f:(int -> 'a -> bool)
      -> (int * 'a) option

    val find_mapi
      : 'a 't 'b.
      iteri_until:('t, 'a, 'b option) iteri_until
      -> 't
      -> f:(int -> 'a -> 'b option)
      -> 'b option
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

  module Derived : Derived
  include module type of Derived
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
