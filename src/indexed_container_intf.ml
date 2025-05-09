open! Import

[@@@warning "-incompatible-with-upstream"]

module Definitions = struct
  type ('t, 'a, 'accum) fold =
    't -> init:'accum -> f:('accum -> 'a -> 'accum) @ local -> 'accum

  type ('t, 'a, 'accum) foldi =
    't -> init:'accum -> f:(int -> 'accum -> 'a -> 'accum) @ local -> 'accum

  type ('t, 'a) iteri = 't -> f:(int -> 'a -> unit) @ local -> unit

  module type%template Iterators_with_index = sig
    include Container.Generic_types [@kind k]

    (** These are all like their equivalents in [Container] except that an index starting
        at 0 is added as the first argument to [f]. *)

    val foldi : (('a, _, _) t, 'a elt, _) foldi
    val iteri : (('a, _, _) t, 'a elt) iteri
    val existsi : ('a, _, _) t -> f:(int -> 'a elt -> bool) @ local -> bool
    val for_alli : ('a, _, _) t -> f:(int -> 'a elt -> bool) @ local -> bool
    val counti : ('a, _, _) t -> f:(int -> 'a elt -> bool) @ local -> int
    val findi : ('a, _, _) t -> f:(int -> 'a elt -> bool) @ local -> (int * 'a elt) option
    val find_mapi : ('a, _, _) t -> f:(int -> 'a elt -> 'b option) @ local -> 'b option
  end
  [@@kind k = (value, immediate, immediate64)]

  module type Generic = sig
    include Container.Generic

    include
      Iterators_with_index
      with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) t
       and type 'a elt := 'a elt
  end

  (** Like [Generic], but [mem] does not accept an [equal] function, since [Make0] already
      takes [Elt.equal]. *)
  module type S0 = sig
    include Container.S0
    include Iterators_with_index with type (_, _, _) t := t and type _ elt := elt
  end

  module type%template S1 = sig
    include Container.S1 [@kind k]

    include
      Iterators_with_index [@kind k] with type ('a, _, _) t := 'a t and type 'a elt := 'a
  end
  [@@kind k = (value, immediate, immediate64)]

  module type Creators_with_index = sig
    include Container.Generic_types

    (** [init n ~f] is equivalent to [of_list [f 0; f 1; ...; f (n-1)]]. It raises an
        exception if [n < 0]. *)
    val init : int -> f:(int -> 'a elt) @ local -> ('a, _, _) t

    (** [mapi] is like map. Additionally, it passes in the index of each element as the
        first argument to the mapped function. *)
    val mapi : ('a, 'p1, 'p2) t -> f:(int -> 'a elt -> 'b elt) @ local -> ('b, 'p1, 'p2) t

    val filteri
      :  ('a, 'p1, 'p2) t
      -> f:(int -> 'a elt -> bool) @ local
      -> ('a, 'p1, 'p2) t

    (** filter_mapi is like [filter_map]. Additionally, it passes in the index of each
        element as the first argument to the mapped function. *)
    val filter_mapi
      :  ('a, 'p1, 'p2) t
      -> f:(int -> 'a elt -> 'b elt option) @ local
      -> ('b, 'p1, 'p2) t

    (** [concat_mapi t ~f] is like concat_map. Additionally, it passes the index as an
        argument. *)
    val concat_mapi
      :  ('a, 'p1, 'p2) t
      -> f:(int -> 'a elt -> ('b, 'p1, 'p2) t) @ local
      -> ('b, 'p1, 'p2) t

    (** [partitioni_tf t ~f] is like partition_tf. Additionally, it passes the index as an
        argument. *)
    val partitioni_tf
      :  ('a, 'p1, 'p2) t
      -> f:(int -> 'a elt -> bool) @ local
      -> ('a, 'p1, 'p2) t * ('a, 'p1, 'p2) t

    (** [partition_mapi t ~f] is like partition_map. Additionally, it passes the index as
        an argument. *)
    val partition_mapi
      :  ('a, 'p1, 'p2) t
      -> f:(int -> 'a elt -> ('b elt, 'c elt) Either0.t) @ local
      -> ('b, 'p1, 'p2) t * ('c, 'p1, 'p2) t
  end

  module type Generic_with_creators = sig
    include Generic

    include
      Container.Creators
      with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) t
       and type 'a elt := 'a elt

    include
      Creators_with_index
      with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) t
       and type 'a elt := 'a elt
  end

  (** Like [Generic_with_creators], but [mem] does not accept an [equal] function, since
      [Make0_with_creators] already takes [Elt.equal]. *)
  module type S0_with_creators = sig
    include S0

    include
      Container.Creators
      with type (_, _, _) t := t
       and type _ elt := elt
       and type (_, _, _) concat := t list

    include Creators_with_index with type (_, _, _) t := t and type _ elt := elt
  end

  module type S1_with_creators = sig
    include S1

    include
      Container.Creators
      with type ('a, _, _) t := 'a t
       and type 'a elt := 'a
       and type ('a, _, _) concat := 'a t

    include Creators_with_index with type ('a, _, _) t := 'a t and type 'a elt := 'a
  end

  module type Make_gen_arg = sig
    include Container.Make_gen_arg

    val iteri : [ `Define_using_fold | `Custom of (('a, _, _) t, 'a elt) iteri ]
    val foldi : [ `Define_using_fold | `Custom of (('a, _, _) t, 'a elt, _) foldi ]
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

  module type Derived = sig
    (** Generic definitions of [foldi] and [iteri] in terms of [fold].

        E.g., [iteri ~fold t ~f = ignore (fold t ~init:0 ~f:(fun i x -> f i x; i + 1))]. *)

    val foldi : fold:('t, 'a, 'acc) fold -> ('t, 'a, 'acc) foldi
    val iteri : fold:('t, 'a, int) fold -> ('t, 'a) iteri

    (** Generic definitions of indexed container operations in terms of [foldi]. *)

    val counti : foldi:('t, 'a, int) foldi -> 't -> f:(int -> 'a -> bool) @ local -> int

    (** Generic definitions of indexed container operations in terms of [iteri]. *)

    val existsi : iteri:('t, 'a) iteri -> 't -> f:(int -> 'a -> bool) @ local -> bool
    val for_alli : iteri:('t, 'a) iteri -> 't -> f:(int -> 'a -> bool) @ local -> bool

    val findi
      :  iteri:('t, 'a) iteri
      -> 't
      -> f:(int -> 'a -> bool) @ local
      -> (int * 'a) option

    val find_mapi
      :  iteri:('t, 'a) iteri
      -> 't
      -> f:(int -> 'a -> 'b option) @ local
      -> 'b option
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
  module%template.portable Make (T : Make_arg) : S1 with type 'a t := 'a T.t

  module%template.portable Make0 (T : Make0_arg) :
    S0 with type t := T.t and type elt := T.Elt.t

  module%template.portable Make_gen (T : Make_gen_arg) :
    Generic
    with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) T.t
     and type 'a elt := 'a T.elt

  module%template.portable Make_with_creators (T : Make_with_creators_arg) :
    S1_with_creators with type 'a t := 'a T.t

  module%template.portable Make0_with_creators (T : Make0_with_creators_arg) :
    S0_with_creators with type t := T.t and type elt := T.Elt.t

  module%template.portable Make_gen_with_creators (T : Make_gen_with_creators_arg) :
    Generic_with_creators
    with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) T.t
     and type 'a elt := 'a T.elt
     and type ('a, 'phantom1, 'phantom2) concat := ('a, 'phantom1, 'phantom2) T.concat
end
