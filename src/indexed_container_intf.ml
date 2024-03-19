type ('t, 'a, 'accum) fold = 't -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum

type ('t, 'a, 'accum) foldi =
  't -> init:'accum -> f:(int -> 'accum -> 'a -> 'accum) -> 'accum

type ('t, 'a) iteri = 't -> f:(int -> 'a -> unit) -> unit

module type S0 = sig
  include Container.S0

  (** These are all like their equivalents in [Container] except that an index starting at
      0 is added as the first argument to [f]. *)

  val foldi : (t, elt, _) foldi
  val iteri : (t, elt) iteri
  val existsi : t -> f:(int -> elt -> bool) -> bool
  val for_alli : t -> f:(int -> elt -> bool) -> bool
  val counti : t -> f:(int -> elt -> bool) -> int
  val findi : t -> f:(int -> elt -> bool) -> (int * elt) option
  val find_mapi : t -> f:(int -> elt -> 'a option) -> 'a option
end

module type S1 = sig
  include Container.S1

  (** These are all like their equivalents in [Container] except that an index starting at
      0 is added as the first argument to [f]. *)

  val foldi : ('a t, 'a, _) foldi
  val iteri : ('a t, 'a) iteri
  val existsi : 'a t -> f:(int -> 'a -> bool) -> bool
  val for_alli : 'a t -> f:(int -> 'a -> bool) -> bool
  val counti : 'a t -> f:(int -> 'a -> bool) -> int
  val findi : 'a t -> f:(int -> 'a -> bool) -> (int * 'a) option
  val find_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b option
end

module type Generic = sig
  include Container.Generic

  (** These are all like their equivalents in [Container] except that an index starting at
      0 is added as the first argument to [f]. *)

  val foldi : (('a, _, _) t, 'a elt, _) foldi
  val iteri : (('a, _, _) t, 'a elt) iteri
  val existsi : ('a, _, _) t -> f:(int -> 'a elt -> bool) -> bool
  val for_alli : ('a, _, _) t -> f:(int -> 'a elt -> bool) -> bool
  val counti : ('a, _, _) t -> f:(int -> 'a elt -> bool) -> int
  val findi : ('a, _, _) t -> f:(int -> 'a elt -> bool) -> (int * 'a elt) option
  val find_mapi : ('a, _, _) t -> f:(int -> 'a elt -> 'b option) -> 'b option
end

module type S0_with_creators = sig
  include Container.S0_with_creators
  include S0 with type t := t and type elt := elt

  (** [init n ~f] is equivalent to [of_list [f 0; f 1; ...; f (n-1)]]. It raises an
      exception if [n < 0]. *)
  val init : int -> f:(int -> elt) -> t

  (** [mapi] is like map. Additionally, it passes in the index of each element as the
      first argument to the mapped function. *)
  val mapi : t -> f:(int -> elt -> elt) -> t

  val filteri : t -> f:(int -> elt -> bool) -> t

  (** filter_mapi is like [filter_map]. Additionally, it passes in the index of each
      element as the first argument to the mapped function. *)
  val filter_mapi : t -> f:(int -> elt -> elt option) -> t

  (** [concat_mapi t ~f] is like concat_map. Additionally, it passes the index as an
      argument. *)
  val concat_mapi : t -> f:(int -> elt -> t) -> t
end

module type S1_with_creators = sig
  include Container.S1_with_creators
  include S1 with type 'a t := 'a t

  (** [init n ~f] is equivalent to [of_list [f 0; f 1; ...; f (n-1)]]. It raises an
      exception if [n < 0]. *)
  val init : int -> f:(int -> 'a) -> 'a t

  (** [mapi] is like map. Additionally, it passes in the index of each element as the
      first argument to the mapped function. *)
  val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t

  val filteri : 'a t -> f:(int -> 'a -> bool) -> 'a t

  (** filter_mapi is like [filter_map]. Additionally, it passes in the index of each
      element as the first argument to the mapped function. *)
  val filter_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b t

  (** [concat_mapi t ~f] is like concat_map. Additionally, it passes the index as an
      argument. *)
  val concat_mapi : 'a t -> f:(int -> 'a -> 'b t) -> 'b t
end

module type Generic_with_creators = sig
  include Container.Generic_with_creators

  include
    Generic
      with type 'a elt := 'a elt
       and type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) t

  val init : int -> f:(int -> 'a elt) -> ('a, _, _) t
  val mapi : ('a, 'p1, 'p2) t -> f:(int -> 'a elt -> 'b elt) -> ('b, 'p1, 'p2) t
  val filteri : ('a, 'p1, 'p2) t -> f:(int -> 'a elt -> bool) -> ('a, 'p1, 'p2) t

  val filter_mapi
    :  ('a, 'p1, 'p2) t
    -> f:(int -> 'a elt -> 'b elt option)
    -> ('b, 'p1, 'p2) t

  val concat_mapi
    :  ('a, 'p1, 'p2) t
    -> f:(int -> 'a elt -> ('b, 'p1, 'p2) t)
    -> ('b, 'p1, 'p2) t
end

module type Make_gen_arg = sig
  include Container_intf.Make_gen_arg

  val iteri : [ `Define_using_fold | `Custom of (('a, _, _) t, 'a elt) iteri ]
  val foldi : [ `Define_using_fold | `Custom of (('a, _, _) t, 'a elt, _) foldi ]
end

module type Make_arg = sig
  include Container_intf.Make_arg
  include Make_gen_arg with type ('a, _, _) t := 'a t and type 'a elt := 'a
end

module type Make0_arg = sig
  include Container_intf.Make0_arg
  include Make_gen_arg with type ('a, _, _) t := t and type 'a elt := Elt.t
end

module type Make_common_with_creators_arg = sig
  include Container_intf.Make_common_with_creators_arg

  include
    Make_gen_arg with type ('a, 'p1, 'p2) t := ('a, 'p1, 'p2) t and type 'a elt := 'a elt

  val init
    : [ `Define_using_of_array | `Custom of int -> f:(int -> 'a elt) -> ('a, _, _) t ]

  val concat_mapi
    : [ `Define_using_concat
      | `Custom of ('a, _, _) t -> f:(int -> 'a elt -> ('b, _, _) t) -> ('b, _, _) t
      ]
end

module type Make_gen_with_creators_arg = sig
  include Container_intf.Make_gen_with_creators_arg

  include
    Make_common_with_creators_arg
      with type ('a, 'p1, 'p2) t := ('a, 'p1, 'p2) t
       and type 'a elt := 'a elt
       and type ('a, 'p1, 'p2) concat := ('a, 'p1, 'p2) concat
end

module type Make_with_creators_arg = sig
  include Container_intf.Make_with_creators_arg

  include
    Make_common_with_creators_arg
      with type ('a, _, _) t := 'a t
       and type 'a elt := 'a
       and type ('a, _, _) concat := 'a t
end

module type Make0_with_creators_arg = sig
  include Container_intf.Make0_with_creators_arg

  include
    Make_common_with_creators_arg
      with type ('a, _, _) t := t
       and type 'a elt := Elt.t
       and type ('a, _, _) concat := 'a list
end

module type Derived = sig
  (** Generic definitions of [foldi] and [iteri] in terms of [fold].

      E.g., [iteri ~fold t ~f = ignore (fold t ~init:0 ~f:(local_ (fun i x -> f i x; i + 1)))]. *)

  val foldi : fold:('t, 'a, 'acc) fold -> ('t, 'a, 'acc) foldi
  val iteri : fold:('t, 'a, int) fold -> ('t, 'a) iteri

  (** Generic definitions of indexed container operations in terms of [foldi]. *)

  val counti : foldi:('t, 'a, int) foldi -> 't -> f:(int -> 'a -> bool) -> int

  (** Generic definitions of indexed container operations in terms of [iteri]. *)

  val existsi : iteri:('t, 'a) iteri -> 't -> f:(int -> 'a -> bool) -> bool
  val for_alli : iteri:('t, 'a) iteri -> 't -> f:(int -> 'a -> bool) -> bool
  val findi : iteri:('t, 'a) iteri -> 't -> f:(int -> 'a -> bool) -> (int * 'a) option
  val find_mapi : iteri:('t, 'a) iteri -> 't -> f:(int -> 'a -> 'b option) -> 'b option
end

module type Indexed_container = sig
  (** Provides generic signatures for containers that support indexed iteration ([iteri],
      [foldi], ...). In principle, any container that has [iter] can also implement [iteri],
      but the idea is that [Indexed_container_intf] should be included only for containers
      that have a meaningful underlying ordering. *)

  module type Derived = Derived
  module type Generic = Generic
  module type Generic_with_creators = Generic_with_creators
  module type S0 = S0
  module type S0_with_creators = S0_with_creators
  module type S1 = S1
  module type S1_with_creators = S1_with_creators

  include Derived
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
