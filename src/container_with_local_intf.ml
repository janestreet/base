(** Interfaces similar to [Container] and [Indexed_container] in [Base], updated to use
    locally allocated inputs and outputs. *)

open! Import
module Continue_or_stop = Container.Continue_or_stop

module Definitions = struct
  module type Summable = sig
    type t

    val zero : t
    val ( + ) : t -> t -> t
  end

  module type Generic_accessors = sig
    type ('a, 'phantom1, 'phantom2) t
    type 'a elt

    val length : (_, _, _) t -> int
    val is_empty : (_, _, _) t -> bool
    val mem : ('a, _, _) t -> 'a elt -> equal:('a elt -> 'a elt -> bool) -> bool
    val iter : ('a, _, _) t -> f:('a elt -> unit) -> unit
    val fold : ('a, _, _) t -> init:'acc -> f:('acc -> 'a elt -> 'acc) -> 'acc

    val fold_result
      :  ('a, _, _) t
      -> init:'acc
      -> f:('acc -> 'a elt -> ('acc, 'e) Result.t)
      -> ('acc, 'e) Result.t

    val fold_until
      :  ('a, _, _) t
      -> init:'acc
      -> f:('acc -> 'a elt -> ('acc, 'final) Continue_or_stop.t)
      -> finish:('acc -> 'final)
      -> 'final

    val exists : ('a, _, _) t -> f:('a elt -> bool) -> bool
    val for_all : ('a, _, _) t -> f:('a elt -> bool) -> bool
    val count : ('a, _, _) t -> f:('a elt -> bool) -> int

    val sum
      :  (module Summable with type t = 'sum)
      -> ('a, _, _) t
      -> f:('a elt -> 'sum)
      -> 'sum

    val find : ('a, _, _) t -> f:('a elt -> bool) -> 'a elt option
    val find_map : ('a, _, _) t -> f:('a elt -> 'b option) -> 'b option
    val to_list : ('a, _, _) t -> 'a elt list
    val min_elt : ('a, _, _) t -> compare:('a elt -> 'a elt -> int) -> 'a elt option
    val max_elt : ('a, _, _) t -> compare:('a elt -> 'a elt -> int) -> 'a elt option
  end

  module type Generic_creators = sig
    type (_, _, _) t
    type _ elt
    type (_, _, _) concat

    val of_list : 'a elt list -> ('a, _, _) t
    val append : ('a, 'p1, 'p2) t -> ('a, 'p1, 'p2) t -> ('a, 'p1, 'p2) t
    val concat : (('a, 'p1, 'p2) t, 'p1, 'p2) concat -> ('a, 'p1, 'p2) t
    val map : ('a, 'p1, 'p2) t -> f:('a elt -> 'b elt) -> ('b, 'p1, 'p2) t
    val map_to_global : ('a, 'p1, 'p2) t -> f:('a elt -> 'b elt) -> ('b, 'p1, 'p2) t
    val map_of_global : ('a, 'p1, 'p2) t -> f:('a elt -> 'b elt) -> ('b, 'p1, 'p2) t
    val filter : ('a, 'p1, 'p2) t -> f:('a elt -> bool) -> ('a, 'p1, 'p2) t
    val filter_map : ('a, 'p1, 'p2) t -> f:('a elt -> 'b elt option) -> ('b, 'p1, 'p2) t

    val concat_map
      :  ('a, 'p1, 'p2) t
      -> f:('a elt -> ('b, 'p1, 'p2) t)
      -> ('b, 'p1, 'p2) t

    val partition_tf
      :  ('a, 'p1, 'p2) t
      -> f:('a elt -> bool)
      -> ('a, 'p1, 'p2) t * ('a, 'p1, 'p2) t

    val partition_map
      :  ('a, 'p1, 'p2) t
      -> f:('a elt -> ('b elt, 'c elt) Either.t)
      -> ('b, 'p1, 'p2) t * ('c, 'p1, 'p2) t
  end

  module type Generic_indexed_accessors = sig
    type (_, _, _) t
    type _ elt

    val foldi : ('a, _, _) t -> init:'acc -> f:(int -> 'acc -> 'a elt -> 'acc) -> 'acc
    val iteri : ('a, _, _) t -> f:(int -> 'a elt -> unit) -> unit
    val existsi : ('a, _, _) t -> f:(int -> 'a elt -> bool) -> bool
    val for_alli : ('a, _, _) t -> f:(int -> 'a elt -> bool) -> bool
    val counti : ('a, _, _) t -> f:(int -> 'a elt -> bool) -> int
    val findi : ('a, _, _) t -> f:(int -> 'a elt -> bool) -> (int * 'a elt) option
    val find_mapi : ('a, _, _) t -> f:(int -> 'a elt -> 'b option) -> 'b option

    val partitioni_tf
      :  ('a, 'p1, 'p2) t
      -> f:(int -> 'a elt -> bool)
      -> ('a, 'p1, 'p2) t * ('a, 'p1, 'p2) t

    val partition_mapi
      :  ('a, 'p1, 'p2) t
      -> f:(int -> 'a elt -> ('b elt, 'c elt) Either.t)
      -> ('b, 'p1, 'p2) t * ('c, 'p1, 'p2) t
  end

  module type Generic_indexed_creators = sig
    type (_, _, _) t
    type _ elt

    val init : int -> f:(int -> 'a elt) -> ('a, _, _) t
    val mapi : ('a, 'p1, 'p2) t -> f:(int -> 'a elt -> 'b elt) -> ('b, 'p1, 'p2) t

    val mapi_to_global
      :  ('a, 'p1, 'p2) t
      -> f:(int -> 'a elt -> 'b elt)
      -> ('b, 'p1, 'p2) t

    val mapi_of_global
      :  ('a, 'p1, 'p2) t
      -> f:(int -> 'a elt -> 'b elt)
      -> ('b, 'p1, 'p2) t

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

  module type Generic_indexed_with_creators = sig
    type (_, _, _) t
    type _ elt
    type (_, _, _) concat

    include
      Generic_accessors
      with type ('a, 'p1, 'p2) t := ('a, 'p1, 'p2) t
       and type 'a elt := 'a elt

    include
      Generic_creators
      with type ('a, 'p1, 'p2) t := ('a, 'p1, 'p2) t
       and type 'a elt := 'a elt
       and type ('a, 'p1, 'p2) concat := ('a, 'p1, 'p2) concat

    include
      Generic_indexed_accessors
      with type ('a, 'p1, 'p2) t := ('a, 'p1, 'p2) t
       and type 'a elt := 'a elt

    include
      Generic_indexed_creators
      with type ('a, 'p1, 'p2) t := ('a, 'p1, 'p2) t
       and type 'a elt := 'a elt
  end

  module type S0_indexed_with_creators = sig
    type t
    type elt

    include
      Generic_indexed_with_creators
      with type (_, _, _) t := t
       and type _ elt := elt
       and type ('a, _, _) concat := 'a list
  end

  module type S1_indexed_with_creators = sig
    type 'a t

    include
      Generic_indexed_with_creators
      with type ('a, _, _) t := 'a t
       and type 'a elt := 'a
       and type ('a, _, _) concat := 'a t
  end
end

module type Container_with_local = sig
  include module type of struct
    include Definitions
  end
end
