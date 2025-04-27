(** Interfaces similar to [Container] and [Indexed_container] in [Base], updated to use
    locally allocated inputs and outputs. *)

open! Import
module Continue_or_stop = Container.Continue_or_stop

module Definitions = struct
  module type Summable = sig
    type t

    val zero : t
    val ( + ) : local_ t -> local_ t -> local_ t
  end

  module type Generic_accessors = sig
    type ('a, 'phantom1, 'phantom2) t
    type 'a elt

    val length : local_ (_, _, _) t -> int
    val is_empty : local_ (_, _, _) t -> bool

    val mem
      :  local_ ('a, _, _) t
      -> local_ 'a elt
      -> equal:local_ (local_ 'a elt -> local_ 'a elt -> bool)
      -> bool

    val iter : local_ ('a, _, _) t -> f:local_ (local_ 'a elt -> unit) -> unit

    val fold
      :  local_ ('a, _, _) t
      -> init:local_ 'acc
      -> f:local_ (local_ 'acc -> local_ 'a elt -> local_ 'acc)
      -> local_ 'acc

    val fold_result
      :  local_ ('a, _, _) t
      -> init:local_ 'acc
      -> f:local_ (local_ 'acc -> local_ 'a elt -> local_ ('acc, 'e) Result.t)
      -> local_ ('acc, 'e) Result.t

    val fold_until
      :  local_ ('a, _, _) t
      -> init:local_ 'acc
      -> f:
           local_ (local_ 'acc
                   -> local_ 'a elt
                   -> local_ ('acc, 'final) Continue_or_stop.t)
      -> finish:local_ (local_ 'acc -> local_ 'final)
      -> local_ 'final

    val exists : local_ ('a, _, _) t -> f:local_ (local_ 'a elt -> bool) -> bool
    val for_all : local_ ('a, _, _) t -> f:local_ (local_ 'a elt -> bool) -> bool
    val count : local_ ('a, _, _) t -> f:local_ (local_ 'a elt -> bool) -> int

    val sum
      :  (module Summable with type t = 'sum)
      -> local_ ('a, _, _) t
      -> f:local_ (local_ 'a elt -> local_ 'sum)
      -> local_ 'sum

    val find
      :  local_ ('a, _, _) t
      -> f:local_ (local_ 'a elt -> bool)
      -> local_ 'a elt option

    val find_map
      :  local_ ('a, _, _) t
      -> f:local_ (local_ 'a elt -> local_ 'b option)
      -> local_ 'b option

    val to_list : local_ ('a, _, _) t -> local_ 'a elt list

    val min_elt
      :  local_ ('a, _, _) t
      -> compare:local_ (local_ 'a elt -> local_ 'a elt -> int)
      -> local_ 'a elt option

    val max_elt
      :  local_ ('a, _, _) t
      -> compare:local_ (local_ 'a elt -> local_ 'a elt -> int)
      -> local_ 'a elt option
  end

  module type Generic_creators = sig
    type (_, _, _) t
    type _ elt
    type (_, _, _) concat

    val of_list : local_ 'a elt list -> local_ ('a, _, _) t

    val append
      :  local_ ('a, 'p1, 'p2) t
      -> local_ ('a, 'p1, 'p2) t
      -> local_ ('a, 'p1, 'p2) t

    val concat : local_ (('a, 'p1, 'p2) t, 'p1, 'p2) concat -> local_ ('a, 'p1, 'p2) t

    val map
      :  local_ ('a, 'p1, 'p2) t
      -> f:local_ (local_ 'a elt -> local_ 'b elt)
      -> local_ ('b, 'p1, 'p2) t

    val map_to_global
      :  local_ ('a, 'p1, 'p2) t
      -> f:local_ (local_ 'a elt -> 'b elt)
      -> ('b, 'p1, 'p2) t

    val map_of_global
      :  ('a, 'p1, 'p2) t
      -> f:local_ ('a elt -> local_ 'b elt)
      -> local_ ('b, 'p1, 'p2) t

    val filter
      :  local_ ('a, 'p1, 'p2) t
      -> f:local_ (local_ 'a elt -> bool)
      -> local_ ('a, 'p1, 'p2) t

    val filter_map
      :  local_ ('a, 'p1, 'p2) t
      -> f:local_ (local_ 'a elt -> local_ 'b elt option)
      -> local_ ('b, 'p1, 'p2) t

    val concat_map
      :  local_ ('a, 'p1, 'p2) t
      -> f:local_ (local_ 'a elt -> local_ ('b, 'p1, 'p2) t)
      -> local_ ('b, 'p1, 'p2) t

    val partition_tf
      :  local_ ('a, 'p1, 'p2) t
      -> f:local_ (local_ 'a elt -> bool)
      -> local_ ('a, 'p1, 'p2) t * ('a, 'p1, 'p2) t

    val partition_map
      :  local_ ('a, 'p1, 'p2) t
      -> f:local_ (local_ 'a elt -> local_ ('b elt, 'c elt) Either.t)
      -> local_ ('b, 'p1, 'p2) t * ('c, 'p1, 'p2) t
  end

  module type Generic_indexed_accessors = sig
    type (_, _, _) t
    type _ elt

    val foldi
      :  local_ ('a, _, _) t
      -> init:local_ 'acc
      -> f:local_ (int -> local_ 'acc -> local_ 'a elt -> local_ 'acc)
      -> local_ 'acc

    val iteri : local_ ('a, _, _) t -> f:local_ (int -> local_ 'a elt -> unit) -> unit
    val existsi : local_ ('a, _, _) t -> f:local_ (int -> local_ 'a elt -> bool) -> bool
    val for_alli : local_ ('a, _, _) t -> f:local_ (int -> local_ 'a elt -> bool) -> bool
    val counti : local_ ('a, _, _) t -> f:local_ (int -> local_ 'a elt -> bool) -> int

    val findi
      :  local_ ('a, _, _) t
      -> f:local_ (int -> local_ 'a elt -> bool)
      -> local_ (int * 'a elt) option

    val find_mapi
      :  local_ ('a, _, _) t
      -> f:local_ (int -> local_ 'a elt -> local_ 'b option)
      -> local_ 'b option

    val partitioni_tf
      :  local_ ('a, 'p1, 'p2) t
      -> f:local_ (int -> local_ 'a elt -> bool)
      -> local_ ('a, 'p1, 'p2) t * ('a, 'p1, 'p2) t

    val partition_mapi
      :  local_ ('a, 'p1, 'p2) t
      -> f:local_ (int -> local_ 'a elt -> local_ ('b elt, 'c elt) Either.t)
      -> local_ ('b, 'p1, 'p2) t * ('c, 'p1, 'p2) t
  end

  module type Generic_indexed_creators = sig
    type (_, _, _) t
    type _ elt

    val init : int -> f:local_ (int -> local_ 'a elt) -> local_ ('a, _, _) t

    val mapi
      :  local_ ('a, 'p1, 'p2) t
      -> f:local_ (int -> local_ 'a elt -> local_ 'b elt)
      -> local_ ('b, 'p1, 'p2) t

    val mapi_to_global
      :  local_ ('a, 'p1, 'p2) t
      -> f:local_ (int -> local_ 'a elt -> 'b elt)
      -> ('b, 'p1, 'p2) t

    val mapi_of_global
      :  ('a, 'p1, 'p2) t
      -> f:local_ (int -> 'a elt -> local_ 'b elt)
      -> local_ ('b, 'p1, 'p2) t

    val filteri
      :  local_ ('a, 'p1, 'p2) t
      -> f:local_ (int -> local_ 'a elt -> bool)
      -> local_ ('a, 'p1, 'p2) t

    val filter_mapi
      :  local_ ('a, 'p1, 'p2) t
      -> f:local_ (int -> local_ 'a elt -> local_ 'b elt option)
      -> local_ ('b, 'p1, 'p2) t

    val concat_mapi
      :  local_ ('a, 'p1, 'p2) t
      -> f:local_ (int -> local_ 'a elt -> local_ ('b, 'p1, 'p2) t)
      -> local_ ('b, 'p1, 'p2) t
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
