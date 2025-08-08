include List0_intf.List0 with module Constructors = List0.Constructors

val is_empty : 'a list -> bool
val partition_map : 'a list -> f:('a -> ('b, 'c) Either0.t) -> 'b list * 'c list
