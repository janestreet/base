@@ portable

include List0_intf.List0 with module Constructors = List0.Constructors

val is_empty : 'a list @ local -> bool
val partition_map : 'a list -> f:('a -> ('b, 'c) Either0.t) @ local -> 'b list * 'c list
