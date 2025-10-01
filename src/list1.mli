@@ portable

include List0_intf.List0 with module Constructors = List0.Constructors

val is_empty : ('a : value_or_null). 'a list @ local -> bool

val partition_map
  : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
  'a list -> f:('a -> ('b, 'c) Either0.t) @ local -> 'b list * 'c list
