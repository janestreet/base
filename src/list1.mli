@@ portable

include List0_intf.List0 with module Constructors = List0.Constructors

val is_empty : ('a : value_or_null). 'a list @ local -> bool

val%template partition_map
  : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
  'a list @ mi -> f:('a @ mi -> ('b, 'c) Either0.t @ mo) @ local -> 'b list * 'c list @ mo
[@@mode mi = (global, local)] [@@alloc a @ mo = (heap_global, stack_local)]
