include List0_intf.List0 with module Constructors = List0.Constructors

val is_empty : 'a. 'a list -> bool

val%template partition_map
  : 'a 'b 'c.
  'a list -> f:('a -> ('b, 'c) Either0.t) -> 'b list * 'c list
[@@mode mi = (global, local)] [@@alloc a @ mo = (heap_global, stack_local)]
