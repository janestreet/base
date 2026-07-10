[%%template
[@@@kind_set.define
  all_ks_non_value
  = ( base_non_value
    , value_or_null & value_or_null
    , value_or_null & value_or_null & value_or_null
    , value_or_null & value_or_null & value_or_null & value_or_null )]

[@@@kind_set.define all_ks = (all_ks_non_value, value_or_null)]

module type List0 = sig @@ portable
  module%template Constructors : sig
    type ('a : k) t =
      | []
      | ( :: ) of 'a * ('a t[@kind.explicit k])
    [@@kind.explicit k = all_ks_non_value] [@@deriving compare ~localize, equal ~localize]

    type ('a : value_or_null) t = 'a list [@@deriving compare ~localize, equal ~localize]

    (*_ Expose the main [t] with explicit mangling. Having this last also makes it the
        default constructors. *)
    type ('a : value_or_null) t = 'a list =
      | []
      | ( :: ) of 'a * 'a t
    [@@kind.explicit value_or_null]
  end

  open Constructors

  val max_non_tailcall : int

  val%template hd_exn : ('a : value_or_null). 'a t @ l -> 'a @ l
  [@@mode l = (global, local)]

  val%template tl_exn : ('a : value_or_null). 'a t @ l -> 'a t @ l
  [@@mode l = (global, local)]

  val unzip : ('a : value_or_null) ('b : value_or_null). ('a * 'b) t -> 'a t * 'b t
  val is_empty : ('a : value_or_null). 'a list @ local -> bool

  val%template partition_map
    : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
    'a list @ li
    -> f:('a @ li -> ('b, 'c) Either0.t @ lo) @ local
    -> 'b list * 'c list @ lo
  [@@mode li = (global, local)] [@@alloc a @ lo = (heap_global, stack_local)]

  [%%template:
  [@@@kind.default k = all_ks]

  val length : ('a : k). ('a t[@kind k]) @ immutable local -> int

  val exists : ('a : k). ('a t[@kind k]) @ l -> f:('a @ l -> bool) @ local -> bool
  [@@mode l = (local, global)]

  val iter : ('a : k). ('a t[@kind k]) @ l -> f:('a @ l -> unit) @ local -> unit
  [@@mode l = (local, global)]

  val rev_append
    : ('a : k).
    ('a t[@kind k]) @ l -> ('a t[@kind k]) @ l -> ('a t[@kind k]) @ l
  [@@alloc __ @ l = (stack_local, heap_global)]

  val rev : ('a : k). ('a t[@kind k]) @ l u -> ('a t[@kind k]) @ l u
  [@@mode u = (aliased, unique)] [@@alloc __ @ l = (stack_local, heap_global)]

  val for_all : ('a : k). ('a t[@kind k]) @ l -> f:('a @ l -> bool) @ local -> bool
  [@@mode l = (local, global)]]

  val fold
    : ('a : ka) ('b : kb).
    ('a t[@kind ka]) @ ma
    -> init:'b @ mb
    -> f:('b @ mb -> 'a @ ma -> 'b @ mb) @ local
    -> 'b @ mb
  [@@mode ma = (local, global), mb = (local, global)]
  [@@kind ka = all_ks, kb = (all_ks, value_or_null & all_ks)]

  val rev_map
    : ('a : ka) ('b : kb).
    ('a t[@kind ka]) @ ma -> f:('a @ ma -> 'b @ mb) @ local -> ('b t[@kind kb]) @ mb
  [@@mode ma = (local, global)]
  [@@alloc __ @ mb = (stack_local, heap_global)]
  [@@kind ka = all_ks, kb = all_ks]

  val fold2_ok
    : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
    'a t -> 'b t -> init:'c -> f:('c -> 'a -> 'b -> 'c) @ local -> 'c

  val exists2_ok
    : ('a : value_or_null) ('b : value_or_null).
    'a t -> 'b t -> f:('a -> 'b -> bool) @ local -> bool

  val%template iter2_ok
    : ('a : value_or_null) ('b : value_or_null).
    'a t @ l -> 'b t @ l -> f:('a @ l -> 'b @ l -> unit) @ local -> unit
  [@@mode l = (global, local)]

  val for_all2_ok
    : ('a : value_or_null) ('b : value_or_null).
    'a t -> 'b t -> f:('a -> 'b -> bool) @ local -> bool

  val nontail_map
    : ('a : value_or_null) ('b : value_or_null).
    'a t -> f:('a -> 'b) @ local -> 'b t

  val rev_map2_ok
    : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
    'a t -> 'b t -> f:('a -> 'b -> 'c) @ local -> 'c t

  val nontail_mapi
    : ('a : value_or_null) ('b : value_or_null).
    'a t -> f:(int -> 'a -> 'b) -> 'b t

  val partition : ('a : value_or_null). 'a t -> f:('a -> bool) -> 'a t * 'a t

  val fold_right
    : ('a : value_or_null) ('acc : value_or_null).
    'a t @ l
    -> f:('a @ l -> 'acc @ lcc -> 'acc @ lcc) @ local
    -> init:'acc @ lcc
    -> 'acc @ lcc
  [@@mode l = (local, global), lcc = (local, global)]

  val fold_right2_ok
    : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
    'a t -> 'b t -> f:('a -> 'b -> 'c -> 'c) @ local -> init:'c -> 'c
end]
