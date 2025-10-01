[@@@warning "-incompatible-with-upstream"]

module type List0 = sig @@ portable
  module Constructors : sig
    type%template ('a : k) t =
      | []
      | ( :: ) of 'a * ('a t[@kind k])
    [@@kind
      k
      = ( float64
        , bits32
        , bits64
        , word
        , immediate
        , immediate64
        , value_or_null & value_or_null
        , value_or_null & value_or_null & value_or_null
        , value_or_null & value_or_null & value_or_null & value_or_null )]
    [@@deriving compare ~localize, equal ~localize]

    type ('a : value_or_null) t = 'a list =
      | []
      | ( :: ) of 'a * 'a t
  end

  open Constructors

  val hd_exn : ('a : value_or_null). 'a t -> 'a
  val tl_exn : ('a : value_or_null). 'a t -> 'a t
  val unzip : ('a : value_or_null) ('b : value_or_null). ('a * 'b) t -> 'a t * 'b t

  [%%template:
  [@@@kind.default
    k
    = ( float64
      , bits32
      , bits64
      , word
      , immediate
      , immediate64
      , value_or_null
      , value_or_null & value_or_null
      , value_or_null & value_or_null & value_or_null
      , value_or_null & value_or_null & value_or_null & value_or_null )]

  val length : ('a : k). ('a t[@kind k]) @ immutable local -> int
  val exists : ('a : k). ('a t[@kind k]) -> f:('a -> bool) @ local -> bool
  val iter : ('a : k). ('a t[@kind k]) -> f:('a -> unit) @ local -> unit

  val rev_append
    : ('a : k).
    ('a t[@kind k]) @ m -> ('a t[@kind k]) @ m -> ('a t[@kind k]) @ m
  [@@alloc __ @ m = (stack_local, heap_global)]

  val rev : ('a : k). ('a t[@kind k]) @ m -> ('a t[@kind k]) @ m
  [@@alloc __ @ m = (stack_local, heap_global)]

  val for_all : ('a : k). ('a t[@kind k]) -> f:('a -> bool) @ local -> bool

  [@@@kind ka = k]

  val fold
    : ('a : ka) ('b : kb).
    ('a t[@kind ka]) @ ma
    -> init:'b @ mb
    -> f:('b @ mb -> 'a @ ma -> 'b @ mb) @ local
    -> 'b @ mb
  [@@mode ma = (local, global), mb = (local, global)]
  [@@kind
    ka = ka
    , kb
      = ( float64
        , bits32
        , bits64
        , word
        , immediate
        , immediate64
        , value_or_null
        , value_or_null & float64
        , value_or_null & bits32
        , value_or_null & bits64
        , value_or_null & word
        , value_or_null & immediate
        , value_or_null & immediate64
        , value_or_null & value_or_null
        , value_or_null & value_or_null & value_or_null
        , value_or_null & value_or_null & value_or_null & value_or_null )]

  [@@@kind.default
    kb = (float64, bits32, bits64, word, immediate, immediate64, value_or_null)]

  val rev_map
    : ('a : ka) ('b : kb).
    ('a t[@kind ka]) -> f:('a -> 'b) @ local -> ('b t[@kind kb])]

  val fold2_ok
    : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
    'a t -> 'b t -> init:'c -> f:('c -> 'a -> 'b -> 'c) @ local -> 'c

  val exists2_ok
    : ('a : value_or_null) ('b : value_or_null).
    'a t -> 'b t -> f:('a -> 'b -> bool) @ local -> bool

  val iter2_ok
    : ('a : value_or_null) ('b : value_or_null).
    'a t -> 'b t -> f:('a -> 'b -> unit) @ local -> unit

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

  val%template fold_right
    : ('a : value_or_null) ('acc : value_or_null).
    'a t @ m
    -> f:('a @ m -> 'acc @ mcc -> 'acc @ mcc) @ local
    -> init:'acc @ mcc
    -> 'acc @ mcc
  [@@mode m = (local, global), mcc = (local, global)]

  val fold_right2_ok
    : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
    'a t -> 'b t -> f:('a -> 'b -> 'c -> 'c) @ local -> init:'c -> 'c
end
