module type List0 = sig @@ portable
  module Constructors : sig
    type%template ('a : k) t =
      | []
      | ( :: ) of 'a * ('a t[@kind k])
    [@@kind k = (float64, bits32, bits64, word)]
    [@@deriving compare ~localize, equal ~localize]

    type 'a t = 'a list =
      | []
      | ( :: ) of 'a * 'a t
  end

  open Constructors

  val hd_exn : 'a t -> 'a
  val tl_exn : 'a t -> 'a t
  val unzip : ('a * 'b) t -> 'a t * 'b t

  [%%template:
  [@@@kind.default k = (float64, bits32, bits64, word, value)]

  val length : ('a t[@kind k]) @ local -> int
  val exists : ('a t[@kind k]) -> f:('a -> bool) @ local -> bool
  val iter : ('a t[@kind k]) -> f:('a -> unit) @ local -> unit

  val rev_append : ('a t[@kind k]) @ m -> ('a t[@kind k]) @ m -> ('a t[@kind k]) @ m
  [@@alloc __ @ m = (stack_local, heap_global)]

  val rev : ('a t[@kind k]) @ m -> ('a t[@kind k]) @ m
  [@@alloc __ @ m = (stack_local, heap_global)]

  val for_all : ('a t[@kind k]) -> f:('a -> bool) @ local -> bool

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
        , value
        , value & float64
        , value & bits32
        , value & bits64
        , value & word
        , value & value )]

  [@@@kind.default kb = (float64, bits32, bits64, word, value)]

  val rev_map : ('a t[@kind ka]) -> f:('a -> 'b) @ local -> ('b t[@kind kb])]

  val fold2_ok : 'a t -> 'b t -> init:'c -> f:('c -> 'a -> 'b -> 'c) @ local -> 'c
  val exists2_ok : 'a t -> 'b t -> f:('a -> 'b -> bool) @ local -> bool
  val iter2_ok : 'a t -> 'b t -> f:('a -> 'b -> unit) @ local -> unit
  val for_all2_ok : 'a t -> 'b t -> f:('a -> 'b -> bool) @ local -> bool
  val nontail_map : 'a t -> f:('a -> 'b) @ local -> 'b t
  val rev_map2_ok : 'a t -> 'b t -> f:('a -> 'b -> 'c) @ local -> 'c t
  val nontail_mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t
  val partition : 'a t -> f:('a -> bool) -> 'a t * 'a t

  val%template fold_right
    :  'a t @ m
    -> f:('a @ m -> 'acc @ mcc -> 'acc @ mcc) @ local
    -> init:'acc @ mcc
    -> 'acc @ mcc
  [@@mode m = (local, global), mcc = (local, global)]

  val fold_right2_ok : 'a t -> 'b t -> f:('a -> 'b -> 'c -> 'c) @ local -> init:'c -> 'c
end
