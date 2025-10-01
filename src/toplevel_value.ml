[@@@warning "-incompatible-with-upstream"]

[%%template
type ('a : k) t = 'a [@@kind k = (value, immediate, immediate64)]

type ('a : k) t = unit -> 'a
[@@kind
  k
  = ( float64
    , bits32
    , bits64
    , word
    , value & value
    , value & value & value
    , value & value & value & value )]

external get : ('a t[@kind k]) -> 'a @@ portable = "%identity"
[@@kind k = (immediate, immediate64, value)]

let[@inline always] get t = (t [@inlined hint]) ()
[@@kind
  k
  = ( float64
    , bits32
    , bits64
    , word
    , value & value
    , value & value & value
    , value & value & value & value )]
;;]
