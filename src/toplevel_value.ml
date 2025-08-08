[%%template
type 'a t = 'a [@@kind k = (value, immediate, immediate64)]
type 'a t = unit -> 'a [@@kind k = (float64, bits32, bits64, word)]

external get : ('a t[@kind k]) -> 'a = "%identity"
[@@kind k = (immediate, immediate64, value)]

let[@inline always] get t = (t [@inlined hint]) ()
[@@kind k = (float64, bits32, bits64, word)]
;;]
