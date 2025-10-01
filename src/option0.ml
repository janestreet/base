open! Import

[@@@warning "-incompatible-with-upstream"]

[%%template
[@@@kind kr1 = (value & value)]
[@@@kind kr2 = (value & value & value)]
[@@@kind kr3 = (value & value & value & value)]

type nonrec ('a : k) t =
  | None
  | Some of 'a
[@@kind
  k
  = ( float64
    , bits32
    , bits64
    , word
    , immediate
    , immediate64
    , value & float64
    , value & bits32
    , value & bits64
    , value & word
    , value & immediate
    , value & immediate64
    , value & value
    , value & kr1
    , value & kr2
    , value & kr3 )]
[@@deriving compare ~localize]]

type ('a : value_or_null) t = 'a option =
  | None
  | Some of 'a

[%%template
[@@@kind kr1 = (value & value)]
[@@@kind kr2 = (value & value & value)]
[@@@kind kr3 = (value & value & value & value)]

[@@@kind.default
  k
  = ( value
    , float64
    , bits32
    , bits64
    , word
    , immediate
    , immediate64
    , value & float64
    , value & bits32
    , value & bits64
    , value & word
    , value & immediate
    , value & immediate64
    , value & value
    , value & kr1
    , value & kr2
    , value & kr3 )]

let is_none : (_ t[@kind k]) @ contended local -> bool = function
  | None -> true
  | _ -> false
;;

let is_some : (_ t[@kind k]) @ contended local -> bool = function
  | Some _ -> true
  | _ -> false
;;]
