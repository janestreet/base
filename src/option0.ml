open! Import

[%%template
  type nonrec 'a t =
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
      , value & value )]
  [@@deriving compare ~localize]]

type 'a t = 'a option =
  | None
  | Some of 'a
