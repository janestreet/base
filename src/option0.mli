@@ portable

[%%template:
type ('a : k) t =
  | None
  | Some of 'a
[@@kind k = (float64, bits32, bits64, word)] [@@deriving compare ~localize]

type ('a : k) t = 'a option =
  | None
  | Some of 'a
[@@kind k = value]]
