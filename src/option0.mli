[%%template:
[@@@kind kr1 = (value & value)]
[@@@kind kr2 = (value & value & value)]
[@@@kind kr3 = (value & value & value & value)]
[@@@kind_set.define all_non_val = (base_non_value, value & (base, kr1, kr2, kr3))]

type nonrec 'a t =
  | None
  | Some of 'a
[@@kind k = all_non_val] [@@deriving compare ~localize]

type 'a t = 'a option =
  | None
  | Some of 'a

[@@@kind.default k = (value_or_null, all_non_val)]

val is_none : 'a. ('a t[@kind k]) -> bool
val is_some : 'a. ('a t[@kind k]) -> bool]
