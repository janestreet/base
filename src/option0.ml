open! Import

[%%template
[@@@kind kr1 = (value & value)]
[@@@kind kr2 = (value & value & value)]
[@@@kind kr3 = (value & value & value & value)]

[@@@kind_set.define
  all_non_val = (base_non_value, value & (base_with_imm, kr1, kr2, kr3))]

[@@@kind_set.define
  all_val
  = (immediate, immediate64, value mod external_, value mod external64, value_or_null)]

type nonrec 'a t =
  | None
  | Some of 'a
[@@kind k = all_non_val] [@@deriving compare ~localize]

type 'a t = 'a option =
  | None
  | Some of 'a
[@@kind __ = all_val]

[@@@kind.default k = (all_val, all_non_val)]

let is_none : (_ t[@kind k]) -> bool = function
  | None -> true
  | _ -> false
;;

let is_some : (_ t[@kind k]) -> bool = function
  | Some _ -> true
  | _ -> false
;;]
