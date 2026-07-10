open! Import

[%%template
[@@@kind kr1 = (value_or_null & value_or_null)]
[@@@kind kr2 = (value_or_null & kr1)]

[@@@kind_set.define
  all_ks_non_value = (base_non_value, value_or_null & (base_or_null, kr2))]

[@@@kind_set.define all_ks = (all_ks_non_value, value_or_null)]

type nonrec ('a : k) t =
  | None
  | Some of 'a
[@@kind k = all_ks_non_value] [@@deriving compare ~localize]

type ('a : value_or_null) t = 'a option =
  | None
  | Some of 'a

(*_ Also expose the main [t] with explicit mangling. *)
type ('a : value_or_null) t = 'a option =
  | None
  | Some of 'a
[@@kind.explicit value_or_null]

[@@@kind.default k = all_ks]

let is_none : (_ t[@kind k]) @ contended local -> bool = function
  | None -> true
  | _ -> false
;;

let is_some : (_ t[@kind k]) @ contended local -> bool = function
  | Some _ -> true
  | _ -> false
;;]
