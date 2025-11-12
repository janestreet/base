[@@@warning "-incompatible-with-upstream"]

[%%template
[@@@kind_set.define
  supported_non_values
  = (base_non_value, value & value, value & value & value, value & value & value & value)]

[@@@kind_set.define
  supported_values
  = (value_or_null_with_imm, value_or_null mod external_, value_or_null mod external64)]

type ('a : k) t = 'a [@@kind k = supported_values]
type ('a : k) t = unit -> 'a [@@kind k = supported_non_values]

external get : ('a : k). ('a t[@kind k]) -> 'a @@ portable = "%identity"
[@@kind k = supported_values]

let[@inline always] get t = (t [@inlined hint]) () [@@kind k = supported_non_values]]
