open! Import

[%%template
type nonrec ('ok : k, 'err : value_or_null) t =
  | Ok of 'ok
  | Error of 'err
[@@deriving sexp ~stackify, compare ~localize, equal ~localize, globalize]
[@@kind k = base_non_value]

type nonrec ('a : value_or_null, 'b : value_or_null) t = ('a, 'b) Stdlib.result =
  | Ok of 'a
  | Error of 'b
[@@deriving sexp ~stackify, sexp_grammar, compare ~localize, equal ~localize, hash]
[@@kind k = (value_or_null_with_imm, value mod external_, value mod external64)]

let globalize = globalize_result
[@@kind k = (value_with_imm, value mod external_, value mod external64)]
;;]
