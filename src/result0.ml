open! Import

[%%template
type nonrec ('ok : k, 'err : value_or_null) t =
  | Ok of 'ok
  | Error of 'err
[@@deriving sexp ~stackify, compare ~localize, equal ~localize, globalize]
[@@kind k = (float64, bits32, bits64, word)]

type nonrec ('a : value_or_null, 'b : value_or_null) t = ('a, 'b) Stdlib.result =
  | Ok of 'a
  | Error of 'b
[@@deriving sexp ~stackify, sexp_grammar, compare ~localize, equal ~localize, hash]
[@@kind k = (value_or_null, immediate, immediate64)]

let globalize = globalize_result [@@kind k = (value, immediate, immediate64)]]
