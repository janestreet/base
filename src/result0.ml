open! Import

[%%template
type nonrec ('ok : k, 'err) t =
  | Ok of 'ok
  | Error of 'err
[@@deriving sexp ~localize, compare ~localize, equal ~localize]
[@@kind k = (float64, bits32, bits64, word)]

type nonrec ('a, 'b) t = ('a, 'b) Stdlib.result =
  | Ok of 'a
  | Error of 'b
[@@deriving sexp ~localize, sexp_grammar, compare ~localize, equal ~localize, hash]
[@@kind k = (value, immediate, immediate64)]

let globalize = globalize_result
let globalize_t = globalize_result [@@kind k = (immediate, immediate64)]]
