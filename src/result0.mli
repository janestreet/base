[%%template:
type ('ok, 'err) t =
  | Ok of 'ok
  | Error of 'err
[@@deriving sexp ~stackify, compare ~localize, equal ~localize, globalize]
[@@kind k = (float64, bits32, bits64, word)]

type ('ok, 'err) t = ('ok, 'err) Stdlib.result =
  | Ok of 'ok
  | Error of 'err
[@@deriving
  sexp ~stackify, sexp_grammar, compare ~localize, equal ~localize, hash, globalize]
[@@kind k = (value_or_null, immediate, immediate64)]]
