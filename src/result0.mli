@@ portable

[%%template:
type ('ok : k, 'err) t =
  | Ok of 'ok
  | Error of 'err
[@@deriving sexp ~localize, compare ~localize, equal ~localize]
[@@kind k = (float64, bits32, bits64, word)]

type ('ok, 'err) t = ('ok, 'err) Stdlib.result =
  | Ok of 'ok
  | Error of 'err
[@@deriving
  sexp ~localize, sexp_grammar, compare ~localize, equal ~localize, hash, globalize]
[@@kind k = (value, immediate, immediate64)]]
