type ('ok, 'err) t = ('ok, 'err) Stdlib.result =
  | Ok of 'ok
  | Error of 'err
[@@deriving sexp, sexp_grammar, compare, equal, hash, globalize]
