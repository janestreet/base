open! Import

type nonrec ('a, 'b) t = ('a, 'b) Stdlib.result =
  | Ok of 'a
  | Error of 'b
[@@deriving sexp, sexp_grammar, compare, equal, hash]

let globalize = globalize_result
