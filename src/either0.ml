open! Import

type ('f, 's) t =
  | First of 'f
  | Second of 's
[@@deriving compare ~localize, globalize, hash, sexp ~localize, sexp_grammar]
