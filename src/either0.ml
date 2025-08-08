open! Import

type%template ('f, 's) t =
  | First of 'f
  | Second of 's
[@@kind
  kf = (float64, bits32, bits64, word, value, immediate, immediate64)
  , ks = (float64, bits32, bits64, word, value, immediate, immediate64)]
[@@deriving compare ~localize, sexp ~stackify, sexp_grammar]

[%%rederive.portable
  type nonrec ('f, 's) t = ('f, 's) t =
    | First of 'f
    | Second of 's
  [@@deriving globalize, hash]]
