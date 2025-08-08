type%template ('f, 's) t =
  | First of 'f
  | Second of 's
[@@kind
  kf = (float64, bits32, bits64, word, immediate, immediate64, value)
  , ks = (float64, bits32, bits64, word, immediate, immediate64, value)]
[@@deriving compare ~localize, sexp ~stackify, sexp_grammar]

[%%rederive:
  type nonrec ('f, 's) t = ('f, 's) t =
    | First of 'f
    | Second of 's
  [@@deriving globalize, hash]]
