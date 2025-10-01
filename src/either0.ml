open! Import

[@@@warning "-incompatible-with-upstream"]

type%template ('f : kf, 's : ks) t =
  | First of 'f
  | Second of 's
[@@kind
  kf = (float64, bits32, bits64, word, value_or_null, immediate, immediate64)
  , ks = (float64, bits32, bits64, word, value_or_null, immediate, immediate64)]
[@@deriving compare ~localize, sexp ~stackify, sexp_grammar]

type%template nonrec ('f : value_or_null, 's : ks) t =
      (('f, 's) t[@kind value_or_null ks]) =
  | First of 'f
  | Second of 's
[@@kind kf = value, ks = (float64, bits32, bits64, word, immediate, immediate64)]
[@@deriving compare ~localize, sexp ~stackify, sexp_grammar]

type%template nonrec ('f : kf, 's : value_or_null) t =
      (('f, 's) t[@kind kf value_or_null]) =
  | First of 'f
  | Second of 's
[@@kind kf = (float64, bits32, bits64, word, immediate, immediate64), ks = value]
[@@deriving compare ~localize, sexp ~stackify, sexp_grammar]

[%%rederive.portable
  type nonrec ('f : value_or_null, 's : value_or_null) t = ('f, 's) t =
    | First of 'f
    | Second of 's
  [@@deriving globalize, hash]]
