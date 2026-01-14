open! Import

type%template ('f, 's) t =
  | First of 'f
  | Second of 's
[@@kind kf = base_or_null, ks = base_or_null]
[@@deriving compare ~localize, sexp ~stackify, sexp_grammar]

[%%rederive.portable
  type nonrec ('f, 's) t = ('f, 's) t =
    | First of 'f
    | Second of 's
  [@@deriving globalize, hash]]
