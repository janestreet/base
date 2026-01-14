open! Import

[@@@warning "-incompatible-with-upstream"]

type%template ('f : kf, 's : ks) t =
  | First of 'f
  | Second of 's
[@@kind kf = base_or_null, ks = base_or_null]
[@@deriving compare ~localize, sexp ~stackify, sexp_grammar]

[%%rederive.portable
  type nonrec ('f : value_or_null, 's : value_or_null) t = ('f, 's) t =
    | First of 'f
    | Second of 's
  [@@deriving globalize, hash]]
