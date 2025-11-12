open! Import

[@@@warning "-incompatible-with-upstream"]

type%template ('f : kf, 's : ks) t =
  | First of 'f
  | Second of 's
[@@kind
  kf = (base_or_null_with_imm, value mod external_, value mod external64)
  , ks = (base_or_null_with_imm, value mod external_, value mod external64)]
[@@deriving compare ~localize, sexp ~stackify, sexp_grammar]

type%template nonrec ('f : value_or_null, 's : ks) t =
      (('f, 's) t[@kind value_or_null ks]) =
  | First of 'f
  | Second of 's
[@@kind kf = value, ks = (base_non_value, immediate, immediate64)]
[@@deriving compare ~localize, sexp ~stackify, sexp_grammar]

type%template nonrec ('f : kf, 's : value_or_null) t =
      (('f, 's) t[@kind kf value_or_null]) =
  | First of 'f
  | Second of 's
[@@kind kf = (base_non_value, immediate, immediate64), ks = value]
[@@deriving compare ~localize, sexp ~stackify, sexp_grammar]

[%%rederive.portable
  type nonrec ('f : value_or_null, 's : value_or_null) t = ('f, 's) t =
    | First of 'f
    | Second of 's
  [@@deriving globalize, hash]]
