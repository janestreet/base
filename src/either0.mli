type ('f, 's) t =
  | First of 'f
  | Second of 's
[@@deriving compare, sexp, sexp_grammar]

[%%rederive:
  type nonrec ('f, 's) t = ('f, 's) t =
    | First of 'f
    | Second of 's
  [@@deriving globalize, hash]]
