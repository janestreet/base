type t = Basement.nothing = |
[@@deriving compare, enumerate, equal, globalize, hash, sexp, sexp_grammar]

val unreachable_code : t -> _
val to_string : t -> string
val of_string : string -> t
