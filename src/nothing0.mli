@@ portable

type t = |
[@@deriving
  compare ~localize
  , enumerate
  , equal ~localize
  , globalize
  , hash
  , sexp ~stackify
  , sexp_grammar]

val unreachable_code_local : t @ local -> _
val unreachable_code : t -> _
val to_string : t -> string
val of_string : string -> t
