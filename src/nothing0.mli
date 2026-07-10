@@ portable

type t = Basement.nothing = |
[@@deriving
  compare ~localize
  , enumerate
  , equal ~localize
  , globalize
  , hash
  , sexp ~stackify
  , sexp_grammar]

val unreachable_code_local : t @ local -> _ @ unique
val unreachable_code : t -> _ @ unique
val to_string : t -> string
val of_string : string -> t
