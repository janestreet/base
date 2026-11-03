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

val unreachable_code : t @ local -> _ @ unique

val%template to_string : t @ m -> string @ m [@@alloc a @ m = (heap_global, stack_local)]

val of_string : string @ local -> t
