@@ portable

type t = Stdlib.Lexing.position =
  { pos_fname : string
  ; pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  }
[@@deriving compare ~localize, equal ~localize, hash, sexp_of ~stackify]

include%template Comparator.S [@mode portable] with type t := t

val make_location_string
  :  pos_fname:string
  -> pos_lnum:int
  -> pos_cnum:int
  -> pos_bol:int
  -> string

val to_string : t -> string
val of_pos : string * int * int * int -> t
val here_or_there : here:[%call_pos] -> t option -> t
val is_dummy : t @ local -> bool
