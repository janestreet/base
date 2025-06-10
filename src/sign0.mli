@@ portable

type t =
  | Neg
  | Zero
  | Pos
[@@deriving sexp ~localize, sexp_grammar, compare ~localize, hash, enumerate]

module%template Replace_polymorphic_compare : sig
  include Comparisons.S [@mode local] with type t := t

  val ascending : t -> t -> int
  val descending : t -> t -> int
end

val to_string : t -> string
val of_string : string -> t
val to_int : t -> int
val of_int : int -> t
val module_name : string
