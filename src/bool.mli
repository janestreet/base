open! Import

type t = bool [@@deriving hash, sexp]

include Comparable.S with type t := t
include Hashable.S   with type t := t
include Stringable.S with type t := t

(** - [to_int true = 1]
    - [to_int false = 0]
*)
val to_int : t -> int
