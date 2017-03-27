type t = Sexplib0.Sexp.t = Atom of string | List of t list
[@@deriving_inline compare, hash]
val hash_fold_t :
  Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state
val hash : t -> Ppx_hash_lib.Std.Hash.hash_value
val compare : t -> t -> int
[@@@end]

include module type of struct include Sexplib0.Sexp end with type t := t
