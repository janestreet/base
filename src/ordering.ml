open! Import0
module Bool = Bool0

type t =
  | Less
  | Equal
  | Greater
[@@deriving compare, hash, enumerate, sexp, sexp_grammar]

let equal a b = compare a b = 0

module Export = struct
  type _ordering = t =
    | Less
    | Equal
    | Greater
end

let of_int n = Bool.select (n < 0) Less (Bool.select (n = 0) Equal Greater)

external magic_transparent : t -> int = "%identity"

let to_int t = magic_transparent t - 1
