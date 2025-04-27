open! Import

type t =
  | Less
  | Equal
  | Greater
[@@deriving compare ~localize, hash, enumerate, sexp ~localize, sexp_grammar]

let equal a b = compare a b = 0
let equal__local a b = compare__local a b = 0

module Export = struct
  type _ordering = t =
    | Less
    | Equal
    | Greater
end

let of_int n = Bool0.select (n < 0) Less (Bool0.select (n = 0) Equal Greater)

external magic_transparent : t -> int @@ portable = "%identity"

let to_int t = magic_transparent t - 1
