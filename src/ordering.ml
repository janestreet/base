open! Import

type t =
  | Less
  | Equal
  | Greater
[@@deriving compare ~localize, hash, enumerate, sexp ~stackify, sexp_grammar]

let%template[@mode m = (local, global)] equal a b = (compare [@mode m]) a b = 0

module Export = struct
  type _ordering = t =
    | Less
    | Equal
    | Greater
end

let of_int n = Bool0.select (n < 0) Less (Bool0.select (n = 0) Equal Greater)

external magic_transparent : t -> int @@ portable = "%identity"

let to_int t = magic_transparent t - 1
