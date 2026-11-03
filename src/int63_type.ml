open! Import0
module Sys = Sys0

module T =
  Sys.Make_immediate64
    (struct
      type t = int
    end)
    (struct
      type t = int64
    end)

type t = T.t

module Repr = struct
  type _ t =
    | Int : int t
    | Int64 : int64 t
end

let repr : t Repr.t =
  match T.repr with
  | Immediate -> Int
  | Non_immediate -> Int64
;;
