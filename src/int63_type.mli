open! Import0

type t [@@immediate64]

module Repr : sig
  type _ t =
    | Int : int t
    | Int64 : int64 t
end

val repr : t Repr.t
