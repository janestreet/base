open! Import

module type S = sig
  type t
  val hash : t -> int
  val compare : t -> t -> int
end

module Make (T :
             sig
               type t [@@deriving compare, sexp_of]
               val hash : t -> int
             end) : S with type t := T.t = struct
  include T
end
