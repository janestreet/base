open! Import

type ('a, 'witness) t =
  { compare   : 'a -> 'a -> int
  ; sexp_of_t : 'a -> Sexp.t
  }

type ('a, 'b) comparator = ('a, 'b) t

module type S = sig
  type t
  type comparator_witness
  val comparator : (t, comparator_witness) comparator
end

module type S1 = sig
  type 'a t
  type comparator_witness
  val comparator : ('a t, comparator_witness) comparator
end

module type S_fc = sig
  type comparable_t
  include S with type t := comparable_t
end

let make (type t) ~compare ~sexp_of_t =
  (module struct
    type comparable_t = t
    type comparator_witness
    let comparator = { compare; sexp_of_t }
  end : S_fc with type comparable_t = t)

module S_to_S1 (S : S) = struct
  type 'a t = S.t
  type comparator_witness = S.comparator_witness
  open S
  let comparator = comparator
end

module Make (M : sig type t [@@deriving compare, sexp_of] end) = struct
  include M
  type comparator_witness
  let comparator = M.({ compare; sexp_of_t })
end

module Make1 (M : sig
  type 'a t
  val compare : 'a t -> 'a t -> int
  val sexp_of_t : 'a t -> Sexp.t
end) = struct
  type comparator_witness
  let comparator = M.({ compare; sexp_of_t })
end

module Poly = struct
  type 'a t = 'a
  include Make1 (struct
    type 'a t = 'a
    let compare = Pervasives.compare
    let sexp_of_t = [%sexp_of: _]
  end)
end
