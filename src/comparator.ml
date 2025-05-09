open! Import
module Sexp = Sexp0

type ('a, 'witness) t : value mod contended portable with 'witness @@ contended =
  { compare : 'a -> 'a -> int
  ; sexp_of_t : 'a -> Sexp.t
  }
[@@unsafe_allow_any_mode_crossing]
(* SAFETY
   ======

   The type system is not currently expressive enough to express something like "this
   field has a portable modality, iff this type parameter has a particular kind". So we
   use [@@unsafe_allow_any_mode_crossing] to "lie" about the actual kind of this type, and
   expose a safe interface.

   Importantly for safety, we must never expose a constructor which allows constructing a
   [('a, 'witness) t] with a ['witness] that crosses portability, unless the [compare] and
   [sexp_of_t] functions are known to be portable. *)

let[@inline] compare { compare; _ } = compare
let[@inline] sexp_of_t { sexp_of_t; _ } = sexp_of_t

type ('a, 'b) comparator = ('a, 'b) t

module type Witness = sig
  type comparator_witness
end

module type%template [@modality portable] Witness = sig
  type comparator_witness : value mod portable
end

[%%template
[@@@modality.default m = (portable, nonportable)]

module type S = sig @@ m
  type t

  include Witness [@modality m]

  val comparator : (t, comparator_witness) comparator
end

module type S1 = sig @@ m
  type 'a t

  include Witness [@modality m]

  val comparator : ('a t, comparator_witness) comparator
end

module type S_fc = sig
  type comparable_t

  include S [@modality m] with type t := comparable_t
end]

module Module = struct
  type ('a, 'b) t = (module S with type t = 'a and type comparator_witness = 'b)
end

let of_module (type a b) ((module M) : (a, b) Module.t) = M.comparator

let to_module (type a b) t : (a, b) Module.t =
  (module struct
    type t = a
    type comparator_witness = b

    let comparator = t
  end)
;;

let make (type t) ~compare ~sexp_of_t =
  (module struct
    type comparable_t = t
    type comparator_witness

    let comparator : (t, comparator_witness) comparator = { compare; sexp_of_t }
  end : S_fc
    with type comparable_t = t)
;;

let%template[@modality portable] make (type t) ~compare ~sexp_of_t =
  (module struct
    type comparable_t = t
    type comparator_witness : value mod portable

    let comparator : (t, comparator_witness) comparator = { compare; sexp_of_t }
  end : S_fc
    with type comparable_t = t[@modality portable])
;;

module%template.portable S_to_S1 (S : S) = struct
  type 'a t = S.t
  type comparator_witness = S.comparator_witness

  open S

  let comparator = comparator
end

module Witness : Witness = struct
  type comparator_witness
end

module%template [@modality p = portable] Witness : Witness [@modality p] = struct
  type comparator_witness : value mod portable
end

module%template.portable
  [@modality m] Make (M : sig
    type t [@@deriving compare, sexp_of]
  end) =
struct
  include M
  include Witness [@modality m]

  let comparator = M.{ compare; sexp_of_t }
end

module%template.portable
  [@modality m] Make1 (M : sig
    type 'a t

    val compare : 'a t -> 'a t -> int
    val sexp_of_t : 'a t -> Sexp.t
  end) : S1 [@modality m] with type 'a t := 'a M.t = struct
  include Witness [@modality m]

  let comparator = M.{ compare; sexp_of_t }
end

module Poly = struct
  type 'a t = 'a

  include%template Make1 [@modality portable] (struct
      type 'a t = 'a

      let compare = Poly.compare
      let sexp_of_t _ = Sexp.Atom "_"
    end)
end

module type Derived = sig
  type 'a t
  type !'cmp comparator_witness

  val comparator : ('a, 'cmp) comparator -> ('a t, 'cmp comparator_witness) comparator
end

module%template.portable Derived (M : sig
    type 'a t [@@deriving compare, sexp_of]
  end) =
struct
  type !'cmp comparator_witness

  let comparator a =
    { compare = M.compare a.compare; sexp_of_t = M.sexp_of_t a.sexp_of_t }
  ;;
end

module type Derived2 = sig
  type ('a, 'b) t
  type (!'cmp_a, !'cmp_b) comparator_witness

  val comparator
    :  ('a, 'cmp_a) comparator
    -> ('b, 'cmp_b) comparator
    -> (('a, 'b) t, ('cmp_a, 'cmp_b) comparator_witness) comparator
end

module%template.portable Derived2 (M : sig
    type ('a, 'b) t [@@deriving compare, sexp_of]
  end) =
struct
  type (!'cmp_a, !'cmp_b) comparator_witness

  let comparator a b =
    { compare = M.compare a.compare b.compare
    ; sexp_of_t = M.sexp_of_t a.sexp_of_t b.sexp_of_t
    }
  ;;
end

module type Derived_phantom = sig
  type ('a, 'b) t
  type 'cmp comparator_witness

  val comparator
    :  ('a, 'cmp) comparator
    -> (('a, _) t, 'cmp comparator_witness) comparator
end

module%template.portable Derived_phantom (M : sig
    type ('a, 'b) t

    val compare : ('a -> 'a -> int) -> ('a, 'b) t -> ('a, 'b) t -> int
    val sexp_of_t : ('a -> Sexp.t) -> ('a, _) t -> Sexp.t
  end) =
struct
  type 'cmp_a comparator_witness

  let comparator a =
    { compare = M.compare a.compare; sexp_of_t = M.sexp_of_t a.sexp_of_t }
  ;;
end

module type Derived2_phantom = sig
  type ('a, 'b, 'c) t
  type (!'cmp_a, !'cmp_b) comparator_witness

  val comparator
    :  ('a, 'cmp_a) comparator
    -> ('b, 'cmp_b) comparator
    -> (('a, 'b, _) t, ('cmp_a, 'cmp_b) comparator_witness) comparator
end

module%template.portable Derived2_phantom (M : sig
    type ('a, 'b, 'c) t

    val compare
      :  ('a -> 'a -> int)
      -> ('b -> 'b -> int)
      -> ('a, 'b, 'c) t
      -> ('a, 'b, 'c) t
      -> int

    val sexp_of_t : ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b, _) t -> Sexp.t
  end) =
struct
  type (!'cmp_a, !'cmp_b) comparator_witness

  let comparator a b =
    { compare = M.compare a.compare b.compare
    ; sexp_of_t = M.sexp_of_t a.sexp_of_t b.sexp_of_t
    }
  ;;
end
