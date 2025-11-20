open! Import
module Sexp = Sexp0

type ('a, 'witness) t : value mod contended portable with 'witness @@ contended =
  { compare : 'a -> 'a -> int
  ; sexp_of_t : 'a -> Sexp.t
  }
[@@unsafe_allow_any_mode_crossing]
(*=SAFETY
   ======
   The type system is not currently expressive enough to express something like "this
   field has a portable modality, iff this type parameter has a particular kind". So we
   use [@@unsafe_allow_any_mode_crossing] to "lie" about the actual kind of this type, and
   expose a safe interface.

   Importantly for safety, we must never expose a constructor which allows constructing a
   [('a, 'witness) t] with a ['witness] that crosses portability, unless the [compare] and
   [sexp_of_t] functions are known to be portable. In each of the functors below, we
   provide a demonstration that, in the cases where a [comparator_witness] is produced
   that crosses portability, the comparator is constructed [@ portable].

   The particularly interesting cases for this are the derived comparators. A derived
   comparator_witness can cross portability if both the compare function on which the
   comparator is being derived is portable, and if all of the comparators passed into the
   derived comparator are portable. We express this using with-kinds, and it should be
   thought about carefully. *)

let[@inline] compare { compare; _ } = compare
let[@inline] sexp_of_t { sexp_of_t; _ } = sexp_of_t

module T = struct
  type ('a, 'b) comparator = ('a, 'b) t
end

include T
include Comparator_intf.Definitions (T)

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

let%template[@modality p = (nonportable, portable)] make (type t) ~compare ~sexp_of_t =
  (module struct
    type comparable_t = t
    type comparator_witness : value mod p

    let comparator : (t, comparator_witness) comparator = { compare; sexp_of_t }
  end : S_fc
    with type comparable_t = t[@modality p])
;;

module%template.portable S_to_S1 (S : S) = struct
  type 'a t = S.t
  type comparator_witness = S.comparator_witness

  open S

  let comparator = comparator
end

[%%template
[@@@mode.default m = (local, global)]

module%template.portable
  [@modality p] Make (M : sig
    type t [@@deriving (compare [@mode m]), sexp_of]
  end) =
struct
  type comparator_witness : value mod p

  let comparator =
    (* Demonstrate that [compare] and [sexp_of_t] are portable when [value mod p] is
       [value mod portable]. *)
    let compare @ p = M.compare in
    let sexp_of_t @ p = M.sexp_of_t in
    { compare; sexp_of_t }
  ;;
end

module%template.portable
  [@modality p] Make1 (M : sig
    type 'a t

    val compare : 'a t @ m -> 'a t @ m -> int [@@mode m = (global, m)]
    val sexp_of_t : 'a t -> Sexp.t
  end) : S1 [@modality p] with type 'a t := 'a M.t = struct
  type comparator_witness : value mod p

  let comparator =
    (* Demonstrate that [compare] and [sexp_of_t] are portable when [value mod p] is
       [value mod portable]. *)
    let compare @ p = M.compare in
    let sexp_of_t @ p = M.sexp_of_t in
    { compare; sexp_of_t }
  ;;
end]

module Poly = struct
  type 'a t = 'a

  include%template Make1 [@modality portable] (struct
      type 'a t = 'a

      let compare = Poly.compare
      let sexp_of_t _ = Sexp.Atom "_"
    end)
end

[%%template
[@@@mode.default m = (local, global)]

module%template.portable
  [@modality p] Derived_phantom (M : sig
    type ('a, 'b) t

    val compare : ('a @ m -> 'a @ m -> int) -> ('a, 'b) t @ m -> ('a, 'b) t @ m -> int
    [@@mode m = (global, m)]

    val sexp_of_t : ('a -> Sexp.t) -> ('a, _) t -> Sexp.t
  end) =
struct
  type !'cmp_a comparator_witness : value mod p with 'cmp_a

  open struct
    (* Demonstrate that [compare] and [sexp_of_t] are portable when
       [value mod p with 'cmp_a] is [value mod portable]. *)
    let _comparator (type a (cmp_a : value mod portable)) (a : (a, cmp_a) comparator) =
      let compare @ p = [%eta2 M.compare a.compare] in
      let sexp_of_t @ p = [%eta1 M.sexp_of_t a.sexp_of_t] in
      { compare; sexp_of_t }
    ;;
  end

  let comparator a =
    let compare = [%eta2 M.compare a.compare] in
    let sexp_of_t = [%eta1 M.sexp_of_t a.sexp_of_t] in
    { compare; sexp_of_t }
  ;;
end

module%template.portable
  [@modality p] Derived2_phantom (M : sig
    type ('a, 'b, 'c) t

    val compare
      :  ('a @ m -> 'a @ m -> int)
      -> ('b @ m -> 'b @ m -> int)
      -> ('a, 'b, 'c) t @ m
      -> ('a, 'b, 'c) t @ m
      -> int
    [@@mode m = (global, m)]

    val sexp_of_t : ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b, _) t -> Sexp.t
  end) =
struct
  type (!'cmp_a, !'cmp_b) comparator_witness : value mod p with 'cmp_a with 'cmp_b

  open struct
    (* Demonstrate that [compare] and [sexp_of_t] are portable when
       [value mod p with 'cmp_a with 'cmp_b] is [value mod portable]. *)
    let _comparator
      (type a (cmp_a : value mod portable) b (cmp_b : value mod portable))
      (a : (a, cmp_a) comparator)
      (b : (b, cmp_b) comparator)
      =
      let compare @ p = [%eta2 M.compare a.compare b.compare] in
      let sexp_of_t @ p = [%eta1 M.sexp_of_t a.sexp_of_t b.sexp_of_t] in
      { compare; sexp_of_t }
    ;;
  end

  let comparator a b =
    let compare = [%eta2 M.compare a.compare b.compare] in
    let sexp_of_t = [%eta1 M.sexp_of_t a.sexp_of_t b.sexp_of_t] in
    { compare; sexp_of_t }
  ;;
end

module%template.portable
  [@modality p] Derived (M : sig
    type 'a t [@@deriving (compare [@mode m]), sexp_of]
  end) =
Derived_phantom [@mode m] [@modality p] (struct
    include M

    type ('a, _) t = 'a M.t
  end)

module%template.portable
  [@modality p] Derived2 (M : sig
    type ('a, 'b) t [@@deriving (compare [@mode m]), sexp_of]
  end) =
Derived2_phantom [@mode m] [@modality p] (struct
    include M

    type ('a, 'b, _) t = ('a, 'b) M.t
  end)]
