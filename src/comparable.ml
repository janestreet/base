open! Import
include Comparable_intf

module With_zero (T : sig
    type t [@@deriving_inline compare]

    include Ppx_compare_lib.Comparable.S with type t := t

    [@@@end]

    val zero : t
  end) =
struct
  open T

  let is_positive t = compare t zero > 0
  let is_non_negative t = compare t zero >= 0
  let is_negative t = compare t zero < 0
  let is_non_positive t = compare t zero <= 0
  let sign t = Sign0.of_int (compare t zero)
end

module Poly (T : sig
    type t [@@deriving_inline sexp_of]

    val sexp_of_t : t -> Sexplib0.Sexp.t

    [@@@end]
  end) =
struct
  module Replace_polymorphic_compare = struct
    type t = T.t [@@deriving_inline sexp_of]

    let sexp_of_t = (T.sexp_of_t : t -> Sexplib0.Sexp.t)

    [@@@end]

    include Poly
  end

  include Poly

  let between t ~low ~high = low <= t && t <= high
  let clamp_unchecked t ~min:min_ ~max:max_ = max min_ (min max_ t)

  let clamp_exn t ~min ~max =
    assert (min <= max);
    clamp_unchecked t ~min ~max
  ;;

  let clamp t ~min ~max =
    if min > max
    then
      Or_error.error_s
        (Sexp.message
           "clamp requires [min <= max]"
           [ "min", T.sexp_of_t min; "max", T.sexp_of_t max ])
    else Ok (clamp_unchecked t ~min ~max)
  ;;

  module C = struct
    include T
    include Comparator.Make (Replace_polymorphic_compare)
  end

  include C
end

let gt cmp a b = cmp a b > 0
let lt cmp a b = cmp a b < 0
let geq cmp a b = cmp a b >= 0
let geq_local cmp a b = cmp a b >= 0
let leq cmp a b = cmp a b <= 0
let leq_local cmp a b = cmp a b <= 0
let equal cmp a b = cmp a b = 0
let equal_local cmp a b = cmp a b = 0
let not_equal cmp a b = cmp a b <> 0
let min cmp t t' = Bool0.select (leq cmp t t') t t'
let min_local cmp t t' = exclave_ Bool0.select (leq_local cmp t t') t t'
let max cmp t t' = Bool0.select (geq cmp t t') t t'
let max_local cmp t t' = exclave_ Bool0.select (geq_local cmp t t') t t'

module Infix (T : sig
    type t [@@deriving_inline compare]

    include Ppx_compare_lib.Comparable.S with type t := t

    [@@@end]
  end) : Infix with type t := T.t = struct
  let ( > ) a b = gt T.compare a b
  let ( < ) a b = lt T.compare a b
  let ( >= ) a b = geq T.compare a b
  let ( <= ) a b = leq T.compare a b
  let ( = ) a b = equal T.compare a b
  let ( <> ) a b = not_equal T.compare a b
end
[@@inline always]

module Comparisons (T : sig
    type t [@@deriving_inline compare]

    include Ppx_compare_lib.Comparable.S with type t := t

    [@@@end]
  end) : Comparisons with type t := T.t = struct
  include Infix (T)

  let compare = T.compare
  let equal = ( = )
  let min t t' = min compare t t'
  let max t t' = max compare t t'
end
[@@inline always]

module Make_using_comparator (T : sig
    type t [@@deriving_inline sexp_of]

    val sexp_of_t : t -> Sexplib0.Sexp.t

    [@@@end]

    include Comparator.S with type t := t
  end) : S with type t := T.t and type comparator_witness = T.comparator_witness = struct
  module T = struct
    include T

    let compare = comparator.compare
  end

  include T
  module Replace_polymorphic_compare = Comparisons (T)
  include Replace_polymorphic_compare

  let ascending = compare
  let descending t t' = compare t' t
  let between t ~low ~high = low <= t && t <= high
  let clamp_unchecked t ~min:min_ ~max:max_ = max min_ (min max_ t)

  let clamp_exn t ~min ~max =
    assert (min <= max);
    clamp_unchecked t ~min ~max
  ;;

  let clamp t ~min ~max =
    if min > max
    then
      Or_error.error_s
        (Sexp.message
           "clamp requires [min <= max]"
           [ "min", T.sexp_of_t min; "max", T.sexp_of_t max ])
    else Ok (clamp_unchecked t ~min ~max)
  ;;
end

module Make (T : sig
    type t [@@deriving_inline compare, sexp_of]

    include Ppx_compare_lib.Comparable.S with type t := t

    val sexp_of_t : t -> Sexplib0.Sexp.t

    [@@@end]
  end) =
Make_using_comparator [@inlined hint] (struct
    include T
    include Comparator.Make (T)
  end)

module Inherit
    (C : sig
       type t [@@deriving_inline compare]

       include Ppx_compare_lib.Comparable.S with type t := t

       [@@@end]
     end)
    (T : sig
       type t [@@deriving_inline sexp_of]

       val sexp_of_t : t -> Sexplib0.Sexp.t

       [@@@end]

       val component : t -> C.t
     end) =
Make (struct
    type t = T.t [@@deriving_inline sexp_of]

    let sexp_of_t = (T.sexp_of_t : t -> Sexplib0.Sexp.t)

    [@@@end]

    let compare t t' = C.compare (T.component t) (T.component t')
  end)

(* compare [x] and [y] lexicographically using functions in the list [cmps] *)
let rec lexicographic_gen ~apply cmps x y =
  match cmps with
  | cmp :: cmps ->
    let res = apply cmp x y in
    if res = 0 then lexicographic_gen ~apply cmps x y else res
  | [] -> 0
;;

let[@inline] lexicographic cmps x y =
  let open Modes.Export in
  lexicographic_gen
    ~apply:(fun [@inline] cmp { global = x } { global = y } -> cmp x y)
    cmps
    { global = x }
    { global = y }
;;

let[@inline] lexicographic_local cmps x y =
  lexicographic_gen ~apply:(fun [@inline] cmp x y -> cmp x y) cmps x y
;;

let lift cmp ~f x y = cmp (f x) (f y)
let lift_local cmp ~f x y = cmp (f x) (f y) [@nontail]
let reverse cmp x y = cmp y x
let reverse_local cmp x y = cmp y x

type 'a reversed = 'a

let compare_reversed cmp x y = cmp y x
let compare_reversed_local cmp x y = cmp y x

module Local = struct
  let lexicographic = lexicographic_local
  let lift = lift_local
  let reverse = reverse_local

  type 'a reversed = 'a

  let compare_reversed = compare_reversed_local
  let equal = equal_local
  let max = max_local
  let min = min_local
end
