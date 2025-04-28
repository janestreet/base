open! Import
module Sexp = Sexp0
include Comparable_intf

module%template With_zero (T : sig
    type t [@@deriving compare [@mode m]]

    val zero : t
  end) =
struct
  open T

  let is_positive t = (compare [@mode m]) t zero > 0
  let is_non_negative t = (compare [@mode m]) t zero >= 0
  let is_negative t = (compare [@mode m]) t zero < 0
  let is_non_positive t = (compare [@mode m]) t zero <= 0
  let sign t = Sign0.of_int ((compare [@mode m]) t zero)
end
[@@mode m = (global, local)]

module With_zero__portable (T : sig
    type t [@@deriving compare]

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

module%template.portable
  [@modality p] Poly (T : sig
    type t [@@deriving sexp_of]
  end) =
struct
  module Replace_polymorphic_compare = struct
    type t = T.t [@@deriving sexp_of]

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
    include Comparator.Make [@modality p] (Replace_polymorphic_compare)
  end

  include C
end

let gt cmp a b = cmp a b > 0
let lt cmp a b = cmp a b < 0
let not_equal cmp a b = cmp a b <> 0

[%%template
[@@@mode.default m = (global, local)]
[@@@kind.default k = (value, float64, bits32, bits64, word)]

let geq cmp a b = cmp a b >= 0
let leq cmp a b = cmp a b <= 0
let equal cmp a b = cmp a b = 0

let min cmp t t' =
  let is_leq = (leq [@mode m] [@kind k]) cmp t t' in
  (Bool0.select [@mode m] [@kind k]) is_leq t t' [@exclave_if_local m]
;;

let max cmp t t' =
  let is_geq = (geq [@mode m] [@kind k]) cmp t t' in
  (Bool0.select [@mode m] [@kind k]) is_geq t t' [@exclave_if_local m]
;;]

module%template.portable Infix (T : sig
    type t [@@deriving compare]
  end) : Infix with type t := T.t = struct
  let ( > ) a b = gt T.compare a b
  let ( < ) a b = lt T.compare a b
  let ( >= ) a b = geq T.compare a b
  let ( <= ) a b = leq T.compare a b
  let ( = ) a b = equal T.compare a b
  let ( <> ) a b = not_equal T.compare a b
end
[@@inline always]

module%template.portable
  [@modality p] Comparisons (T : sig
    type t [@@deriving compare]
  end) : Comparisons with type t := T.t = struct
  include Infix [@modality p] (T)

  let compare = T.compare
  let equal = ( = )
  let min t t' = min compare t t'
  let max t t' = max compare t t'
end
[@@inline always]

module%template.portable
  [@modality p] Make_using_comparator (T : sig
    type t [@@deriving sexp_of]

    include Comparator.S with type t := t
  end) : S with type t := T.t and type comparator_witness = T.comparator_witness = struct
  module T = struct
    include T

    let compare = comparator.compare
  end

  include T
  module Replace_polymorphic_compare = Comparisons [@modality p] (T)
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

module%template.portable
  [@modality p] Make (T : sig
    type t [@@deriving compare, sexp_of]
  end) =
Make_using_comparator [@inlined hint] [@modality p] (struct
    include T
    include Comparator.Make [@modality p] (T)
  end)

module%template.portable
  [@modality p] Inherit
    (C : sig
       type t [@@deriving compare]
     end)
    (T : sig
       type t [@@deriving sexp_of]

       val component : t -> C.t
     end) =
Make [@modality p] (struct
    type t = T.t [@@deriving sexp_of]

    let compare t t' = C.compare (T.component t) (T.component t')
  end)

type 'a reversed = 'a

[%%template
[@@@mode.default m = (global, local)]
[@@@kind.default k = (value, float64, bits32, bits64, word)]

(* compare [x] and [y] lexicographically using functions in the list [cmps] *)
let rec lexicographic cmps x y =
  match cmps with
  | cmp :: cmps ->
    let res = cmp x y in
    if res = 0 then (lexicographic [@mode m] [@kind k]) cmps x y else res
  | [] -> 0
;;

let lift cmp ~f x y = cmp (f x) (f y) [@nontail]
let reverse cmp x y = cmp y x
let compare_reversed cmp x y = cmp y x]
