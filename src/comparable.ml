open! Import
module Bool = Bool0
module Error = Error0
module Sexp = Sexp0
module Sign = Sign0
include Comparable_intf.Definitions

module With_zero (T : sig
    type t [@@deriving compare]

    val zero : t
  end) =
struct
  open T

  let is_positive t = compare t zero > 0
  let is_non_negative t = compare t zero >= 0
  let is_negative t = compare t zero < 0
  let is_non_positive t = compare t zero <= 0
  let sign t = Sign.of_int (compare t zero)
end

module Poly (T : sig
    type t [@@deriving sexp_of]
  end) =
struct
  module Replace_polymorphic_compare = struct
    type t = T.t [@@deriving sexp_of]

    include (Poly : module type of Poly)
  end

  include (Poly : module type of Poly)

  let between t ~low ~high = low <= t && t <= high
  let clamp_unchecked t ~min:min_ ~max:max_ = max min_ (min max_ t)

  let clamp_exn t ~min ~max =
    assert (min <= max);
    clamp_unchecked t ~min ~max
  ;;

  let clamp t ~min ~max =
    if min > max
    then
      Error
        (Error.create_s
           (Sexp.message
              "clamp requires [min <= max]"
              [ "min", T.sexp_of_t min; "max", T.sexp_of_t max ]))
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
let not_equal cmp a b = cmp a b <> 0
let geq cmp a b = cmp a b >= 0
let leq cmp a b = cmp a b <= 0
let equal cmp a b = cmp a b = 0

let min cmp t t' =
  let is_leq = leq cmp t t' in
  Bool.select is_leq t t'
;;

let max cmp t t' =
  let is_geq = geq cmp t t' in
  Bool.select is_geq t t'
;;

module Infix (T : sig
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

module Infix_with_zero_alloc (T : sig
    type t [@@deriving compare ~zero_alloc]
  end) : Infix_with_zero_alloc with type t := T.t = struct
  let _ : _ = T.compare
  let ( > ) a b = T.compare a b > 0
  let ( < ) a b = T.compare a b < 0
  let ( >= ) a b = T.compare a b >= 0
  let ( <= ) a b = T.compare a b <= 0
  let ( = ) a b = T.compare a b = 0
  let ( <> ) a b = T.compare a b <> 0
end
[@@inline always]

module Comparisons (T : sig
    type t [@@deriving compare]
  end) : Comparisons with type t := T.t = struct
  include Infix (T)

  let compare = T.compare
  let[@inline] equal x y = equal T.compare x y
  let min t t' = min compare t t'
  let max t t' = max compare t t'
end
[@@inline always]

module Comparisons_with_zero_alloc (T : sig
    type t [@@deriving compare ~zero_alloc]
  end) : Comparisons_with_zero_alloc with type t := T.t = struct
  include Infix_with_zero_alloc (T)

  let compare x y = T.compare x y
  let[@inline] equal x y = equal_int (T.compare x y) 0
  let min t t' = min compare t t'
  let max t t' = max compare t t'
end
[@@inline always]

module Make_using_comparator (T : sig
    type t [@@deriving sexp_of]

    include Using_comparator_arg with type t := t
  end) : S with type t := T.t and type comparator_witness = T.comparator_witness = struct
  module T = struct
    include T

    let compare = [%eta2 Comparator.compare T.comparator]
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
      Error
        (Error.create_s
           (Sexp.message
              "clamp requires [min <= max]"
              [ "min", T.sexp_of_t min; "max", T.sexp_of_t max ]))
    else Ok (clamp_unchecked t ~min ~max)
  ;;
end

module Make (T : sig
    type t [@@deriving compare, sexp_of]
  end) =
Make_using_comparator [@inlined hint] (struct
    include T
    include Comparator.Make (T)
  end)

module Inherit
    (C : sig
       type t [@@deriving compare]
     end)
    (T : sig
       type t [@@deriving sexp_of]

       val component : t -> C.t
     end) =
Make (struct
    type t = T.t [@@deriving sexp_of]

    let compare t t' = C.compare (T.component t) (T.component t') [@nontail]
  end)

type 'a reversed = 'a

(* compare [x] and [y] lexicographically using functions in the list [cmps] *)
let rec lexicographic cmps x y =
  match cmps with
  | cmp :: cmps ->
    let res = cmp x y in
    if res = 0 then lexicographic cmps x y else res
  | [] -> 0
;;

let lift cmp ~f x y = cmp (f x) (f y) [@nontail]
let reverse cmp x y = cmp y x
let compare_reversed cmp x y = cmp y x
