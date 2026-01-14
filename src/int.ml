open! Import
module Sexp = Sexp0
include Int_intf.Definitions
include Int0

module T = struct
  type t = int [@@deriving globalize, hash, sexp ~stackify, sexp_grammar]

  let hashable : t Hashable.t = { hash; compare; sexp_of_t }
  let compare x y = Int_replace_polymorphic_compare.compare x y

  let of_string s =
    try of_string s with
    | _ -> Printf.failwithf "Int.of_string: %S" (globalize_string s) ()
  ;;

  let to_string = to_string
end

let num_bits = Int_conversions.num_bits_int
let float_lower_bound = Float0.lower_bound_for_int num_bits
let float_upper_bound = Float0.upper_bound_for_int num_bits
let to_float = (Stdlib.float_of_int :> local_ int -> _)
let of_float_unchecked = Stdlib.int_of_float

let of_float f =
  if Float_replace_polymorphic_compare.( >= ) f float_lower_bound
     && Float_replace_polymorphic_compare.( <= ) f float_upper_bound
  then Stdlib.int_of_float f
  else
    Printf.invalid_argf
      "Int.of_float: argument (%f) is out of range or NaN"
      (Float0.box f)
      ()
;;

let zero = 0
let one = 1
let minus_one = -1

include T

include%template Comparator.Make [@modality portable] (T)

include%template Comparable.With_zero [@modality portable] (struct
    include T

    let zero = zero
  end)

module Conv = Int_conversions
include Int_string_conversions.Make (T)

include Int_string_conversions.Make_hex (struct
    open Int_replace_polymorphic_compare

    type t = int [@@deriving compare ~localize, hash]

    let zero = zero

    external neg : local_ t -> t @@ portable = "%negint"

    let ( < ) = ( < )
    let to_string i = format "%x" i
    let of_string s = Stdlib.Scanf.sscanf s "%x" Fn.id
    let module_name = "Base.Int.Hex"
  end)

include%template Pretty_printer.Register [@modality portable] (struct
    type nonrec t = t

    let to_string = to_string
    let module_name = "Base.Int"
  end)

(* Open replace_polymorphic_compare after including functor instantiations so they do not
   shadow its definitions. This is here so that efficient versions of the comparison
   functions are available within this module. *)
open! Int_replace_polymorphic_compare

let invariant (_ : t) = ()
let between t ~low ~high = low <= t && t <= high
let clamp_unchecked t ~min:min_ ~max:max_ = min t max_ |> max min_

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

external to_int32_trunc
  :  (t[@local_opt])
  -> (int32[@local_opt])
  @@ portable
  = "%int32_of_int"

external of_int32_trunc : (int32[@local_opt]) -> t @@ portable = "%int32_to_int"
external of_int64_trunc : (int64[@local_opt]) -> t @@ portable = "%int64_to_int"

external of_nativeint_trunc
  :  (nativeint[@local_opt])
  -> t
  @@ portable
  = "%nativeint_to_int"

let pred i = i - 1
let succ i = i + 1
let to_int i = i
let to_int_exn = to_int
let of_int i = i
let of_int_exn = of_int
let max_value = Stdlib.max_int
let min_value = Stdlib.min_int
let max_value_30_bits = 0x3FFF_FFFF
let of_int32 = Conv.int32_to_int
let of_int32_exn = Conv.int32_to_int_exn
let of_local_int32_exn = Conv.int32_to_int_exn
let to_int32 = Conv.int_to_int32
let to_local_int32_exn = Conv.int_to_int32_exn
let to_int32_exn i = globalize_int32 (to_local_int32_exn i) [@nontail]
let of_int64 = Conv.int64_to_int
let of_int64_exn = Conv.int64_to_int_exn
let of_local_int64_exn = Conv.int64_to_int_exn
let to_int64 = Conv.int_to_int64
let to_local_int64 = Conv.int_to_int64
let of_nativeint = Conv.nativeint_to_int
let of_nativeint_exn = Conv.nativeint_to_int_exn
let of_local_nativeint_exn = Conv.nativeint_to_int_exn

external to_nativeint : int -> (nativeint[@local_opt]) @@ portable = "%nativeint_of_int"

let to_nativeint_exn = to_nativeint
let to_local_nativeint_exn = to_nativeint
let abs x = abs x
let abs_local = (abs : local_ int -> local_ int)

(* note that rem is not same as % *)
let rem a b = a mod b
let incr = Stdlib.incr
let decr = Stdlib.decr
let shift_right a b = a asr b
let shift_right_logical a b = a lsr b
let shift_left a b = a lsl b
let bit_not a = lnot a
let bit_or a b = a lor b
let bit_and a b = a land b
let bit_xor a b = a lxor b
let pow = Int_math.Private.int_pow
let ( ** ) b e = pow b e

module Pow2 = struct
  open! Import

  let raise_s = Error.raise_s

  let non_positive_argument () =
    Printf.invalid_argf "argument must be strictly positive" ()
  ;;

  (** "ceiling power of 2 - 1" - Least power of 2 minus 1 greater than or equal to x. *)
  let ceil_pow2_minus_1 n =
    (* This does not let [Nativeint]s escape and allocations are optimized away.
       [Stdlib.Nativeint] is used to avoid a dependency cycle. *)
    let open Stdlib.Nativeint in
    let n = of_int n in
    let n = logor n (shift_right_logical n 1) in
    let n = logor n (shift_right_logical n 2) in
    let n = logor n (shift_right_logical n 4) in
    let n = logor n (shift_right_logical n 8) in
    let n = logor n (shift_right_logical n 16) in
    to_int
      (if (* This condition should get constant folded and there should be no branch. *)
          Stdlib.Sys.int_size > 32
       then logor n (shift_right_logical n 32)
       else n)
  ;;

  (** "ceiling power of 2" - Least power of 2 greater than or equal to x. *)
  let ceil_pow2 x =
    if x <= 0 then non_positive_argument ();
    let x = x - 1 in
    let x = ceil_pow2_minus_1 x in
    x + 1
  ;;

  (** "floor power of 2" - Largest power of 2 less than or equal to x. *)
  let floor_pow2 x =
    if x <= 0 then non_positive_argument ();
    let x = ceil_pow2_minus_1 x in
    x - (x lsr 1)
  ;;

  let is_pow2 x =
    if x <= 0 then non_positive_argument ();
    x land (x - 1) = 0
  ;;

  let clz = Ocaml_intrinsics_kernel.Int.count_leading_zeros
  let ctz = Ocaml_intrinsics_kernel.Int.count_trailing_zeros

  let[@cold] log2_bad_input i =
    raise_s (Sexp.message "[Int.floor_log2] got invalid input" [ "", sexp_of_int i ])
  ;;

  (** Hacker's Delight Second Edition p106 *)
  let floor_log2 i = if i <= 0 then log2_bad_input i else num_bits - 1 - clz i

  let[@cold] log2_bad_input i =
    raise_s (Sexp.message "[Int.ceil_log2] got invalid input" [ "", sexp_of_int i ])
  ;;

  let ceil_log2 i =
    if i <= 0 then log2_bad_input i;
    if i = 1 then 0 else num_bits - clz (i - 1)
  ;;
end

include Pow2

let sign = Sign.of_int
let popcount = Popcount.int_popcount

include Int_string_conversions.Make_binary (struct
    type t = int [@@deriving compare ~localize, equal ~localize, hash]

    let ( land ) = ( land )

    external ( lsr ) : local_ t -> int -> t @@ portable = "%lsrint"

    let clz = clz
    let num_bits = num_bits
    let one = one
    let to_int_trunc (i : t) = i
    let zero = zero
    let ( - ) = ( - )
  end)

module Pre_O = struct
  external ( + ) : (t[@local_opt]) -> (t[@local_opt]) -> t @@ portable = "%addint"
  external ( - ) : (t[@local_opt]) -> (t[@local_opt]) -> t @@ portable = "%subint"
  external ( * ) : (t[@local_opt]) -> (t[@local_opt]) -> t @@ portable = "%mulint"
  external ( / ) : (t[@local_opt]) -> (t[@local_opt]) -> t @@ portable = "%divint"
  external ( ~- ) : (t[@local_opt]) -> t @@ portable = "%negint"

  let ( ** ) = ( ** )

  include Int_replace_polymorphic_compare

  let abs = abs
  let abs_local = abs_local

  external neg : (t[@local_opt]) -> t @@ portable = "%negint"

  let zero = zero
  let of_int_exn = of_int_exn
end

module O = struct
  include Pre_O

  module F = Int_math.Make (struct
      type nonrec t = t

      let globalize = globalize

      include Pre_O

      let rem = rem
      let to_float = to_float
      let of_float = of_float
      let of_string = T.of_string
      let to_string = T.to_string
    end)

  include F

  external bswap16 : (int[@local_opt]) -> int @@ portable = "%bswap16"

  (* These inlined versions of (%), (/%), and (//) perform better than their functorized
     counterparts in [F] (see benchmarks below).

     The reason these functions are inlined in [Int] but not in any of the other integer
     modules is that they existed in [Int] and [Int] alone prior to the introduction of
     the [Int_math.Make] functor, and we didn't want to degrade their performance.

     We won't pre-emptively do the same for new functions, unless someone cares, on a case
     by case fashion. *)

  let ( % ) x y =
    if y <= zero
    then
      Printf.invalid_argf
        "%s %% %s in core_int.ml: modulus should be positive"
        (to_string x)
        (to_string y)
        ();
    let rval = rem x y in
    if rval < zero then rval + y else rval
  ;;

  let ( /% ) x y =
    if y <= zero
    then
      Printf.invalid_argf
        "%s /%% %s in core_int.ml: divisor should be positive"
        (to_string x)
        (to_string y)
        ();
    if x < zero then ((x + one) / y) - one else x / y
  ;;

  let ( // ) x y = to_float x /. to_float y

  external ( land )
    :  (int[@local_opt])
    -> (int[@local_opt])
    -> int
    @@ portable
    = "%andint"

  external ( lor ) : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%orint"

  external ( lxor )
    :  (int[@local_opt])
    -> (int[@local_opt])
    -> int
    @@ portable
    = "%xorint"

  let lnot x = x lxor -1

  external ( lsl ) : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%lslint"
  external ( lsr ) : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%lsrint"
  external ( asr ) : (int[@local_opt]) -> (int[@local_opt]) -> int @@ portable = "%asrint"
end

include O

(* [Int] and [Int.O] agree value-wise *)

module Summable = struct
  type nonrec t = t

  let zero = zero
  let[@inline] ( + ) x y = x + y
  let[@inline] ( - ) x y = x - y
end

module Private = struct
  module O_F = O.F
end

(* Include type-specific [Replace_polymorphic_compare] at the end, after including functor
   application that could shadow its definitions. This is here so that efficient versions
   of the comparison functions are exported by this module. *)
include Int_replace_polymorphic_compare
