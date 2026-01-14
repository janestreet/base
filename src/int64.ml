open! Import
open! Stdlib.Int64
module Sexp = Sexp0

module T = struct
  type t = int64 [@@deriving globalize, hash, sexp ~stackify, sexp_grammar]

  let hashable : t Hashable.t = { hash; compare; sexp_of_t }
  let compare = Int64_replace_polymorphic_compare.compare

  external format : string -> local_ int64 -> string @@ portable = "caml_int64_format"

  let to_string = Integer_to_string.int64_to_string

  external of_string
    :  local_ string
    -> (int64[@unboxed])
    @@ portable
    = "caml_int64_of_string" "caml_int64_of_string_unboxed"

  let of_string_opt s =
    try Some (of_string s) with
    | Failure _ -> None
  ;;
end

include T

include%template Comparator.Make [@modality portable] (T)

let num_bits = 64
let float_lower_bound = Float0.lower_bound_for_int num_bits
let float_upper_bound = Float0.upper_bound_for_int num_bits
let num_bits = of_int num_bits

external float_of_bits
  :  local_ t
  -> float
  @@ portable
  = "caml_int64_float_of_bits" "caml_int64_float_of_bits_unboxed"
[@@unboxed] [@@noalloc]

external bits_of_float
  :  local_ float
  -> t
  @@ portable
  = "caml_int64_bits_of_float" "caml_int64_bits_of_float_unboxed"
[@@unboxed] [@@noalloc]

external shift_right_logical : local_ t -> int -> t @@ portable = "%int64_lsr"
external shift_right : local_ t -> int -> t @@ portable = "%int64_asr"
external shift_left : local_ t -> int -> t @@ portable = "%int64_lsl"
external bit_xor : local_ t -> local_ t -> t @@ portable = "%int64_xor"
external bit_or : local_ t -> local_ t -> t @@ portable = "%int64_or"
external bit_and : local_ t -> local_ t -> t @@ portable = "%int64_and"

let bit_not n = bit_xor n (-1L)
let min_value = min_int
let max_value = max_int
let abs = abs

let abs_local n = exclave_
  if Int64_replace_polymorphic_compare.(n >= 0L) then n else neg n
;;

let pow = Int_math.Private.int64_pow

external rem : local_ t -> local_ t -> t @@ portable = "%int64_mod"

let minus_one = minus_one
let one = one
let zero = zero

external to_float
  :  local_ t
  -> float
  @@ portable
  = "caml_int64_to_float" "caml_int64_to_float_unboxed"
[@@unboxed] [@@noalloc]

external of_float_unchecked
  :  local_ float
  -> t
  @@ portable
  = "caml_int64_of_float" "caml_int64_of_float_unboxed"
[@@unboxed] [@@noalloc]

let of_float f =
  if Float_replace_polymorphic_compare.( >= ) f float_lower_bound
     && Float_replace_polymorphic_compare.( <= ) f float_upper_bound
  then of_float_unchecked f
  else
    Printf.invalid_argf
      "Int64.of_float: argument (%f) is out of range or NaN"
      (Float0.box f)
      ()
;;

(* Not eta-expanding here can lead to less allocations: the function call sites can avoid
   boxing the int64s more often. *)
let ( ** ) = pow

external bswap64 : local_ t -> (t[@local_opt]) @@ portable = "%bswap_int64"

let[@inline always] bswap16 x = shift_right_logical (bswap64 x) 48

let[@inline always] bswap32 x =
  (* This is strictly better than coercing to an int32 to perform byteswap. Coercing from
     an int32 will add unnecessary shift operations to sign extend the number
     appropriately.
  *)
  shift_right_logical (bswap64 x) 32
;;

let[@inline always] bswap48 x = shift_right_logical (bswap64 x) 16

include%template Comparable.With_zero [@modality portable] (struct
    include T

    let zero = zero
  end)

(* Open replace_polymorphic_compare after including functor instantiations so they do not
   shadow its definitions. This is here so that efficient versions of the comparison
   functions are available within this module. *)
open Int64_replace_polymorphic_compare

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

let incr r = r := add !r one
let decr r = r := sub !r one

external of_int64 : (t[@local_opt]) -> (t[@local_opt]) @@ portable = "%identity"

let to_int64 t = t
let of_int64_exn t = t
let to_local_int64 t = t
let of_local_int64_exn t = t
let popcount = Popcount.int64_popcount

module Conv = Int_conversions

external to_int_trunc : local_ t -> int @@ portable = "%int64_to_int"
external to_int32_trunc : local_ t -> (int32[@local_opt]) @@ portable = "%int64_to_int32"

external to_nativeint_trunc
  :  local_ t
  -> (nativeint[@local_opt])
  @@ portable
  = "%int64_to_nativeint"

external of_int : int -> (t[@local_opt]) @@ portable = "%int64_of_int"
external of_int32 : local_ int32 -> (t[@local_opt]) @@ portable = "%int64_of_int32"

let of_int_exn = of_int
let to_int = Conv.int64_to_int
let to_int_exn = Conv.int64_to_int_exn
let of_int32_exn = of_int32
let of_local_int32_exn = of_int32
let to_int32 = Conv.int64_to_int32
let to_local_int32_exn = Conv.int64_to_int32_exn
let to_int32_exn t = globalize_int32 (to_local_int32_exn t) [@nontail]

external of_nativeint
  :  local_ nativeint
  -> (t[@local_opt])
  @@ portable
  = "%int64_of_nativeint"

let of_nativeint_exn = of_nativeint
let of_local_nativeint_exn = of_nativeint
let to_nativeint = Conv.int64_to_nativeint
let to_local_nativeint_exn = Conv.int64_to_nativeint_exn
let to_nativeint_exn t = globalize_nativeint (to_local_nativeint_exn t) [@nontail]

module Pre_O = struct
  external ( + ) : local_ t -> local_ t -> (t[@local_opt]) @@ portable = "%int64_add"
  external ( - ) : local_ t -> local_ t -> (t[@local_opt]) @@ portable = "%int64_sub"
  external ( * ) : local_ t -> local_ t -> (t[@local_opt]) @@ portable = "%int64_mul"
  external ( / ) : local_ t -> local_ t -> (t[@local_opt]) @@ portable = "%int64_div"
  external ( ~- ) : local_ t -> (t[@local_opt]) @@ portable = "%int64_neg"

  let ( ** ) = ( ** )

  include Int64_replace_polymorphic_compare

  let abs = abs
  let abs_local = abs_local

  external neg : local_ t -> (t[@local_opt]) @@ portable = "%int64_neg"

  let zero = zero
  let of_int_exn = of_int_exn
end

module O = struct
  include Pre_O

  include Int_math.Make (struct
      type nonrec t = t

      let globalize = globalize

      include Pre_O

      let rem = rem
      let to_float = to_float
      let of_float = of_float
      let of_string = T.of_string
      let to_string = T.to_string
    end)

  external ( land ) : local_ t -> local_ t -> (t[@local_opt]) @@ portable = "%int64_and"
  external ( lor ) : local_ t -> local_ t -> (t[@local_opt]) @@ portable = "%int64_or"
  external ( lxor ) : local_ t -> local_ t -> (t[@local_opt]) @@ portable = "%int64_xor"

  let lnot = bit_not

  external ( lsl ) : local_ t -> int -> (t[@local_opt]) @@ portable = "%int64_lsl"
  external ( asr ) : local_ t -> local_ int -> (t[@local_opt]) @@ portable = "%int64_asr"
  external ( lsr ) : local_ t -> local_ int -> (t[@local_opt]) @@ portable = "%int64_lsr"
end

include O

let pred n = n - 1L
let succ n = n + 1L

module Pow2 = struct
  open O

  let raise_s = Error.raise_s

  let non_positive_argument () =
    Printf.invalid_argf "argument must be strictly positive" ()
  ;;

  (** "ceiling power of 2" - Least power of 2 greater than or equal to x. *)
  let ceil_pow2 x =
    if x <= 0L then non_positive_argument ();
    let x = x - 1L in
    let x = x lor (x lsr 1) in
    let x = x lor (x lsr 2) in
    let x = x lor (x lsr 4) in
    let x = x lor (x lsr 8) in
    let x = x lor (x lsr 16) in
    let x = x lor (x lsr 32) in
    x + 1L
  ;;

  (** "floor power of 2" - Largest power of 2 less than or equal to x. *)
  let floor_pow2 x =
    if x <= 0L then non_positive_argument ();
    let x = x lor (x lsr 1) in
    let x = x lor (x lsr 2) in
    let x = x lor (x lsr 4) in
    let x = x lor (x lsr 8) in
    let x = x lor (x lsr 16) in
    let x = x lor (x lsr 32) in
    x - (x lsr 1)
  ;;

  let is_pow2 x =
    if x <= 0L then non_positive_argument ();
    x land (x - 1L) = 0L
  ;;

  let clz = Ocaml_intrinsics_kernel.Int64.count_leading_zeros
  let ctz = Ocaml_intrinsics_kernel.Int64.count_trailing_zeros

  (** Hacker's Delight Second Edition p106 *)
  let floor_log2 i =
    if i <= 0L
    then
      raise_s
        (Sexp.message
           "[Int64.floor_log2] got invalid input"
           [ "", sexp_of_int64 (globalize i) ]);
    num_bits - 1L - clz i
  ;;

  (** Hacker's Delight Second Edition p106 *)
  let ceil_log2 i =
    if i <= 0L
    then
      raise_s
        (Sexp.message
           "[Int64.ceil_log2] got invalid input"
           [ "", sexp_of_int64 (globalize i) ]);
    if i = 1L
    then 0L
    else (
      let i = i - 1L in
      num_bits - clz i)
  ;;
end

include Pow2
include Int_string_conversions.Make (T)

include Int_string_conversions.Make_hex (struct
    type t = int64 [@@deriving compare ~localize, hash]

    let zero = zero
    let neg = neg
    let ( < ) = ( < )
    let to_string i = format "%Lx" i
    let of_string s = Stdlib.Scanf.sscanf s "%Lx" Fn.id
    let module_name = "Base.Int64.Hex"
  end)

include Int_string_conversions.Make_binary (struct
    type t = int64 [@@deriving compare ~localize, equal ~localize, hash]

    let ( land ) = ( land )
    let ( lsr ) = ( lsr )
    let clz = clz
    let num_bits = num_bits
    let one = one
    let to_int_trunc = to_int_trunc
    let zero = zero
    let ( - ) = O.( - )
  end)

include%template Pretty_printer.Register [@modality portable] (struct
    type nonrec t = t

    let to_string = to_string
    let module_name = "Base.Int64"
  end)

(* [Int64] and [Int64.O] agree value-wise *)

module Summable = struct
  type nonrec t = t

  let zero = zero
  let[@inline] ( + ) x y = x + y
  let[@inline] ( - ) x y = x - y
end

(* Include type-specific [Replace_polymorphic_compare] at the end, after including functor
   application that could shadow its definitions. This is here so that efficient versions
   of the comparison functions are exported by this module. *)
include Int64_replace_polymorphic_compare
