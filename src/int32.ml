open! Import
open! Stdlib.Int32
module Sexp = Sexp0

module T = struct
  type t = int32 [@@deriving globalize, hash, sexp ~stackify, sexp_grammar]

  let hashable : t Hashable.t = { hash; compare; sexp_of_t }
  let compare (x : t) y = compare x y

  external format : string -> local_ int32 -> string @@ portable = "caml_int32_format"

  let to_string = Integer_to_string.int32_to_string

  external of_string
    :  local_ string
    -> (t[@unboxed])
    @@ portable
    = "caml_int32_of_string" "caml_int32_of_string_unboxed"

  let of_string_opt s =
    try Some (of_string s) with
    | Failure _ -> None
  ;;
end

include T

include%template Comparator.Make [@modality portable] (T)

let num_bits = 32l
let float_lower_bound = Float0.lower_bound_for_int (num_bits |> to_int)
let float_upper_bound = Float0.upper_bound_for_int (num_bits |> to_int)

external float_of_bits
  :  local_ t
  -> float
  @@ portable
  = "caml_int32_float_of_bits" "caml_int32_float_of_bits_unboxed"
[@@unboxed] [@@noalloc]

external bits_of_float
  :  local_ float
  -> t
  @@ portable
  = "caml_int32_bits_of_float" "caml_int32_bits_of_float_unboxed"
[@@unboxed] [@@noalloc]

external shift_right_logical : local_ t -> int -> t @@ portable = "%int32_lsr"
external shift_right : local_ t -> int -> t @@ portable = "%int32_asr"
external shift_left : local_ t -> int -> t @@ portable = "%int32_lsl"
external bit_xor : local_ t -> local_ t -> t @@ portable = "%int32_xor"
external bit_or : local_ t -> local_ t -> t @@ portable = "%int32_or"
external bit_and : local_ t -> local_ t -> t @@ portable = "%int32_and"

let bit_not n = bit_xor n (-1l)
let min_value = min_int
let max_value = max_int
let abs = abs

let abs_local n = exclave_
  if Int32_replace_polymorphic_compare.(n >= 0l) then n else neg n
;;

external ( / ) : local_ t -> local_ t -> t @@ portable = "%int32_div"
external ( * ) : local_ t -> local_ t -> t @@ portable = "%int32_mul"
external ( - ) : local_ t -> local_ t -> t @@ portable = "%int32_sub"
external ( + ) : local_ t -> local_ t -> t @@ portable = "%int32_add"

let pred n = n - 1l
let succ n = n + 1l

external rem : local_ t -> local_ t -> t @@ portable = "%int32_mod"
external neg : local_ t -> t @@ portable = "%int32_neg"

let minus_one = minus_one
let one = one
let zero = zero

external to_float
  :  local_ int32
  -> float
  @@ portable
  = "caml_int32_to_float" "caml_int32_to_float_unboxed"
[@@unboxed] [@@noalloc]

external of_float_unchecked
  :  local_ float
  -> int32
  @@ portable
  = "caml_int32_of_float" "caml_int32_of_float_unboxed"
[@@unboxed] [@@noalloc]

let of_float f =
  if Float_replace_polymorphic_compare.( >= ) f float_lower_bound
     && Float_replace_polymorphic_compare.( <= ) f float_upper_bound
  then of_float_unchecked f
  else
    Printf.invalid_argf
      "Int32.of_float: argument (%f) is out of range or NaN"
      (Float0.box f)
      ()
;;

include%template Comparable.With_zero [@modality portable] (struct
    include T

    let zero = zero
  end)

module Compare = struct
  include Int32_replace_polymorphic_compare

  let ascending = compare
  let descending x y = compare y x
  let min x y = Bool0.select (x <= y) x y
  let max x y = Bool0.select (x >= y) x y
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
end

include Compare

let invariant (_ : t) = ()
let ( ~- ) = neg
let incr r = r := !r + one
let decr r = r := !r - one

external of_int32 : (int32[@local_opt]) -> (t[@local_opt]) @@ portable = "%identity"
external to_int32 : (t[@local_opt]) -> (int32[@local_opt]) @@ portable = "%identity"

let of_int32_exn t = t
let to_int32_exn t = t
let of_local_int32_exn t = t
let to_local_int32_exn t = t
let popcount = Popcount.int32_popcount

module Conv = Int_conversions

let of_int = Conv.int_to_int32
let of_local_int_exn = Conv.int_to_int32_exn
let of_int_exn i = globalize (of_local_int_exn i) [@nontail]

external of_int_trunc : int -> (t[@local_opt]) @@ portable = "%int32_of_int"

let to_int = Conv.int32_to_int
let to_int_exn = Conv.int32_to_int_exn
let to_int_trunc = Conv.int32_to_int_trunc
let of_int64 = Conv.int64_to_int32
let of_local_int64_exn (local_ i) = exclave_ Conv.int64_to_int32_exn i
let of_int64_exn i = globalize (of_local_int64_exn i) [@nontail]

external of_int64_trunc : local_ int64 -> (t[@local_opt]) @@ portable = "%int64_to_int32"

let to_int64 = Conv.int32_to_int64
let to_local_int64 = Conv.int32_to_int64
let of_nativeint = Conv.nativeint_to_int32
let of_local_nativeint_exn = Conv.nativeint_to_int32_exn
let of_nativeint_exn i = globalize (of_local_nativeint_exn i) [@nontail]

external of_nativeint_trunc
  :  local_ nativeint
  -> (t[@local_opt])
  @@ portable
  = "%nativeint_to_int32"

external to_nativeint
  :  local_ t
  -> (nativeint[@local_opt])
  @@ portable
  = "%nativeint_of_int32"

let to_nativeint_exn = to_nativeint
let to_local_nativeint_exn = to_nativeint
let pow b e = of_int_exn (Int_math.Private.int_pow (to_int_exn b) (to_int_exn e))
let ( ** ) b e = pow b e

external bswap32 : local_ t -> (t[@local_opt]) @@ portable = "%bswap_int32"

let bswap16 x = shift_right_logical (bswap32 x) 16

module Pre_O = struct
  let ( + ) = ( + )
  let ( - ) = ( - )
  let ( * ) = ( * )
  let ( / ) = ( / )
  let ( ~- ) = ( ~- )
  let ( ** ) = ( ** )

  include Compare

  let abs = abs
  let abs_local = abs_local
  let neg = neg
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

  let ( land ) = bit_and
  let ( lor ) = bit_or
  let ( lxor ) = bit_xor
  let lnot = bit_not
  let ( lsl ) = shift_left
  let ( asr ) = shift_right
  let ( lsr ) = shift_right_logical
end

include O

module Pow2 = struct
  open O

  let raise_s = Error.raise_s

  let non_positive_argument () =
    Printf.invalid_argf "argument must be strictly positive" ()
  ;;

  (** "ceiling power of 2" - Least power of 2 greater than or equal to x. *)
  let ceil_pow2 x =
    if x <= 0l then non_positive_argument ();
    let x = x - 1l in
    let x = x lor (x lsr 1) in
    let x = x lor (x lsr 2) in
    let x = x lor (x lsr 4) in
    let x = x lor (x lsr 8) in
    let x = x lor (x lsr 16) in
    x + 1l
  ;;

  (** "floor power of 2" - Largest power of 2 less than or equal to x. *)
  let floor_pow2 x =
    if x <= 0l then non_positive_argument ();
    let x = x lor (x lsr 1) in
    let x = x lor (x lsr 2) in
    let x = x lor (x lsr 4) in
    let x = x lor (x lsr 8) in
    let x = x lor (x lsr 16) in
    x - (x lsr 1)
  ;;

  let is_pow2 x =
    if x <= 0l then non_positive_argument ();
    x land (x - 1l) = 0l
  ;;

  let clz = Ocaml_intrinsics_kernel.Int32.count_leading_zeros
  let ctz = Ocaml_intrinsics_kernel.Int32.count_trailing_zeros

  (** Hacker's Delight Second Edition p106 *)
  let floor_log2 i =
    if i <= 0l
    then
      raise_s
        (Sexp.message
           "[Int32.floor_log2] got invalid input"
           [ "", sexp_of_int32 (globalize i) ]);
    num_bits - 1l - clz i
  ;;

  (** Hacker's Delight Second Edition p106 *)
  let ceil_log2 i =
    if i <= 0l
    then
      raise_s
        (Sexp.message
           "[Int32.ceil_log2] got invalid input"
           [ "", sexp_of_int32 (globalize i) ]);
    (* The [i = 1] check is needed because clz(0) is undefined *)
    if i = 1l
    then 0l
    else (
      let i = i - 1l in
      num_bits - clz i)
  ;;
end

include Pow2
include Int_string_conversions.Make (T)

include Int_string_conversions.Make_hex (struct
    type t = int32 [@@deriving compare ~localize, hash]

    let zero = zero

    external neg : local_ t -> t @@ portable = "%int32_neg"

    let ( < ) = ( < )
    let to_string i = format "%lx" i
    let of_string s = Stdlib.Scanf.sscanf s "%lx" Fn.id
    let module_name = "Base.Int32.Hex"
  end)

include Int_string_conversions.Make_binary (struct
    type t = int32 [@@deriving compare ~localize, equal ~localize, hash]

    let ( land ) = ( land )

    external ( lsr ) : local_ t -> int -> t @@ portable = "%int32_lsr"

    let clz = clz
    let num_bits = num_bits
    let one = one
    let to_int_trunc = to_int_trunc
    let zero = zero
    let ( - ) = ( - )
  end)

include%template Pretty_printer.Register [@modality portable] (struct
    type nonrec t = t

    let to_string = to_string
    let module_name = "Base.Int32"
  end)

(* [Int32] and [Int32.O] agree value-wise *)

module Summable = struct
  type nonrec t = t

  let zero = zero
  let[@inline] ( + ) x y = x + y
  let[@inline] ( - ) x y = x - y
end
