open! Import
open! Stdlib.Nativeint
module Sexp = Sexp0
include Nativeint_replace_polymorphic_compare

module T = struct
  type t = nativeint [@@deriving globalize, hash, sexp ~stackify, sexp_grammar]

  let hashable : t Hashable.t = { hash; compare; sexp_of_t }
  let compare = Nativeint_replace_polymorphic_compare.compare

  external format
    :  string
    -> local_ nativeint
    -> string
    @@ portable
    = "caml_nativeint_format"

  let to_string = Integer_to_string.nativeint_to_string

  external of_string
    :  local_ string
    -> (t[@unboxed])
    @@ portable
    = "caml_nativeint_of_string" "caml_nativeint_of_string_unboxed"

  let of_string_opt s =
    try Some (of_string s) with
    | Failure _ -> None
  ;;
end

include T

include%template Comparator.Make [@modality portable] (T)

include%template Comparable.With_zero [@modality portable] (struct
    include T

    let zero = zero
  end)

module Conv = Int_conversions
include Int_string_conversions.Make (T)

include Int_string_conversions.Make_hex (struct
    open Nativeint_replace_polymorphic_compare

    type t = nativeint [@@deriving compare ~localize, hash]

    let zero = zero

    external neg : local_ t -> t @@ portable = "%nativeint_neg"

    let ( < ) = ( < )
    let to_string i = format "%nx" i
    let of_string s = Stdlib.Scanf.sscanf s "%nx" Fn.id
    let module_name = "Base.Nativeint.Hex"
  end)

include%template Pretty_printer.Register [@modality portable] (struct
    type nonrec t = t

    let to_string = to_string
    let module_name = "Base.Nativeint"
  end)

(* Open replace_polymorphic_compare after including functor instantiations so they do not
   shadow its definitions. This is here so that efficient versions of the comparison
   functions are available within this module. *)
open! Nativeint_replace_polymorphic_compare

let invariant (_ : t) = ()
let num_bits = Word_size.num_bits Word_size.word_size |> of_int
let float_lower_bound = Float0.lower_bound_for_int (num_bits |> to_int)
let float_upper_bound = Float0.upper_bound_for_int (num_bits |> to_int)

external shift_right_logical : local_ t -> int -> t @@ portable = "%nativeint_lsr"
external shift_right : local_ t -> int -> t @@ portable = "%nativeint_asr"
external shift_left : local_ t -> int -> t @@ portable = "%nativeint_lsl"
external bit_xor : local_ t -> local_ t -> t @@ portable = "%nativeint_xor"
external bit_or : local_ t -> local_ t -> t @@ portable = "%nativeint_or"
external bit_and : local_ t -> local_ t -> t @@ portable = "%nativeint_and"

let bit_not n = bit_xor n (-1n)
let min_value = min_int
let max_value = max_int

external neg : local_ t -> t @@ portable = "%nativeint_neg"

let abs = abs
let abs_local n = if n >= 0n then n else neg n

external ( / ) : local_ t -> local_ t -> t @@ portable = "%nativeint_div"
external ( * ) : local_ t -> local_ t -> t @@ portable = "%nativeint_mul"
external ( - ) : local_ t -> local_ t -> t @@ portable = "%nativeint_sub"
external ( + ) : local_ t -> local_ t -> t @@ portable = "%nativeint_add"

let pred n = n - 1n
let succ n = n + 1n

external rem : local_ t -> local_ t -> t @@ portable = "%nativeint_mod"

let minus_one = minus_one
let one = one
let zero = zero

external to_float
  :  local_ t
  -> float
  @@ portable
  = "caml_nativeint_to_float" "caml_nativeint_to_float_unboxed"
[@@unboxed] [@@noalloc]

external of_float_unchecked
  :  local_ float
  -> t
  @@ portable
  = "caml_nativeint_of_float" "caml_nativeint_of_float_unboxed"
[@@unboxed] [@@noalloc]

let of_float f =
  if Float_replace_polymorphic_compare.( >= ) f float_lower_bound
     && Float_replace_polymorphic_compare.( <= ) f float_upper_bound
  then of_float_unchecked f
  else
    Printf.invalid_argf
      "Nativeint.of_float: argument (%f) is out of range or NaN"
      (Float0.box f)
      ()
;;

let of_int_exn = of_int
let to_int = Conv.nativeint_to_int
let to_int_exn = Conv.nativeint_to_int_exn
let to_int_trunc = Conv.nativeint_to_int_trunc
let ( ~- ) = neg
let pow b e = of_int_exn (Int_math.Private.int_pow (to_int_exn b) (to_int_exn e))
let ( ** ) b e = pow b e

module Pre_O = struct
  let ( + ) = ( + )
  let ( - ) = ( - )
  let ( * ) = ( * )
  let ( / ) = ( / )
  let ( ~- ) = ( ~- )
  let ( ** ) = ( ** )

  include Nativeint_replace_polymorphic_compare

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
  let raise_s = Error.raise_s

  let non_positive_argument () =
    Printf.invalid_argf "argument must be strictly positive" ()
  ;;

  external ( land ) : local_ t -> local_ t -> t @@ portable = "%nativeint_and"

  (** "ceiling power of 2" - Least power of 2 greater than or equal to x. *)
  let ceil_pow2 (local_ (x : nativeint)) =
    if x <= 0n then non_positive_argument ();
    let x = x - 1n in
    let x = x lor (x lsr 1) in
    let x = x lor (x lsr 2) in
    let x = x lor (x lsr 4) in
    let x = x lor (x lsr 8) in
    let x = x lor (x lsr 16) in
    (* The next line is superfluous on 32-bit architectures, but it's faster to do it
       anyway than to branch *)
    let x = x lor (x lsr 32) in
    x + 1n
  ;;

  (** "floor power of 2" - Largest power of 2 less than or equal to x. *)
  let floor_pow2 x =
    if x <= 0n then non_positive_argument ();
    let x = x lor (x lsr 1) in
    let x = x lor (x lsr 2) in
    let x = x lor (x lsr 4) in
    let x = x lor (x lsr 8) in
    let x = x lor (x lsr 16) in
    let x = x lor (x lsr 32) in
    x - (x lsr 1)
  ;;

  let is_pow2 x =
    if x <= 0n then non_positive_argument ();
    x land (x - 1n) = 0n
  ;;

  let clz = Ocaml_intrinsics_kernel.Nativeint.count_leading_zeros
  let ctz = Ocaml_intrinsics_kernel.Nativeint.count_trailing_zeros

  (** Hacker's Delight Second Edition p106 *)
  let floor_log2 i =
    if i <= 0n
    then
      raise_s
        (Sexp.message
           "[Nativeint.floor_log2] got invalid input"
           [ "", sexp_of_nativeint (globalize i) ]);
    num_bits - 1n - clz i
  ;;

  (** Hacker's Delight Second Edition p106 *)
  let ceil_log2 i =
    if i <= 0n
    then
      raise_s
        (Sexp.message
           "[Nativeint.ceil_log2] got invalid input"
           [ "", sexp_of_nativeint (globalize i) ]);
    if i = 1n
    then 0n
    else (
      let i = i - 1n in
      num_bits - clz i)
  ;;
end

include Pow2

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

let incr r = r := !r + one
let decr r = r := !r - one

external of_nativeint : (t[@local_opt]) -> (t[@local_opt]) @@ portable = "%identity"
external to_nativeint : (t[@local_opt]) -> (t[@local_opt]) @@ portable = "%identity"

let of_nativeint_exn = of_nativeint
let to_nativeint_exn = to_nativeint
let of_local_nativeint_exn = of_nativeint
let to_local_nativeint_exn = to_nativeint
let popcount = Popcount.nativeint_popcount

external of_int : int -> (t[@local_opt]) @@ portable = "%nativeint_of_int"
external of_int32 : local_ int32 -> (t[@local_opt]) @@ portable = "%nativeint_of_int32"

let of_int32_exn = of_int32
let of_local_int32_exn = of_int32
let to_int32 = Conv.nativeint_to_int32
let to_local_int32_exn = Conv.nativeint_to_int32_exn
let to_int32_exn t = globalize_int32 (to_local_int32_exn t) [@nontail]

external to_int32_trunc
  :  local_ t
  -> (int32[@local_opt])
  @@ portable
  = "%nativeint_to_int32"

let of_int64 = Conv.int64_to_nativeint
let of_local_int64_exn = Conv.int64_to_nativeint_exn
let of_int64_exn i = globalize (of_local_int64_exn i) [@nontail]

external of_int64_trunc
  :  local_ int64
  -> (nativeint[@local_opt])
  @@ portable
  = "%int64_to_nativeint"

let to_int64 = Conv.nativeint_to_int64
let to_local_int64 = Conv.nativeint_to_int64

include Int_string_conversions.Make_binary (struct
    type t = nativeint [@@deriving compare ~localize, equal ~localize, hash]

    let ( land ) = ( land )

    external ( lsr ) : local_ t -> int -> t @@ portable = "%nativeint_lsr"

    let clz = clz
    let num_bits = num_bits
    let one = one
    let to_int_trunc = to_int_trunc
    let zero = zero
    let ( - ) = ( - )
  end)

(* [Nativeint] and [Nativeint.O] agree value-wise *)

module Summable = struct
  type nonrec t = t

  let zero = zero
  let[@inline] ( + ) x y = x + y
  let[@inline] ( - ) x y = x - y
end

(* Include type-specific [Replace_polymorphic_compare] at the end, after including functor
   application that could shadow its definitions. This is here so that efficient versions
   of the comparison functions are exported by this module. *)
include Nativeint_replace_polymorphic_compare

external bswap : local_ t -> (t[@local_opt]) @@ portable = "%bswap_native"
