open! Import

let invalid_argf = Printf.invalid_argf
let negative_exponent () = Printf.invalid_argf "exponent can not be negative" ()
let overflow () = Printf.invalid_argf "integer overflow in pow" ()

(* To implement [int64_pow], we use C code rather than OCaml to eliminate allocation. *)
external int_math_int_pow : int -> int -> int @@ portable = "Base_int_math_int_pow_stub"
[@@noalloc]

external int_math_int64_pow
  :  local_ int64
  -> local_ int64
  -> int64
  @@ portable
  = "Base_int_math_int64_pow_stub" "Base_int_math_int64_pow_stub_unboxed"
[@@unboxed] [@@noalloc]

let int_pow base exponent =
  if exponent < 0 then negative_exponent ();
  if abs base > 1
     && (exponent > 63
         || abs base
            > (Portability_hacks.Cross.Contended.(cross (iarray infer))
                 Pow_overflow_bounds.int_positive_overflow_bounds).:(exponent))
  then overflow ();
  int_math_int_pow base exponent
;;

module Int64_with_comparisons = struct
  include Stdlib.Int64

  external ( < )
    :  (int64[@local_opt])
    -> (int64[@local_opt])
    -> bool
    @@ portable
    = "%lessthan"

  external ( > )
    :  (int64[@local_opt])
    -> (int64[@local_opt])
    -> bool
    @@ portable
    = "%greaterthan"

  external ( >= )
    :  (int64[@local_opt])
    -> (int64[@local_opt])
    -> bool
    @@ portable
    = "%greaterequal"

  external neg : (int64[@local_opt]) -> (int64[@local_opt]) @@ portable = "%int64_neg"

  let abs_local n = exclave_ if n >= 0L then n else neg n
end

(* we don't do [abs] in int64 case to avoid allocation *)
let int64_pow base exponent =
  let open Int64_with_comparisons in
  if exponent < 0L then negative_exponent ();
  if (base > 1L || base < -1L)
     && (exponent > 63L
         || (base >= 0L
             && base
                > (Portability_hacks.Cross.Contended.(cross (iarray infer))
                     Pow_overflow_bounds.int64_positive_overflow_bounds).:(to_int exponent)
            )
         || (base < 0L
             && base
                < (Portability_hacks.Cross.Contended.(cross (iarray infer))
                     Pow_overflow_bounds.int64_negative_overflow_bounds).:(to_int exponent)
            ))
  then overflow ();
  int_math_int64_pow base exponent
;;

let int63_pow_on_int64 base exponent =
  let open Int64_with_comparisons in
  if exponent < 0L then negative_exponent ();
  if abs_local base > 1L
     && (exponent > 63L
         || abs_local base
            > (Portability_hacks.Cross.Contended.(cross (iarray infer))
                 Pow_overflow_bounds.int63_on_int64_positive_overflow_bounds).:(to_int
                                                                                  exponent)
        )
  then overflow ();
  int_math_int64_pow base exponent
;;

module type Make_arg = sig @@ portable
  type t : value mod contended portable

  val globalize : local_ t -> t

  include Floatable.S_local_input with type t := t
  include Stringable.S with type t := t

  val ( + ) : local_ t -> local_ t -> t
  val ( - ) : local_ t -> local_ t -> t
  val ( * ) : local_ t -> local_ t -> t
  val ( / ) : local_ t -> local_ t -> t
  val ( ~- ) : local_ t -> t
  val ( <> ) : local_ t -> local_ t -> bool
  val ( <= ) : local_ t -> local_ t -> bool
  val ( >= ) : local_ t -> local_ t -> bool
  val ( = ) : local_ t -> local_ t -> bool
  val ( < ) : local_ t -> local_ t -> bool
  val ( > ) : local_ t -> local_ t -> bool
  val abs : t -> t
  val neg : t -> t
  val zero : t
  val of_int_exn : int -> t
  val rem : local_ t -> local_ t -> t
end

module Make (X : Make_arg) = struct
  open X

  let ( % ) x y =
    if y <= zero
    then
      invalid_argf
        "%s %% %s in core_int.ml: modulus should be positive"
        (to_string (globalize x))
        (to_string (globalize y))
        ();
    let rval = X.rem x y in
    if rval < zero then rval + y else rval
  ;;

  let one = of_int_exn 1

  let ( /% ) x y =
    if y <= zero
    then
      invalid_argf
        "%s /%% %s in core_int.ml: divisor should be positive"
        (to_string (globalize x))
        (to_string (globalize y))
        ();
    if x < zero then ((x + one) / y) - one else x / y
  ;;

  (** float division of integers *)
  let ( // ) x y = to_float x /. to_float y

  let round_down i ~to_multiple_of:modulus = i - (i % modulus)

  let round_up i ~to_multiple_of:modulus =
    let remainder = i % modulus in
    if remainder = zero
    then
      (* [+ zero] is essentially [globalize], but we suspect the compiler can optimize it
         away more readily *)
      i + zero
    else i + modulus - remainder
  ;;

  let round_towards_zero i ~to_multiple_of =
    if i = zero
    then zero
    else if i > zero
    then round_down i ~to_multiple_of
    else round_up i ~to_multiple_of
  ;;

  let round_nearest i ~to_multiple_of:modulus =
    let remainder = i % modulus in
    let modulus_minus_remainder = modulus - remainder in
    if modulus_minus_remainder <= remainder
    then i + modulus_minus_remainder
    else i - remainder
  ;;

  let[@inline always] round ?(dir = `Nearest) i ~to_multiple_of =
    match dir with
    | `Nearest -> round_nearest i ~to_multiple_of
    | `Down -> round_down i ~to_multiple_of
    | `Up -> round_up i ~to_multiple_of
    | `Zero -> round_towards_zero i ~to_multiple_of
  ;;
end

module Private = struct
  let int_pow = int_pow
  let int64_pow = int64_pow
  let int63_pow_on_int64 = int63_pow_on_int64

  module Pow_overflow_bounds = Pow_overflow_bounds
end
