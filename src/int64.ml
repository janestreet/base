open! Import
open! Caml.Int64

module T = struct
  type t = int64 [@@deriving_inline hash, sexp]
  let (hash_fold_t :
         Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
    hash_fold_int64
  and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = hash_int64 in fun x -> func x
  let t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t = int64_of_sexp
  let sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t = sexp_of_int64
  [@@@end]

  let compare = Int64_replace_polymorphic_compare.compare

  let to_string = to_string
  let of_string = of_string
end

include T
include Comparator.Make(T)

let num_bits = 64
let float_lower_bound = Float0.lower_bound_for_int num_bits
let float_upper_bound = Float0.upper_bound_for_int num_bits

let float_of_bits = float_of_bits
let bits_of_float = bits_of_float
let shift_right_logical = shift_right_logical
let shift_right = shift_right
let shift_left = shift_left
let bit_not = lognot
let bit_xor = logxor
let bit_or = logor
let bit_and = logand
let min_value = min_int
let max_value = max_int
let abs = abs
let pred = pred
let succ = succ
let pow = Int_math.int64_pow
let rem = rem
let neg = neg
let minus_one = minus_one
let one = one
let zero = zero
let to_float = to_float
let of_float_unchecked = Caml.Int64.of_float
let of_float f =
  if Float_replace_polymorphic_compare.(>=) f float_lower_bound
  && Float_replace_polymorphic_compare.(<=) f float_upper_bound
  then
    Caml.Int64.of_float f
  else
    Printf.invalid_argf "Int64.of_float: argument (%f) is out of range or NaN"
      (Float0.box f)
      ()

let ( ** ) b e = pow b e

include Comparable.Validate_with_zero (struct
    include T
    let zero = zero
  end)

(* Open replace_polymorphic_compare after including functor instantiations so they do not
   shadow its definitions. This is here so that efficient versions of the comparison
   functions are available within this module. *)
open Int64_replace_polymorphic_compare

let between t ~low ~high = low <= t && t <= high
let clamp_unchecked t ~min ~max =
  if t < min then min else if t <= max then t else max

let clamp_exn t ~min ~max =
  assert (min <= max);
  clamp_unchecked t ~min ~max

let clamp t ~min ~max =
  if min > max then
    Or_error.error_s
      (Sexp.message "clamp requires [min <= max]"
         [ "min", T.sexp_of_t min
         ; "max", T.sexp_of_t max
         ])
  else
    Ok (clamp_unchecked t ~min ~max)

let ( / ) = div
let ( * ) = mul
let ( - ) = sub
let ( + ) = add
let ( ~- ) = neg

let incr r = r := !r + one
let decr r = r := !r - one

let of_int64 t = t
let of_int64_exn = of_int64
let to_int64 t = t

let popcount = Popcount.int64_popcount

module Conv = Int_conversions
let of_int = Conv.int_to_int64
let of_int_exn = of_int
let to_int = Conv.int64_to_int
let to_int_exn = Conv.int64_to_int_exn
let to_int_trunc = Conv.int64_to_int_trunc
let of_int32 = Conv.int32_to_int64
let of_int32_exn = of_int32
let to_int32 = Conv.int64_to_int32
let to_int32_exn = Conv.int64_to_int32_exn
let to_int32_trunc = Conv.int64_to_int32_trunc
let of_nativeint = Conv.nativeint_to_int64
let of_nativeint_exn = of_nativeint
let to_nativeint = Conv.int64_to_nativeint
let to_nativeint_exn = Conv.int64_to_nativeint_exn
let to_nativeint_trunc = Conv.int64_to_nativeint_trunc

module Pow2 = struct
  open! Import
  open Int64_replace_polymorphic_compare

  module Sys = Sys0

  let raise_s = Error.raise_s

  let non_positive_argument () =
    Printf.invalid_argf "argument must be strictly positive" ()

  let ( lor ) = Caml.Int64.logor;;
  let ( lsr ) = Caml.Int64.shift_right_logical;;
  let ( land ) = Caml.Int64.logand;;

  (** "ceiling power of 2" - Least power of 2 greater than or equal to x. *)
  let ceil_pow2 x =
    if x <= Caml.Int64.zero then non_positive_argument ();
    let x = Caml.Int64.pred x in
    let x = x lor (x lsr 1) in
    let x = x lor (x lsr 2) in
    let x = x lor (x lsr 4) in
    let x = x lor (x lsr 8) in
    let x = x lor (x lsr 16) in
    let x = x lor (x lsr 32) in
    Caml.Int64.succ x
  ;;

  (** "floor power of 2" - Largest power of 2 less than or equal to x. *)
  let floor_pow2 x =
    if x <= Caml.Int64.zero then non_positive_argument ();
    let x = x lor (x lsr 1) in
    let x = x lor (x lsr 2) in
    let x = x lor (x lsr 4) in
    let x = x lor (x lsr 8) in
    let x = x lor (x lsr 16) in
    let x = x lor (x lsr 32) in
    Caml.Int64.sub x (x lsr 1)
  ;;

  let is_pow2 x =
    if x <= Caml.Int64.zero then non_positive_argument ();
    (x land (Caml.Int64.pred x)) = Caml.Int64.zero
  ;;

  (* C stub for int clz to use the CLZ/BSR instruction where possible *)
  external int64_clz : int64 -> int = "Base_int_math_int64_clz" [@@noalloc]

  (** Hacker's Delight Second Edition p106 *)
  let floor_log2 i =
    if i <= Caml.Int64.zero then
      raise_s (Sexp.message "[Int64.floor_log2] got invalid input"
                 ["", sexp_of_int64 i]);
    num_bits - 1 - int64_clz i
  ;;

  (** Hacker's Delight Second Edition p106 *)
  let ceil_log2 i =
    if Poly.( <= ) i Caml.Int64.zero then
      raise_s (Sexp.message "[Int64.ceil_log2] got invalid input"
                 ["", sexp_of_int64 i]);
    if Caml.Int64.equal i Caml.Int64.one
    then 0
    else num_bits - int64_clz (Caml.Int64.pred i)
  ;;
end
include Pow2

include Conv.Make (T)

include Conv.Make_hex(struct

    type t = int64 [@@deriving_inline compare, hash]
    let compare : t -> t -> int = compare_int64
    let (hash_fold_t :
           Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
      hash_fold_int64
    and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
      let func = hash_int64 in fun x -> func x
    [@@@end]

    let zero = zero
    let neg = (~-)
    let (<) = (<)
    let to_string i = Printf.sprintf "%Lx" i
    let of_string s = Caml.Scanf.sscanf s "%Lx" Fn.id

    let module_name = "Base.Int64.Hex"

  end)

include Pretty_printer.Register (struct
    type nonrec t = t
    let to_string = to_string
    let module_name = "Base.Int64"
  end)

module Pre_O = struct
  let ( + ) = ( + )
  let ( - ) = ( - )
  let ( * ) = ( * )
  let ( / ) = ( / )
  let ( ~- ) = ( ~- )
  let ( ** ) = ( ** )
  include (Int64_replace_polymorphic_compare : Comparisons.Infix with type t := t)
  let abs = abs
  let neg = neg
  let zero = zero
  let of_int_exn = of_int_exn
end

module O = struct
  include Pre_O
  include Int_math.Make (struct
      type nonrec t = t
      include Pre_O
      let rem = rem
      let to_float = to_float
      let of_float = of_float
      let of_string = T.of_string
      let to_string = T.to_string
    end)

  let ( land ) = bit_and
  let ( lor  ) = bit_or
  let ( lxor ) = bit_xor
  let ( lnot ) = bit_not
  let ( lsl  ) = shift_left
  let ( asr  ) = shift_right
  let ( lsr  ) = shift_right_logical
end

include O (* [Int64] and [Int64.O] agree value-wise *)

(* Include type-specific [Replace_polymorphic_compare] at the end, after
   including functor application that could shadow its definitions. This is
   here so that efficient versions of the comparison functions are exported by
   this module. *)
include Int64_replace_polymorphic_compare
