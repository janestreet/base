(* A 63bit integer is a 64bit integer with its bits shifted to the left and its lowest bit
   set to 0. This is the same kind of encoding as OCaml int on 64bit architecture. The
   only difference being the lowest bit (immediate bit) set to 1. *)

open! Import
module Sexp = Sexp0
include Int64_replace_polymorphic_compare
module Conv = Int_conversions

module W : sig @@ portable
  type t = int64
  [@@deriving compare ~localize, globalize, hash, sexp ~stackify, sexp_grammar]

  include%template Comparator.S [@modality portable] with type t := t

  val wrap_exn : local_ int64 -> local_ t
  val wrap_modulo : local_ int64 -> t
  val unwrap : local_ t -> local_ int64

  (** Returns a non-negative int64 that is equal to the input int63 modulo 2^63. *)
  val unwrap_unsigned : local_ t -> int64

  val invariant : t -> unit
  val add : local_ t -> local_ t -> t
  val sub : local_ t -> local_ t -> t
  val neg : local_ t -> t
  val abs : t -> t
  val abs_local : local_ t -> local_ t
  val succ : local_ t -> t
  val pred : local_ t -> t
  val mul : local_ t -> local_ t -> t
  val pow : local_ t -> local_ t -> t
  val div : local_ t -> local_ t -> t
  val rem : local_ t -> local_ t -> t
  val popcount : local_ t -> t
  val bit_not : local_ t -> t
  val bit_xor : local_ t -> local_ t -> t
  val bit_or : local_ t -> local_ t -> t
  val bit_and : local_ t -> local_ t -> t
  val shift_left : local_ t -> int -> t
  val shift_right : local_ t -> int -> t
  val shift_right_logical : local_ t -> int -> t
  val min_value : t
  val max_value : t
  val to_int64 : local_ t -> local_ int64
  val of_int64 : local_ int64 -> t option
  val of_int64_exn : local_ int64 -> local_ t
  val of_int64_trunc : local_ int64 -> t
  val compare : t -> t -> int
  val compare__local : local_ t -> local_ t -> int
  val equal__local : local_ t -> local_ t -> bool
  val ceil_pow2 : local_ t -> t
  val floor_pow2 : local_ t -> t
  val ceil_log2 : local_ t -> t
  val floor_log2 : local_ t -> t
  val is_pow2 : local_ t -> bool
  val clz : local_ t -> t
  val ctz : local_ t -> t
end = struct
  module T = struct
    type t = int64 [@@deriving compare ~localize, globalize, hash, sexp_of, sexp_grammar]
  end

  include T

  include%template Comparator.Make [@modality portable] (T)

  let wrap_exn x = exclave_
    (* Raises if the int64 value does not fit on int63. *)
    Conv.int64_fit_on_int63_exn x;
    Stdlib.Int64.mul x 2L
  ;;

  let wrap x =
    if Conv.int64_is_representable_as_int63 x
    then Some (Stdlib.Int64.mul (globalize x) 2L)
    else None
  ;;

  let wrap_modulo x = Stdlib.Int64.mul (globalize x) 2L
  let unwrap x = exclave_ Stdlib.Int64.shift_right x 1
  let unwrap_unsigned x = Stdlib.Int64.shift_right_logical (globalize x) 1

  (* This does not use wrap or unwrap to avoid generating exceptions in the case of
     overflows. This is to preserve the semantics of int type on 64 bit architecture. *)
  let f2 f a b =
    Stdlib.Int64.mul (f (Stdlib.Int64.shift_right a 1) (Stdlib.Int64.shift_right b 1)) 2L
  ;;

  let mask = 0xffff_ffff_ffff_fffeL
  let m x = Int64.O.(x land mask)
  let invariant t = assert (m t = t)
  let add = Int64.( + )
  let sub = Int64.( - )
  let neg = Int64.neg
  let abs = Int64.abs
  let abs_local = Int64.abs_local
  let one = globalize (wrap_exn 1L)
  let succ a = add a one
  let pred a = sub a one
  let min_value = m Stdlib.Int64.min_int
  let max_value = m Stdlib.Int64.max_int
  let bit_not x = m (Int64.lnot x)
  let bit_and = Int64.bit_and
  let bit_xor = Int64.bit_xor
  let bit_or = Int64.bit_or
  let shift_left = Int64.shift_left
  let shift_right x i = m Int64.O.(x asr i) [@nontail]
  let shift_right_logical x i = m Int64.O.(x lsr i) [@nontail]
  let pow = f2 Int_math.Private.int63_pow_on_int64
  let mul a b = Int64.O.(a * (b asr 1))
  let div a b = wrap_modulo Int64.O.(a / b)
  let rem = Int64.rem
  let popcount x = Popcount.int64_popcount x |> wrap_modulo
  let to_int64 = unwrap
  let of_int64 = wrap
  let of_int64_exn = wrap_exn
  let of_int64_trunc t = wrap_modulo t
  let t_of_sexp x = globalize (wrap_exn (int64_of_sexp x)) [@nontail]
  let sexp_of_t x = sexp_of_int64 (globalize_int64 (unwrap x))
  let sexp_of_t__stack x = exclave_ sexp_of_int64__stack (unwrap x)
  let compare (x : t) y = compare x y
  let compare__local (x : t) y = compare__local x y
  let equal__local (x : t) y = equal__local x y
  let is_pow2 x = Int64.is_pow2 (unwrap x) [@nontail]

  let clz x =
    (* We run Int64.clz directly on the wrapped int63 value. This is correct because the
       bits of the int63_emul are left-aligned in the Int64. *)
    Int64.clz x |> wrap_modulo
  ;;

  let ctz x = (Int64.ctz (unwrap x) |> wrap_modulo) [@nontail]
  let floor_pow2 x = globalize (wrap_exn (Int64.floor_pow2 (unwrap x))) [@nontail]
  let ceil_pow2 x = globalize (wrap_exn (Int64.floor_pow2 (unwrap x))) [@nontail]
  let floor_log2 x = Int64.floor_log2 (unwrap x) |> wrap_modulo
  let ceil_log2 x = Int64.ceil_log2 (unwrap x) |> wrap_modulo
end

open W

module T = struct
  type t = W.t [@@deriving globalize, hash, sexp ~stackify, sexp_grammar]
  type comparator_witness = W.comparator_witness

  let comparator = W.comparator
  let compare = W.compare
  let compare__local = W.compare__local
  let equal__local = W.equal__local
  let invariant = W.invariant

  (* We don't expect [hash] to follow the behavior of int in 64bit architecture *)
  let _ = hash
  let hash (x : t) = Stdlib.Hashtbl.hash x
  let hashable : t Hashable.t = { hash; compare; sexp_of_t }

  let invalid_str x =
    Printf.failwithf "Int63.of_string: invalid input %S" (globalize_string x) ()
  ;;

  (* "sign" refers to whether the number starts with a '-' "signedness = false" means the
     rest of the number is parsed as unsigned and then cast to signed with wrap-around
     modulo 2^i "signedness = true" means no such craziness happens

     The terminology and the logic is due to the code in byterun/ints.c in ocaml 4.03
     ([parse_sign_and_base] function).

     Signedness equals true for plain decimal number (e.g. 1235, -6789)

     Signedness equals false in the following cases:
     - [0xffff], [-0xffff] (hexadecimal representation)
     - [0b0101], [-0b0101] (binary representation)
     - [0o1237], [-0o1237] (octal representation)
     - [0u9812], [-0u9812] (unsigned decimal representation - available from OCaml 4.03) *)
  let sign_and_signedness x =
    let len = String.length x in
    let open Int_replace_polymorphic_compare in
    let pos, sign =
      if 0 < len
      then (
        match x.[0] with
        | '-' -> 1, `Neg
        | '+' -> 1, `Pos
        | _ -> 0, `Pos)
      else 0, `Pos
    in
    if pos + 2 < len
    then (
      let c1 = x.[pos] in
      let c2 = x.[pos + 1] in
      match c1, c2 with
      | '0', '0' .. '9' -> sign, true
      | '0', _ -> sign, false
      | _ -> sign, true)
    else sign, true
  ;;

  let to_string x = Integer_to_string.int64_to_string (unwrap x) [@nontail]

  let of_string_raw str =
    let sign, signedness = sign_and_signedness str in
    if signedness
    then globalize (of_int64_exn (Int64.of_string str)) [@nontail]
    else (
      let pos_str =
        match sign with
        | `Neg -> String.unsafe_sub str ~pos:1 ~len:(String.length str - 1)
        | `Pos -> str
      in
      let int64 = Int64.of_string pos_str in
      (* unsigned 63-bit int must parse as a positive signed 64-bit int *)
      if Int64_replace_polymorphic_compare.( < ) int64 0L then invalid_str str;
      let int63 = wrap_modulo int64 in
      match sign with
      | `Neg -> neg int63
      | `Pos -> int63)
  ;;

  let of_string str =
    try of_string_raw str with
    | _ -> invalid_str str
  ;;

  let of_string_opt str =
    match of_string_raw str with
    | t -> Some t
    | exception _ -> None
  ;;

  let bswap16 t = wrap_modulo (Int64.bswap16 (unwrap t))
  let bswap32 t = wrap_modulo (Int64.bswap32 (unwrap t))
  let bswap48 t = wrap_modulo (Int64.bswap48 (unwrap t))
end

include T

let num_bits = 63
let float_lower_bound = Float0.lower_bound_for_int num_bits
let float_upper_bound = Float0.upper_bound_for_int num_bits
let shift_right_logical = shift_right_logical
let shift_right = shift_right
let shift_left = shift_left
let bit_not = bit_not
let bit_xor = bit_xor
let bit_or = bit_or
let bit_and = bit_and
let popcount = popcount
let abs = abs
let abs_local = abs_local
let pred = pred
let succ = succ
let pow = pow
let rem = rem
let neg = neg
let max_value = max_value
let min_value = min_value
let minus_one = globalize (wrap_exn Stdlib.Int64.minus_one)
let one = globalize (wrap_exn Stdlib.Int64.one)
let zero = globalize (wrap_exn Stdlib.Int64.zero)
let is_pow2 = is_pow2
let floor_pow2 = floor_pow2
let ceil_pow2 = ceil_pow2
let floor_log2 = floor_log2
let ceil_log2 = ceil_log2
let clz = clz
let ctz = ctz
let to_float x = Int64.to_float (unwrap x) [@nontail]
let of_float_unchecked x = wrap_modulo (Int64.of_float_unchecked x)

let of_float t =
  let open Float_replace_polymorphic_compare in
  if t >= float_lower_bound && t <= float_upper_bound
  then of_float_unchecked t
  else
    Printf.invalid_argf
      "Int63.of_float: argument (%f) is out of range or NaN"
      (Float0.box t)
      ()
;;

let of_int64 = of_int64
let of_local_int64_exn = of_int64_exn
let of_int64_exn x = globalize (of_local_int64_exn x) [@nontail]
let of_int64_trunc = of_int64_trunc
let to_local_int64 = to_int64
let to_int64 x = globalize_int64 (to_local_int64 x) [@nontail]

include%template Comparable.With_zero [@modality portable] (struct
    include T

    let zero = zero
  end)

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

let ( / ) = div
let ( * ) = mul
let ( - ) = sub
let ( + ) = add
let ( ~- ) = neg
let ( ** ) b e = pow b e
let incr r = r := !r + one
let decr r = r := !r - one

(* We can reuse conversion function from/to int64 here. *)
let of_int x = globalize (wrap_exn (Conv.int_to_int64 x)) [@nontail]
let of_int_exn = of_int
let to_int x = Conv.int64_to_int (unwrap x) [@nontail]
let to_int_exn x = Conv.int64_to_int_exn (unwrap x) [@nontail]
let to_int_trunc x = Conv.int64_to_int_trunc (unwrap x) [@nontail]
let of_local_int32 x = exclave_ wrap_exn (Conv.int32_to_int64 x)
let of_local_int32_exn = of_local_int32
let of_int32 x = globalize (of_local_int32 x) [@nontail]
let of_int32_exn = of_int32
let to_int32 x = Conv.int64_to_int32 (unwrap x) [@nontail]
let to_local_int32_exn x = exclave_ Conv.int64_to_int32_exn (unwrap x)
let to_int32_exn x = globalize_int32 (to_local_int32_exn x) [@nontail]
let to_int32_trunc x = Conv.int64_to_int32_trunc (unwrap x) [@nontail]
let of_nativeint x = of_int64 (Conv.nativeint_to_int64 x) [@nontail]
let of_local_nativeint_exn x = exclave_ wrap_exn (Conv.nativeint_to_int64 x)
let of_nativeint_exn x = globalize (of_local_nativeint_exn x) [@nontail]
let of_nativeint_trunc x = of_int64_trunc (Conv.nativeint_to_int64 x)
let to_nativeint x = Conv.int64_to_nativeint (unwrap x) [@nontail]
let to_local_nativeint_exn x = exclave_ Conv.int64_to_nativeint_exn (unwrap x)
let to_nativeint_exn x = globalize_nativeint (to_local_nativeint_exn x) [@nontail]
let to_nativeint_trunc x = Conv.int64_to_nativeint_trunc (unwrap x)
let num_bits = of_int_exn num_bits

include Int_string_conversions.Make (T)

include Int_string_conversions.Make_hex (struct
    type t = T.t [@@deriving compare ~localize, hash]

    let zero = zero
    let neg = ( ~- )
    let ( < ) = ( < )

    let to_string i =
      (* the use of [unwrap_unsigned] here is important for the case of [min_value] *)
      Printf.sprintf "%Lx" (unwrap_unsigned i)
    ;;

    let of_string s = of_string ("0x" ^ s)
    let module_name = "Base.Int63.Hex"
  end)

include%template Pretty_printer.Register [@modality portable] (struct
    type nonrec t = t

    let to_string x = to_string x
    let module_name = "Base.Int63"
  end)

module Pre_O = struct
  let ( + ) = ( + )
  let ( - ) = ( - )
  let ( * ) = ( * )
  let ( / ) = ( / )
  let ( ~- ) = ( ~- )
  let ( ** ) = ( ** )

  include Int64_replace_polymorphic_compare

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

include Int_string_conversions.Make_binary (struct
    type t = T.t [@@deriving compare ~localize, equal ~localize, hash]

    let ( land ) = ( land )
    let ( lsr ) = ( lsr )
    let clz = clz
    let num_bits = num_bits
    let one = one
    let to_int_trunc = to_int_trunc
    let zero = zero
    let ( - ) = ( - )
  end)

(* [Int63] and [Int63.O] agree value-wise *)

module Summable = struct
  type nonrec t = t

  let zero = zero
  let[@inline] ( + ) x y = x + y
  let[@inline] ( - ) x y = x - y
end

module Repr = struct
  type emulated = t

  type ('underlying_type, 'intermediate_type) t =
    | Int : (int, int) t
    | Int64 : (int64, emulated) t
end

let repr = Repr.Int64

(* Include type-specific [Replace_polymorphic_compare] at the end, after including functor
   application that could shadow its definitions. This is here so that efficient versions
   of the comparison functions are exported by this module. *)
include Int64_replace_polymorphic_compare
