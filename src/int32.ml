open! Import
open! Stdlib.Int32

module T = struct
  type t = int32 [@@deriving_inline globalize, hash, sexp, sexp_grammar]

  let (globalize : t -> t) = (globalize_int32 : t -> t)

  let (hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
    hash_fold_int32

  and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = hash_int32 in
    fun x -> func x
  ;;

  let t_of_sexp = (int32_of_sexp : Sexplib0.Sexp.t -> t)
  let sexp_of_t = (sexp_of_int32 : t -> Sexplib0.Sexp.t)
  let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) = int32_sexp_grammar

  [@@@end]

  let hashable : t Hashable.t = { hash; compare; sexp_of_t }
  let compare (x : t) y = compare x y
  let to_string = to_string
  let of_string = of_string
  let of_string_opt = of_string_opt
end

include T
include Comparator.Make (T)

let num_bits = 32
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
let rem = rem
let neg = neg
let minus_one = minus_one
let one = one
let zero = zero
let compare = compare
let compare__local = Stdlib.compare
let to_float = to_float
let of_float_unchecked = of_float

let of_float f =
  if Float_replace_polymorphic_compare.( >= ) f float_lower_bound
     && Float_replace_polymorphic_compare.( <= ) f float_upper_bound
  then of_float f
  else
    Printf.invalid_argf
      "Int32.of_float: argument (%f) is out of range or NaN"
      (Float0.box f)
      ()
;;

include Comparable.With_zero (struct
  include T

  let zero = zero
end)

module Infix_compare = struct
  open Poly

  let ( >= ) (x : t) y = x >= y
  let ( <= ) (x : t) y = x <= y
  let ( = ) (x : t) y = x = y
  let ( > ) (x : t) y = x > y
  let ( < ) (x : t) y = x < y
  let ( <> ) (x : t) y = x <> y
end

module Compare = struct
  include Infix_compare

  let compare = compare
  let compare__local = compare__local
  let ascending = compare
  let descending x y = compare y x
  let min x y = Bool0.select (x <= y) x y
  let max x y = Bool0.select (x >= y) x y
  let equal (x : t) y = x = y
  let equal__local (x : t) y = Poly.equal x y
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
let ( / ) = div
let ( * ) = mul
let ( - ) = sub
let ( + ) = add
let ( ~- ) = neg
let incr r = r := !r + one
let decr r = r := !r - one
let of_int32 t = t
let of_int32_exn = of_int32
let to_int32 t = t
let to_int32_exn = to_int32
let popcount = Popcount.int32_popcount

module Conv = Int_conversions

let of_int = Conv.int_to_int32
let of_int_exn = Conv.int_to_int32_exn
let of_int_trunc = Conv.int_to_int32_trunc
let to_int = Conv.int32_to_int
let to_int_exn = Conv.int32_to_int_exn
let to_int_trunc = Conv.int32_to_int_trunc
let of_int64 = Conv.int64_to_int32
let of_int64_exn = Conv.int64_to_int32_exn
let of_int64_trunc = Conv.int64_to_int32_trunc
let to_int64 = Conv.int32_to_int64
let of_nativeint = Conv.nativeint_to_int32
let of_nativeint_exn = Conv.nativeint_to_int32_exn
let of_nativeint_trunc = Conv.nativeint_to_int32_trunc
let to_nativeint = Conv.int32_to_nativeint
let to_nativeint_exn = to_nativeint
let pow b e = of_int_exn (Int_math.Private.int_pow (to_int_exn b) (to_int_exn e))
let ( ** ) b e = pow b e

external bswap32 : (t[@local_opt]) -> (t[@local_opt]) = "%bswap_int32"

let bswap16 x = Stdlib.Int32.shift_right_logical (bswap32 x) 16

module Pow2 = struct
  open! Import
  open Int32_replace_polymorphic_compare

  let raise_s = Error.raise_s

  let non_positive_argument () =
    Printf.invalid_argf "argument must be strictly positive" ()
  ;;

  let ( lor ) = Stdlib.Int32.logor
  let ( lsr ) = Stdlib.Int32.shift_right_logical
  let ( land ) = Stdlib.Int32.logand

  (** "ceiling power of 2" - Least power of 2 greater than or equal to x. *)
  let ceil_pow2 x =
    if x <= Stdlib.Int32.zero then non_positive_argument ();
    let x = Stdlib.Int32.pred x in
    let x = x lor (x lsr 1) in
    let x = x lor (x lsr 2) in
    let x = x lor (x lsr 4) in
    let x = x lor (x lsr 8) in
    let x = x lor (x lsr 16) in
    Stdlib.Int32.succ x
  ;;

  (** "floor power of 2" - Largest power of 2 less than or equal to x. *)
  let floor_pow2 x =
    if x <= Stdlib.Int32.zero then non_positive_argument ();
    let x = x lor (x lsr 1) in
    let x = x lor (x lsr 2) in
    let x = x lor (x lsr 4) in
    let x = x lor (x lsr 8) in
    let x = x lor (x lsr 16) in
    Stdlib.Int32.sub x (x lsr 1)
  ;;

  let is_pow2 x =
    if x <= Stdlib.Int32.zero then non_positive_argument ();
    x land Stdlib.Int32.pred x = Stdlib.Int32.zero
  ;;

  (* C stubs for int32 clz and ctz to use the CLZ/BSR/CTZ/BSF instruction where possible *)
  external clz
    :  (int32[@unboxed])
    -> (int[@untagged])
    = "Base_int_math_int32_clz" "Base_int_math_int32_clz_unboxed"
    [@@noalloc]

  external ctz
    :  (int32[@unboxed])
    -> (int[@untagged])
    = "Base_int_math_int32_ctz" "Base_int_math_int32_ctz_unboxed"
    [@@noalloc]

  (** Hacker's Delight Second Edition p106 *)
  let floor_log2 i =
    if i <= Stdlib.Int32.zero
    then
      raise_s
        (Sexp.message "[Int32.floor_log2] got invalid input" [ "", sexp_of_int32 i ]);
    num_bits - 1 - clz i
  ;;

  (** Hacker's Delight Second Edition p106 *)
  let ceil_log2 i =
    if i <= Stdlib.Int32.zero
    then
      raise_s (Sexp.message "[Int32.ceil_log2] got invalid input" [ "", sexp_of_int32 i ]);
    (* The [i = 1] check is needed because clz(0) is undefined *)
    if Stdlib.Int32.equal i Stdlib.Int32.one
    then 0
    else num_bits - clz (Stdlib.Int32.pred i)
  ;;
end

include Pow2
include Int_string_conversions.Make (T)

include Int_string_conversions.Make_hex (struct
  type t = int32 [@@deriving_inline compare ~localize, hash]

  let compare__local = (compare_int32__local : t -> t -> int)
  let compare = (fun a b -> compare__local a b : t -> t -> int)

  let (hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
    hash_fold_int32

  and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = hash_int32 in
    fun x -> func x
  ;;

  [@@@end]

  let zero = zero
  let neg = ( ~- )
  let ( < ) = ( < )
  let to_string i = Printf.sprintf "%lx" i
  let of_string s = Stdlib.Scanf.sscanf s "%lx" Fn.id
  let module_name = "Base.Int32.Hex"
end)

include Int_string_conversions.Make_binary (struct
  type t = int32 [@@deriving_inline compare ~localize, equal ~localize, hash]

  let compare__local = (compare_int32__local : t -> t -> int)
  let compare = (fun a b -> compare__local a b : t -> t -> int)
  let equal__local = (equal_int32__local : t -> t -> bool)
  let equal = (fun a b -> equal__local a b : t -> t -> bool)

  let (hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
    hash_fold_int32

  and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = hash_int32 in
    fun x -> func x
  ;;

  [@@@end]

  let ( land ) = ( land )
  let ( lsr ) = ( lsr )
  let clz = clz
  let num_bits = num_bits
  let one = one
  let to_int_exn = to_int_exn
  let zero = zero
end)

include Pretty_printer.Register (struct
  type nonrec t = t

  let to_string = to_string
  let module_name = "Base.Int32"
end)

module Pre_O = struct
  let ( + ) = ( + )
  let ( - ) = ( - )
  let ( * ) = ( * )
  let ( / ) = ( / )
  let ( ~- ) = ( ~- )
  let ( ** ) = ( ** )

  include (Compare : Comparisons.Infix with type t := t)

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
  let ( lor ) = bit_or
  let ( lxor ) = bit_xor
  let lnot = bit_not
  let ( lsl ) = shift_left
  let ( asr ) = shift_right
  let ( lsr ) = shift_right_logical
end

include O

(* [Int32] and [Int32.O] agree value-wise *)
