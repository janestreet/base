open! Import
open! Base_printf

module List   = Base_list
module String = Base_string

include Float0

let raise_s = Error.raise_s

module T = struct
  type t = float [@@deriving hash, sexp]
  let compare (x : t) y = compare x y
  let equal (x : t) y = x = y

  external specialized_hash : float -> int = "caml_hash_double" [@@noalloc]

  let%test_unit _ =
    (* on 64-bit platform ppx_hash hashes floats exactly the same as polymorphic hash *)
    if Base_int.num_bits = 63 then
      List.iter ~f:(fun float ->
        let hash1 = Caml.Hashtbl.hash float in
        let hash2 = [%hash: float] float in
        let hash3 = specialized_hash float in
        if (not (hash1 = hash2 && hash1 = hash3))
        then ksprintf failwith "bad %x %x %x" hash1 hash2 hash3
      )
        [ 0.926038888360971146
        ; 34.1638588598232076
        ]
  ;;

  let hash = specialized_hash
  ;;

end

include T
include Comparator.Make(T)

let to_float x = x
let of_float x = x

let of_string s =
  try Pervasives.float_of_string s with
  | _ -> invalid_argf "Float.of_string %s" s ()
;;

external format_float : string -> float -> string = "caml_format_float"

(* Stolen from [pervasives.ml].  Adds a "." at the end if needed.  It is in
   [pervasives.mli], but it also says not to use it directly, so we copy and paste the
   code. It makes the assumption on the string passed in argument that it was returned by
   [format_float] *)
let valid_float_lexem s =
  let l = String.length s in
  let rec loop i =
    if i >= l then s ^ "." else
    match s.[i] with
    | '0' .. '9' | '-' -> loop (i + 1)
    | _ -> s
  in
  loop 0
;;

(* Standard 12 significant digits, exponential notation used as necessary, guaranteed to
   be a valid OCaml float lexem, not to look like an int.  *)
let to_string x = valid_float_lexem (format_float "%.12g" x);;

(* Let [y] be a power of 2.  Then the next representable float is:
     [z = y * (1 + 2 ** -52)]
   and the previous one is
     [x = y * (1 - 2 ** -53)]

   In general, every two adjacent floats are within a factor of between [1 + 2**-53]
   and [1 + 2**-52] from each other, that is within [1 + 1.1e-16] and [1 + 2.3e-16].

   So if the decimal representation of a float starts with "1", then its adjacent floats
   will usually differ from it by 1, and sometimes by 2, at the 17th significant digit
   (counting from 1).

   On the other hand, if the decimal representation starts with "9", then the adjacent
   floats will be off by no more than 23 at the 16th and 17th significant digits.

   E.g.:

   # sprintf "%.17g" (1024. *. (1. -. 2.** (-53.)));;
                           11111111
                 1234 5678901234567
   - : string = "1023.9999999999999"

   Printing a couple of extra digits reveals that the difference indeed is roughly 11 at
   digits 17th and 18th (that is, 13th and 14th after "."):

   # sprintf "%.19g" (1024. *. (1. -. 2.** (-53.)));;
                           1111111111
                 1234 567890123456789
   - : string = "1023.999999999999886"

   The ulp (the difference between adjacent floats) is twice as big on the other side of
   1024.:

   # sprintf "%.19g" (1024. *. (1. +. 2.** (-52.)));;
                           1111111111
                 1234 567890123456789
   - : string = "1024.000000000000227"

   Now take a power of 2 which starts with 99:

   # 2.**93. ;;
                        1111111111
               1 23456789012345678
   - : float = 9.9035203142830422e+27

   # 2.**93. *. (1. +. 2.** (-52.));;
   - : float = 9.9035203142830444e+27

   # 2.**93. *. (1. -. 2.** (-53.));;
   - : float = 9.9035203142830411e+27

   The difference between 2**93 and its two neighbors is slightly more than, respectively,
   1 and 2 at significant digit 16.

   Those examples show that:
    - 17 significant digits is always sufficient to represent a float without ambiguity
    - 15th significant digit can always be represented accurately
    - converting a decimal number with 16 significant digits to its nearest float and back
      can change the last decimal digit by no more than 1

   To make sure that floats obtained by conversion from decimal fractions (e.g. "3.14")
   are printed without trailing non-zero digits, one should choose the first among the
   '%.15g', '%.16g', and '%.17g' representations which does round-trip:

   # sprintf "%.15g" 3.14;;
   - : string = "3.14"                     (* pick this one *)
   # sprintf "%.16g" 3.14;;
   - : string = "3.14"
   # sprintf "%.17g" 3.14;;
   - : string = "3.1400000000000001"       (* do not pick this one *)

   # sprintf "%.15g" 8.000000000000002;;
   - : string = "8"                        (* do not pick this one--does not round-trip *)
   # sprintf "%.16g" 8.000000000000002;;
   - : string = "8.000000000000002"        (* prefer this one *)
   # sprintf "%.17g" 8.000000000000002;;
   - : string = "8.0000000000000018"       (* this one has one digit of junk at the end *)

   Skipping the '%.16g' in the above procedure saves us some time, but it means that, as
   seen in the second example above, occasionally numbers with exactly 16 significant
   digits will have an error introduced at the 17th digit.  That is probably OK for
   typical use, because a number with 16 significant digits is "ugly" already.  Adding one
   more doesn't make it much worse for a human reader.

   On the other hand, we cannot skip '%.15g' and only look at '%.16g' and '%.17g', since
   the inaccuracy at the 16th digit might introduce the noise we want to avoid:

   # sprintf "%.15g" 9.992;;
   - : string = "9.992"                    (* pick this one *)
   # sprintf "%.16g" 9.992;;
   - : string = "9.992000000000001"        (* do not pick this one--junk at the end *)
   # sprintf "%.17g" 9.992;;
   - : string = "9.9920000000000009"
*)
let to_string_round_trippable x =
  valid_float_lexem (
    let y = format_float "%.15g" x in
    if float_of_string y = x then
      y
    else
      format_float "%.17g" x)
;;

let nan = Pervasives.nan

let infinity = Pervasives.infinity
let neg_infinity = Pervasives.neg_infinity

let max_value = infinity
let min_value = neg_infinity

let max_finite_value = Pervasives.max_float

let min_positive_subnormal_value = 2. ** -1074.
let min_positive_normal_value = 2. ** -1022.

let zero = 0.
let one = 1.
let minus_one = -1.

let%test _ = to_string_round_trippable 3.14                             = "3.14"
let%test _ = to_string_round_trippable 3.1400000000000001               = "3.14"
let%test _ = to_string_round_trippable 3.1400000000000004               = "3.1400000000000006"
let%test _ = to_string_round_trippable 8.000000000000002                = "8.0000000000000018"
let%test _ = to_string_round_trippable 9.992                            = "9.992"
let%test _ = to_string_round_trippable (2.**63. *. (1. +. 2.** (-52.))) = "9.2233720368547779e+18"
let%test _ = to_string_round_trippable (-3.)                            = "-3."
let%test _ = to_string_round_trippable nan                              = "nan"
let%test _ = to_string_round_trippable infinity                         = "inf"
let%test _ = to_string_round_trippable neg_infinity                     = "-inf"
let%test _ = to_string_round_trippable 3e100                            = "3e+100"
let%test _ = to_string_round_trippable max_finite_value                 = "1.7976931348623157e+308"
let%test _ = to_string_round_trippable min_positive_subnormal_value     = "4.94065645841247e-324"

(* The bits of INRIA's [Pervasives] that we just want to expose in
   [Float]. Most are already deprecated in [Base_pervasives], and
   eventually all of them should be. *)
include (Pervasives : sig
  external frexp : float -> float * int = "caml_frexp_float"
  external ldexp : (float [@unboxed]) -> (int [@untagged]) -> (float [@unboxed]) = "caml_ldexp_float" "caml_ldexp_float_unboxed" [@@noalloc]
  external log10 : float -> float = "caml_log10_float" "log10"
    [@@unboxed] [@@noalloc]
  external expm1 : float -> float = "caml_expm1_float" "caml_expm1"
    [@@unboxed] [@@noalloc]
  external log1p : float -> float = "caml_log1p_float" "caml_log1p"
    [@@unboxed] [@@noalloc]
  external copysign : float -> float -> float = "caml_copysign_float" "caml_copysign"
    [@@unboxed] [@@noalloc]
  external cos : float -> float = "caml_cos_float" "cos"
    [@@unboxed] [@@noalloc]
  external sin : float -> float = "caml_sin_float" "sin"
    [@@unboxed] [@@noalloc]
  external tan : float -> float = "caml_tan_float" "tan"
    [@@unboxed] [@@noalloc]
  external acos : float -> float = "caml_acos_float" "acos"
    [@@unboxed] [@@noalloc]
  external asin : float -> float = "caml_asin_float" "asin"
    [@@unboxed] [@@noalloc]
  external atan : float -> float = "caml_atan_float" "atan"
    [@@unboxed] [@@noalloc]
  external atan2 : float -> float -> float = "caml_atan2_float" "atan2"
    [@@unboxed] [@@noalloc]
  external hypot : float -> float -> float = "caml_hypot_float" "caml_hypot"
    [@@unboxed] [@@noalloc]
  external cosh : float -> float = "caml_cosh_float" "cosh"
    [@@unboxed] [@@noalloc]
  external sinh : float -> float = "caml_sinh_float" "sinh"
    [@@unboxed] [@@noalloc]
  external tanh : float -> float = "caml_tanh_float" "tanh"
    [@@unboxed] [@@noalloc]
  external sqrt : float -> float = "caml_sqrt_float" "sqrt"
    [@@unboxed] [@@noalloc]
  external exp : float -> float = "caml_exp_float" "exp"
    [@@unboxed] [@@noalloc]
  external log : float -> float = "caml_log_float" "log"
    [@@unboxed] [@@noalloc]
end)

(* We need this indirection because these are exposed as "val" instead of "external" *)
let frexp = frexp
let ldexp = ldexp

let epsilon_float = Pervasives.epsilon_float
let%test _ = epsilon_float = (one_ulp `Up 1.) -. 1.

include Hashable.Make (T)

let of_int = Base_int.to_float
let to_int = Base_int.of_float

let of_int64 i = Int64.to_float i

let to_int64 = Base_int64.of_float

let iround_lbound = lower_bound_for_int Base_int.num_bits
let iround_ubound = upper_bound_for_int Base_int.num_bits

(* The performance of the "exn" rounding functions is important, so they are written
   out separately, and tuned individually.  (We could have the option versions call
   the "exn" versions, but that imposes arguably gratuitous overhead---especially
   in the case where the capture of backtraces is enabled upon "with"---and that seems
   not worth it when compared to the relatively small amount of code duplication.) *)

(* Error reporting below is very carefully arranged so that, e.g., [iround_nearest_exn]
   itself can be inlined into callers such that they don't need to allocate a box for the
   [float] argument.  This is done with a box [box] function carefully chosen to allow the
   compiler to create a separate box for the float only in error cases.  See, e.g.,
   [../../zero/test/price_test.ml] for a mechanical test of this property when building
   with [X_LIBRARY_INLINING=true]. *)

let iround_up t =
  if t > 0.0 then begin
    let t' = ceil t in
    if t' <= iround_ubound then
      Some (int_of_float t')
    else
      None
  end
  else begin
    if t >= iround_lbound then
      Some (int_of_float t)
    else
      None
  end

let iround_up_exn t =
  if t > 0.0 then begin
    let t' = ceil t in
    if t' <= iround_ubound then
      int_of_float t'
    else
      invalid_argf "Float.iround_up_exn: argument (%f) is too large" (box t) ()
  end
  else begin
    if t >= iround_lbound then
      int_of_float t
    else
      invalid_argf "Float.iround_up_exn: argument (%f) is too small or NaN" (box t) ()
  end
[@@ocaml.inline always]

let iround_down t =
  if t >= 0.0 then begin
    if t <= iround_ubound then
      Some (int_of_float t)
    else
      None
  end
  else begin
    let t' = floor t in
    if t' >= iround_lbound then
      Some (int_of_float t')
    else
      None
  end

let iround_down_exn t =
  if t >= 0.0 then begin
    if t <= iround_ubound then
      int_of_float t
    else
      invalid_argf "Float.iround_down_exn: argument (%f) is too large" (box t) ()
  end
  else begin
    let t' = floor t in
    if t' >= iround_lbound then
      int_of_float t'
    else
      invalid_argf "Float.iround_down_exn: argument (%f) is too small or NaN" (box t) ()
  end
[@@ocaml.inline always]

let iround_towards_zero t =
  if t >= iround_lbound && t <= iround_ubound then
    Some (int_of_float t)
  else
    None

let iround_towards_zero_exn t =
  if t >= iround_lbound && t <= iround_ubound then
    int_of_float t
  else
    invalid_argf "Float.iround_towards_zero_exn: argument (%f) is out of range or NaN"
      (box t)
      ()

(* Outside of the range (round_nearest_lb..round_nearest_ub), all representable doubles
   are integers in the mathematical sense, and [round_nearest] should be identity.

   However, for odd numbers with the absolute value between 2**52 and 2**53, the formula
   [round_nearest x = floor (x + 0.5)] does not hold:

   # let naive_round_nearest x = floor (x +. 0.5);;
   # let x = 2. ** 52. +. 1.;;
   val x : float = 4503599627370497.
   # naive_round_nearest x;;
   - :     float = 4503599627370498.
*)

let round_nearest_lb = -.(2. ** 52.)
let round_nearest_ub =    2. ** 52.

(* For [x = one_ulp `Down 0.5], the formula [floor (x +. 0.5)] for rounding to nearest
   does not work, because the exact result is halfway between [one_ulp `Down 1.] and [1.],
   and it gets rounded up to [1.] due to the round-ties-to-even rule. *)
let one_ulp_less_than_half = one_ulp `Down 0.5
let%test _ = one_ulp_less_than_half = 0.49999999999999994
let add_half_for_round_nearest t =
  t +. (if t = one_ulp_less_than_half then
          one_ulp_less_than_half (* since t < 0.5, make sure the result is < 1.0 *)
        else
          0.5)

let iround_nearest_32 t =
  if t >= 0. then
    let t' = add_half_for_round_nearest t in
    if t' <= iround_ubound then
      Some (int_of_float t')
    else
      None
  else
    let t' = floor (t +. 0.5) in
    if t' >= iround_lbound then
      Some (int_of_float t')
    else
      None

let iround_nearest_64 t =
  if t >= 0. then
    if t < round_nearest_ub then
      Some (int_of_float (add_half_for_round_nearest t))
    else
    if t <= iround_ubound then
      Some (int_of_float t)
    else
      None
  else
  if t > round_nearest_lb then
    Some (int_of_float (floor (t +. 0.5)))
  else
  if t >= iround_lbound then
    Some (int_of_float t)
  else
    None

let iround_nearest =
  if Sys.word_size = 64
  then iround_nearest_64
  else iround_nearest_32

let iround_nearest_exn_32 t =
  if t >= 0. then
    let t' = add_half_for_round_nearest t in
    if t' <= iround_ubound then
      int_of_float t'
    else
      invalid_argf "Float.iround_nearest_exn: argument (%f) is too large" (box t) ()
  else
    let t' = floor (t +. 0.5) in
    if t' >= iround_lbound then
      int_of_float t'
    else
      invalid_argf "Float.iround_nearest_exn: argument (%f) is too small" (box t) ()

let iround_nearest_exn_64 t =
  if t >= 0. then
    if t < round_nearest_ub then
      int_of_float (add_half_for_round_nearest t)
    else
    if t <= iround_ubound then
      int_of_float t
    else
      invalid_argf "Float.iround_nearest_exn: argument (%f) is too large" (box t) ()
  else
  if t > round_nearest_lb then
    int_of_float (floor (t +. 0.5))
  else
    if t >= iround_lbound then
      int_of_float t
    else
      invalid_argf "Float.iround_nearest_exn: argument (%f) is too small or NaN" (box t) ()
[@@ocaml.inline always]

let iround_nearest_exn =
  if Sys.word_size = 64
  then iround_nearest_exn_64
  else iround_nearest_exn_32

let%bench_module "round_nearest 64/runtime" = (module struct
  let f = if Random.bool () then 1.0 else 2.0
  let%bench "iround_nearest_exn_64"      = iround_nearest_exn_64 f
  let%bench "iround_nearest_exn_runtime" = iround_nearest_exn f
end)

(* The following [iround_exn] and [iround] functions are slower than the ones above.
   Their equivalence to those functions is tested in the unit tests below. *)

let iround_exn ?(dir=`Nearest) t =
  match dir with
  | `Zero    -> iround_towards_zero_exn t
  | `Nearest -> iround_nearest_exn t
  | `Up      -> iround_up_exn t
  | `Down    -> iround_down_exn t

let iround ?(dir=`Nearest) t =
  try Some (iround_exn ~dir t)
  with _ -> None

let is_inf x = (Pervasives.classify_float x = Pervasives.FP_infinite);;

let min_inan (x : t) y =
  if is_nan y then x
  else if is_nan x then y
  else if x < y then x else y

let max_inan (x : t) y =
  if is_nan y then x
  else if is_nan x then y
  else if x > y then x else y

let add = (+.)
let sub = (-.)
let neg = (~-.)
let abs = Pervasives.abs_float
let scale = ( *. )

let min (x : t) y =
  if is_nan x || is_nan y then nan
  else if x < y then x else y

let max (x : t) y =
  if is_nan x || is_nan y then nan
  else if x > y then x else y

module Parts : sig
  type t

  val fractional : t -> float
  val integral : t -> float
  val modf : float -> t
end = struct
  type t = float * float

  let fractional t = fst t
  let integral t = snd t
  let modf = modf
end
let modf = Parts.modf

let round_down = floor
let%test _ =
  round_down      3.6  =  3.
  && round_down (-3.6) = -4.

let round_up = ceil
let%test _ =
  round_up      3.6  =  4.
  && round_up (-3.6) = -3.

let round_towards_zero t =
  if t >= 0.
  then round_down t
  else round_up   t
let%test _ =
  round_towards_zero      3.6  =  3.
  && round_towards_zero (-3.6) = -3.

(* see the comment above [round_nearest_lb] and [round_nearest_ub] for an explanation *)
let round_nearest t =
  if t > round_nearest_lb && t < round_nearest_ub then
    floor (add_half_for_round_nearest t)
  else
    t

let round_nearest_half_to_even t =
  if t <= round_nearest_lb || t >= round_nearest_ub then
    t
  else
    let floor        = floor t           in
    (* [ceil_or_succ = if t is an integer then t +. 1. else ceil t].  Faster than [ceil]. *)
    let ceil_or_succ = floor +. 1.       in
    let diff_floor   = t -. floor        in
    let diff_ceil    = ceil_or_succ -. t in
    if diff_floor < diff_ceil then
      floor
    else
    if diff_floor > diff_ceil then
      ceil_or_succ
    else
      (* exact tie, pick the even *)
      if mod_float floor 2. = 0. then
        floor
      else
        ceil_or_succ

let%test _ = round_nearest_half_to_even                  0.    =  0.
let%test _ = round_nearest_half_to_even                  0.5   =  0.
let%test _ = round_nearest_half_to_even                (-0.5)  =  0.
let%test _ = round_nearest_half_to_even (one_ulp `Up     0.5)  =  1.
let%test _ = round_nearest_half_to_even (one_ulp `Down   0.5)  =  0.
let%test _ = round_nearest_half_to_even (one_ulp `Up   (-0.5)) =  0.
let%test _ = round_nearest_half_to_even (one_ulp `Down (-0.5)) = -1.
let%test _ = round_nearest_half_to_even                  3.5   =  4.
let%test _ = round_nearest_half_to_even                  4.5   =  4.
let%test _ = round_nearest_half_to_even (one_ulp `Up   (-5.5)) = -5.
let%test _ = round_nearest_half_to_even                  5.5   =  6.
let%test _ = round_nearest_half_to_even                  6.5   =  6.
let%test _ = round_nearest_half_to_even (one_ulp `Up (-. (2. ** 52.))) =    -. (2. ** 52.)
let%test _ = round_nearest              (one_ulp `Up (-. (2. ** 52.))) = 1. -. (2. ** 52.)

let int63_round_lbound = lower_bound_for_int Base_int63.num_bits
let int63_round_ubound = upper_bound_for_int Base_int63.num_bits

let int63_round_up_exn t =
  if t > 0.0 then begin
    let t' = ceil t in
    if t' <= int63_round_ubound then
      Base_int63.of_float_unchecked t'
    else
      invalid_argf "Float.int63_round_up_exn: argument (%f) is too large" (Float0.box t) ()
  end
  else begin
    if t >= int63_round_lbound then
      Base_int63.of_float_unchecked t
    else
      invalid_argf "Float.int63_round_up_exn: argument (%f) is too small or NaN"
        (Float0.box t) ()
  end

let int63_round_down_exn t =
  if t >= 0.0 then begin
    if t <= int63_round_ubound then
      Base_int63.of_float_unchecked t
    else
      invalid_argf "Float.int63_round_down_exn: argument (%f) is too large"
        (Float0.box t) ()
  end
  else begin
    let t' = floor t in
    if t' >= int63_round_lbound then
      Base_int63.of_float_unchecked t'
    else
      invalid_argf "Float.int63_round_down_exn: argument (%f) is too small or NaN"
        (Float0.box t) ()
  end

let int63_round_nearest_portable_alloc_exn t0 =
  let t = round_nearest t0 in
  if t > 0.
  then begin
    if t <= int63_round_ubound
    then Base_int63.of_float_unchecked t
    else invalid_argf
           "Float.int63_round_nearest_portable_alloc_exn: argument (%f) is too large"
           (box t0)
           ()
  end
  else begin
    if t >= int63_round_lbound
    then Base_int63.of_float_unchecked t
    else invalid_argf
           "Float.int63_round_nearest_portable_alloc_exn: argument (%f) is too small or NaN"
           (box t0)
           ()
  end

let%test_module _ = (module struct
  (* check we raise on invalid input *)
  let must_fail f x = try ignore (f x); false with _ -> true
  let must_succeed f x = ignore (f x); true
  let%test _ = must_fail int63_round_nearest_portable_alloc_exn nan
  let%test _ = must_fail int63_round_nearest_portable_alloc_exn max_value
  let%test _ = must_fail int63_round_nearest_portable_alloc_exn min_value
  let%test _ = must_fail int63_round_nearest_portable_alloc_exn (2. ** 63.)
  let%test _ = must_fail int63_round_nearest_portable_alloc_exn (~-. (2. ** 63.))
  let%test _ = must_succeed int63_round_nearest_portable_alloc_exn (2. ** 62. -. 512.)
  let%test _ = must_fail int63_round_nearest_portable_alloc_exn (2. ** 62.)
  let%test _ = must_fail int63_round_nearest_portable_alloc_exn (~-. (2. ** 62.) -. 1024.)
  let%test _ = must_succeed int63_round_nearest_portable_alloc_exn (~-. (2. ** 62.))
end)

let int63_round_nearest_arch64_noalloc_exn f = Base_int63.of_int (iround_nearest_exn f)

let int63_round_nearest_exn =
  if Sys.word_size = 64 then
    int63_round_nearest_arch64_noalloc_exn
  else
    int63_round_nearest_portable_alloc_exn

let%bench_module "round_nearest portability/performance" = (module struct
  let f = if Random.bool () then 1.0 else 2.0
  let%bench "int63_round_nearest_portable_alloc_exn" = int63_round_nearest_portable_alloc_exn f
  let%bench "int63_round_nearest_arch64_noalloc_exn" = int63_round_nearest_arch64_noalloc_exn f
  let%bench "int63_round_nearest_exn"                = int63_round_nearest_exn f

  (* Here is a comparison of both of these rounding operators on a 64-bit machine. Hence
     we have special-cased this so that we get the faster operation on 64-bit machines.
     We also benchmark the selected operator to make sure we actually select the right one

     ┌────────────────────────────────────────┬──────────┬─────────┬────────────┐
     │ Name                                   │ Time/Run │ mWd/Run │ Percentage │
     ├────────────────────────────────────────┼──────────┼─────────┼────────────┤
     │ int63_round_nearest_portable_alloc_exn │  18.41ns │   2.00w │    100.00% │
     │ int63_round_nearest_arch64_noalloc_exn │   4.27ns │         │     23.17% │
     │ int63_round_nearest_exn                │   4.27ns │         │     23.18% │
     └────────────────────────────────────────┴──────────┴─────────┴────────────┘
  *)

end)

let%test _ =
  round_nearest      3.6  =  4.
  && round_nearest (-3.6) = -4.


let round ?(dir=`Nearest) t =
  match dir with
  | `Nearest -> round_nearest      t
  | `Down    -> round_down         t
  | `Up      -> round_up           t
  | `Zero    -> round_towards_zero t

let mod_float = Pervasives.mod_float

module Class = struct
  type t =
  | Infinite
  | Nan
  | Normal
  | Subnormal
  | Zero
  [@@deriving sexp]

  let to_string t = string_of_sexp (sexp_of_t t)
  let of_string s = t_of_sexp (sexp_of_string s)
end

let classify t =
  let module C = Class in
  let module P = Pervasives in
  match P.classify_float t with
  | P.FP_normal    -> C.Normal
  | P.FP_subnormal -> C.Subnormal
  | P.FP_zero      -> C.Zero
  | P.FP_infinite  -> C.Infinite
  | P.FP_nan       -> C.Nan
;;

let is_finite t =
  not (t = infinity || t = neg_infinity || is_nan t)
;;

let insert_underscores ?(delimiter='_') ?(strip_zero=false) string =
  match String.lsplit2 string ~on:'.' with
  | None ->
    Int_conversions.insert_delimiter string ~delimiter
  | Some (left, right) ->
    let left = Int_conversions.insert_delimiter left ~delimiter in
    let right =
      if strip_zero
      then String.rstrip right ~drop:(fun c -> c = '0')
      else right
    in
    match right with
    | "" -> left
    | _ -> left ^ "." ^ right
;;

let to_string_hum ?delimiter ?(decimals=3) ?strip_zero f =
  if decimals < 0 then
    invalid_argf "to_string_hum: invalid argument ~decimals=%d" decimals ();
  match classify f with
  | Class.Infinite -> if f > 0. then "inf" else "-inf"
  | Class.Nan -> "nan"
  | Class.Normal
  | Class.Subnormal
  | Class.Zero -> insert_underscores (sprintf "%.*f" decimals f) ?delimiter ?strip_zero
;;

(* The redefinition of [sexp_of_t] below assumes sexp conversion uses E rather than e. *)
let%test "e vs E" = [%sexp (1.4e100 : t)] = Atom "1.4E+100"

let sexp_of_t t =
  let sexp = [%sexp (t : t)] in
  match !Sexp.of_float_style with
  | `No_underscores -> sexp
  | `Underscores ->
    match sexp with
    | List _ -> raise_s [%message [%here] "[sexp_of_float] produced strange sexp"
                                    (sexp : Sexp.t)]
    | Atom string ->
      if String.contains string 'E'
      then sexp
      else Atom (insert_underscores string)
;;

let%test_module _ = (module struct
  let test ?delimiter ~decimals f s s_strip_zero =
    let s' = to_string_hum ?delimiter ~decimals ~strip_zero:false f in
    if s' <> s then
      raise_s
        [%message
          "to_string_hum ~strip_zero:false"
            ~input:(f : float)
            (decimals : int)
            ~got:(s' : string)
            ~expected:(s : string)
        ];
    let s_strip_zero' = to_string_hum ?delimiter ~decimals ~strip_zero:true f in
    if s_strip_zero' <> s_strip_zero then
      raise_s
        [%message
          "to_string_hum ~strip_zero:true"
            ~input:(f : float)
            (decimals : int)
            ~got:(s_strip_zero : string)
            ~expected:(s_strip_zero' : string)
        ];
  ;;

  let%test_unit _ = test ~decimals:3 0.99999 "1.000" "1"
  let%test_unit _ = test ~decimals:3 0.00001 "0.000" "0"
  let%test_unit _ = test ~decimals:3 ~-.12345.1 "-12_345.100" "-12_345.1"
  let%test_unit _ = test ~delimiter:',' ~decimals:3 ~-.12345.1 "-12,345.100" "-12,345.1"
  let%test_unit _ = test ~decimals:0 0.99999 "1" "1"
  let%test_unit _ = test ~decimals:0 0.00001 "0" "0"
  let%test_unit _ = test ~decimals:0 ~-.12345.1 "-12_345" "-12_345"
  let%test_unit _ = test ~decimals:0 (5.0 /. 0.0) "inf" "inf"
  let%test_unit _ = test ~decimals:0 (-5.0 /. 0.0) "-inf" "-inf"
  let%test_unit _ = test ~decimals:0 (0.0 /. 0.0) "nan" "nan"
  let%test_unit _ = test ~decimals:2 (5.0 /. 0.0) "inf" "inf"
  let%test_unit _ = test ~decimals:2 (-5.0 /. 0.0) "-inf" "-inf"
  let%test_unit _ = test ~decimals:2 (0.0 /. 0.0) "nan" "nan"
  let%test_unit _ = test ~decimals:5 (10_000.0 /. 3.0) "3_333.33333" "3_333.33333"
  let%test_unit _ = test ~decimals:2 ~-.0.00001 "-0.00" "-0"

  let rand_test n =
    let go () =
      let f = Random.float 1_000_000.0 -. 500_000.0 in
      let repeatable to_str =
        let s = to_str f in
        if (String.split s ~on:',' |> String.concat |> of_string |> to_str) <> s
        then failwithf "failed on testing %f" f ()
      in
      repeatable (to_string_hum ~decimals:3 ~strip_zero:false);
    in
    try
      for _ = 0 to n - 1 do go () done;
      true
    with e ->
      Printf.eprintf "%s\n%!" (Exn.to_string e);
      false
  ;;

  let%test _ = rand_test 10_000
  ;;
end)
;;

let to_padded_compact_string t =

  (* Round a ratio toward the nearest integer, resolving ties toward the nearest even
     number.  For sane inputs (in particular, when [denominator] is an integer and
     [abs numerator < 2e52]) this should be accurate.  Otherwise, the result might be a
     little bit off, but we don't really use that case. *)
  let iround_ratio_exn ~numerator ~denominator =
    let k = floor (numerator /. denominator) in
    (* if [abs k < 2e53], then both [k] and [k +. 1.] are accurately represented, and in
       particular [k +. 1. > k].  If [denominator] is also an integer, and
       [abs (denominator *. (k +. 1)) < 2e53] (and in some other cases, too), then [lower]
       and [higher] are actually both accurate.  Since (roughly)
       [numerator = denominator *. k] then for [abs numerator < 2e52] we should be
       fine. *)
    let lower  = denominator *. k  in
    let higher = denominator *. (k +. 1.) in
    (* Subtracting numbers within a factor of two from each other is accurate.
       So either the two subtractions below are accurate, or k = 0, or k = -1.
       In case of a tie, round to even. *)
    let diff_right = higher -. numerator in
    let diff_left = numerator -. lower in
    let k = iround_nearest_exn k in
    if diff_right < diff_left then
      k + 1
    else if diff_right > diff_left then
      k
    else
      (* a tie *)
    if k mod 2 = 0 then k else k + 1
  in

  match classify t with
  | Class.Infinite -> if t < 0.0 then "-inf  " else "inf  "
  | Class.Nan -> "nan  "
  | Class.Subnormal | Class.Normal | Class.Zero ->
    let go t =
      let conv_one t =
        assert (0. <= t && t < 999.95);
        let x = format_float "%.1f" t in
        (* Fix the ".0" suffix *)
        if String.is_suffix x ~suffix:".0" then begin
          let n = String.length x in
          x.[n - 1] <- ' ';
          x.[n - 2] <- ' ';
        end;
        x
      in
      let conv mag t denominator =
        assert (denominator  = 100.     && t >= 999.95
             || denominator >= 100_000. && t >= round_nearest (denominator *. 9.999_5));
        assert (t < round_nearest (denominator *. 9_999.5));
        let i, d =
          let k = iround_ratio_exn ~numerator:t ~denominator in
          (* [mod] is okay here because we know i >= 0. *)
          k / 10, k mod 10
        in
        assert (0 <= i && i < 1000);
        assert (0 <= d && d < 10);
        if d = 0 then
          sprintf "%d%c " i mag
        else
          sprintf "%d%c%d" i mag d
      in
      (* While the standard metric prefixes (e.g. capital "M" rather than "m", [1]) are
         nominally more correct, this hinders readability in our case.  E.g., 10G6 and
         1066 look too similar.  That's an extreme example, but in general k,m,g,t,p
         probably stand out better than K,M,G,T,P when interspersed with digits.

         [1] http://en.wikipedia.org/wiki/Metric_prefix *)
      (* The trick here is that:
          - the first boundary (999.95) as a float is slightly over-represented (so it is
            better approximated as "1k" than as "999.9"),
          - the other boundaries are accurately represented, because they are integers.
         That's why the strict equalities below do exactly what we want. *)
      if t < 999.95E0       then conv_one t
      else if t < 999.95E3  then conv 'k' t 100.
      else if t < 999.95E6  then conv 'm' t 100_000.
      else if t < 999.95E9  then conv 'g' t 100_000_000.
      else if t < 999.95E12 then conv 't' t 100_000_000_000.
      else if t < 999.95E15 then conv 'p' t 100_000_000_000_000.
      else sprintf "%.1e" t
    in
    if t >= 0.
    then go t
    else "-" ^ (go ~-.t)

let%test_module _ = (module struct
  let test f expect =
    let actual = to_padded_compact_string f  in
    if actual <> expect
    then failwithf "%f: expected '%s', got '%s'" f expect actual ()

  let both f expect =
    assert (f > 0.);
    test f expect;
    test (~-.f) ("-"^expect);
  ;;

  let decr = one_ulp `Down
  let incr = one_ulp `Up

  let boundary f ~closer_to_zero ~at =
    assert (f > 0.);
    (* If [f] looks like an odd multiple of 0.05, it might be slightly under-represented
       as a float, e.g.

       1. -. 0.95 = 0.0500000000000000444

       In such case, sadly, the right way for [to_padded_compact_string], just as for
       [sprintf "%.1f"], is to round it down.  However, the next representable number
       should be rounded up:

       # let x = 0.95 in sprintf "%.0f / %.1f / %.2f / %.3f / %.20f" x x x x x;;
       - : string = "1 / 0.9 / 0.95 / 0.950 / 0.94999999999999995559"

       # let x = incr 0.95 in sprintf "%.0f / %.1f / %.2f / %.3f / %.20f" x x x x x ;;
       - : string = "1 / 1.0 / 0.95 / 0.950 / 0.95000000000000006661"

    *)
    let f =
      if f >= 1000. then
        f
      else
        let x = sprintf "%.20f" f in
        let spot = String.index_exn x '.' in
        (* the following condition is only meant to work for small multiples of 0.05 *)
        if x.[spot + 2] = '4' && x.[spot + 3] = '9' && x.[spot + 4] = '9' then
          (* something like 0.94999999999999995559 *)
          incr f
        else
          f
    in
    both (decr f) closer_to_zero;
    both f at;
  ;;

  let%test_unit _ = test nan                            "nan  "
  let%test_unit _ = test 0.0                              "0  "
  let%test_unit _ = both min_positive_subnormal_value     "0  "
  let%test_unit _ = both infinity                       "inf  "

  let%test_unit _ = boundary                       0.05 ~closer_to_zero:  "0  " ~at:    "0.1"
  let%test_unit _ = boundary                       0.15 ~closer_to_zero:  "0.1" ~at:    "0.2"
  (* glibc printf resolves ties to even, cf.
     http://www.exploringbinary.com/inconsistent-rounding-of-printed-floating-point-numbers/
     Ties are resolved differently in JavaScript - mark some tests as no running with JavaScript.
  *)
  let%test_unit _ [@tags "no-js"] =
                    boundary (* tie *)             0.25 ~closer_to_zero:  "0.2" ~at:    "0.2"
  let%test_unit _ [@tags "no-js"] =
                    boundary                 (incr 0.25)~closer_to_zero:  "0.2" ~at:    "0.3"
  let%test_unit _ = boundary                       0.35 ~closer_to_zero:  "0.3" ~at:    "0.4"
  let%test_unit _ = boundary                       0.45 ~closer_to_zero:  "0.4" ~at:    "0.5"
  let%test_unit _ = both                           0.50                                 "0.5"
  let%test_unit _ = boundary                       0.55 ~closer_to_zero:  "0.5" ~at:    "0.6"
  let%test_unit _ = boundary                       0.65 ~closer_to_zero:  "0.6" ~at:    "0.7"
  (* this time tie-to-even means round away from 0 *)
  let%test_unit _ = boundary (* tie *)             0.75 ~closer_to_zero:  "0.7" ~at:    "0.8"
  let%test_unit _ = boundary                       0.85 ~closer_to_zero:  "0.8" ~at:    "0.9"
  let%test_unit _ = boundary                       0.95 ~closer_to_zero:  "0.9" ~at:    "1  "
  let%test_unit _ = boundary                       1.05 ~closer_to_zero:  "1  " ~at:    "1.1"
  let%test_unit _ [@tags "no-js"] =
                    boundary                       3.25 ~closer_to_zero:  "3.2" ~at:    "3.2"
  let%test_unit _ [@tags "no-js"] =
                    boundary                 (incr 3.25)~closer_to_zero:  "3.2" ~at:    "3.3"
  let%test_unit _ = boundary                       3.75 ~closer_to_zero:  "3.7" ~at:    "3.8"
  let%test_unit _ = boundary                       9.95 ~closer_to_zero:  "9.9" ~at:   "10  "
  let%test_unit _ = boundary                      10.05 ~closer_to_zero: "10  " ~at:   "10.1"
  let%test_unit _ = boundary                     100.05 ~closer_to_zero:"100  " ~at:  "100.1"
  let%test_unit _ [@tags "no-js"] =
                    boundary (* tie *)           999.25 ~closer_to_zero:"999.2" ~at:  "999.2"
  let%test_unit _ [@tags "no-js"] =
                    boundary               (incr 999.25)~closer_to_zero:"999.2" ~at:  "999.3"
  let%test_unit _ = boundary                     999.75 ~closer_to_zero:"999.7" ~at:  "999.8"
  let%test_unit _ = boundary                     999.95 ~closer_to_zero:"999.9" ~at:    "1k "
  let%test_unit _ = both                        1000.                                   "1k "

  (* some ties which we resolve manually in [iround_ratio_exn] *)
  let%test_unit _ = boundary                    1050.   ~closer_to_zero:  "1k " ~at:    "1k "
  let%test_unit _ = boundary              (incr 1050.)  ~closer_to_zero:  "1k " ~at:    "1k1"
  let%test_unit _ = boundary                    1950.   ~closer_to_zero:  "1k9" ~at:    "2k "
  let%test_unit _ = boundary                    3250.   ~closer_to_zero:  "3k2" ~at:    "3k2"
  let%test_unit _ = boundary              (incr 3250.)  ~closer_to_zero:  "3k2" ~at:    "3k3"
  let%test_unit _ = boundary                    9950.   ~closer_to_zero:  "9k9" ~at:   "10k "
  let%test_unit _ = boundary                  33_250.   ~closer_to_zero: "33k2" ~at:   "33k2"
  let%test_unit _ = boundary            (incr 33_250.)  ~closer_to_zero: "33k2" ~at:   "33k3"
  let%test_unit _ = boundary                  33_350.   ~closer_to_zero: "33k3" ~at:   "33k4"
  let%test_unit _ = boundary                  33_750.   ~closer_to_zero: "33k7" ~at:   "33k8"
  let%test_unit _ = boundary                 333_250.   ~closer_to_zero:"333k2" ~at:  "333k2"
  let%test_unit _ = boundary           (incr 333_250.)  ~closer_to_zero:"333k2" ~at:  "333k3"
  let%test_unit _ = boundary                 333_750.   ~closer_to_zero:"333k7" ~at:  "333k8"
  let%test_unit _ = boundary                 999_850.   ~closer_to_zero:"999k8" ~at:  "999k8"
  let%test_unit _ = boundary           (incr 999_850.)  ~closer_to_zero:"999k8" ~at:  "999k9"
  let%test_unit _ = boundary                 999_950.   ~closer_to_zero:"999k9" ~at:    "1m "
  let%test_unit _ = boundary               1_050_000.   ~closer_to_zero:  "1m " ~at:    "1m "
  let%test_unit _ = boundary         (incr 1_050_000.)  ~closer_to_zero:  "1m " ~at:    "1m1"

  let%test_unit _ = boundary             999_950_000.   ~closer_to_zero:"999m9" ~at:    "1g "
  let%test_unit _ = boundary         999_950_000_000.   ~closer_to_zero:"999g9" ~at:    "1t "
  let%test_unit _ = boundary     999_950_000_000_000.   ~closer_to_zero:"999t9" ~at:    "1p "
  let%test_unit _ = boundary 999_950_000_000_000_000.   ~closer_to_zero:"999p9" ~at:"1.0e+18"

  (* Test the boundary between the subnormals and the normals. *)
  let%test_unit _ = boundary min_positive_normal_value ~closer_to_zero:"0  " ~at:"0  "
end)

(* Performance note: Initializing the accumulator to 1 results in one extra
   multiply; e.g., to compute x ** 4, we in principle only need 2 multiplies,
   but this function will have 3 multiplies.  However, attempts to avoid this
   (like decrementing n and initializing accum to be x, or handling small
   exponents as a special case) have not yielded anything that is a net
   improvement.
*)
let int_pow x n =
  if n = 0 then
    1.
  else begin
    (* Using [x +. (-0.)] on the following line convinces the compiler to avoid a certain
       boxing (that would result in allocation in each iteration).  Soon, the compiler
       shouldn't need this "hint" to avoid the boxing.  The reason we add -0 rather than 0
       is that [x +. (-0.)] is apparently always the same as [x], whereas [x +. 0.] is
       not, in that it sends [-0.] to [0.].  This makes a difference because we want
       [int_pow (-0.) (-1)] to return neg_infinity just like [-0. ** -1.] would.  *)
    let x = ref (x +. (-0.)) in
    let n = ref n in
    let accum = ref 1. in
    if !n < 0 then begin
      (* x ** n = (1/x) ** -n *)
      x := 1. /. !x;
      n := ~- !n;
      if !n < 0 then begin
        (* n must have been min_int, so it is now so big that it has wrapped around.
           We decrement it so that it looks positive again, but accordingly have
           to put an extra factor of x in the accumulator.
        *)
        accum := !x;
        decr n
      end
    end;
    (* Letting [a] denote (the original value of) [x ** n], we maintain
       the invariant that [(x ** n) *. accum = a]. *)
    while !n > 1 do
      if !n land 1 <> 0 then accum := !x *. !accum;
      x := !x *. !x;
      n := !n lsr 1
    done;
    (* n is necessarily 1 at this point, so there is one additional
       multiplication by x. *)
    !x *. !accum
  end

let%test "int_pow" =
  let tol = 1e-15 in
  let test (x, n) =
    let reference_value = x ** float n in
    let relative_error = (int_pow x n -. reference_value) /. reference_value in
    abs relative_error < tol
  in
  List.for_all ~f:test
    [(1.5, 17); (1.5, 42); (0.99, 64); (2., -5); (2., -1)
    ; (-1.3, 2); (-1.3, -1); (-1.3, -2); (5., 0)
    ; (nan, 0); (0., 0); (infinity, 0)
    ]

let%test "int_pow misc" =
  int_pow 0. (-1) = infinity
  && int_pow (-0.) (-1) = neg_infinity
  && int_pow (-0.) (-2) = infinity
  && int_pow 1.5 5000 = infinity
  && int_pow 1.5 (-5000) = 0.
  && int_pow (-1.) Pervasives.max_int = -1.
  && int_pow (-1.) Pervasives.min_int = 1.

(* some ugly corner cases with extremely large exponents and some serious precision loss *)
let%test "int_pow bad cases" [@tags "no-js"] =
  let a = one_ulp `Down 1. in
  let b = one_ulp `Up 1. in
  let large = 1 lsl 61 in
  let small = ~- large in
  (* this huge discrepancy comes from the fact that [1 / a = b] but this is a very poor
     approximation, and in particular [1 / b = one_ulp `Down a = a * a]. *)
  a **    float small = 1.5114276650041252e+111
  &&  int_pow a small = 2.2844048619719663e+222
  &&  int_pow b large = 2.2844048619719663e+222
  && b ** float large = 2.2844135865396268e+222

module Replace_polymorphic_compare = struct
  let equal = equal
  let compare (x : t) y = compare x y
  let ascending = compare
  let descending x y = compare y x
  let min = min
  let max = max
  let ( >= ) (x : t) y = x >= y
  let ( <= ) (x : t) y = x <= y
  let ( = ) (x : t) y = x = y
  let ( > ) (x : t) y = x > y
  let ( < ) (x : t) y = x < y
  let ( <> ) (x : t) y = x <> y
  let between t ~low ~high = low <= t && t <= high
end

include Replace_polymorphic_compare

let clamp_exn t ~min ~max =
  (* Also fails if [min] or [max] is nan *)
  assert (min <= max);
  (* clamp_unchecked is in float0.ml *)
  clamp_unchecked t ~min ~max

let clamp t ~min ~max =
  (* Also fails if [min] or [max] is nan *)
  if min <= max then
    Ok (clamp_unchecked t ~min ~max)
  else
    Or_error.error "clamp requires [min <= max]"
      (`Min min, `Max max) [%sexp_of: [`Min of T.t] * [`Max of T.t]]

let ( + ) = ( +. )
let ( - ) = ( -. )
let ( * ) = ( *. )
let ( / ) = ( /. )
let ( ~- ) = ( ~-. )
let ( ** ) = ( ** )

let sign_exn t : Sign.t =
  if t > 0.
  then Pos
  else if t < 0.
  then Neg
  else if t = 0.
  then Zero
  else Error.raise_s [%message "Float.sign_exn of NAN" ~_:(t : t)]

module Sign_or_nan = struct type t = Neg | Zero | Pos | Nan end

let sign_or_nan t : Sign_or_nan.t =
  if t > 0.
  then Pos
  else if t < 0.
  then Neg
  else if t = 0.
  then Zero
  else Nan

let%test_unit "sign_exn" =
  List.iter ~f:(fun (input,expect) -> assert (Sign.equal (sign_exn input) expect))
    [ (1e-30,        Sign.Pos)
    ; (-0.,          Zero)
    ; (0.,           Zero)
    ; (neg_infinity, Neg)
    ]

let%test _ =
  match sign_exn nan with
  | Neg | Zero | Pos -> false
  | exception _ -> true

let ieee_negative t =
  let bits = Int64.bits_of_float t in
  Pervasives.(bits < Int64.zero)

let exponent_bits = 11
let mantissa_bits = 52

let exponent_mask64 = Base_int64.((shift_left one exponent_bits) - one)
let exponent_mask = Base_int64.to_int_exn exponent_mask64
let mantissa_mask = Base_int63.((shift_left one mantissa_bits) - one)
let mantissa_mask64 = Base_int63.to_int64 mantissa_mask

let ieee_exponent t =
  let bits = Int64.bits_of_float t in
  Base_int64.((bit_and (shift_right_logical bits mantissa_bits) exponent_mask64))
  |> Int64.to_int

let ieee_mantissa t =
  let bits = Int64.bits_of_float t in
  Base_int63.of_int64_exn Int64.(logand bits mantissa_mask64)

let create_ieee_exn ~negative ~exponent ~mantissa =
  if Base_int.(bit_and exponent exponent_mask <> exponent)
  then failwithf !"exponent %{Base_int} out of range [0, %{Base_int}]"
         exponent exponent_mask ()
  else if Base_int63.(bit_and mantissa mantissa_mask <> mantissa)
  then failwithf !"mantissa %{Base_int63} out of range [0, %{Base_int63}]"
         mantissa mantissa_mask ()
  else
    let sign_bits = if negative then Int64.min_int else Int64.zero in
    let expt_bits = Int64.shift_left (Int64.of_int exponent) mantissa_bits in
    let mant_bits = Base_int63.to_int64 mantissa in
    let bits = Int64.(logor sign_bits (logor expt_bits mant_bits)) in
    Int64.float_of_bits bits

let create_ieee ~negative ~exponent ~mantissa =
  Or_error.try_with (fun () -> create_ieee_exn ~negative ~exponent ~mantissa)

module Nan_dist = struct
  type t = Without | With_single | With_all [@@deriving sexp]
end

module Terse = struct
  type nonrec t = t
  let t_of_sexp = t_of_sexp

  let to_string x = Base_printf.sprintf "%.8G" x
  let sexp_of_t x = Sexp.Atom (to_string x)
  let of_string x = of_string x
end

let validate_ordinary t =
  Validate.of_error_opt (
    let module C = Class in
    match classify t with
    | C.Normal | C.Subnormal | C.Zero -> None
    | C.Infinite -> Some "value is infinite"
    | C.Nan -> Some "value is NaN")
;;

module V = struct
  module ZZ = Comparable.Validate (T)

  let validate_bound ~min ~max t =
    Validate.first_failure (validate_ordinary t) (ZZ.validate_bound t ~min ~max)
  ;;

  let validate_lbound ~min t =
    Validate.first_failure (validate_ordinary t) (ZZ.validate_lbound t ~min)
  ;;

  let validate_ubound ~max t =
    Validate.first_failure (validate_ordinary t) (ZZ.validate_ubound t ~max)
  ;;
end

include V

include Comparable.With_zero (struct
  include T
  let zero = zero
  include V
end)

include Pretty_printer.Register(struct
  include T
  let module_name = "Base.Float"
  let to_string = to_string
end)

module O = struct
  let ( +  ) = ( +  )
  let ( -  ) = ( -  )
  let ( *  ) = ( *  )
  let ( /  ) = ( /  )
  let ( ~- ) = ( ~- )
  include (Replace_polymorphic_compare : Polymorphic_compare_intf.Infix with type t := t)
  let abs        = abs
  let neg        = neg
  let zero       = zero
  let of_int     = of_int
  let of_float x = x
end

let%test_module _ = (module struct
  let check v expect =
    match Validate.result v, expect with
    | Ok (), `Ok | Error _, `Error -> ()
    | r, expect ->
      raise_s
        [%message "mismatch" (r : unit Or_error.t) (expect : [ `Ok | `Error ])]
  ;;

  let%test_unit _ = check (validate_lbound ~min:(Incl 0.) nan)          `Error
  let%test_unit _ = check (validate_lbound ~min:(Incl 0.) infinity)     `Error
  let%test_unit _ = check (validate_lbound ~min:(Incl 0.) neg_infinity) `Error
  let%test_unit _ = check (validate_lbound ~min:(Incl 0.) (-1.))        `Error
  let%test_unit _ = check (validate_lbound ~min:(Incl 0.) 0.)           `Ok
  let%test_unit _ = check (validate_lbound ~min:(Incl 0.) 1.)           `Ok

  let%test_unit _ = check (validate_ubound ~max:(Incl 0.) nan)          `Error
  let%test_unit _ = check (validate_ubound ~max:(Incl 0.) infinity)     `Error
  let%test_unit _ = check (validate_ubound ~max:(Incl 0.) neg_infinity) `Error
  let%test_unit _ = check (validate_ubound ~max:(Incl 0.) (-1.))        `Ok
  let%test_unit _ = check (validate_ubound ~max:(Incl 0.) 0.)           `Ok
  let%test_unit _ = check (validate_ubound ~max:(Incl 0.) 1.)           `Error

  (* Some of the following tests used to live in lib_test/core_float_test.ml. *)

  let () = Random.init 137

  let (=) = Pervasives.(=)
  let (>=) = Pervasives.(>=)
  let (+) = Pervasives.(+)
  let (-) = Pervasives.(-)
  let ( * ) = Pervasives.( * )

  (* round:
     ...  <-)[-><-)[-><-)[-><-)[-><-)[-><-)[->   ...
     ... -+-----+-----+-----+-----+-----+-----+- ...
     ... -3    -2    -1     0     1     2     3  ...
     so round x -. x should be in (-0.5,0.5]
  *)
  let round_test x =
    let y = round x in
    -0.5 < y -. x && y -. x <= 0.5

  let iround_up_vs_down_test x =
    let expected_difference =
      if Parts.fractional (modf x) = 0. then
        0
      else
        1
    in
    match iround_up x, iround_down x with
    | Some x, Some y -> x - y = expected_difference
    | _, _ -> true

  let test_all_six x
        ~specialized_iround ~specialized_iround_exn ~float_rounding
        ~dir ~validate =
    let result1 = iround x ~dir in
    let result2 = try Some (iround_exn x ~dir) with _exn -> None in
    let result3 = specialized_iround x in
    let result4 = try Some (specialized_iround_exn x) with _exn -> None in
    let result5 = try Some (Base_int.of_float (float_rounding x)) with _exn -> None in
    let result6 = try Some (Base_int.of_float (round ~dir x)) with _exn -> None in
    let (=) = Pervasives.(=) in
    if result1 = result2 && result2 = result3 && result3 = result4
       && result4 = result5 && result5 = result6 then
      validate result1
    else
      false

  (* iround ~dir:`Nearest built so this should always be true *)
  let iround_nearest_test x =
    test_all_six x
      ~specialized_iround:iround_nearest
      ~specialized_iround_exn:iround_nearest_exn
      ~float_rounding:round_nearest
      ~dir:`Nearest
      ~validate:(function
        | None -> true
        | Some y ->
          let y = of_int y in
          -0.5 < y -. x && y -. x <= 0.5)

  (* iround_down:
     ... )[<---)[<---)[<---)[<---)[<---)[<---)[  ...
     ... -+-----+-----+-----+-----+-----+-----+- ...
     ... -3    -2    -1     0     1     2     3  ...
     so x -. iround_down x should be in [0,1)
  *)
  let iround_down_test x =
    test_all_six x
      ~specialized_iround:iround_down
      ~specialized_iround_exn:iround_down_exn
      ~float_rounding:round_down
      ~dir:`Down
      ~validate:(function
        | None -> true
        | Some y ->
          let y = of_int y in
          0. <= x -. y && x -. y < 1.)

  (* iround_up:
     ...  ](--->](--->](--->](--->](--->](--->]( ...
     ... -+-----+-----+-----+-----+-----+-----+- ...
     ... -3    -2    -1     0     1     2     3  ...
     so iround_up x -. x should be in [0,1)
  *)
  let iround_up_test x =
    test_all_six x
      ~specialized_iround:iround_up
      ~specialized_iround_exn:iround_up_exn
      ~float_rounding:round_up
      ~dir:`Up
      ~validate:(function
        | None -> true
        | Some y ->
          let y = of_int y in
          0. <= y -. x && y -. x < 1.)

  (* iround_towards_zero:
     ...  ](--->](--->](---><--->)[<---)[<---)[  ...
     ... -+-----+-----+-----+-----+-----+-----+- ...
     ... -3    -2    -1     0     1     2     3  ...
     so abs x -. abs (iround_towards_zero x) should be in [0,1)
  *)
  let iround_towards_zero_test x =
    test_all_six x
      ~specialized_iround:iround_towards_zero
      ~specialized_iround_exn:iround_towards_zero_exn
      ~float_rounding:round_towards_zero
      ~dir:`Zero
      ~validate:(function
        | None -> true
        | Some y ->
          let x = abs x in
          let y = abs (of_int y) in
          0. <= x -. y && x -. y < 1. && (sign_exn x = sign_exn y || y = 0.0))

  (* Easy cases that used to live inline with the code above. *)
  let%test _ = iround_up (-3.4) = Some (-3)
  let%test _ = iround_up   0.0  = Some   0
  let%test _ = iround_up   3.4  = Some   4

  let%test _ = iround_up_exn (-3.4) = -3
  let%test _ = iround_up_exn   0.0  =  0
  let%test _ = iround_up_exn   3.4  =  4

  let%test _ = iround_down (-3.4) = Some (-4)
  let%test _ = iround_down   0.0  = Some   0
  let%test _ = iround_down   3.4  = Some   3

  let%test _ = iround_down_exn (-3.4) = -4
  let%test _ = iround_down_exn   0.0  =  0
  let%test _ = iround_down_exn   3.4  =  3

  let%test _ = iround_towards_zero (-3.4) = Some (-3)
  let%test _ = iround_towards_zero   0.0  = Some   0
  let%test _ = iround_towards_zero   3.4  = Some   3

  let%test _ = iround_towards_zero_exn (-3.4) = -3
  let%test _ = iround_towards_zero_exn   0.0  =  0
  let%test _ = iround_towards_zero_exn   3.4  =  3

  let%test _ = iround_nearest (-3.6) = Some (-4)
  let%test _ = iround_nearest (-3.5) = Some (-3)
  let%test _ = iround_nearest (-3.4) = Some (-3)
  let%test _ = iround_nearest   0.0  = Some   0
  let%test _ = iround_nearest   3.4  = Some   3
  let%test _ = iround_nearest   3.5  = Some   4
  let%test _ = iround_nearest   3.6  = Some   4

  let%test _ = iround_nearest_exn (-3.6) = -4
  let%test _ = iround_nearest_exn (-3.5) = -3
  let%test _ = iround_nearest_exn (-3.4) = -3
  let%test _ = iround_nearest_exn   0.0  =  0
  let%test _ = iround_nearest_exn   3.4  =  3
  let%test _ = iround_nearest_exn   3.5  =  4
  let%test _ = iround_nearest_exn   3.6  =  4

  let special_values_test () =
    round (-.1.50001) = -.2. &&
    round (-.1.5) = -.1. &&
    round (-.0.50001) = -.1. &&
    round (-.0.5) = 0. &&
    round 0.49999 = 0. &&
    round 0.5 = 1. &&
    round 1.49999 = 1. &&
    round 1.5 = 2. &&
    iround_exn ~dir:`Up (-.2.) = -2 &&
    iround_exn ~dir:`Up (-.1.9999) = -1 &&
    iround_exn ~dir:`Up (-.1.) = -1 &&
    iround_exn ~dir:`Up (-.0.9999) = 0 &&
    iround_exn ~dir:`Up 0. = 0 &&
    iround_exn ~dir:`Up 0.00001 = 1 &&
    iround_exn ~dir:`Up 1. = 1 &&
    iround_exn ~dir:`Up 1.00001 = 2 &&
    iround_up_exn (-.2.) = -2 &&
    iround_up_exn (-.1.9999) = -1 &&
    iround_up_exn (-.1.) = -1 &&
    iround_up_exn (-.0.9999) = 0 &&
    iround_up_exn 0. = 0 &&
    iround_up_exn 0.00001 = 1 &&
    iround_up_exn 1. = 1 &&
    iround_up_exn 1.00001 = 2 &&
    iround_exn ~dir:`Down (-.1.00001) = -2 &&
    iround_exn ~dir:`Down (-.1.) = -1 &&
    iround_exn ~dir:`Down (-.0.00001) = -1 &&
    iround_exn ~dir:`Down 0. = 0 &&
    iround_exn ~dir:`Down 0.99999 = 0 &&
    iround_exn ~dir:`Down 1. = 1 &&
    iround_exn ~dir:`Down 1.99999 = 1 &&
    iround_exn ~dir:`Down 2. = 2 &&
    iround_down_exn (-.1.00001) = -2 &&
    iround_down_exn (-.1.) = -1 &&
    iround_down_exn (-.0.00001) = -1 &&
    iround_down_exn 0. = 0 &&
    iround_down_exn 0.99999 = 0 &&
    iround_down_exn 1. = 1 &&
    iround_down_exn 1.99999 = 1 &&
    iround_down_exn 2. = 2 &&
    iround_exn ~dir:`Zero (-.2.) = -2 &&
    iround_exn ~dir:`Zero (-.1.99999) = -1 &&
    iround_exn ~dir:`Zero (-.1.) = -1 &&
    iround_exn ~dir:`Zero (-.0.99999) = 0 &&
    iround_exn ~dir:`Zero 0.99999 = 0 &&
    iround_exn ~dir:`Zero 1. = 1 &&
    iround_exn ~dir:`Zero 1.99999 = 1 &&
    iround_exn ~dir:`Zero 2. = 2

  let is_64_bit_platform = of_int max_int >= 2. ** 60.

  (* Tests for values close to [iround_lbound] and [iround_ubound]. *)
  let extremities_test ~round =
    if is_64_bit_platform then
      (* 64 bits *)
      round (2.0 ** 62. -. 512.) = Some (max_int - 511)
        && round (2.0 ** 62. -. 1024.) = Some (max_int - 1023)
        && round (-. (2.0 ** 62.)) = Some min_int
        && round (-. (2.0 ** 62. -. 512.)) = Some (min_int + 512)
        && round (2.0 ** 62.) = None
        && round (-. (2.0 ** 62. +. 1024.)) = None
    else
      let int_size_minus_one = float_of_int (Base_int.num_bits - 1) in
      (* 32 bits *)
      round (2.0 ** int_size_minus_one -. 1.) = Some max_int
        && round (2.0 ** int_size_minus_one -. 2.) = Some (max_int - 1)
        && round (-. (2.0 ** int_size_minus_one)) = Some min_int
        && round (-. (2.0 ** int_size_minus_one -. 1.)) = Some (min_int + 1)
        && round (2.0 ** int_size_minus_one) = None
        && round (-. (2.0 ** int_size_minus_one +. 1.)) = None

  let%test _ = extremities_test ~round:iround_down
  let%test _ = extremities_test ~round:iround_up
  let%test _ = extremities_test ~round:iround_nearest
  let%test _ = extremities_test ~round:iround_towards_zero

  (* test values beyond the integers range *)
  let large_value_test x =
    true

    && iround_down x = None
    && iround ~dir:`Down x = None
    && iround_up x = None
    && iround ~dir:`Up x = None
    && iround_towards_zero x = None
    && iround ~dir:`Zero x = None
    && iround_nearest x = None
    && iround ~dir:`Nearest x = None

    && (try ignore (iround_down_exn x); false with _ -> true)
    && (try ignore (iround_exn ~dir:`Down x); false with _ -> true)
    && (try ignore (iround_up_exn x); false with _ -> true)
    && (try ignore (iround_exn ~dir:`Up x); false with _ -> true)
    && (try ignore (iround_towards_zero_exn x); false with _ -> true)
    && (try ignore (iround_exn ~dir:`Zero x); false with _ -> true)
    && (try ignore (iround_nearest_exn x); false with _ -> true)
    && (try ignore (iround_exn ~dir:`Nearest x); false with _ -> true)

    && round_down x = x
    && round ~dir:`Down x = x
    && round_up x = x
    && round ~dir:`Up x = x
    && round_towards_zero x = x
    && round ~dir:`Zero x = x
    && round_nearest x = x
    && round ~dir:`Nearest x = x

  let large_numbers =
    List.concat (
      List.init (1024 - 64) ~f:(fun x ->
        let x = float (x + 64) in
        let y =
          [2. ** x;
           2. ** x -. 2. ** (x -. 53.); (* one ulp down *)
           2. ** x +. 2. ** (x -. 52.)] (* one ulp up *)
        in
        y @ (List.map y ~f:neg)))
      @
      [infinity;
       neg_infinity]

  let%test _ = List.for_all large_numbers ~f:large_value_test

  let numbers_near_powers_of_two =
    List.concat (
      List.init 64 ~f:(fun i ->
        let pow2 = 2. ** float i in
        let x =
          [ pow2;
            one_ulp `Down (pow2 +. 0.5);
            pow2 +. 0.5;
            one_ulp `Down (pow2 +. 1.0);
            pow2 +. 1.0;
            one_ulp `Down (pow2 +. 1.5);
            pow2 +. 1.5;
            one_ulp `Down (pow2 +. 2.0);
            pow2 +. 2.0;
            one_ulp `Down (pow2 *. 2.0 -. 1.0);
            one_ulp `Down pow2;
            one_ulp `Up pow2
          ]
        in
        x @ (List.map x ~f:neg)
      ))

  let%test _ = List.for_all numbers_near_powers_of_two ~f:iround_up_vs_down_test
  let%test _ = List.for_all numbers_near_powers_of_two ~f:iround_nearest_test
  let%test _ = List.for_all numbers_near_powers_of_two ~f:iround_down_test
  let%test _ = List.for_all numbers_near_powers_of_two ~f:iround_up_test
  let%test _ = List.for_all numbers_near_powers_of_two ~f:iround_towards_zero_test
  let%test _ = List.for_all numbers_near_powers_of_two ~f:round_test

  (* code for generating random floats on which to test functions *)
  let rec absirand () =
    let rec aux acc cnt =
      if cnt = 0 then
        acc
      else
        let bit = if Random.bool () then 1 else 0 in
        aux (2 * acc + bit) (cnt - 1)
    in
    let result = aux 0 (if is_64_bit_platform then 62 else 30) in
    if result >= max_int - 255 then
      (* On a 64-bit box, [float x > max_int] when [x >= max_int - 255], so
         [iround (float x)] would be out of bounds.  So we try again.  This branch of code
         runs with probability 6e-17 :-)  As such, we have some fixed tests in
         [extremities_test] above, to ensure that we do always check some examples in
         that range. *)
      absirand ()
    else
      result

  (* -max_int <= frand () <= max_int *)
  let frand () =
    let x = (float (absirand ())) +. Random.float 1.0 in
    if Random.bool () then
      -1.0 *. x
    else
      x

  let randoms = List.init ~f:(fun _ -> frand ()) 10_000

  let%test _ = List.for_all randoms ~f:iround_up_vs_down_test
  let%test _ = List.for_all randoms ~f:iround_nearest_test
  let%test _ = List.for_all randoms ~f:iround_down_test
  let%test _ = List.for_all randoms ~f:iround_up_test
  let%test _ = List.for_all randoms ~f:iround_towards_zero_test
  let%test _ = List.for_all randoms ~f:round_test
  let%test _ = special_values_test ()
  let%test _ = iround_nearest_test (of_int max_int)
  let%test _ = iround_nearest_test (of_int min_int)
end)

module Test_bounds (
    I : sig
      type t
      val num_bits : int
      val of_float : float -> t
      val to_int64 : t -> Int64.t
      val max_value : t
      val min_value : t
    end
  ) = struct
  open I

  let float_lower_bound = lower_bound_for_int num_bits
  let float_upper_bound = upper_bound_for_int num_bits

  let%test_unit "lower bound is valid" = ignore (of_float float_lower_bound : t)
  let%test_unit "upper bound is valid" = ignore (of_float float_upper_bound : t)

  let does_raise f x = try ignore (f x : t); false with _ -> true

  let%test "smaller than lower bound is not valid" =
    does_raise of_float (one_ulp `Down float_lower_bound)
  let%test "bigger than upper bound is not valid" =
    does_raise of_float (one_ulp `Up float_upper_bound)

  let%test "smaller than lower bound overflows" =
    let lower_bound = Int64.of_float float_lower_bound in
    let lower_bound_minus_epsilon = Int64.of_float (one_ulp `Down float_lower_bound) in
    let min_value = to_int64 min_value in
    if Base_int.(=) num_bits 64
    (* We cannot detect overflow because on Intel overflow results in min_value. *)
    then true
    else begin
      assert (Base_int64.(<=) lower_bound_minus_epsilon lower_bound);
      (* a value smaller than min_value would overflow if converted to [t] *)
      Base_int64.(<) lower_bound_minus_epsilon min_value
    end

  let%test "bigger than upper bound overflows" =
    let upper_bound = Int64.of_float float_upper_bound in
    let upper_bound_plus_epsilon = Int64.of_float (one_ulp `Up float_upper_bound) in
    let max_value = to_int64 max_value in
    if Base_int.(=) num_bits 64
    (* upper_bound_plus_epsilon is not representable as a Int64.t, it has overflowed *)
    then Base_int64.(<) upper_bound_plus_epsilon upper_bound
    else begin
      assert (Base_int64.(>=) upper_bound_plus_epsilon upper_bound);
      (* a value greater than max_value would overflow if converted to [t] *)
      Base_int64.(>) upper_bound_plus_epsilon max_value
    end
end

let%test_module "Base_int"        = (module Test_bounds(Base_int))
let%test_module "Base_int32"      = (module Test_bounds(Base_int32))
let%test_module "Base_int63"      = (module Test_bounds(Base_int63))
let%test_module "Base_int63_emul" = (module Test_bounds(Base_int63_emul))
let%test_module "Base_int64"      = (module Test_bounds(Base_int64))
let%test_module "Base_nativeint"  = (module Test_bounds(Base_nativeint))
