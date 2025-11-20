open! Import

(* Open replace_polymorphic_compare after including functor instantiations so they do not
   shadow its definitions. This is here so that efficient versions of the comparison
   functions are available within this module. *)
open! Float_replace_polymorphic_compare

let is_nan x = (x : float) <> x

(* An order-preserving bijection between all floats except for NaNs, and 99.95% of int64s.

   Note we don't distinguish 0. and -0. as separate values here, they both map to 0L,
   which maps back to 0.

   This should work both on little-endian and high-endian CPUs. Wikipedia says: "on modern
   standard computers (i.e., implementing IEEE 754), one may in practice safely assume
   that the endianness is the same for floating point numbers as for integers"
   (http://en.wikipedia.org/wiki/Endianness#Floating-point_and_endianness).

   N.b. the calls to [globalize_float] are no-ops at runtime since the float is unboxed
   and then consumed by [bits_of_float], so the compiler knows not to re-box it. *)
let to_int64_preserve_order (local_ t) =
  if is_nan t
  then None
  else if t = 0.
  then (* also includes -0. *)
    Some 0L
  else if t > 0.
  then Some (Stdlib.Int64.bits_of_float (globalize_float t))
  else Some (Stdlib.Int64.neg (Stdlib.Int64.bits_of_float (globalize_float (-.t))))
;;

let to_int64_preserve_order_exn (local_ x) = Option.value_exn (to_int64_preserve_order x)

(* N.b. the calls to [globalize_int64] are no-ops at runtime since the int64 is unboxed
   and then consumed by [float_of_bits], so the compiler knows not to re-box it. *)
let of_int64_preserve_order (local_ x) =
  if Int64_replace_polymorphic_compare.( >= ) x 0L
  then Stdlib.Int64.float_of_bits (globalize_int64 x)
  else ~-.(Stdlib.Int64.float_of_bits (globalize_int64 (Stdlib.Int64.neg x)))
;;

let one_ulp dir (local_ t) =
  match to_int64_preserve_order t with
  | None -> Stdlib.nan
  | Some x ->
    of_int64_preserve_order
      (Stdlib.Int64.add
         x
         (match dir with
          | `Up -> 1L
          | `Down -> -1L))
;;

(* [upper_bound_for_int] and [lower_bound_for_int] are for calculating the max/min float
   that fits in a given-size integer when rounded towards 0 (using [int_of_float]).

   max_int/min_int depend on [num_bits], e.g. +/- 2^30, +/- 2^62 if 31-bit, 63-bit
   (respectively) while float is IEEE standard for double (52 significant bits).

   In all cases, we want to guarantee that
   [lower_bound_for_int <= x <= upper_bound_for_int] iff [int_of_float x] fits in an int
   with [num_bits] bits.

   [2 ** (num_bits - 1)] is the first float greater that max_int, we use the preceding
   float as upper bound.

   [- (2 ** (num_bits - 1))] is equal to min_int. For lower bound we look for the smallest
   float [f] satisfying [f > min_int - 1] so that [f] rounds toward zero to [min_int]

   So in particular we will have: [lower_bound_for_int x <= - (2 ** (1-x))]
   [upper_bound_for_int x  <    2 ** (1-x) ]
*)
let upper_bound_for_int num_bits =
  let exp = Stdlib.float_of_int (num_bits - 1) in
  one_ulp `Down (2. ** exp)
;;

let is_x_minus_one_exact x =
  (* [x = x -. 1.] does not work with x87 floating point arithmetic backend (which is used
     on 32-bit ocaml) because of 80-bit register precision of intermediate computations.

     An alternative way of computing this: [x -. one_ulp `Down x <= 1.] is also prone to
     the same precision issues: you need to make sure [x] is 64-bit.
  *)
  let open Int64_replace_polymorphic_compare in
  not (Stdlib.Int64.bits_of_float x = Stdlib.Int64.bits_of_float (x -. 1.))
;;

let lower_bound_for_int num_bits =
  let exp = Stdlib.float_of_int (num_bits - 1) in
  let min_int_as_float = ~-.(2. ** exp) in
  let open Int_replace_polymorphic_compare in
  if num_bits - 1 < 53 (* 53 = #bits in the float's mantissa with sign included *)
  then (
    (* The smallest float that rounds towards zero to [min_int] is [min_int - 1 + epsilon] *)
    assert (is_x_minus_one_exact min_int_as_float);
    one_ulp `Up (min_int_as_float -. 1.))
  else (
    (* [min_int_as_float] is already the smallest float [f] satisfying [f > min_int - 1]. *)
    assert (not (is_x_minus_one_exact min_int_as_float));
    min_int_as_float)
;;

let box =
  (* Prevent potential constant folding of [+. 0.] in the near ocamlopt future. The reason
     we add -0 rather than 0 is that [x +. (-0.)] is apparently always the same as [x],
     whereas [x +. 0.] is not, in that it sends [-0.] to [0.].

     N.b. the call to [globalize_float] is a no-op at runtime since the float is unboxed
     and then consumed by addition, so the compiler knows not to re-box it. The
     implementation of flambda's heuristics is such that an arithmetic operation is
     necessary for the behavior we want; it is insufficient to simply globalize it. *)
  let x = Sys0.opaque_identity ~-.0. in
  fun (local_ f) -> globalize_float f +. x
;;
