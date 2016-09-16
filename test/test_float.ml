open! Import
open! Float

let exponent_bits = 11
let mantissa_bits = 52

let exponent_mask64 = Int64.((shift_left one exponent_bits) - one)
let exponent_mask = Int64.to_int_exn exponent_mask64
let mantissa_mask = Int63.((shift_left one mantissa_bits) - one)
let _mantissa_mask64 = Int63.to_int64 mantissa_mask

let test_both_ways (a : t) (b : int64) =
  Int64.(=) (to_int64_preserve_order_exn a) b && Float.(=) (of_int64_preserve_order b) a
;;

let%test _ = test_both_ways          0.  0L
let%test _ = test_both_ways        (-0.) 0L
let%test _ = test_both_ways          1.  Int64.(shift_left 1023L 52)
let%test _ = test_both_ways        (-2.) Int64.(neg (shift_left 1024L 52))
let%test _ = test_both_ways     infinity Int64.(shift_left 2047L 52)
let%test _ = test_both_ways neg_infinity Int64.(neg (shift_left 2047L 52))

let%test _ = one_ulp `Down infinity = max_finite_value
let%test _ = is_nan (one_ulp `Up infinity)
let%test _ = is_nan (one_ulp `Down neg_infinity)
let%test _ = one_ulp `Up neg_infinity = ~- max_finite_value

(* Some tests to make sure that the compiler is generating code for handling subnormal
   numbers at runtime accurately. *)
let x () = min_positive_subnormal_value
let y () = min_positive_normal_value

let%test _ = test_both_ways  (x ())  1L
let%test _ = test_both_ways  (y ())  Int64.(shift_left 1L 52)

let%test _ = x () > 0.
let%test_unit _ = [%test_result: float] (x () / 2.) ~expect:0.

let%test _ = one_ulp `Up 0. = x ()
let%test _ = one_ulp `Down 0. = ~- (x ())

let are_one_ulp_apart a b = one_ulp `Up a = b

let%test _ = are_one_ulp_apart (x ()) (2. * x ())
let%test _ = are_one_ulp_apart (2. * x ()) (3. * x ())

let one_ulp_below_y () = y () - x ()
let%test _ = one_ulp_below_y () < y ()
let%test _ = y () - one_ulp_below_y () = x ()
let%test _ = are_one_ulp_apart (one_ulp_below_y ()) (y ())

let one_ulp_above_y () = y () + x ()
let%test _ = y () < one_ulp_above_y ()
let%test _ = one_ulp_above_y () - y () = x ()
let%test _ = are_one_ulp_apart (y ()) (one_ulp_above_y ())

let%test _ = not (are_one_ulp_apart (one_ulp_below_y ()) (one_ulp_above_y ()))

(* [2 * min_positive_normal_value] is where the ulp increases for the first time. *)
let z () = 2. * y ()
let one_ulp_below_z () = z () - x ()
let%test _ = one_ulp_below_z () < z ()
let%test _ = z () - one_ulp_below_z () = x ()
let%test _ = are_one_ulp_apart (one_ulp_below_z ()) (z ())

let one_ulp_above_z () = z () + 2. * x ()
let%test _ = z () < one_ulp_above_z ()
let%test _ = one_ulp_above_z () - z () = 2. * x ()
let%test _ = are_one_ulp_apart (z ()) (one_ulp_above_z ())

let%test_module "clamp" =
  (module struct
    let%test _ = clamp_exn 1.0 ~min:2. ~max:3. = 2.
    let%test _ = clamp_exn 2.5 ~min:2. ~max:3. = 2.5
    let%test _ = clamp_exn 3.5 ~min:2. ~max:3. = 3.

    let%test_unit "clamp" =
      [%test_result: float Or_error.t] (clamp 3.5 ~min:2. ~max:3.) ~expect:(Ok 3.)

    let%test_unit "clamp nan" =
      [%test_result: float Or_error.t] (clamp nan ~min:2. ~max:3.) ~expect:(Ok nan)

    let%test "clamp bad" = Or_error.is_empty (clamp 2.5 ~min:3. ~max:2.)
  end)

let%test_unit _ =
  [%test_result: Int64.t]
    (Int64.bits_of_float 1.1235582092889474E+307) ~expect:0x7fb0000000000000L

let%test_module "IEEE" =
  (module struct
    (* Note: IEEE 754 defines NaN values to be those where the exponent is all 1s and the
       mantissa is nonzero.  test_result<t> sees nan values as equal because it is based
       on [compare] rather than [=].  (If [x] and [x'] are nan, [compare x x'] returns 0,
       whereas [x = x'] returns [false].  This is the case regardless of whether or not
       [x] and [x'] are bit-identical values of nan.)  *)
    let f (t : t) (negative : bool) (exponent : int) (mantissa : Int63.t) : unit =
      let str = to_string_round_trippable t in
      let is_nan = is_nan t in
      (* the sign doesn't matter when nan *)
      if not is_nan then
        [%test_result: bool] ~message:("ieee_negative " ^ str)
          (ieee_negative t) ~expect:negative;
      [%test_result: int] ~message:("ieee_exponent " ^ str)
        (ieee_exponent t) ~expect:exponent;
      if is_nan
      then assert (Int63.(zero <> ieee_mantissa t))
      else [%test_result: Int63.t] ~message:("ieee_mantissa " ^ str)
             (ieee_mantissa t) ~expect:mantissa;
      [%test_result: t]
        ~message:(Printf.sprintf !"create_ieee ~negative:%B ~exponent:%d ~mantissa:%{Int63}"
                    negative exponent mantissa)
        (create_ieee_exn ~negative ~exponent ~mantissa)
        ~expect:t

    let%test_unit _ =
      let (!!) x = Int63.of_int x in
      f zero                         false 0                                 (!! 0);
      f min_positive_subnormal_value false 0                                 (!! 1);
      f min_positive_normal_value    false 1                                 (!! 0);
      f epsilon_float                false Pervasives.(1023 - mantissa_bits) (!! 0);
      f one                          false 1023                              (!! 0);
      f minus_one                    true  1023                              (!! 0);
      f max_finite_value             false Pervasives.(exponent_mask - 1)    mantissa_mask;
      f infinity                     false exponent_mask                     (!! 0);
      f neg_infinity                 true  exponent_mask                     (!! 0);
      f nan                          false exponent_mask                     (!! 1)

    (* test the normalized case, that is, 1 <= exponent <= 2046 *)
    let%test_unit _ =
      let g ~negative ~exponent ~mantissa =
        assert (create_ieee_exn ~negative ~exponent
                  ~mantissa:(Int63.of_int64_exn mantissa)
                =
                (if negative then -1. else 1.)
                * 2. ** (Float.of_int exponent - 1023.)
                * (1. + (2. ** -52.) * Int64.to_float mantissa))
      in
      g ~negative:false ~exponent:1 ~mantissa:147L;
      g ~negative:true ~exponent:137 ~mantissa:13L;
      g ~negative:false ~exponent:1015 ~mantissa:1370001L;
      g ~negative:true ~exponent:2046 ~mantissa:137000100945L
  end)
