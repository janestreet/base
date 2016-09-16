open! Import

let failwithf = Base_printf.failwithf
let invalid_argf = Base_printf.invalid_argf

let negative_exponent () =
  Base_printf.invalid_argf "exponent can not be negative" ()

let overflow () =
  Base_printf.invalid_argf "integer overflow in pow" ()

(* To implement [int64_pow], we use C code rather than OCaml to eliminate allocation. *)
external int_math_int_pow   : int   -> int   -> int   = "int_math_int_pow_stub" [@@noalloc]
external int_math_int64_pow : int64 -> int64 -> int64 = "int_math_int64_pow_stub"

let int_pow base exponent =
  if exponent < 0 then negative_exponent ();

  if abs(base) > 1 &&
     (exponent > 63 ||
      abs(base) > Pow_overflow_bounds.int_positive_overflow_bounds.(exponent))
  then overflow ();

  int_math_int_pow base exponent
;;

(* we don't do [abs] in int64 case to avoid allocation *)
let int64_pow base exponent =
  if exponent < 0L then negative_exponent ();

  if (base > 1L || base < (-1L)) &&
     (exponent > 63L ||
      (base >= 0L &&
       base > Pow_overflow_bounds.int64_positive_overflow_bounds.(Int64.to_int exponent))
      ||
      (base < 0L &&
       base < Pow_overflow_bounds.int64_negative_overflow_bounds.(Int64.to_int exponent)))
  then overflow ();

  int_math_int64_pow base exponent
;;

let int63_pow_on_int64 base exponent =
  if exponent < 0L then negative_exponent ();

  if Int64.abs(base) > 1L &&
     (exponent > 63L ||
      Int64.abs(base) > Pow_overflow_bounds.int63_on_int64_positive_overflow_bounds.(Int64.to_int exponent))
  then overflow ();

  int_math_int64_pow base exponent
;;

let%test_unit _ =
  let x = match Word_size.word_size with W32 -> 9 | W64 -> 10 in
  for i = 0 to x do
    for j = 0 to x do
      assert (int_pow i j
              = Pervasives.(int_of_float ((float_of_int i) ** (float_of_int j))))
    done
  done

module type T = sig
  type t
  include Floatable.S  with type t := t
  include Stringable.S with type t := t

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( ~- ) : t -> t
  include Polymorphic_compare_intf.Infix with type t := t

  val abs    : t -> t
  val neg    : t -> t
  val zero   : t
  val of_int_exn : int -> t
  val rem : t -> t -> t
end

module Make (X : T) = struct
  open X

  let ( % ) x y =
    if y <= zero then
      invalid_argf
        "%s %% %s in core_int.ml: modulus should be positive"
        (to_string x) (to_string y) ();
    let rval = X.rem x y in
    if rval < zero
    then rval + y
    else rval
  ;;

  let one = of_int_exn 1
  ;;

  let ( /% ) x y =
    if y <= zero then
      invalid_argf
        "%s /%% %s in core_int.ml: divisor should be positive"
        (to_string x) (to_string y) ();
    if x < zero
    then (x + one) / y - one
    else x / y
  ;;

  (** float division of integers *)
  let (//) x y = to_float x /. to_float y
  ;;

  let round_down i ~to_multiple_of:modulus = i - (i % modulus)
  ;;

  let round_up i ~to_multiple_of:modulus =
    let remainder = i % modulus in
    if remainder = zero
    then i
    else i + modulus - remainder
  ;;

  let round_towards_zero i ~to_multiple_of =
    if i = zero then zero else
    if i > zero
    then round_down i ~to_multiple_of
    else round_up   i ~to_multiple_of
  ;;

  let round_nearest i ~to_multiple_of:modulus =
    let remainder = i % modulus in
    if remainder * of_int_exn 2 < modulus
    then i - remainder
    else i - remainder + modulus
  ;;

  let round ?(dir=`Nearest) i ~to_multiple_of =
    match dir with
    | `Nearest -> round_nearest      i ~to_multiple_of
    | `Down    -> round_down         i ~to_multiple_of
    | `Up      -> round_up           i ~to_multiple_of
    | `Zero    -> round_towards_zero i ~to_multiple_of
  ;;

  let%test_module "integer-rounding" = (module struct

    let check dir ~range:(lower, upper) ~modulus expected =
      let modulus = of_int_exn modulus in
      let expected = of_int_exn expected in
      for i = lower to upper do
        let observed = round ~dir ~to_multiple_of:modulus (of_int_exn i) in
        if observed <> expected then failwithf "invalid result for i = %d" i ()
      done
    ;;

    let%test_unit _ = check ~modulus:10 `Down    ~range:( 10,  19)   10
    let%test_unit _ = check ~modulus:10 `Down    ~range:(  0,   9)    0
    let%test_unit _ = check ~modulus:10 `Down    ~range:(-10,  -1) (-10)
    let%test_unit _ = check ~modulus:10 `Down    ~range:(-20, -11) (-20)

    let%test_unit _ = check ~modulus:10 `Up      ~range:( 11,  20)   20
    let%test_unit _ = check ~modulus:10 `Up      ~range:(  1,  10)   10
    let%test_unit _ = check ~modulus:10 `Up      ~range:( -9,   0)    0
    let%test_unit _ = check ~modulus:10 `Up      ~range:(-19, -10) (-10)

    let%test_unit _ = check ~modulus:10 `Zero    ~range:( 10,  19)   10
    let%test_unit _ = check ~modulus:10 `Zero    ~range:( -9,   9)    0
    let%test_unit _ = check ~modulus:10 `Zero    ~range:(-19, -10) (-10)

    let%test_unit _ = check ~modulus:10 `Nearest ~range:( 15,  24)   20
    let%test_unit _ = check ~modulus:10 `Nearest ~range:(  5,  14)   10
    let%test_unit _ = check ~modulus:10 `Nearest ~range:( -5,   4)    0
    let%test_unit _ = check ~modulus:10 `Nearest ~range:(-15,  -6) (-10)
    let%test_unit _ = check ~modulus:10 `Nearest ~range:(-25, -16) (-20)

    let%test_unit _ = check ~modulus:5 `Nearest ~range:(  8, 12)   10
    let%test_unit _ = check ~modulus:5 `Nearest ~range:(  3,  7)    5
    let%test_unit _ = check ~modulus:5 `Nearest ~range:( -2,  2)    0
    let%test_unit _ = check ~modulus:5 `Nearest ~range:( -7, -3)  (-5)
    let%test_unit _ = check ~modulus:5 `Nearest ~range:(-12, -8) (-10)
  end)

  let%test_module "remainder-and-modulus" = (module struct

    let check_integers x y =
      let check_raises f desc =
        match f () with
        | exception _ -> ()
        | z -> failwithf "%s: failed for x = %s, y = %s; produced %s rather than raising"
                 desc (to_string x) (to_string y) (to_string z) ()
      in
      let check_true cond desc =
        if not cond
        then failwithf "%s: failed for x = %s, y = %s" desc (to_string x) (to_string y) ()
      in
      if y = zero
      then
        begin
          check_raises (fun () -> x / y) "division by zero";
          check_raises (fun () -> rem x y) "rem _ zero";
          check_raises (fun () -> x % y) "_ % zero";
          check_raises (fun () -> x /% y) "_ /% zero";
        end
      else
        begin
          if x < zero
          then check_true (rem x y <= zero) "non-positive remainder"
          else check_true (rem x y >= zero) "non-negative remainder";
          check_true (abs (rem x y) <= abs y - one) "range of remainder";
          if y < zero then begin
            check_raises (fun () -> x % y) "_ % negative";
            check_raises (fun () -> x /% y) "_ /% negative"
          end
          else begin
            check_true (x = (x /% y) * y + (x % y)) "(/%) and (%) identity";
            check_true (x = (x /  y) * y + (rem x y)) "(/) and rem identity";
            check_true (x % y >= zero) "non-negative (%)";
            check_true (x % y <= y - one) "range of (%)";
            if x > zero && y > zero
            then begin
              check_true (x /% y = x / y) "(/%) and (/) identity";
              check_true (x % y = rem x y) "(%) and rem identity"
            end;
          end
        end
    ;;

    let check_natural_numbers x y =
      Base_list.iter [ x ; -x ; x+one ; -(x + one) ] ~f:(fun x ->
        Base_list.iter [ y ; -y ; y+one ; -(y + one) ] ~f:(fun y ->
          check_integers x y))

    let%test_unit "deterministic" =
      let big1 = of_int_exn 118_310_344 in
      let big2 = of_int_exn 828_172_408 in
      (* Important to test the case where one value is a multiple of the other.  Note that
         the [x + one] and [y + one] cases in [check_natural_numbers] ensure that we also
         test non-multiple cases. *)
      assert (big2 = big1 * of_int_exn 7);
      let values = [ zero ; one ; big1 ; big2 ] in
      Base_list.iter values ~f:(fun x ->
        Base_list.iter values ~f:(fun y ->
          check_natural_numbers x y))

    let%test_unit "random" =
      let rand = Base_random.State.make [| 8; 67; -5_309 |] in
      for _ = 0 to 1_000 do
        let max_value = 1_000_000_000 in
        let x = of_int_exn (Base_random.State.int rand max_value) in
        let y = of_int_exn (Base_random.State.int rand max_value) in
        check_natural_numbers x y
      done
  end)

end

let%test_module "pow" = (module struct
  let%test _ = int_pow 0  0 = 1
  let%test _ = int_pow 0  1 = 0
  let%test _ = int_pow 10 1 = 10
  let%test _ = int_pow 10 2 = 100
  let%test _ = int_pow 10 3 = 1_000
  let%test _ = int_pow 10 4 = 10_000
  let%test _ = int_pow 10 5 = 100_000
  let%test _ = int_pow 2 10 = 1024

  let%test _ = int_pow 0 1_000_000 = 0
  let%test _ = int_pow 1 1_000_000 = 1
  let%test _ = int_pow (-1) 1_000_000 = 1
  let%test _ = int_pow (-1) 1_000_001 = -1

  let%test _ = int64_pow 0L 0L = 1L
  let%test _ = int64_pow 0L 1_000_000L = 0L
  let%test _ = int64_pow 1L 1_000_000L = 1L
  let%test _ = int64_pow (-1L) 1_000_000L = 1L
  let%test _ = int64_pow (-1L) 1_000_001L = -1L

  let%test _ = int64_pow 10L 1L  = 10L
  let%test _ = int64_pow 10L 2L  = 100L
  let%test _ = int64_pow 10L 3L  = 1_000L
  let%test _ = int64_pow 10L 4L  = 10_000L
  let%test _ = int64_pow 10L 5L  = 100_000L
  let%test _ = int64_pow 2L  10L = 1_024L
  let%test _ = int64_pow 5L  27L = 7450580596923828125L

  let exception_thrown pow b e = try let _ = pow b e in false with _ -> true;;

  let%test _ = exception_thrown int_pow 10 60
  let%test _ = exception_thrown int64_pow 10L 60L
  let%test _ = exception_thrown int_pow 10 (-1)
  let%test _ = exception_thrown int64_pow 10L (-1L)

  let%test _ = exception_thrown int64_pow 2L 63L
  let%test _ = not (exception_thrown int64_pow 2L 62L)

  let%test _ = exception_thrown int64_pow (-2L) 63L
  let%test _ = not (exception_thrown int64_pow (-2L) 62L)
end)
