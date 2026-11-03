(* [Int0] defines integer functions that are primitives or can be simply defined in terms
   of [Stdlib]. [Int0] is intended to completely express the part of [Stdlib] that [Base]
   uses for integers -- no other file in Base other than int0.ml should use these
   functions directly through [Stdlib]. [Int0] has few dependencies, and so is available
   early in Base's build order.

   All Base files that need to use ints and come before [Base.Int] in build order should
   do:

   {[
     module Int = Int0
   ]}

   Defining [module Int = Int0] is also necessary because it prevents ocamldep from
   mistakenly causing a file to depend on [Base.Int]. *)

external format : string -> int -> string = "caml_format_int"

let to_string n = Integer_to_string.int_to_string n

external of_string : string -> int = "caml_int_of_string"

let of_string_opt s =
  try Some (of_string s) with
  | Failure _ -> None
;;

let to_float = Stdlib.float_of_int
let of_float = Stdlib.int_of_float
let max_value = Stdlib.max_int
let min_value = Stdlib.min_int
let succ = Stdlib.succ
let pred = Stdlib.pred

module Pow2 = struct
  let num_bits = Stdlib.Sys.int_size
  let non_positive_argument () = invalid_arg "argument must be strictly positive"

  (** "ceiling power of 2 - 1" - Least power of 2 minus 1 greater than or equal to x. *)
  let ceil_pow2_minus_1 n =
    (* This does not let [Nativeint]s escape and allocations are optimized away.
       [Stdlib.Nativeint] is used to avoid a dependency cycle. *)
    let open Stdlib.Nativeint in
    let n = of_int n in
    let n = logor n (shift_right_logical n 1) in
    let n = logor n (shift_right_logical n 2) in
    let n = logor n (shift_right_logical n 4) in
    let n = logor n (shift_right_logical n 8) in
    let n = logor n (shift_right_logical n 16) in
    to_int
      (if (* This condition should get constant folded and there should be no branch. *)
          Stdlib.Sys.int_size > 32
       then logor n (shift_right_logical n 32)
       else n)
  ;;

  (** "ceiling power of 2" - Least power of 2 greater than or equal to x. *)
  let ceil_pow2 x =
    if x <= 0 then non_positive_argument ();
    let x = x - 1 in
    let x = ceil_pow2_minus_1 x in
    x + 1
  ;;

  (** "floor power of 2" - Largest power of 2 less than or equal to x. *)
  let floor_pow2 x =
    if x <= 0 then non_positive_argument ();
    let x = ceil_pow2_minus_1 x in
    x - (x lsr 1)
  ;;

  let is_pow2 x =
    if x <= 0 then non_positive_argument ();
    x land (x - 1) = 0
  ;;

  let clz = Ocaml_intrinsics_kernel.Int.count_leading_zeros
  let ctz = Ocaml_intrinsics_kernel.Int.count_trailing_zeros
  let[@cold] log2_bad_input i = invalid_arg ("Int.floor_log2: " ^ to_string i)

  (** Hacker's Delight Second Edition p106 *)
  let floor_log2 i = if i <= 0 then log2_bad_input i else num_bits - 1 - clz i

  let[@cold] log2_bad_input i = invalid_arg ("Int.ceil_log2: " ^ to_string i)

  let ceil_log2 i =
    if i <= 0 then log2_bad_input i;
    if i = 1 then 0 else num_bits - clz (i - 1)
  ;;
end

include Pow2
