open! Import

let raise_s = Error.raise_s

include Base_int63_backend

module Overflow_exn = struct
  let ( + ) t u =
    let sum = t + u in
    if bit_or (bit_xor t u) (bit_xor t (bit_not sum)) < zero
    then sum
    else raise_s [%message "( + ) overflow" (t : t) (u : t) (sum : t)]
  ;;

  let ( - ) t u =
    let diff = t - u in
    let pos_diff = t > u in
    if t <> u && Bool.(<>) pos_diff (is_positive diff) then
      raise_s [%message "( - ) overflow" (t : t) (u : t) (diff : t)]
    else diff
  ;;

  let abs t = if t = min_value then failwith "abs overflow" else abs t
  let neg t = if t = min_value then failwith "neg overflow" else neg t
end

let () = assert (Base_int.(=) num_bits 63)

(* Even for ARCH_SIXTYFOUR, we can't use Base_random.State.int, because the bound is very
   limited in range.  We actually want a bound that spans the type. *)
let random ?(state = Base_random.State.default) bound =
  of_int64_exn (Base_random.State.int64 state (to_int64 bound))
;;
