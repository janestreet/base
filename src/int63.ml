open! Import

let raise_s = Error.raise_s
module Repr = Int63_emul.Repr

include Int63_backend

module Overflow_exn = struct
  let ( + ) t u =
    let sum = t + u in
    if bit_or (bit_xor t u) (bit_xor t (bit_not sum)) < zero
    then sum
    else raise_s (Sexp.message "( + ) overflow"
                    [ "t"  , sexp_of_t t
                    ; "u"  , sexp_of_t u
                    ; "sum", sexp_of_t sum
                    ])
  ;;

  let ( - ) t u =
    let diff = t - u in
    let pos_diff = t > u in
    if t <> u && Bool.(<>) pos_diff (is_positive diff) then
      raise_s (Sexp.message "( - ) overflow"
                 [ "t"   , sexp_of_t t
                 ; "u"   , sexp_of_t u
                 ; "diff", sexp_of_t diff
                 ])
    else diff
  ;;

  let abs t = if t = min_value then failwith "abs overflow" else abs t
  let neg t = if t = min_value then failwith "neg overflow" else neg t
end

let () = assert (Int.(=) num_bits 63)

(* Even for ARCH_SIXTYFOUR, we can't use Random.State.int, because the bound is very
   limited in range.  We actually want a bound that spans the type. *)
let random ?(state = Random.State.default) bound =
  of_int64_exn (Random.State.int64 state (to_int64 bound))
;;

module Private = struct
  module Repr = Repr
  let repr = repr
end
