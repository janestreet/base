open! Import
module Sexp = Sexp0
module _ = Popcount

let raise_s = Error.raise_s

module Repr = Int63_emul.Repr
include Sys0.Make_immediate64 (Int) (Int63_emul)

module Backend = struct
  module type S = sig @@ portable
      type t [@@deriving globalize]

      include Int.S with type t := t
      include Replace_polymorphic_compare.S with type t := t

      val of_int : int -> t
      val to_int : local_ t -> int option
      val to_int_trunc : local_ t -> int
      val of_int32 : local_ int32 -> t
      val to_int32 : local_ t -> int32 option
      val to_int32_trunc : local_ t -> int32
      val of_int64 : local_ int64 -> t option
      val of_int64_exn : local_ int64 -> t
      val of_int64_trunc : local_ int64 -> t
      val of_nativeint : local_ nativeint -> t option
      val to_nativeint : local_ t -> nativeint option
      val of_nativeint_trunc : local_ nativeint -> t
      val to_nativeint_trunc : local_ t -> nativeint
      val of_float_unchecked : local_ float -> t
      val repr : (t, t) Int63_emul.Repr.t
      val bswap16 : local_ t -> t
      val bswap32 : local_ t -> t
      val bswap48 : local_ t -> t
    end
    with type t := t

  module Native = struct
    include Int

    let to_int (x : t) = Some x
    let to_int_trunc (x : t) = x
    let to_int32 x = to_int32 x
    let to_int32_trunc x = to_int32_trunc x

    (* [of_local_int32_exn] is a safe operation on platforms with 64-bit word sizes. *)
    let of_int32 x = globalize_int (of_local_int32_exn x)
    let to_nativeint_trunc x = to_nativeint x
    let to_nativeint x = Some (to_nativeint x)
    let repr = Int63_emul.Repr.Int
    let bswap16 = (bswap16 :> local_ t -> t)
    let bswap32 t = Int64.to_int_trunc (Int64.bswap32 (Int64.of_int t))
    let bswap48 t = Int64.to_int_trunc (Int64.bswap48 (Int64.of_int t))
  end

  let impl : (module S) =
    match repr with
    | Immediate -> (module Native : S)
    | Non_immediate -> (module Int63_emul : S)
  ;;
end

include (val Backend.impl : Backend.S)

module Overflow_exn = struct
  let ( + ) t u =
    let sum = t + u in
    if bit_or (bit_xor t u) (bit_xor t (bit_not sum)) < zero
    then sum
    else
      raise_s
        (Sexp.message
           "( + ) overflow"
           [ "t", sexp_of_t t; "u", sexp_of_t u; "sum", sexp_of_t sum ])
  ;;

  let ( - ) t u =
    let diff = t - u in
    let pos_diff = t > u in
    if t <> u && Bool.( <> ) pos_diff (is_positive diff)
    then
      raise_s
        (Sexp.message
           "( - ) overflow"
           [ "t", sexp_of_t t; "u", sexp_of_t u; "diff", sexp_of_t diff ])
    else diff
  ;;

  let negative_one = of_int (-1)
  let div_would_overflow t u = t = min_value && u = negative_one

  let ( * ) t u =
    let product = t * u in
    if u <> zero && (div_would_overflow product u || product / u <> t)
    then
      raise_s
        (Sexp.message
           "( * ) overflow"
           [ "t", sexp_of_t t; "u", sexp_of_t u; "product", sexp_of_t product ])
    else product
  ;;

  let ( / ) t u =
    if div_would_overflow t u
    then
      raise_s
        (Sexp.message
           "( / ) overflow"
           [ "t", sexp_of_t t; "u", sexp_of_t u; "product", sexp_of_t (t / u) ])
    else t / u
  ;;

  let abs t = if t = min_value then failwith "abs overflow" else abs t
  let abs_local t = if t = min_value then failwith "abs_local overflow" else abs_local t
  let neg t = if t = min_value then failwith "neg overflow" else neg t
end

let () = assert (Int.( = ) (num_bits |> to_int_trunc) 63)

let random_of_int ?(state = Random.State.get_default ()) bound =
  of_int (Random.State.int state (to_int_exn bound))
;;

let random_of_int64 ?(state = Random.State.get_default ()) bound =
  of_int64_exn (Random.State.int64 state (to_int64 bound))
;;

let random =
  match Word_size.word_size with
  | W64 -> random_of_int
  | W32 -> random_of_int64
;;

let random_incl_of_int ?(state = Random.State.get_default ()) lo hi =
  of_int (Random.State.int_incl state (to_int_exn lo) (to_int_exn hi))
;;

let random_incl_of_int64 ?(state = Random.State.get_default ()) lo hi =
  of_int64_exn (Random.State.int64_incl state (to_int64 lo) (to_int64 hi))
;;

let random_incl =
  match Word_size.word_size with
  | W64 -> random_incl_of_int
  | W32 -> random_incl_of_int64
;;

module Private = struct
  module Repr = Repr

  let repr = repr

  module Emul = Int63_emul
end
