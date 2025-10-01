open! Import

module T = struct
  type t =
    | Neg
    | Zero
    | Pos
    | Nan
  [@@deriving sexp ~stackify, sexp_grammar, compare ~localize, hash, enumerate]

  let of_string s = t_of_sexp (sexp_of_string s)
  let to_string t = string_of_sexp (sexp_of_t t)
  let module_name = "Base.Sign_or_nan"
end

module Replace_polymorphic_compare = struct
  let ( < ) (x : T.t) y = Poly.( < ) x y
  let ( <= ) (x : T.t) y = Poly.( <= ) x y
  let ( <> ) (x : T.t) y = Poly.( <> ) x y
  let ( = ) (x : T.t) y = Poly.( = ) x y
  let ( > ) (x : T.t) y = Poly.( > ) x y
  let ( >= ) (x : T.t) y = Poly.( >= ) x y
  let ascending (x : T.t) y = Poly.ascending x y
  let descending (x : T.t) y = Poly.descending x y
  let compare (x : T.t) y = Poly.compare x y
  let compare__local (local_ (x : T.t)) (local_ y) = Poly.compare x y
  let equal (x : T.t) y = Poly.equal x y
  let equal__local (local_ (x : T.t)) (local_ y) = Poly.equal x y
  let max (x : T.t) y = if x >= y then x else y
  let min (x : T.t) y = if x <= y then x else y
end

include T

include%template Identifiable.Make [@modality portable] (T)

(* Open [Replace_polymorphic_compare] after including functor applications so they do not
   shadow its definitions. This is here so that efficient versions of the comparison
   functions are available within this module. *)
open! Replace_polymorphic_compare

let of_sign = function
  | Sign.Neg -> Neg
  | Sign.Zero -> Zero
  | Sign.Pos -> Pos
;;

let to_sign_exn = function
  | Neg -> Sign.Neg
  | Zero -> Sign.Zero
  | Pos -> Sign.Pos
  | Nan -> invalid_arg "Base.Sign_or_nan.to_sign_exn: Nan"
;;

let of_int n = of_sign (Sign.of_int n)
let to_int_exn t = Sign.to_int (to_sign_exn t)

let flip = function
  | Neg -> Pos
  | Zero -> Zero
  | Pos -> Neg
  | Nan -> Nan
;;

let ( * ) t t' =
  match t, t' with
  | Nan, _ | _, Nan -> Nan
  | _ -> of_sign (Sign.( * ) (to_sign_exn t) (to_sign_exn t'))
;;

let to_string_hum = function
  | Neg -> "negative"
  | Zero -> "zero"
  | Pos -> "positive"
  | Nan -> "not-a-number"
;;

(* Include [Replace_polymorphic_compare] at the end, after any functor applications that
   could shadow its definitions. This is here so that efficient versions of the comparison
   functions are exported by this module. *)
include Replace_polymorphic_compare
