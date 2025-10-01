(* This is broken off to avoid circular dependency between Sign and Comparable. *)

open! Import

type t =
  | Neg
  | Zero
  | Pos
[@@deriving sexp ~stackify, sexp_grammar, compare ~localize, hash, enumerate]

module Replace_polymorphic_compare = struct
  let ( < ) (x : t) y = Poly.( < ) x y
  let ( <= ) (x : t) y = Poly.( <= ) x y
  let ( <> ) (x : t) y = Poly.( <> ) x y
  let ( = ) (x : t) y = Poly.( = ) x y
  let ( > ) (x : t) y = Poly.( > ) x y
  let ( >= ) (x : t) y = Poly.( >= ) x y
  let ascending (x : t) y = Poly.ascending x y
  let descending (x : t) y = Poly.descending x y
  let compare (x : t) y = Poly.compare x y
  let compare__local (local_ (x : t)) (local_ y) = Poly.compare x y
  let equal (x : t) y = Poly.equal x y
  let equal__local (local_ (x : t)) (local_ y) = Poly.equal x y
  let max (x : t) y = if x >= y then x else y
  let min (x : t) y = if x <= y then x else y
end

let of_string s = t_of_sexp (sexp_of_string s)
let to_string t = string_of_sexp (sexp_of_t t)

external magic_transparent : t -> int @@ portable = "%identity"

let to_int t = magic_transparent t - 1
let _ = hash

(* Ignore the hash function produced by [@@deriving hash] *)
let hash = to_int
let module_name = "Base.Sign"
let of_int n = Bool0.select (n < 0) Neg (Bool0.select (n = 0) Zero Pos)
