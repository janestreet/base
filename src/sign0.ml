(* This is broken off to avoid circular dependency between Sign and Comparable. *)

open! Import

type t = Neg | Zero | Pos [@@deriving sexp, compare, hash, enumerate]

let of_string s = t_of_sexp (sexp_of_string s)
let to_string t = string_of_sexp (sexp_of_t t)

let to_int = function
  | Neg  -> -1
  | Zero ->  0
  | Pos  ->  1

let hash = to_int

let module_name = "Base.Sign"

let of_int n =
  if n < 0
  then Neg
  else if n = 0
  then Zero
  else Pos
