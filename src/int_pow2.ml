(** While I would have preferred to put these functions with Int, there is a
    dependency between Hashtbl, where it is used, and Int that prevents it. *)

open! Import
open! Sexplib.Conv

module Sys = Sys0


let raise_s = Error.raise_s

(** Common bit-twiddling hacks for computing floor/ceiling power of 2, without a built in
    fast bsr (bit scan reverse). As some have observed, these would return 0 for values of
    0, and do not support negative numbers *)
let non_positive_argument () =
  Printf.invalid_argf "argument must be strictly positive" ()

(** "ceiling power of 2" - Least power of 2 greater than or equal to x. *)
let ceil_pow2 x =
  if (x <= 0) then non_positive_argument ();
  let x = x - 1 in
  let x = x lor (x lsr 1) in
  let x = x lor (x lsr 2) in
  let x = x lor (x lsr 4) in
  let x = x lor (x lsr 8) in
  let x = x lor (x lsr 16) in
  let x = x lor (x lsr 32) in
  x + 1

(** "floor power of 2" - Largest power of 2 less than or equal to x. *)
let floor_pow2 x =
  if (x <= 0) then non_positive_argument ();
  let x = x lor (x lsr 1) in
  let x = x lor (x lsr 2) in
  let x = x lor (x lsr 4) in
  let x = x lor (x lsr 8) in
  let x = x lor (x lsr 16) in
  let x = x lor (x lsr 32) in
  x - (x lsr 1)

let is_pow2 x =
  if x <= 0 then non_positive_argument ();
  (x land (x-1)) = 0
;;

(* C stub for int clz to use the CLZ/BSR instruction where possible *)
external int_clz : int -> int = "Base_int_math_int_clz" [@@noalloc]

let floor_log2 i =
  if i <= 0 then
    raise_s (Sexp.message "[Int.floor_log2] got invalid input"
               ["", sexp_of_int i]);
  Sys.word_size_in_bits - 1 - int_clz i
;;

let ceil_log2 i =
  let r = floor_log2 i in
  if 1 lsl r = i
  then r
  else r + 1
;;
