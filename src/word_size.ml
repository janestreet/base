open! Import
module Sys = Sys0

type t =
  | W32
  | W64
[@@deriving sexp_of ~stackify]

let num_bits = function
  | W32 -> 32
  | W64 -> 64
;;

let word_size =
  match Sys.word_size_in_bits with
  | 32 -> W32
  | 64 -> W64
  | _ -> failwith "unknown word size"
;;
