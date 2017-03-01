open! Import

module Sys = Sys0

type t = W32 | W64 [@@deriving_inline sexp_of]
let sexp_of_t : t -> Sexplib.Sexp.t =
  function
  | W32  -> Sexplib.Sexp.Atom "W32"
  | W64  -> Sexplib.Sexp.Atom "W64"
[@@@end]

let num_bits = function W32 -> 32 | W64 -> 64

let word_size =
  match Sys.word_size_in_bits with
  | 32 -> W32
  | 64 -> W64
  | _ -> failwith "unknown word size"
;;
