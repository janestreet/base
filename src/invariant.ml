open! Import
include Invariant_intf.Definitions
module Sexp = Sexp0

let raise_s = Error.raise_s

[%%template
[@@@kind.default k = (value, float64, bits32, bits64, word, immediate, immediate64)]

let invariant ?(here = Stdlib.Lexing.dummy_pos) t sexp_of_t f : unit =
  try f () with
  | exn ->
    let fields = [ "exn", sexp_of_exn exn; "", sexp_of_t t ] in
    let fields =
      if Source_code_position0.is_dummy here
      then fields
      else ("", Source_code_position0.sexp_of_t here) :: fields
    in
    raise_s (Sexp.message "invariant failed" fields)
;;

let check_field t f field =
  try f ((Field.get [@kind k]) field t) with
  | exn ->
    raise_s
      (Sexp.message
         "problem with field"
         [ "field", sexp_of_string (Field.name field); "exn", sexp_of_exn exn ])
;;]
