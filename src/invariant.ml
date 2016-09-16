open! Import

include Invariant_intf

let raise_s = Error.raise_s

let invariant here t sexp_of_t f : unit =
  try
    f ()
  with exn ->
    raise_s [%message
      "invariant failed"
        ~_:(here : Source_code_position0.t)
        (exn : exn)
        ~_:(t : t)]
;;

let check_field t f field =
  try
    f (Field.get field t)
  with exn ->
    raise_s [%message
      "problem with field" [%here]
        ~field:(Field.name field : string)
        (exn : exn)]
;;
