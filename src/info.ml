open! Import
include Info0

module Utf8 = struct
  let to_string_hum t =
    match Internal_repr.of_info t with
    | String s -> s
    | _message -> sexp_of_t t |> Sexp.Utf8.to_string_hum |> String.Utf8.to_string
  ;;

  let to_string_mach t = sexp_of_t t |> Sexp.Utf8.to_string_mach |> String.Utf8.to_string
end
