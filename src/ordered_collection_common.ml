open! Import0
module Error = Error0
module Result = Result0
include Ordered_collection_common0

let get_pos_len ?pos ?len () ~total_length =
  try Result.Ok (get_pos_len_exn () ?pos ?len ~total_length) with
  | Invalid_argument s -> Error (Error.of_string s)
;;
