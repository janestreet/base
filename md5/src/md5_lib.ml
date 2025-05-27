type t = string

(* Share the digest of the empty string *)
let empty = Digest.string ""
let make s = if s = empty then empty else s
let make_local s = if s = empty then empty else s
let compare__local = compare
let compare = compare
let length = 16
let to_binary s = s
let to_binary_local s = s

let of_binary_exn s =
  assert (String.length s = length);
  make s
;;

let unsafe_of_binary = make
let unsafe_of_binary_local = make_local
let unsafe_of_binary__local = make_local

external globalize : local_ t -> t @@ portable = "%obj_dup"

let to_hex = Digest.to_hex
let of_hex_exn s = make (Digest.from_hex s)
let string s = make (Digest.string s)
let bytes s = make (Digest.bytes s)
let subbytes bytes ~pos ~len = make (Digest.subbytes bytes pos len)
