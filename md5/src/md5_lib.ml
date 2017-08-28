type t = string

let compare = compare

let length = 16

let to_binary s = s
let of_binary_exn s = assert (String.length s = length); s
let unsafe_of_binary s = s

let to_hex = Digest.to_hex
let of_hex_exn = Digest.from_hex

let string = Digest.string

let bytes = Digest.bytes

let subbytes bytes ~pos ~len = Digest.subbytes bytes pos len
