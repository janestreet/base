open! Import
include Buffer_intf
include Stdlib.Buffer

let contents_bytes = to_bytes
let add_substring t s ~pos ~len = add_substring t s pos len
let add_subbytes t s ~pos ~len = add_subbytes t s pos len
let sexp_of_t t = sexp_of_string (contents t)
let caml_buffer_length = (Stdlib.Obj.magic (Stdlib.Buffer.length : t -> int) : t -> int)

let caml_buffer_blit =
  (Stdlib.Obj.magic
     (Stdlib.Buffer.blit : Stdlib.Buffer.t -> int -> Bytes.t -> int -> int -> unit)
    : Stdlib.Buffer.t -> int -> Bytes.t -> int -> int -> unit)
;;

module To_bytes =
  Blit.Make_distinct
    (struct
      type nonrec t = t

      let length = caml_buffer_length
    end)
    (struct
      type t = Bytes.t

      let create ~len = Bytes.create len
      let length = Bytes.length

      let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
        caml_buffer_blit src src_pos dst dst_pos len
      ;;
    end)

include To_bytes
module To_string = Blit.Make_to_string (Stdlib.Buffer) (To_bytes)
