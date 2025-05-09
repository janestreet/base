open! Import
open Modes.Export
include Buffer_intf.Definitions
include Stdlib.Buffer

let contents_bytes = to_bytes
let add_substring t s ~pos ~len = add_substring t s pos len
let add_subbytes t s ~pos ~len = add_subbytes t s pos len
let sexp_of_t t = sexp_of_string (contents t)

module%template To_bytes =
  Blit.Make_distinct [@modality portable]
    (struct
      type nonrec t = t Modes.Global.t

      let length t = length t.global
    end)
    (struct
      type t = Bytes.t Modes.Global.t

      let create ~len = { global = Bytes.create len }
      let length t = Bytes.length t.global

      let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
        blit src.global src_pos dst.global dst_pos len
      ;;
    end)

let blit ~src ~src_pos ~dst ~dst_pos ~len =
  To_bytes.blit ~src:{ global = src } ~src_pos ~dst:{ global = dst } ~dst_pos ~len
;;

let blito ~src ?src_pos ?src_len ~dst ?dst_pos () =
  To_bytes.blito ~src:{ global = src } ?src_pos ?src_len ~dst:{ global = dst } ?dst_pos ()
;;

let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
  To_bytes.unsafe_blit ~src:{ global = src } ~src_pos ~dst:{ global = dst } ~dst_pos ~len
;;

let sub t ~pos ~len = (To_bytes.sub { global = t } ~pos ~len).global
let subo ?pos ?len t = (To_bytes.subo ?pos ?len { global = t }).global

module To_string = struct
  let sub t ~pos ~len =
    let bytes = sub t ~pos ~len in
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes
  ;;

  let subo ?pos ?len t =
    let bytes = subo ?pos ?len t in
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes
  ;;
end
