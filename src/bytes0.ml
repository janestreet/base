(* [Bytes0] defines string functions that are primitives or can be simply
   defined in terms of [Stdlib.Bytes]. [Bytes0] is intended to completely express
   the part of [Stdlib.Bytes] that [Base] uses -- no other file in Base other
   than bytes0.ml should use [Stdlib.Bytes]. [Bytes0] has few dependencies, and
   so is available early in Base's build order.

   All Base files that need to use strings and come before [Base.Bytes] in
   build order should do:

   {[
     module Bytes  = Bytes0
   ]}

   Defining [module Bytes = Bytes0] is also necessary because it prevents
   ocamldep from mistakenly causing a file to depend on [Base.Bytes]. *)

open! Import0
module Uchar = Uchar0
module Sys = Sys0

module Primitives = struct
  external get : (bytes[@local_opt]) -> (int[@local_opt]) -> char = "%bytes_safe_get"
  external length : (bytes[@local_opt]) -> int = "%bytes_length"

  external unsafe_get
    :  (bytes[@local_opt])
    -> (int[@local_opt])
    -> char
    = "%bytes_unsafe_get"

  external set
    :  (bytes[@local_opt])
    -> (int[@local_opt])
    -> (char[@local_opt])
    -> unit
    = "%bytes_safe_set"

  external unsafe_set
    :  (bytes[@local_opt])
    -> (int[@local_opt])
    -> (char[@local_opt])
    -> unit
    = "%bytes_unsafe_set"

  (* [unsafe_blit_string] is not exported in the [stdlib] so we export it here *)
  external unsafe_blit_string
    :  src:(string[@local_opt])
    -> src_pos:int
    -> dst:(bytes[@local_opt])
    -> dst_pos:int
    -> len:int
    -> unit
    = "caml_blit_string"
    [@@noalloc]

  external unsafe_get_int64
    :  (bytes[@local_opt])
    -> (int[@local_opt])
    -> int64
    = "%caml_bytes_get64u"

  external unsafe_set_int64
    :  (bytes[@local_opt])
    -> (int[@local_opt])
    -> (int64[@local_opt])
    -> unit
    = "%caml_bytes_set64u"

  external unsafe_get_int32
    :  (bytes[@local_opt])
    -> (int[@local_opt])
    -> int32
    = "%caml_bytes_get32u"

  external unsafe_set_int32
    :  (bytes[@local_opt])
    -> (int[@local_opt])
    -> (int32[@local_opt])
    -> unit
    = "%caml_bytes_set32u"

  external unsafe_get_int16
    :  (bytes[@local_opt])
    -> (int[@local_opt])
    -> int
    = "%caml_bytes_get16u"

  external unsafe_set_int16
    :  (bytes[@local_opt])
    -> (int[@local_opt])
    -> (int[@local_opt])
    -> unit
    = "%caml_bytes_set16u"
end

include Primitives

let max_length = Sys.max_string_length
let blit = Stdlib.Bytes.blit
let blit_string = Stdlib.Bytes.blit_string
let compare = Stdlib.Bytes.compare
let copy = Stdlib.Bytes.copy
let create = Stdlib.Bytes.create
let set_uchar_utf_8 = Stdlib.Bytes.set_utf_8_uchar
let set_uchar_utf_16le = Stdlib.Bytes.set_utf_16le_uchar
let set_uchar_utf_16be = Stdlib.Bytes.set_utf_16be_uchar

let set_utf_32_uchar ~set_int32 bytes idx uchar =
  Uchar.to_int uchar
  |> Int_conversions.int_to_int32_trunc (* should never have anything to truncate *)
  |> set_int32 bytes idx;
  4
;;

let set_uchar_utf_32le = set_utf_32_uchar ~set_int32:Stdlib.Bytes.set_int32_le
let set_uchar_utf_32be = set_utf_32_uchar ~set_int32:Stdlib.Bytes.set_int32_be

external unsafe_create_local : int -> bytes = "Base_unsafe_create_local_bytes" 

let create_local len =
  if len > Sys0.max_string_length then invalid_arg "Bytes.create_local";
  unsafe_create_local len
;;

let fill = Stdlib.Bytes.fill
let make = Stdlib.Bytes.make

let map t ~(f : _ -> _) =
  let l = length t in
  if l = 0
  then t
  else (
    let r = create l in
    for i = 0 to l - 1 do
      unsafe_set r i (f (unsafe_get t i))
    done;
    r)
;;

let mapi t ~(f : _ -> _ -> _) =
  let l = length t in
  if l = 0
  then t
  else (
    let r = create l in
    for i = 0 to l - 1 do
      unsafe_set r i (f i (unsafe_get t i))
    done;
    r)
;;

let sub = Stdlib.Bytes.sub

external unsafe_blit
  :  src:(bytes[@local_opt])
  -> src_pos:int
  -> dst:(bytes[@local_opt])
  -> dst_pos:int
  -> len:int
  -> unit
  = "caml_blit_bytes"
  [@@noalloc]

let to_string = Stdlib.Bytes.to_string
let of_string = Stdlib.Bytes.of_string

external unsafe_to_string
  :  no_mutation_while_string_reachable:(bytes[@local_opt])
  -> (string[@local_opt])
  = "%bytes_to_string"

external unsafe_of_string_promise_no_mutation
  :  (string[@local_opt])
  -> (bytes[@local_opt])
  = "%bytes_of_string"
