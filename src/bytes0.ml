(* [Bytes0] defines string functions that are primitives or can be simply defined in terms
   of [Stdlib.Bytes]. [Bytes0] is intended to completely express the part of
   [Stdlib.Bytes] that [Base] uses -- no other file in Base other than bytes0.ml should
   use [Stdlib.Bytes]. [Bytes0] has few dependencies, and so is available early in Base's
   build order.

   All Base files that need to use strings and come before [Base.Bytes] in build order
   should do:

   {[
     module Bytes = Bytes0
   ]}

   Defining [module Bytes = Bytes0] is also necessary because it prevents ocamldep from
   mistakenly causing a file to depend on [Base.Bytes]. *)

open! Import0
module Uchar = Uchar0
module Sys = Sys0

module Primitives = struct
  external get
    :  (bytes[@local_opt]) @ read
    -> (int[@local_opt])
    -> char
    @@ portable
    = "%bytes_safe_get"

  external length : (bytes[@local_opt]) @ immutable -> int @@ portable = "%bytes_length"

  external unsafe_get
    :  (bytes[@local_opt]) @ read
    -> (int[@local_opt])
    -> char
    @@ portable
    = "%bytes_unsafe_get"

  external set
    :  (bytes[@local_opt])
    -> (int[@local_opt])
    -> (char[@local_opt])
    -> unit
    @@ portable
    = "%bytes_safe_set"

  external unsafe_set
    :  (bytes[@local_opt])
    -> (int[@local_opt])
    -> (char[@local_opt])
    -> unit
    @@ portable
    = "%bytes_unsafe_set"

  (* [unsafe_blit_string] is not exported in the [stdlib] so we export it here *)
  external unsafe_blit_string
    :  src:(string[@local_opt])
    -> src_pos:int
    -> dst:(bytes[@local_opt])
    -> dst_pos:int
    -> len:int
    -> unit
    @@ portable
    = "caml_blit_string"
  [@@noalloc]

  external unsafe_get_int64
    :  (bytes[@local_opt]) @ read
    -> (int[@local_opt])
    -> int64
    @@ portable
    = "%caml_bytes_get64u"

  external unsafe_set_int64
    :  (bytes[@local_opt])
    -> (int[@local_opt])
    -> (int64[@local_opt])
    -> unit
    @@ portable
    = "%caml_bytes_set64u"

  external unsafe_get_int32
    :  (bytes[@local_opt]) @ read
    -> (int[@local_opt])
    -> int32
    @@ portable
    = "%caml_bytes_get32u"

  external unsafe_set_int32
    :  (bytes[@local_opt])
    -> (int[@local_opt])
    -> (int32[@local_opt])
    -> unit
    @@ portable
    = "%caml_bytes_set32u"

  external unsafe_get_int16
    :  (bytes[@local_opt]) @ read
    -> (int[@local_opt])
    -> int
    @@ portable
    = "%caml_bytes_get16u"

  external unsafe_set_int16
    :  (bytes[@local_opt])
    -> (int[@local_opt])
    -> (int[@local_opt])
    -> unit
    @@ portable
    = "%caml_bytes_set16u"
end

include Primitives

let max_length = Sys.max_string_length
let blit = Stdlib.Bytes.blit

external string_length : string @ local shared -> int @@ portable = "%string_length"

let blit_string ~(src @ local) ~src_pos ~(dst @ local) ~dst_pos ~len =
  if len < 0
     || src_pos < 0
     || src_pos > string_length src - len
     || dst_pos < 0
     || dst_pos > length dst - len
  then invalid_arg "String.blit / Bytes.blit_string"
  else unsafe_blit_string ~src ~src_pos ~dst ~dst_pos ~len
;;

let compare = Stdlib.Bytes.compare
let create = Stdlib.Bytes.create

include struct
  open struct
    external unsafe_set_uint8
      :  local_ bytes
      -> int
      -> int
      -> unit
      @@ portable
      = "%bytes_unsafe_set"

    external unsafe_set_uint16_ne
      :  local_ bytes
      -> int
      -> int
      -> unit
      @@ portable
      = "%caml_bytes_set16u"

    external set_int8 : local_ bytes -> int -> int -> unit @@ portable = "%bytes_safe_set"

    external set_int32_ne
      :  local_ bytes
      -> int
      -> int32
      -> unit
      @@ portable
      = "%caml_bytes_set32"

    external swap16 : int -> int @@ portable = "%bswap16"
    external swap32 : int32 -> int32 @@ portable = "%bswap_int32"

    let set_uint8 = set_int8

    let unsafe_set_uint16_le b i x =
      if Sys.big_endian
      then unsafe_set_uint16_ne b i (swap16 x)
      else unsafe_set_uint16_ne b i x
    ;;

    let unsafe_set_uint16_be b i x =
      if Sys.big_endian
      then unsafe_set_uint16_ne b i x
      else unsafe_set_uint16_ne b i (swap16 x)
    ;;

    let set_int32_le b i x =
      if Sys.big_endian then set_int32_ne b i (swap32 x) else set_int32_ne b i x
    ;;

    let set_int32_be b i x =
      if not Sys.big_endian then set_int32_ne b i (swap32 x) else set_int32_ne b i x
    ;;
  end

  let set_uchar_utf_8 (local_ b) i u =
    let set = unsafe_set_uint8 in
    let max = length b - 1 in
    match Uchar.to_int u with
    | u when u < 0 -> assert false
    | u when u <= 0x007F ->
      set_uint8 b i u;
      1
    | u when u <= 0x07FF ->
      let last = i + 1 in
      if last > max
      then 0
      else (
        set_uint8 b i (0xC0 lor (u lsr 6));
        set b last (0x80 lor (u land 0x3F));
        2)
    | u when u <= 0xFFFF ->
      let last = i + 2 in
      if last > max
      then 0
      else (
        set_uint8 b i (0xE0 lor (u lsr 12));
        set b (i + 1) (0x80 lor ((u lsr 6) land 0x3F));
        set b last (0x80 lor (u land 0x3F));
        3)
    | u when u <= 0x10FFFF ->
      let last = i + 3 in
      if last > max
      then 0
      else (
        set_uint8 b i (0xF0 lor (u lsr 18));
        set b (i + 1) (0x80 lor ((u lsr 12) land 0x3F));
        set b (i + 2) (0x80 lor ((u lsr 6) land 0x3F));
        set b last (0x80 lor (u land 0x3F));
        4)
    | _ -> assert false
  ;;

  let set_uchar_utf_16le (local_ b) i u =
    let set = unsafe_set_uint16_le in
    let max = length b - 1 in
    if i < 0 || i > max
    then invalid_arg "index out of bounds"
    else (
      match Uchar.to_int u with
      | u when u < 0 -> assert false
      | u when u <= 0xFFFF ->
        let last = i + 1 in
        if last > max
        then 0
        else (
          set b i u;
          2)
      | u when u <= 0x10FFFF ->
        let last = i + 3 in
        if last > max
        then 0
        else (
          let u' = u - 0x10000 in
          let hi = 0xD800 lor (u' lsr 10) in
          let lo = 0xDC00 lor (u' land 0x3FF) in
          set b i hi;
          set b (i + 2) lo;
          4)
      | _ -> assert false)
  ;;

  let set_uchar_utf_16be (local_ b) i u =
    let set = unsafe_set_uint16_be in
    let max = length b - 1 in
    if i < 0 || i > max
    then invalid_arg "index out of bounds"
    else (
      match Uchar.to_int u with
      | u when u < 0 -> assert false
      | u when u <= 0xFFFF ->
        let last = i + 1 in
        if last > max
        then 0
        else (
          set b i u;
          2)
      | u when u <= 0x10FFFF ->
        let last = i + 3 in
        if last > max
        then 0
        else (
          let u' = u - 0x10000 in
          let hi = 0xD800 lor (u' lsr 10) in
          let lo = 0xDC00 lor (u' land 0x3FF) in
          set b i hi;
          set b (i + 2) lo;
          4)
      | _ -> assert false)
  ;;

  let set_utf_32_uchar ~set_int32 (local_ bytes) idx uchar =
    Uchar.to_int uchar
    |> Int_conversions.int_to_int32_trunc (* should never have anything to truncate *)
    |> set_int32 bytes idx;
    4
  ;;

  let set_uchar_utf_32le (b @ local) i c = set_utf_32_uchar ~set_int32:set_int32_le b i c
  let set_uchar_utf_32be (b @ local) i c = set_utf_32_uchar ~set_int32:set_int32_be b i c
end

external unsafe_create_local
  :  int
  -> local_ bytes
  @@ portable
  = "Base_unsafe_create_local_bytes"
[@@noalloc]

let[@zero_alloc] create_local len = exclave_
  if len > Sys0.max_string_length then invalid_arg "Bytes.create_local";
  unsafe_create_local len
;;

let%template[@alloc stack] create x = exclave_ create_local x [@@zero_alloc]

external unsafe_fill
  :  local_ bytes
  -> pos:int
  -> len:int
  -> char
  -> unit
  @@ portable
  = "caml_fill_bytes"
[@@noalloc]

let fill (local_ t) ~pos ~len c =
  if pos < 0 || len < 0 || pos > length t - len
  then invalid_arg "Bytes.fill"
  else unsafe_fill t ~pos ~len c
;;

let%template[@alloc a = (heap, stack)] make n c =
  (let t = (create [@alloc a]) n in
   unsafe_fill t ~pos:0 ~len:n c;
   t)
  [@exclave_if_stack a]
;;

let empty = Stdlib.Bytes.empty
let get_empty () = Portability_hacks.magic_uncontended__promise_deeply_immutable empty

let map (local_ t) ~(local_ f : _ -> _) =
  let l = length t in
  if l = 0
  then get_empty ()
  else (
    let r = create l in
    for i = 0 to l - 1 do
      unsafe_set r i (f (unsafe_get t i))
    done;
    r)
;;

let map__stack (local_ t) ~(local_ f : _ -> _) = exclave_
  let l = length t in
  if l = 0
  then get_empty ()
  else (
    let r = create_local l in
    for i = 0 to l - 1 do
      unsafe_set r i (f (unsafe_get t i))
    done;
    r)
;;

let mapi (local_ t) ~(local_ f : _ -> _ -> _) =
  let l = length t in
  if l = 0
  then get_empty ()
  else (
    let r = create l in
    for i = 0 to l - 1 do
      unsafe_set r i (f i (unsafe_get t i))
    done;
    r)
;;

external unsafe_blit
  :  src:(bytes[@local_opt])
  -> src_pos:int
  -> dst:(bytes[@local_opt])
  -> dst_pos:int
  -> len:int
  -> unit
  @@ portable
  = "caml_blit_bytes"
[@@noalloc]

(* This is lifted from [Stdlib], but templated *)
let%template[@alloc a = (heap, stack)] sub src ~pos:src_pos ~len =
  if [@exclave_if_stack a] src_pos < 0 || len < 0 || src_pos > length src - len
  then invalid_arg "Bytes0.sub"
  else (
    let dst = (create [@alloc a]) len in
    unsafe_blit ~src ~src_pos ~dst ~dst_pos:0 ~len;
    dst)
;;

let to_string = Stdlib.Bytes.to_string
let of_string = Stdlib.Bytes.of_string

external unsafe_to_string
  :  no_mutation_while_string_reachable:(bytes[@local_opt])
  -> (string[@local_opt])
  @@ portable
  = "%bytes_to_string"

external unsafe_of_string_promise_no_mutation
  :  (string[@local_opt])
  -> (bytes[@local_opt])
  @@ portable
  = "%bytes_of_string"

let copy (local_ src) =
  let len = length src in
  let dst = create len in
  unsafe_blit ~src ~src_pos:0 ~dst ~dst_pos:0 ~len;
  dst
;;
