open! Import
module Bytes = Bytes0
module String = String0
include Uchar_intf

let failwithf = Printf.failwithf

include Uchar0

let module_name = "Base.Uchar"
let hash_fold_t state t = Hash.fold_int state (to_int t)
let hash t = Hash.run hash_fold_t t

(* Not for export. String formats exported via [Utf*] modules below. *)
let to_string_internal t = Printf.sprintf "U+%04X" (to_int t)
let sexp_of_t t = Sexp.Atom (to_string_internal t)

let t_of_sexp sexp =
  match sexp with
  | Sexp.List _ -> of_sexp_error "Uchar.t_of_sexp: atom needed" sexp
  | Sexp.Atom s ->
    (try Stdlib.Scanf.sscanf s "U+%X" (fun i -> Uchar0.of_int i) with
     | _ -> of_sexp_error "Uchar.t_of_sexp: atom of the form U+XXXX needed" sexp)
;;

let t_sexp_grammar : t Sexplib0.Sexp_grammar.t =
  Sexplib0.Sexp_grammar.coerce string_sexp_grammar
;;

include Pretty_printer.Register (struct
  type nonrec t = t

  let module_name = module_name
  let to_string = to_string_internal
end)

include Comparable.Make (struct
  type nonrec t = t

  let compare = compare
  let sexp_of_t = sexp_of_t
end)

(* Open replace_polymorphic_compare after including functor instantiations so they do not
   shadow its definitions. This is here so that efficient versions of the comparison
   functions are available within this module. *)
open! Uchar_replace_polymorphic_compare

let invariant (_ : t) = ()
let int_is_scalar = is_valid

let succ_exn c =
  try Uchar0.succ c with
  | Invalid_argument msg -> failwithf "Uchar.succ_exn: %s" msg ()
;;

let succ c =
  try Some (Uchar0.succ c) with
  | Invalid_argument _ -> None
;;

let pred_exn c =
  try Uchar0.pred c with
  | Invalid_argument msg -> failwithf "Uchar.pred_exn: %s" msg ()
;;

let pred c =
  try Some (Uchar0.pred c) with
  | Invalid_argument _ -> None
;;

let of_scalar i = if int_is_scalar i then Some (unsafe_of_int i) else None

let of_scalar_exn i =
  if int_is_scalar i
  then unsafe_of_int i
  else failwithf "Uchar.of_int_exn got a invalid Unicode scalar value: %04X" i ()
;;

let to_scalar t = Uchar0.to_int t
let to_char c = if is_char c then Some (unsafe_to_char c) else None

let to_char_exn c =
  if is_char c
  then unsafe_to_char c
  else failwithf "Uchar.to_char_exn got a non latin-1 character: U+%04X" (to_int c) ()
;;

module Decode_result = struct
  type t = Uchar0.utf_decode

  let compare : t -> t -> int = Poly.compare
  let equal : t -> t -> bool = Poly.equal

  let hash_fold_t : Hash.state -> t -> Hash.state =
    fun state t -> hash_fold_int state (Hashable.hash t)
  ;;

  let hash : t -> int = Hashable.hash
  let is_valid = Uchar0.utf_decode_is_valid
  let bytes_consumed = Uchar0.utf_decode_length
  let uchar_or_replacement_char = Uchar0.utf_decode_uchar
  let sexp_of_t t = sexp_of_t (uchar_or_replacement_char t)

  let uchar t =
    match is_valid t with
    | true -> Some (uchar_or_replacement_char t)
    | false -> None
  ;;

  let[@zero_alloc] uchar_exn t =
    match is_valid t with
    | true -> uchar_or_replacement_char t
    | false ->
      Error.raise_s
        (Atom "Uchar.Decode_result.uchar_exn was called on an invalid decode result")
  ;;
end

module Make_utf (Format : sig
  val codec_name : string
  val module_name : string
  val byte_length : t -> int
  val get_decode_result : string -> byte_pos:int -> Decode_result.t
  val set : bytes -> int -> t -> int
end) : Utf = struct
  let codec_name = Format.codec_name
  let byte_length = Format.byte_length

  let to_string t =
    let len = byte_length t in
    let bytes = Bytes.create len in
    let pos = Format.set bytes 0 t in
    assert (Int_replace_polymorphic_compare.equal pos len);
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes
  ;;

  let of_string_message =
    Format.module_name ^ ".of_string: expected a single Unicode character"
  ;;

  let[@cold] raise_of_string string =
    Error.raise_s (Sexp.message of_string_message [ "string", Atom string ])
  ;;

  let of_string string =
    let decode = Format.get_decode_result string ~byte_pos:0 in
    let string_len = String.length string in
    let decode_len = Decode_result.bytes_consumed decode in
    if Int_replace_polymorphic_compare.equal string_len decode_len
       && Decode_result.is_valid decode
    then Decode_result.uchar_or_replacement_char decode
    else raise_of_string string
  ;;
end

module Utf8 = Make_utf (struct
  let codec_name = "UTF-8"
  let module_name = "Base.Uchar.Utf8"
  let byte_length = utf_8_byte_length
  let get_decode_result = String.get_utf_8_uchar
  let set = Bytes.set_uchar_utf_8
end)

module Utf16le = Make_utf (struct
  let codec_name = "UTF-16LE"
  let module_name = "Base.Uchar.Utf16le"
  let byte_length = utf_16_byte_length
  let get_decode_result = String.get_utf_16le_uchar
  let set = Bytes.set_uchar_utf_16le
end)

module Utf16be = Make_utf (struct
  let codec_name = "UTF-16BE"
  let module_name = "Base.Uchar.Utf16be"
  let byte_length = utf_16_byte_length
  let get_decode_result = String.get_utf_16be_uchar
  let set = Bytes.set_uchar_utf_16be
end)

module Utf32le = Make_utf (struct
  let codec_name = "UTF-32LE"
  let module_name = "Base.Uchar.Utf32le"
  let byte_length _ = 4
  let get_decode_result = String.get_utf_32le_uchar
  let set = Bytes.set_uchar_utf_32le
end)

module Utf32be = Make_utf (struct
  let codec_name = "UTF-32BE"
  let module_name = "Base.Uchar.Utf32be"
  let byte_length _ = 4
  let get_decode_result = String.get_utf_32be_uchar
  let set = Bytes.set_uchar_utf_32be
end)

(* Include type-specific [Replace_polymorphic_compare] at the end, after
   including functor application that could shadow its definitions. This is
   here so that efficient versions of the comparison functions are exported by
   this module. *)
include Uchar_replace_polymorphic_compare
