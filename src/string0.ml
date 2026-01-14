(* [String0] defines string functions that are primitives or can be simply defined in
   terms of [Stdlib.String]. [String0] is intended to completely express the part of
   [Stdlib.String] that [Base] uses -- no other file in Base other than string0.ml should
   use [Stdlib.String]. [String0] has few dependencies, and so is available early in
   Base's build order.

   All Base files that need to use strings, including the subscript syntax [x.[i]] which
   the OCaml parser desugars into calls to [String], and come before [Base.String] in
   build order should do

   {[
     module String = String0
   ]}

   Defining [module String = String0] is also necessary because it prevents ocamldep from
   mistakenly causing a file to depend on [Base.String]. *)

open! Import0

open struct
  module Sys = Sys0
  module Uchar = Uchar0
  module Bytes = Bytes0
end

module String = struct
  external get
    :  (string[@local_opt])
    -> (int[@local_opt])
    -> char
    @@ portable
    = "%string_safe_get"

  external get_uint8 : string @ local -> int -> int @@ portable = "%string_safe_get"
  external length : (string[@local_opt]) -> int @@ portable = "%string_length"

  external unsafe_get
    :  (string[@local_opt])
    -> (int[@local_opt])
    -> char
    @@ portable
    = "%string_unsafe_get"

  external unsafe_get_uint8
    :  string @ local
    -> int
    -> int
    @@ portable
    = "%string_unsafe_get"

  external unsafe_get_uint16_ne
    :  string @ local
    -> int
    -> int
    @@ portable
    = "%caml_string_get16u"

  external swap16 : int -> int @@ portable = "%bswap16"

  let unsafe_get_uint16_le b i =
    if Sys.big_endian then swap16 (unsafe_get_uint16_ne b i) else unsafe_get_uint16_ne b i
  ;;

  let unsafe_get_uint16_be b i =
    if Sys.big_endian then unsafe_get_uint16_ne b i else swap16 (unsafe_get_uint16_ne b i)
  ;;

  external unsafe_get_int32_ne
    :  string @ local
    -> int
    -> int32
    @@ portable
    = "%caml_string_get32u"

  external swap32 : int32 -> int32 @@ portable = "%bswap_int32"

  let unsafe_get_int32_le b i =
    if Sys.big_endian then swap32 (unsafe_get_int32_ne b i) else unsafe_get_int32_ne b i
  ;;

  let unsafe_get_int32_be b i =
    if Sys.big_endian then unsafe_get_int32_ne b i else swap32 (unsafe_get_int32_ne b i)
  ;;
end

include String

let max_length = Sys.max_string_length

let%template[@alloc a = (heap, stack)] append (s1 @ local) (s2 @ local) =
  (let l1 = length s1
   and l2 = length s2 in
   let s = (Bytes.create [@alloc a]) (l1 + l2) in
   Bytes.unsafe_blit_string ~src:s1 ~src_pos:0 ~dst:s ~dst_pos:0 ~len:l1;
   Bytes.unsafe_blit_string ~src:s2 ~src_pos:0 ~dst:s ~dst_pos:l1 ~len:l2;
   Bytes.unsafe_to_string ~no_mutation_while_string_reachable:s)
  [@exclave_if_stack a]
;;

let ( ^ ) s1 s2 = append s1 s2
let capitalize = Stdlib.String.capitalize_ascii
let compare = Stdlib.String.compare
let escaped = Stdlib.String.escaped

let%template[@alloc a = (heap, stack)] make n c =
  Bytes.unsafe_to_string
    ~no_mutation_while_string_reachable:((Bytes.make [@alloc a]) n c)
  [@exclave_if_stack a]
;;

let%template[@alloc heap] sub = Stdlib.String.sub
let uncapitalize = Stdlib.String.uncapitalize_ascii

open struct
  let dec_invalid = Uchar0.utf_decode_invalid
  let[@inline] dec_ret n u = Uchar0.utf_decode n (Uchar0.unsafe_of_int u)
  let[@inline] not_in_x80_to_xBF b = b lsr 6 <> 0b10
  let[@inline] not_in_xA0_to_xBF b = b lsr 5 <> 0b101
  let[@inline] not_in_x80_to_x9F b = b lsr 5 <> 0b100
  let[@inline] not_in_x90_to_xBF b = b < 0x90 || 0xBF < b
  let[@inline] not_in_x80_to_x8F b = b lsr 4 <> 0x8
  let[@inline] utf_8_uchar_2 b0 b1 = ((b0 land 0x1F) lsl 6) lor (b1 land 0x3F)

  let[@inline] utf_8_uchar_3 b0 b1 b2 =
    ((b0 land 0x0F) lsl 12) lor ((b1 land 0x3F) lsl 6) lor (b2 land 0x3F)
  ;;

  let[@inline] utf_8_uchar_4 b0 b1 b2 b3 =
    ((b0 land 0x07) lsl 18)
    lor ((b1 land 0x3F) lsl 12)
    lor ((b2 land 0x3F) lsl 6)
    lor (b3 land 0x3F)
  ;;
end

let get_utf_8_uchar b i =
  let b0 = get_uint8 b i in
  (* raises if [i] is not a valid index. *)
  let get = unsafe_get_uint8 in
  let max = length b - 1 in
  match Char0.unsafe_of_int b0 with
  (* See The Unicode Standard, Table 3.7 *)
  | '\x00' .. '\x7F' -> dec_ret 1 b0
  | '\xC2' .. '\xDF' ->
    let i = i + 1 in
    if i > max
    then dec_invalid 1
    else (
      let b1 = get b i in
      if not_in_x80_to_xBF b1 then dec_invalid 1 else dec_ret 2 (utf_8_uchar_2 b0 b1))
  | '\xE0' ->
    let i = i + 1 in
    if i > max
    then dec_invalid 1
    else (
      let b1 = get b i in
      if not_in_xA0_to_xBF b1
      then dec_invalid 1
      else (
        let i = i + 1 in
        if i > max
        then dec_invalid 2
        else (
          let b2 = get b i in
          if not_in_x80_to_xBF b2
          then dec_invalid 2
          else dec_ret 3 (utf_8_uchar_3 b0 b1 b2))))
  | '\xE1' .. '\xEC' | '\xEE' .. '\xEF' ->
    let i = i + 1 in
    if i > max
    then dec_invalid 1
    else (
      let b1 = get b i in
      if not_in_x80_to_xBF b1
      then dec_invalid 1
      else (
        let i = i + 1 in
        if i > max
        then dec_invalid 2
        else (
          let b2 = get b i in
          if not_in_x80_to_xBF b2
          then dec_invalid 2
          else dec_ret 3 (utf_8_uchar_3 b0 b1 b2))))
  | '\xED' ->
    let i = i + 1 in
    if i > max
    then dec_invalid 1
    else (
      let b1 = get b i in
      if not_in_x80_to_x9F b1
      then dec_invalid 1
      else (
        let i = i + 1 in
        if i > max
        then dec_invalid 2
        else (
          let b2 = get b i in
          if not_in_x80_to_xBF b2
          then dec_invalid 2
          else dec_ret 3 (utf_8_uchar_3 b0 b1 b2))))
  | '\xF0' ->
    let i = i + 1 in
    if i > max
    then dec_invalid 1
    else (
      let b1 = get b i in
      if not_in_x90_to_xBF b1
      then dec_invalid 1
      else (
        let i = i + 1 in
        if i > max
        then dec_invalid 2
        else (
          let b2 = get b i in
          if not_in_x80_to_xBF b2
          then dec_invalid 2
          else (
            let i = i + 1 in
            if i > max
            then dec_invalid 3
            else (
              let b3 = get b i in
              if not_in_x80_to_xBF b3
              then dec_invalid 3
              else dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3))))))
  | '\xF1' .. '\xF3' ->
    let i = i + 1 in
    if i > max
    then dec_invalid 1
    else (
      let b1 = get b i in
      if not_in_x80_to_xBF b1
      then dec_invalid 1
      else (
        let i = i + 1 in
        if i > max
        then dec_invalid 2
        else (
          let b2 = get b i in
          if not_in_x80_to_xBF b2
          then dec_invalid 2
          else (
            let i = i + 1 in
            if i > max
            then dec_invalid 3
            else (
              let b3 = get b i in
              if not_in_x80_to_xBF b3
              then dec_invalid 3
              else dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3))))))
  | '\xF4' ->
    let i = i + 1 in
    if i > max
    then dec_invalid 1
    else (
      let b1 = get b i in
      if not_in_x80_to_x8F b1
      then dec_invalid 1
      else (
        let i = i + 1 in
        if i > max
        then dec_invalid 2
        else (
          let b2 = get b i in
          if not_in_x80_to_xBF b2
          then dec_invalid 2
          else (
            let i = i + 1 in
            if i > max
            then dec_invalid 3
            else (
              let b3 = get b i in
              if not_in_x80_to_xBF b3
              then dec_invalid 3
              else dec_ret 4 (utf_8_uchar_4 b0 b1 b2 b3))))))
  | _ -> dec_invalid 1
;;

let[@inline] get_utf_8_uchar (t @ local) ~byte_pos = get_utf_8_uchar t byte_pos

let is_valid_utf_8 (b @ local) =
  let rec loop max b i =
    if i > max
    then true
    else (
      let get = unsafe_get_uint8 in
      match Char0.unsafe_of_int (get b i) with
      | '\x00' .. '\x7F' -> loop max b (i + 1)
      | '\xC2' .. '\xDF' ->
        let last = i + 1 in
        if last > max || not_in_x80_to_xBF (get b last)
        then false
        else loop max b (last + 1)
      | '\xE0' ->
        let last = i + 2 in
        if last > max
           || not_in_xA0_to_xBF (get b (i + 1))
           || not_in_x80_to_xBF (get b last)
        then false
        else loop max b (last + 1)
      | '\xE1' .. '\xEC' | '\xEE' .. '\xEF' ->
        let last = i + 2 in
        if last > max
           || not_in_x80_to_xBF (get b (i + 1))
           || not_in_x80_to_xBF (get b last)
        then false
        else loop max b (last + 1)
      | '\xED' ->
        let last = i + 2 in
        if last > max
           || not_in_x80_to_x9F (get b (i + 1))
           || not_in_x80_to_xBF (get b last)
        then false
        else loop max b (last + 1)
      | '\xF0' ->
        let last = i + 3 in
        if last > max
           || not_in_x90_to_xBF (get b (i + 1))
           || not_in_x80_to_xBF (get b (i + 2))
           || not_in_x80_to_xBF (get b last)
        then false
        else loop max b (last + 1)
      | '\xF1' .. '\xF3' ->
        let last = i + 3 in
        if last > max
           || not_in_x80_to_xBF (get b (i + 1))
           || not_in_x80_to_xBF (get b (i + 2))
           || not_in_x80_to_xBF (get b last)
        then false
        else loop max b (last + 1)
      | '\xF4' ->
        let last = i + 3 in
        if last > max
           || not_in_x80_to_x8F (get b (i + 1))
           || not_in_x80_to_xBF (get b (i + 2))
           || not_in_x80_to_xBF (get b last)
        then false
        else loop max b (last + 1)
      | _ -> false)
  in
  loop (length b - 1) b 0
;;

(* UTF-16BE *)

let get_utf_16be_uchar b i =
  let get = unsafe_get_uint16_be in
  let max = length b - 1 in
  if i < 0 || i > max
  then invalid_arg "index out of bounds"
  else if i = max
  then dec_invalid 1
  else (
    match get b i with
    | u when u < 0xD800 || u > 0xDFFF -> dec_ret 2 u
    | u when u > 0xDBFF -> dec_invalid 2
    | hi ->
      (* combine [hi] with a low surrogate *)
      let last = i + 3 in
      if last > max
      then dec_invalid (max - i + 1)
      else (
        match get b (i + 2) with
        | u when u < 0xDC00 || u > 0xDFFF -> dec_invalid 2 (* retry here *)
        | lo ->
          let u = (((hi land 0x3FF) lsl 10) lor (lo land 0x3FF)) + 0x10000 in
          dec_ret 4 u))
;;

let[@inline] get_utf_16be_uchar (t @ local) ~byte_pos = get_utf_16be_uchar t byte_pos

let is_valid_utf_16be (b @ local) =
  let rec loop max b i =
    let get = unsafe_get_uint16_be in
    if i > max
    then true
    else if i = max
    then false
    else (
      match get b i with
      | u when u < 0xD800 || u > 0xDFFF -> loop max b (i + 2)
      | u when u > 0xDBFF -> false
      | _hi ->
        let last = i + 3 in
        if last > max
        then false
        else (
          match get b (i + 2) with
          | u when u < 0xDC00 || u > 0xDFFF -> false
          | _lo -> loop max b (i + 4)))
  in
  loop (length b - 1) b 0
;;

(* UTF-16LE *)

let get_utf_16le_uchar b i =
  let get = unsafe_get_uint16_le in
  let max = length b - 1 in
  if i < 0 || i > max
  then invalid_arg "index out of bounds"
  else if i = max
  then dec_invalid 1
  else (
    match get b i with
    | u when u < 0xD800 || u > 0xDFFF -> dec_ret 2 u
    | u when u > 0xDBFF -> dec_invalid 2
    | hi ->
      (* combine [hi] with a low surrogate *)
      let last = i + 3 in
      if last > max
      then dec_invalid (max - i + 1)
      else (
        match get b (i + 2) with
        | u when u < 0xDC00 || u > 0xDFFF -> dec_invalid 2 (* retry here *)
        | lo ->
          let u = (((hi land 0x3FF) lsl 10) lor (lo land 0x3FF)) + 0x10000 in
          dec_ret 4 u))
;;

let[@inline] get_utf_16le_uchar (t @ local) ~byte_pos = get_utf_16le_uchar t byte_pos

let is_valid_utf_16le (b @ local) =
  let rec loop max b i =
    let get = unsafe_get_uint16_le in
    if i > max
    then true
    else if i = max
    then false
    else (
      match get b i with
      | u when u < 0xD800 || u > 0xDFFF -> loop max b (i + 2)
      | u when u > 0xDBFF -> false
      | _hi ->
        let last = i + 3 in
        if last > max
        then false
        else (
          match get b (i + 2) with
          | u when u < 0xDC00 || u > 0xDFFF -> false
          | _lo -> loop max b (i + 4)))
  in
  loop (length b - 1) b 0
;;

open struct
  let get_utf_32_uchar ~get_int32 t ~byte_pos =
    let len = String.length t in
    match byte_pos >= 0 && byte_pos < len with
    | false -> raise (Invalid_argument "index out of bounds")
    | true ->
      (match len - byte_pos with
       | (1 | 2 | 3) as bytes_read ->
         (* Fewer than 4 bytes remain in [t], so we know the decoding is invalid. *)
         Uchar.utf_decode_invalid bytes_read
       | _ ->
         let int32 = get_int32 t byte_pos in
         (match Int_conversions.int32_is_representable_as_int int32 with
          | false -> Uchar.utf_decode_invalid 4
          | true ->
            let int = Int_conversions.int32_to_int_trunc int32 in
            (match Uchar.is_valid int with
             | true -> Uchar.utf_decode 4 (Uchar.unsafe_of_int int)
             | false -> Uchar.utf_decode_invalid 4)))
  ;;
end

let[@inline] get_utf_32le_uchar (t @ local) ~byte_pos =
  get_utf_32_uchar t ~byte_pos ~get_int32:unsafe_get_int32_le
;;

let[@inline] get_utf_32be_uchar (t @ local) ~byte_pos =
  get_utf_32_uchar t ~byte_pos ~get_int32:unsafe_get_int32_be
;;

include struct
  let ensure_ge (x : int) y = if x >= y then x else invalid_arg "String.concat"

  let rec sum_lengths acc seplen = function
    | [] -> acc
    | hd :: [] -> length hd + acc
    | hd :: tl -> sum_lengths (ensure_ge (length hd + seplen + acc) acc) seplen tl
  ;;

  let rec unsafe_blits ~dst ~dst_pos ~sep ~sep_len = function
    | [] -> ()
    | hd :: [] ->
      Bytes.unsafe_blit_string ~src:hd ~src_pos:0 ~dst ~dst_pos ~len:(length hd)
    | hd :: tl ->
      Bytes.unsafe_blit_string ~src:hd ~src_pos:0 ~dst ~dst_pos ~len:(length hd);
      Bytes.unsafe_blit_string
        ~src:sep
        ~src_pos:0
        ~dst
        ~dst_pos:(dst_pos + length hd)
        ~len:sep_len;
      unsafe_blits ~dst ~dst_pos:(dst_pos + length hd + sep_len) ~sep ~sep_len tl
  ;;

  include%template (
  struct
    let[@alloc m = heap] smart_globalize a = Globalize.globalize_string a
    let[@alloc m = stack] smart_globalize a = a
  end :
  sig
  @@ portable
    val smart_globalize : string @ local -> string @ m
    [@@alloc __ @ m = (heap_global, stack_local)]
  end)

  let%template[@alloc a = (heap, stack)] concat ?(sep = "") (l @ local) =
    match[@exclave_if_stack a] l with
    | [] -> ""
    | [ x ] -> (smart_globalize [@alloc a]) x
    | l ->
      let sep_len = length sep in
      let dst = (Bytes.create [@alloc a]) (sum_lengths 0 sep_len l) in
      unsafe_blits ~dst ~dst_pos:0 ~sep ~sep_len l;
      Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst
  ;;
end

let lowercase string =
  let string =
    Bytes.unsafe_of_string_promise_no_mutation string |> Bytes.map ~f:Char0.lowercase
  in
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:string
;;

let lowercase__stack (string @ local) = exclave_
  let string =
    Bytes.unsafe_of_string_promise_no_mutation string
    |> Bytes.map__stack ~f:Char0.lowercase
  in
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:string
;;

let uppercase string =
  let string =
    Bytes.unsafe_of_string_promise_no_mutation string |> Bytes.map ~f:Char0.uppercase
  in
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:string
;;

let uppercase__stack (string @ local) = exclave_
  let string =
    Bytes.unsafe_of_string_promise_no_mutation string
    |> Bytes.map__stack ~f:Char0.uppercase
  in
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:string
;;

let%template[@mode l = (global, local)] iter t ~(local_ f) =
  for i = 0 to length t - 1 do
    f (unsafe_get t i)
  done
;;

let split_lines =
  let back_up_at_newline ~t ~pos ~eol =
    pos := !pos - if !pos > 0 && Char0.equal t.[!pos - 1] '\r' then 2 else 1;
    eol := !pos + 1
  in
  fun t ->
    let n = length t in
    if n = 0
    then []
    else (
      (* Invariant: [-1 <= pos < eol]. *)
      let pos = ref (n - 1) in
      let eol = ref n in
      let ac = ref [] in
      (* We treat the end of the string specially, because if the string ends with a
         newline, we don't want an extra empty string at the end of the output. *)
      if Char0.equal t.[!pos] '\n' then back_up_at_newline ~t ~pos ~eol;
      while !pos >= 0 do
        if not (Char0.equal t.[!pos] '\n')
        then decr pos
        else (
          (* Because [pos < eol], we know that [start <= eol]. *)
          let start = !pos + 1 in
          ac := sub t ~pos:start ~len:(!eol - start) :: !ac;
          back_up_at_newline ~t ~pos ~eol)
      done;
      sub t ~pos:0 ~len:!eol :: !ac)
;;

let%template[@alloc a @ lo = (heap @ global, stack @ local)] init n ~f =
  if n < 0 then Printf.invalid_argf "String.init %d" n ();
  (let t = (Bytes.create [@alloc a]) n in
   for i = 0 to n - 1 do
     Bytes.set t i (f i)
   done;
   Bytes.unsafe_to_string ~no_mutation_while_string_reachable:t)
  [@exclave_if_stack a]
;;

(* [filter t f] is implemented by the following algorithm.

   Let [n = length t].

   1. Find the lowest [i] such that [not (f t.[i])].

   2. If there is no such [i], then return [t].

   3. If there is such an [i], allocate a string, [out], to hold the result. [out] has
      length [n - 1], which is the maximum possible output size given that there is at
      least one character not satisfying [f].

   4. Copy characters at indices 0 ... [i - 1] from [t] to [out].

   5. Walk through characters at indices [i+1] ... [n-1] of [t], copying those that
      satisfy [f] from [t] to [out].

   6. If we completely filled [out], then return it. If not, return the prefix of [out]
      that we did fill in.

   This algorithm has the property that it doesn't allocate a new string if there's
   nothing to filter, which is a common case. *)

[%%template
[@@@alloc.default a @ l = (heap_global, stack_local)]

let filter t ~f =
  let n = length t in
  let i = ref 0 in
  while !i < n && f t.[!i] do
    incr i
  done;
  if [@exclave_if_stack a] !i = n
  then (smart_globalize [@alloc a]) t
  else (
    let out = (Bytes.create [@alloc a]) (n - 1) in
    Bytes.blit_string ~src:t ~src_pos:0 ~dst:out ~dst_pos:0 ~len:!i;
    let out_pos = ref !i in
    incr i;
    while !i < n do
      let c = t.[!i] in
      if f c
      then (
        Bytes.set out !out_pos c;
        incr out_pos);
      incr i
    done;
    let out =
      if !out_pos = n - 1 then out else (Bytes.sub [@alloc a]) out ~pos:0 ~len:!out_pos
    in
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:out)
;;

(* repeated code to avoid requiring an extra allocation for a closure on each call. *)
let filteri t ~f =
  let n = length t in
  let i = ref 0 in
  while !i < n && f !i t.[!i] do
    incr i
  done;
  if [@exclave_if_stack a] !i = n
  then (smart_globalize [@alloc a]) t
  else (
    let out = (Bytes.create [@alloc a]) (n - 1) in
    Bytes.blit_string ~src:t ~src_pos:0 ~dst:out ~dst_pos:0 ~len:!i;
    let out_pos = ref !i in
    incr i;
    while !i < n do
      let c = t.[!i] in
      if f !i c
      then (
        Bytes.set out !out_pos c;
        incr out_pos);
      incr i
    done;
    let out =
      if !out_pos = n - 1 then out else (Bytes.sub [@alloc a]) out ~pos:0 ~len:!out_pos
    in
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:out)
;;]
