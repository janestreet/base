external magic : 'a 'b. ('a[@local_opt]) -> ('b[@local_opt]) = "%identity" [@@layout_poly]
external box_int64 : int64 -> int64 = "%identity"
external unbox_int64 : int64 -> int64 = "%identity"
external box_float : float -> float = "%identity"
external unbox_float : float -> float = "%identity"

external bits_of_float
  :  float
  -> int64
  = "caml_int64_bits_of_float" "caml_int64_bits_of_float_unboxed"
[@@unboxed] [@@noalloc] [@@builtin]

external float_of_bits
  :  int64
  -> float
  = "caml_int64_float_of_bits" "caml_int64_float_of_bits_unboxed"
[@@unboxed] [@@noalloc] [@@builtin]

let[@inline] bits_of_float f = unbox_int64 (bits_of_float (box_float f))
let[@inline] float_of_bits i = unbox_float (float_of_bits (box_int64 i))

[%%template
external select
  :  bool
  -> (int32[@unboxed])
  -> (int32[@unboxed])
  -> (int32[@unboxed])
  = "caml_csel_value" "caml_csel_int32_unboxed"
[@@kind k = bits32] [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

external select
  :  bool
  -> (int64[@unboxed])
  -> (int64[@unboxed])
  -> (int64[@unboxed])
  = "caml_csel_value" "caml_csel_int64_unboxed"
[@@kind k = bits64] [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

external select
  :  bool
  -> (nativeint[@unboxed])
  -> (nativeint[@unboxed])
  -> (nativeint[@unboxed])
  = "caml_csel_value" "caml_csel_nativeint_unboxed"
[@@kind k = word] [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

[@@@alloc a @ m = (stack @ local, heap @ global)]

external select
  : 'a.
  bool -> ('a[@local_opt]) -> ('a[@local_opt]) -> ('a[@local_opt])
  = "caml_csel_value"
[@@kind k = value_or_null_with_imm]
[@@mode m]
[@@noalloc]
[@@no_effects]
[@@no_coeffects]
[@@builtin]

let[@inline] select : type a. bool -> a -> a -> a =
  fun if_ then_ else_ -> magic ((select [@kind k]) if_ (magic then_) (magic else_))
[@@kind k = (bits32, bits64, word)] [@@mode m]
;;

let[@inline] select : type a. bool -> a -> a -> a =
  fun if_ then_ else_ ->
  magic
    (float_of_bits
       ((select [@kind bits64])
          if_
          (bits_of_float (magic then_))
          (bits_of_float (magic else_))))
[@@kind k = float64] [@@mode m]
;;

let[@inline] select : type a. bool -> a -> a -> a =
  fun if_ then_ else_ -> if if_ then then_ else else_
[@@kind k = float32] [@@mode m]
;;

let[@inline] select : type a. bool -> a -> a -> a =
  fun if_ then_ else_ ->
  (let (then1, then2) : Stdlib.Obj.t * Stdlib.Obj.t = magic then_ in
   let (else1, else2) : Stdlib.Obj.t * Stdlib.Obj.t = magic else_ in
   let r1 = (select [@kind value_or_null] [@mode m]) if_ then1 else1 in
   let r2 = (select [@kind value_or_null] [@mode m]) if_ then2 else2 in
   (magic (r1, r2) : a))
  [@exclave_if_stack a]
[@@kind k = (value & value)] [@@mode m]
;;

let[@inline] select : type a. bool -> a -> a -> a =
  fun if_ then_ else_ ->
  (let (then1, then2, then3) : Stdlib.Obj.t * Stdlib.Obj.t * Stdlib.Obj.t = magic then_ in
   let (else1, else2, else3) : Stdlib.Obj.t * Stdlib.Obj.t * Stdlib.Obj.t = magic else_ in
   let r1 = (select [@kind value_or_null] [@mode m]) if_ then1 else1 in
   let r2 = (select [@kind value_or_null] [@mode m]) if_ then2 else2 in
   let r3 = (select [@kind value_or_null] [@mode m]) if_ then3 else3 in
   (magic (r1, r2, r3) : a))
  [@exclave_if_stack a]
[@@kind k = (value & value & value)] [@@mode m]
;;

let[@inline] select : type a. bool -> a -> a -> a =
  fun if_ then_ else_ ->
  (let (then1, then2, then3, then4)
     : Stdlib.Obj.t * Stdlib.Obj.t * Stdlib.Obj.t * Stdlib.Obj.t
     =
     magic then_
   in
   let (else1, else2, else3, else4)
     : Stdlib.Obj.t * Stdlib.Obj.t * Stdlib.Obj.t * Stdlib.Obj.t
     =
     magic else_
   in
   let r1 = (select [@kind value_or_null] [@mode m]) if_ then1 else1 in
   let r2 = (select [@kind value_or_null] [@mode m]) if_ then2 else2 in
   let r3 = (select [@kind value_or_null] [@mode m]) if_ then3 else3 in
   let r4 = (select [@kind value_or_null] [@mode m]) if_ then4 else4 in
   (magic (r1, r2, r3, r4) : a))
  [@exclave_if_stack a]
[@@kind k = (value & value & value & value)] [@@mode m]
;;]
