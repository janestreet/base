external magic
  : ('a : any) ('b : any).
  ('a[@local_opt]) -> ('b[@local_opt])
  @@ portable
  = "%identity"
[@@layout_poly]

external box_int64 : (int64#[@unboxed]) -> local_ int64 @@ portable = "%box_int64"
external unbox_int64 : local_ int64 -> (int64#[@unboxed]) @@ portable = "%unbox_int64"
external box_float : (float#[@unboxed]) -> local_ float @@ portable = "%box_float"
external unbox_float : local_ float -> (float#[@unboxed]) @@ portable = "%unbox_float"

external bits_of_float
  :  float @ local
  -> int64 @ local
  @@ portable
  = "caml_int64_bits_of_float" "caml_int64_bits_of_float_unboxed"
[@@unboxed] [@@noalloc] [@@builtin]

external float_of_bits
  :  int64 @ local
  -> float @ local
  @@ portable
  = "caml_int64_float_of_bits" "caml_int64_float_of_bits_unboxed"
[@@unboxed] [@@noalloc] [@@builtin]

let[@inline] bits_of_float f = unbox_int64 (bits_of_float (box_float f))
let[@inline] float_of_bits i = unbox_float (float_of_bits (box_int64 i))

[%%template
external select
  :  bool
  -> (int32#[@unboxed])
  -> (int32#[@unboxed])
  -> (int32#[@unboxed])
  @@ portable
  = "caml_csel_value" "caml_csel_int32_unboxed"
[@@kind k = bits32] [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

external select
  :  bool
  -> (int64#[@unboxed])
  -> (int64#[@unboxed])
  -> (int64#[@unboxed])
  @@ portable
  = "caml_csel_value" "caml_csel_int64_unboxed"
[@@kind k = bits64] [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

external select
  :  bool
  -> (nativeint#[@unboxed])
  -> (nativeint#[@unboxed])
  -> (nativeint#[@unboxed])
  @@ portable
  = "caml_csel_value" "caml_csel_nativeint_unboxed"
[@@kind k = word] [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

[@@@mode.default m = (local, global)]

external select
  : ('a : k).
  bool -> ('a[@local_opt]) -> ('a[@local_opt]) -> ('a[@local_opt])
  @@ portable
  = "caml_csel_value"
[@@kind k = value_or_null] [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

let[@inline] select : type (a : k). bool -> a @ m -> a @ m -> a @ m =
  fun if_ then_ else_ -> magic ((select [@kind k]) if_ (magic then_) (magic else_))
[@@kind k = (bits32, bits64, word)]
;;

let[@inline] select : type (a : k). bool -> a @ m -> a @ m -> a @ m =
  fun if_ then_ else_ ->
  magic
    (float_of_bits
       ((select [@kind bits64])
          if_
          (bits_of_float (magic then_))
          (bits_of_float (magic else_))))
[@@kind k = float64]
;;]
