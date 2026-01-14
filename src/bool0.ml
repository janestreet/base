[@@@warning "-incompatible-with-upstream"]

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
[@@@mode.default c = (uncontended, shared, contended)]

external magic
  : ('a : any) ('b : any).
  ('a[@local_opt]) @ c -> ('b[@local_opt]) @ c
  @@ portable
  = "%identity"
[@@layout_poly]

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
  bool -> ('a[@local_opt]) @ c -> ('a[@local_opt]) @ c -> ('a[@local_opt]) @ c
  @@ portable
  = "caml_csel_value"
[@@kind k = value_or_null_with_imm]
[@@noalloc]
[@@no_effects]
[@@no_coeffects]
[@@builtin]

let[@inline] select : type (a : k). bool -> a @ c m -> a @ c m -> a @ c m =
  fun if_ then_ else_ ->
  (magic [@mode c])
    ((select [@kind k] [@mode c]) if_ ((magic [@mode c]) then_) ((magic [@mode c]) else_))
[@@kind k = (bits32, bits64, word)]
;;

let[@inline] select : type (a : k). bool -> a @ c m -> a @ c m -> a @ c m =
  fun if_ then_ else_ ->
  (magic [@mode c])
    (float_of_bits
       ((select [@kind bits64])
          if_
          (bits_of_float ((magic [@mode c]) then_))
          (bits_of_float ((magic [@mode c]) else_))))
[@@kind k = float64]
;;

let[@inline] select : type (a : k). bool -> a @ c m -> a @ c m -> a @ c m =
  fun if_ then_ else_ -> if if_ then then_ else else_
[@@kind k = float32]
;;

let[@inline] select : type (a : k). bool -> a @ c m -> a @ c m -> a @ c m =
  fun if_ then_ else_ ->
  (let #(then1, then2) : #(Stdlib.Obj.t * Stdlib.Obj.t) = (magic [@mode c]) then_ in
   let #(else1, else2) : #(Stdlib.Obj.t * Stdlib.Obj.t) = (magic [@mode c]) else_ in
   let r1 = (select [@kind value_or_null] [@mode m c]) if_ then1 else1 in
   let r2 = (select [@kind value_or_null] [@mode m c]) if_ then2 else2 in
   ((magic [@mode c]) #(r1, r2) : a))
  [@exclave_if_local m ~reasons:[ May_return_regional ]]
[@@kind k = (value & value)]
;;

let[@inline] select : type (a : k). bool -> a @ c m -> a @ c m -> a @ c m =
  fun if_ then_ else_ ->
  (let #(then1, then2, then3) : #(Stdlib.Obj.t * Stdlib.Obj.t * Stdlib.Obj.t) =
     (magic [@mode c]) then_
   in
   let #(else1, else2, else3) : #(Stdlib.Obj.t * Stdlib.Obj.t * Stdlib.Obj.t) =
     (magic [@mode c]) else_
   in
   let r1 = (select [@kind value_or_null] [@mode m c]) if_ then1 else1 in
   let r2 = (select [@kind value_or_null] [@mode m c]) if_ then2 else2 in
   let r3 = (select [@kind value_or_null] [@mode m c]) if_ then3 else3 in
   ((magic [@mode c]) #(r1, r2, r3) : a))
  [@exclave_if_local m ~reasons:[ May_return_regional ]]
[@@kind k = (value & value & value)]
;;

let[@inline] select : type (a : k). bool -> a @ c m -> a @ c m -> a @ c m =
  fun if_ then_ else_ ->
  (let #(then1, then2, then3, then4)
     : #(Stdlib.Obj.t * Stdlib.Obj.t * Stdlib.Obj.t * Stdlib.Obj.t)
     =
     (magic [@mode c]) then_
   in
   let #(else1, else2, else3, else4)
     : #(Stdlib.Obj.t * Stdlib.Obj.t * Stdlib.Obj.t * Stdlib.Obj.t)
     =
     (magic [@mode c]) else_
   in
   let r1 = (select [@kind value_or_null] [@mode m c]) if_ then1 else1 in
   let r2 = (select [@kind value_or_null] [@mode m c]) if_ then2 else2 in
   let r3 = (select [@kind value_or_null] [@mode m c]) if_ then3 else3 in
   let r4 = (select [@kind value_or_null] [@mode m c]) if_ then4 else4 in
   ((magic [@mode c]) #(r1, r2, r3, r4) : a))
  [@exclave_if_local m ~reasons:[ May_return_regional ]]
[@@kind k = (value & value & value & value)]
;;]
