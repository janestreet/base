open Basement.Primitives

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

let[@inline] bits_of_float f = Int64_u.unbox (bits_of_float (box_float f))
let[@inline] float_of_bits i = unbox_float (float_of_bits (Int64_u.box i))

external bits_of_float32
  :  float32_u
  -> Int32_u.t
  @@ portable
  = "caml_float32_to_bits_bytecode" "caml_float32_to_bits"
[@@noalloc] [@@builtin]

external float32_of_bits
  :  Int32_u.t
  -> float32_u
  @@ portable
  = "caml_float32_of_bits_bytecode" "caml_float32_of_bits"
[@@noalloc] [@@builtin]

[%%template
[@@@mode.default c = (uncontended, shared, contended)]
[@@@mode.default v = (read_write, read)]

external magic
  : ('a : any) ('b : any).
  ('a[@local_opt]) @ c v -> ('b[@local_opt]) @ c v
  @@ portable
  = "%identity"
[@@layout_poly]

external select
  :  bool
  -> (Int32_u.t[@unboxed])
  -> (Int32_u.t[@unboxed])
  -> (Int32_u.t[@unboxed])
  @@ portable
  = "caml_csel_value" "caml_csel_int32_unboxed"
[@@kind k = bits32] [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

external select
  :  bool
  -> (Int64_u.t[@unboxed])
  -> (Int64_u.t[@unboxed])
  -> (Int64_u.t[@unboxed])
  @@ portable
  = "caml_csel_value" "caml_csel_int64_unboxed"
[@@kind k = bits64] [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

external select
  :  bool
  -> (Nativeint_u.t[@unboxed])
  -> (Nativeint_u.t[@unboxed])
  -> (Nativeint_u.t[@unboxed])
  @@ portable
  = "caml_csel_value" "caml_csel_nativeint_unboxed"
[@@kind k = word] [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

[@@@mode.default m = (local, global)]

external select
  : ('a : k).
  bool -> ('a[@local_opt]) @ c v -> ('a[@local_opt]) @ c v -> ('a[@local_opt]) @ c v
  @@ portable
  = "caml_csel_value"
[@@kind k = value_or_null] [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

let[@inline] select : type (a : k). bool -> a @ c m v -> a @ c m v -> a @ c m v =
  fun if_ then_ else_ ->
  (magic [@mode v c])
    ((select [@kind k] [@mode v c])
       if_
       ((magic [@mode v c]) then_)
       ((magic [@mode v c]) else_))
[@@kind k = (bits32, bits64, word)]
;;

let[@inline] select : type (a : k). bool -> a @ c m v -> a @ c m v -> a @ c m v =
  fun if_ then_ else_ ->
  (magic [@mode v c])
    (float_of_bits
       ((select [@kind bits64])
          if_
          (bits_of_float ((magic [@mode v c]) then_))
          (bits_of_float ((magic [@mode v c]) else_))))
[@@kind k = float64]
;;

let[@inline] select : type (a : k). bool -> a @ c m v -> a @ c m v -> a @ c m v =
  fun if_ then_ else_ ->
  (magic [@mode v c])
    (float32_of_bits
       ((select [@kind bits32])
          if_
          (bits_of_float32 ((magic [@mode v c]) then_))
          (bits_of_float32 ((magic [@mode v c]) else_))))
[@@kind k = float32]
;;

let[@inline] select : type (a : k). bool -> a @ c m v -> a @ c m v -> a @ c m v =
  fun if_ then_ else_ ->
  (let #(then1, then2) : #(Stdlib.Obj.t or_null * Stdlib.Obj.t or_null) =
     (magic [@mode v c]) then_
   in
   let #(else1, else2) : #(Stdlib.Obj.t or_null * Stdlib.Obj.t or_null) =
     (magic [@mode v c]) else_
   in
   let r1 = (select [@kind value_or_null] [@mode m v c]) if_ then1 else1 in
   let r2 = (select [@kind value_or_null] [@mode m v c]) if_ then2 else2 in
   ((magic [@mode v c]) #(r1, r2) : a))
  [@exclave_if_local m ~reasons:[ May_return_regional ]]
[@@kind k = (value_or_null & value_or_null)]
;;

let[@inline] select : type (a : k). bool -> a @ c m v -> a @ c m v -> a @ c m v =
  fun if_ then_ else_ ->
  (let #(then1, then2) : #(Stdlib.Obj.t or_null * Int64_u.t) =
     (magic [@mode v c]) then_
   in
   let #(else1, else2) : #(Stdlib.Obj.t or_null * Int64_u.t) =
     (magic [@mode v c]) else_
   in
   let r1 = (select [@kind value_or_null] [@mode m v c]) if_ then1 else1 in
   let r2 = (select [@kind bits64]) if_ then2 else2 in
   ((magic [@mode v c]) #(r1, r2) : a))
  [@exclave_if_local m ~reasons:[ May_return_regional ]]
[@@kind k = (value_or_null & bits64)]
;;

let[@inline] select : type (a : k). bool -> a @ c m v -> a @ c m v -> a @ c m v =
  fun if_ then_ else_ ->
  (let #(then1, then2, then3)
     : #(Stdlib.Obj.t or_null * Stdlib.Obj.t or_null * Stdlib.Obj.t or_null)
     =
     (magic [@mode v c]) then_
   in
   let #(else1, else2, else3)
     : #(Stdlib.Obj.t or_null * Stdlib.Obj.t or_null * Stdlib.Obj.t or_null)
     =
     (magic [@mode v c]) else_
   in
   let r1 = (select [@kind value_or_null] [@mode m v c]) if_ then1 else1 in
   let r2 = (select [@kind value_or_null] [@mode m v c]) if_ then2 else2 in
   let r3 = (select [@kind value_or_null] [@mode m v c]) if_ then3 else3 in
   ((magic [@mode v c]) #(r1, r2, r3) : a))
  [@exclave_if_local m ~reasons:[ May_return_regional ]]
[@@kind k = (value_or_null & value_or_null & value_or_null)]
;;

let[@inline] select : type (a : k). bool -> a @ c m v -> a @ c m v -> a @ c m v =
  fun if_ then_ else_ ->
  (let #(then1, then2, then3, then4)
     : #(Stdlib.Obj.t or_null
        * Stdlib.Obj.t or_null
        * Stdlib.Obj.t or_null
        * Stdlib.Obj.t or_null)
     =
     (magic [@mode v c]) then_
   in
   let #(else1, else2, else3, else4)
     : #(Stdlib.Obj.t or_null
        * Stdlib.Obj.t or_null
        * Stdlib.Obj.t or_null
        * Stdlib.Obj.t or_null)
     =
     (magic [@mode v c]) else_
   in
   let r1 = (select [@kind value_or_null] [@mode m v c]) if_ then1 else1 in
   let r2 = (select [@kind value_or_null] [@mode m v c]) if_ then2 else2 in
   let r3 = (select [@kind value_or_null] [@mode m v c]) if_ then3 else3 in
   let r4 = (select [@kind value_or_null] [@mode m v c]) if_ then4 else4 in
   ((magic [@mode v c]) #(r1, r2, r3, r4) : a))
  [@exclave_if_local m ~reasons:[ May_return_regional ]]
[@@kind k = (value_or_null & value_or_null & value_or_null & value_or_null)]
;;]
