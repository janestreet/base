(* [Array0] defines array functions that are primitives or can be simply defined in terms
   of [Stdlib.Array]. [Array0] is intended to completely express the part of
   [Stdlib.Array] that [Base] uses -- no other file in Base other than array0.ml should
   use [Stdlib.Array]. [Array0] has few dependencies, and so is available early in Base's
   build order. All Base files that need to use arrays and come before [Base.Array] in
   build order should do [module Array = Array0]. This includes uses of subscript syntax
   ([x.(i)], [x.(i) <- e]), which the OCaml parser desugars into calls to [Array.get] and
   [Array.set]. Defining [module Array = Array0] is also necessary because it prevents
   ocamldep from mistakenly causing a file to depend on [Base.Array]. *)

[@@@warning "-incompatible-with-upstream"]

open! Import0
module Sys = Sys0

let invalid_argf = Printf.invalid_argf

module Array = struct
  [%%template
    external create
      : ('a : any mod separable).
      len:int -> 'a -> 'a array @ m
      @@ portable
      = "%makearray_dynamic"
    [@@alloc __ @ m = (heap_global, stack_local)] [@@layout_poly]]

  external create_local
    : ('a : any mod separable).
    len:int -> 'a -> local_ 'a array
    @@ portable
    = "%makearray_dynamic"
  [@@layout_poly]

  external magic_create_uninitialized
    : ('a : any mod separable).
    len:int -> ('a array[@local_opt])
    @@ portable
    = "%makearray_dynamic_uninit"
  [@@layout_poly]

  external create_float_uninitialized
    :  int
    -> float array
    @@ portable
    = "caml_make_float_vect"

  external%template get
    : ('a : any mod separable).
    ('a array[@local_opt]) @ m -> (int[@local_opt]) -> 'a @ m
    @@ portable
    = "%array_safe_get"
  [@@layout_poly] [@@mode m = (uncontended, shared)]

  external length
    : ('a : any mod separable).
    ('a array[@local_opt]) @ immutable -> int
    @@ portable
    = "%array_length"
  [@@layout_poly]

  external set
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
    @@ portable
    = "%array_safe_set"
  [@@layout_poly]

  external%template unsafe_get
    : ('a : any mod separable).
    ('a array[@local_opt]) @ m -> (int[@local_opt]) -> 'a @ m
    @@ portable
    = "%array_unsafe_get"
  [@@mode m = (uncontended, shared)] [@@layout_poly]

  external unsafe_set
    : ('a : any mod separable).
    ('a array[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
    @@ portable
    = "%array_unsafe_set"
  [@@layout_poly]

  [%%template
  [@@@kind.default k = (value_or_null, value mod external64)]
  [@@@kind k = k mod separable]

  (* [unsafe_blit] can't be [[@noalloc]] because, even though it does not allocate, it
     still can induce a minor GC *)
  external unsafe_blit
    : ('a : k).
    src:('a array[@local_opt])
    -> src_pos:int
    -> dst:('a array[@local_opt])
    -> dst_pos:int
    -> len:int
    -> unit
    @@ portable
    = "caml_array_blit"

  external unsafe_fill
    : ('a : k).
    local_ 'a array -> int -> int -> 'a -> unit
    @@ portable
    = "caml_array_fill"

  external unsafe_sub
    : ('a : k).
    local_ 'a array -> int -> int -> 'a array
    @@ portable
    = "caml_array_sub"

  external concat
    : ('a : k).
    local_ 'a array list -> 'a array
    @@ portable
    = "caml_array_concat"]

  [%%template
  external unsafe_blit
    : ('a : k).
    src:('a array[@local_opt])
    -> src_pos:int
    -> dst:('a array[@local_opt])
    -> dst_pos:int
    -> len:int
    -> unit
    @@ portable
    = "caml_array_blit" "caml_unboxed_int64_vect_blit"
  [@@noalloc] [@@kind k = bits64]

  external unsafe_blit
    : ('a : k).
    src:('a array[@local_opt])
    -> src_pos:int
    -> dst:('a array[@local_opt])
    -> dst_pos:int
    -> len:int
    -> unit
    @@ portable
    = "caml_array_blit" "caml_unboxed_int32_vect_blit"
  [@@noalloc] [@@kind k = bits32]

  external unsafe_blit
    : ('a : k).
    src:('a array[@local_opt])
    -> src_pos:int
    -> dst:('a array[@local_opt])
    -> dst_pos:int
    -> len:int
    -> unit
    @@ portable
    = "caml_array_blit" "caml_unboxed_nativeint_vect_blit"
  [@@noalloc] [@@kind k = word]

  external unsafe_blit
    : ('a : k).
    src:('a array[@local_opt])
    -> src_pos:int
    -> dst:('a array[@local_opt])
    -> dst_pos:int
    -> len:int
    -> unit
    @@ portable
    = "base_array_unsafe_float_blit"
  [@@noalloc] [@@kind k = float64]

  external unsafe_blit
    : ('a : k).
    src:('a array[@local_opt])
    -> src_pos:int
    -> dst:('a array[@local_opt])
    -> dst_pos:int
    -> len:int
    -> unit
    @@ portable
    = "caml_array_blit" "caml_unboxed_float32_vect_blit"
  [@@noalloc] [@@kind k = float32]]
end

include Array

[%%template
[@@@kind.default k = base_non_value]

let unsafe_fill t pos len x =
  for i = pos to pos + len - 1 do
    unsafe_set t i x
  done
;;

let unsafe_sub t pos len =
  let res = magic_create_uninitialized ~len in
  (unsafe_blit [@kind k]) ~src:t ~src_pos:pos ~dst:res ~dst_pos:0 ~len;
  res
;;

let concat (ts @ local) =
  let len =
    (List0.fold [@mode local local]) ts ~init:0 ~f:(fun acc t -> acc + length t)
  in
  let res = magic_create_uninitialized ~len in
  let _total_length =
    (List0.fold [@mode local local]) ts ~init:0 ~f:(fun start t ->
      let len = length t in
      (unsafe_blit [@kind k]) ~src:t ~src_pos:0 ~dst:res ~dst_pos:start ~len;
      start + len)
  in
  res
;;]

let max_length = Sys.max_array_length

let create_float_uninitialized ~len =
  try create_float_uninitialized len with
  | Invalid_argument _ ->
    invalid_argf "Array.create_float_uninitialized ~len:%d: invalid length" len ()
;;

include struct
  external append_prim
    : ('a : value_or_null mod separable).
    'a array -> 'a array -> 'a array
    @@ portable
    = "caml_array_append"

  [%%template
  (* Copied from [Stdlib.Array], with type annotations added for templating *)
  let copy a =
    let l = length a in
    if l = 0 then [||] else unsafe_sub a 0 l
  ;;

  let%template append : type (a : k mod separable). a array -> a array -> a array =
    fun a1 a2 ->
    let l1 = length a1 in
    if l1 = 0
    then copy a2
    else if length a2 = 0
    then unsafe_sub a1 0 l1
    else append_prim a1 a2
  [@@kind k = (value_or_null, value mod external64)]
  ;;]
end

let%template append t1 t2 = (concat [@kind k]) [ t1; t2 ] [@@kind k = base_non_value]

let blit ~(local_ src) ~src_pos ~(local_ dst) ~dst_pos ~len =
  Ordered_collection_common0.check_pos_len_exn
    ~pos:src_pos
    ~len
    ~total_length:(length src);
  Ordered_collection_common0.check_pos_len_exn
    ~pos:dst_pos
    ~len
    ~total_length:(length dst);
  unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len
;;

let make_matrix = Stdlib.Array.make_matrix

let%template fold_right (t @ m) ~(local_ f : _ @ m -> _ -> _) ~init =
  let r = ref init in
  for i = length t - 1 downto 0 do
    r := f ((unsafe_get [@mode m]) t i) !r
  done;
  !r
[@@mode m = (uncontended, shared)]
;;

[%%template
[@@@kind.default k' = (base_or_null, value mod external64)]
[@@@kind k = k' mod separable]

let init len ~(local_ f : _ -> _) =
  if len = 0
  then [||]
  else if len < 0
  then invalid_arg "Array.init"
  else (
    (let res = (create [@alloc a]) ~len (f 0) in
     for i = 1 to Int0.pred len do
       unsafe_set res i (f i)
     done;
     res)
    [@exclave_if_stack a])
[@@alloc a = (heap, stack)]
;;

let sub : type (a : k). a array @ local -> pos:int -> len:int -> a array =
  fun a ~pos ~len ->
  if pos < 0 || len < 0 || pos > length a - len
  then invalid_arg "Array.sub"
  else (unsafe_sub [@kind k']) a pos len
;;

(* Copied from [Stdlib.Array], with type annotations added for templating *)
let to_list
  : type (a : k).
    a array @ local -> (a List0.Constructors.t[@kind k' or value_or_null]) @ m
  =
  fun t ->
  let rec tolist t i res : (_ List0.Constructors.t[@kind k' or value_or_null]) =
    if i < 0 then res else tolist t (i - 1) (unsafe_get t i :: res) [@exclave_if_stack a]
  in
  tolist t (length t - 1) [] [@exclave_if_stack a]
[@@alloc a @ m = (heap_global, stack_local)]
;;

(* Copied from [Stdlib.Array], with type annotations added for templating and some
   functions changed to the equivalent base ones *)
let of_list : type (a : k). (a List0.Constructors.t[@kind k' or value_or_null]) -> a array
  = function
  | ([] : (_ List0.Constructors.t[@kind k' or value_or_null])) -> [||]
  | hd :: tl as l ->
    let a = create ~len:((List0.length [@kind k' or value_or_null]) l) hd in
    let rec fill i = function
      | ([] : (_ List0.Constructors.t[@kind k' or value_or_null])) -> a
      | hd :: tl ->
        unsafe_set a i hd;
        fill (i + 1) tl
    in
    fill 1 tl
;;

let fill : type (a : k). a array @ local -> pos:int -> len:int -> a -> unit =
  fun a ~pos ~len v ->
  if pos < 0 || len < 0 || pos > length a - len
  then invalid_arg "Array.fill"
  else (unsafe_fill [@kind k']) a pos len v
;;

let swap (local_ t) i j =
  let elt_i = t.(i) in
  let elt_j = t.(j) in
  unsafe_set t i elt_j;
  unsafe_set t j elt_i
;;]

[@@@array.iter]

[%%template
  let iter (type a : ki mod separable) (t : a array) ~(local_ f : _ -> _) =
    for i = 0 to length t - 1 do
      f (unsafe_get t i)
    done
  [@@kind ki = (base_or_null, value mod external64)]
  ;;]

[@@@end]
[@@@array.iteri]

[%%template
  let iteri (type a : ki mod separable) (t : a array) ~(local_ f : _ -> _ -> _) =
    for i = 0 to length t - 1 do
      f i (unsafe_get t i)
    done
  [@@kind ki = (base_or_null, value mod external64)]
  ;;]

[@@@end]
[@@@array.fold]

[%%template
  let fold
    (type (a : ki mod separable) (b : ko))
    (t : a array)
    ~init
    ~(local_ f : _ -> _ -> _)
    : b
    =
    let length = length t in
    let rec loop i acc =
      if i < length then loop (i + 1) ((f [@inlined hint]) acc (unsafe_get t i)) else acc
    in
    (loop [@inlined]) 0 init [@nontail]
  [@@kind ki = (base, value mod external64), ko = (base, value mod external64)]
  ;;]

[@@@end]
[@@@array.map]

[%%template
  let map
    (type (a : ki mod separable) (b : ko))
    (local_ (t : a array))
    ~(local_ f : _ -> _)
    : b array
    =
    let len = length t in
    if len = 0
    then [||]
    else (
      let r = magic_create_uninitialized ~len in
      for i = 0 to len - 1 do
        unsafe_set r i (f (unsafe_get t i))
      done;
      r)
  [@@kind ki = (base, value mod external64), ko = base_non_value]
  ;;]

[%%template
  let map
    (type (a : ki mod separable) (b : ko))
    (local_ (t : a array))
    ~(local_ f : _ -> _)
    : b array
    =
    let len = length t in
    if len = 0
    then [||]
    else (
      let r = create ~len (f (unsafe_get t 0)) in
      for i = 1 to len - 1 do
        unsafe_set r i (f (unsafe_get t i))
      done;
      r)
  [@@kind ki = (base, value mod external64), ko = (value, value mod external64)]
  ;;]

[@@@end]
[@@@array.mapi]

[%%template
  let mapi (type (a : ki mod separable) (b : ko)) (t : a array) ~(local_ f : _ -> _ -> _)
    : b array
    =
    let len = length t in
    if len = 0
    then [||]
    else (
      let r = magic_create_uninitialized ~len in
      for i = 0 to len - 1 do
        unsafe_set r i (f i (unsafe_get t i))
      done;
      r)
  [@@kind ki = (base, value mod external64), ko = base_non_value]
  ;;]

[%%template
  let mapi (type (a : ki mod separable) (b : ko)) (t : a array) ~(local_ f : _ -> _ -> _)
    : b array
    =
    let len = length t in
    if len = 0
    then [||]
    else (
      let r = create ~len (f 0 (unsafe_get t 0)) in
      for i = 1 to len - 1 do
        unsafe_set r i (f i (unsafe_get t i))
      done;
      r)
  [@@kind ki = (base, value mod external64), ko = (value, value mod external64)]
  ;;]

[@@@end]

let stable_sort t ~compare = Stdlib.Array.stable_sort t ~cmp:compare
