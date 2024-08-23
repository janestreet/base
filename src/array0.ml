(* [Array0] defines array functions that are primitives or can be simply defined in terms
   of [Stdlib.Array].  [Array0] is intended to completely express the part of [Stdlib.Array]
   that [Base] uses -- no other file in Base other than array0.ml should use [Stdlib.Array].
   [Array0] has few dependencies, and so is available early in Base's build order.  All
   Base files that need to use arrays and come before [Base.Array] in build order should
   do [module Array = Array0].  This includes uses of subscript syntax ([x.(i)], [x.(i) <-
   e]), which the OCaml parser desugars into calls to [Array.get] and [Array.set].
   Defining [module Array = Array0] is also necessary because it prevents ocamldep from
   mistakenly causing a file to depend on [Base.Array]. *)

open! Import0
module Sys = Sys0

let invalid_argf = Printf.invalid_argf

module Array = struct
  external create : int -> 'a -> 'a array = "caml_make_vect"
  external create_local : int -> 'a -> local_ 'a array = "caml_make_local_vect"
  external create_float_uninitialized : int -> float array = "caml_make_float_vect"
  external get : ('a array[@local_opt]) -> (int[@local_opt]) -> 'a = "%array_safe_get"
  external length : ('a array[@local_opt]) -> int = "%array_length"

  external set
    :  ('a array[@local_opt])
    -> (int[@local_opt])
    -> 'a
    -> unit
    = "%array_safe_set"

  external unsafe_get
    :  ('a array[@local_opt])
    -> (int[@local_opt])
    -> 'a
    = "%array_unsafe_get"

  external unsafe_set
    :  ('a array[@local_opt])
    -> (int[@local_opt])
    -> 'a
    -> unit
    = "%array_unsafe_set"

  external unsafe_blit
    :  src:('a array[@local_opt])
    -> src_pos:int
    -> dst:('a array[@local_opt])
    -> dst_pos:int
    -> len:int
    -> unit
    = "caml_array_blit"

  external unsafe_fill : local_ 'a array -> int -> int -> 'a -> unit = "caml_array_fill"
  external unsafe_sub : local_ 'a array -> int -> int -> 'a array = "caml_array_sub"
  external concat : local_ 'a array list -> 'a array = "caml_array_concat"
end

include Array

let max_length = Sys.max_array_length

let create ~len x =
  try create len x with
  | Invalid_argument _ -> invalid_argf "Array.create ~len:%d: invalid length" len ()
;;

let create_local ~len x = exclave_
  try create_local len x with
  | Invalid_argument _ -> invalid_argf "Array.create_local ~len:%d: invalid length" len ()
;;

let create_float_uninitialized ~len =
  try create_float_uninitialized len with
  | Invalid_argument _ ->
    invalid_argf "Array.create_float_uninitialized ~len:%d: invalid length" len ()
;;

let append = Stdlib.Array.append

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

let fill (local_ a) ~pos ~len v =
  if pos < 0 || len < 0 || pos > length a - len
  then invalid_arg "Array.fill"
  else unsafe_fill a pos len v
;;

let init len ~(local_ f : _ -> _) =
  if len = 0
  then [||]
  else if len < 0
  then invalid_arg "Array.init"
  else (
    let res = create ~len (f 0) in
    for i = 1 to Int0.pred len do
      unsafe_set res i (f i)
    done;
    res)
;;

let make_matrix = Stdlib.Array.make_matrix
let of_list = Stdlib.Array.of_list

let sub (local_ a) ~pos ~len =
  if pos < 0 || len < 0 || pos > length a - len
  then invalid_arg "Array.sub"
  else unsafe_sub a pos len
;;

let to_list = Stdlib.Array.to_list

let fold t ~init ~(local_ f : _ -> _ -> _) =
  let r = ref init in
  for i = 0 to length t - 1 do
    r := f !r (unsafe_get t i)
  done;
  !r
;;

let fold_right t ~(local_ f : _ -> _ -> _) ~init =
  let r = ref init in
  for i = length t - 1 downto 0 do
    r := f (unsafe_get t i) !r
  done;
  !r
;;

let iter t ~(local_ f : _ -> _) =
  for i = 0 to length t - 1 do
    f (unsafe_get t i)
  done
;;

let iteri t ~(local_ f : _ -> _ -> _) =
  for i = 0 to length t - 1 do
    f i (unsafe_get t i)
  done
;;

let map (local_ t) ~(local_ f : _ -> _) =
  let len = length t in
  if len = 0
  then [||]
  else (
    let r = create ~len (f (unsafe_get t 0)) in
    for i = 1 to len - 1 do
      unsafe_set r i (f (unsafe_get t i))
    done;
    r)
;;

let mapi t ~(local_ f : _ -> _ -> _) =
  let len = length t in
  if len = 0
  then [||]
  else (
    let r = create ~len (f 0 (unsafe_get t 0)) in
    for i = 1 to len - 1 do
      unsafe_set r i (f i (unsafe_get t i))
    done;
    r)
;;

let stable_sort t ~compare = Stdlib.Array.stable_sort t ~cmp:compare

let swap (local_ t) i j =
  let elt_i = t.(i) in
  let elt_j = t.(j) in
  unsafe_set t i elt_j;
  unsafe_set t j elt_i
;;
