open Hash.Builtin
open Ppx_compare_lib.Builtin

include Sexplib0.Sexp

[@@deriving_inline compare, hash]
let rec (hash_fold_t :
           Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
  (fun hsv  ->
     fun arg  ->
       match arg with
       | Atom _a0 ->
         hash_fold_string (Ppx_hash_lib.Std.Hash.fold_int hsv 0) _a0
       | List _a0 ->
         hash_fold_list (fun hsv  -> fun arg  -> hash_fold_t hsv arg)
           (Ppx_hash_lib.Std.Hash.fold_int hsv 1) _a0 : Ppx_hash_lib.Std.Hash.state
       ->
         t ->
       Ppx_hash_lib.Std.Hash.state)

let (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
  fun arg  ->
    Ppx_hash_lib.Std.Hash.get_hash_value
      (hash_fold_t (Ppx_hash_lib.Std.Hash.create ()) arg)

let rec compare : t -> t -> int =
  fun a__001_  ->
  fun b__002_  ->
    if Ppx_compare_lib.phys_equal a__001_ b__002_
    then 0
    else
      (match (a__001_, b__002_) with
       | (Atom _a__003_,Atom _b__004_) -> compare_string _a__003_ _b__004_
       | (Atom _,_) -> (-1)
       | (_,Atom _) -> 1
       | (List _a__005_,List _b__006_) ->
         compare_list compare _a__005_ _b__006_)

[@@@end]
