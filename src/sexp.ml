open Globalize
open Hash.Builtin
open Ppx_compare_lib.Builtin
include Sexplib0.Sexp

(** Type of S-expressions *)
type t = Sexplib0.Sexp.t =
  | Atom of string
  | List of t list
[@@deriving_inline compare ~localize, globalize, hash]

let rec compare__local =
  (fun a__001_ b__002_ ->
     if Stdlib.( == ) a__001_ b__002_
     then 0
     else (
       match a__001_, b__002_ with
       | Atom _a__003_, Atom _b__004_ -> compare_string__local _a__003_ _b__004_
       | Atom _, _ -> -1
       | _, Atom _ -> 1
       | List _a__005_, List _b__006_ ->
         compare_list__local compare__local _a__005_ _b__006_)
    : t -> t -> int)
;;

let compare = (fun a b -> compare__local a b : t -> t -> int)

let rec (globalize : t -> t) =
  (fun x__009_ ->
     match x__009_ with
     | Atom arg__010_ -> Atom (globalize_string arg__010_)
     | List arg__011_ -> List (globalize_list globalize arg__011_)
    : t -> t)
;;

let rec (hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
  (fun hsv arg ->
     match arg with
     | Atom _a0 ->
       let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 0 in
       let hsv = hsv in
       hash_fold_string hsv _a0
     | List _a0 ->
       let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 1 in
       let hsv = hsv in
       hash_fold_list hash_fold_t hsv _a0
    : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state)

and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
  let func arg =
    Ppx_hash_lib.Std.Hash.get_hash_value
      (let hsv = Ppx_hash_lib.Std.Hash.create () in
       hash_fold_t hsv arg)
  in
  fun x -> func x
;;

[@@@end]

let t_sexp_grammar = Sexplib0.Sexp_conv.sexp_t_sexp_grammar
let of_string = ()
let invariant (_ : t) = ()
let equal__local a b = compare__local a b = 0
