open! Import

type t = Less | Equal | Greater [@@deriving_inline compare, hash, enumerate, sexp]
let compare : t -> t -> int =
  fun a__001_  ->
  fun b__002_  ->
    if Ppx_compare_lib.phys_equal a__001_ b__002_
    then 0
    else
      (match (a__001_, b__002_) with
       | (Less ,Less ) -> 0
       | (Less ,_) -> (-1)
       | (_,Less ) -> 1
       | (Equal ,Equal ) -> 0
       | (Equal ,_) -> (-1)
       | (_,Equal ) -> 1
       | (Greater ,Greater ) -> 0)

let (hash_fold_t :
       Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
  (fun hsv  ->
     fun arg  ->
       match arg with
       | Less  -> Ppx_hash_lib.Std.Hash.fold_int hsv 0
       | Equal  -> Ppx_hash_lib.Std.Hash.fold_int hsv 1
       | Greater  -> Ppx_hash_lib.Std.Hash.fold_int hsv 2 : Ppx_hash_lib.Std.Hash.state
       ->
         t ->
       Ppx_hash_lib.Std.Hash.state)

let (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
  let func arg =
    Ppx_hash_lib.Std.Hash.get_hash_value
      (let hsv = Ppx_hash_lib.Std.Hash.create ()  in hash_fold_t hsv arg)
  in
  fun x  -> func x
let all : t list = [Less; Equal; Greater]
let t_of_sexp : Sexplib.Sexp.t -> t =
  let _tp_loc = "src/ordering.ml.t"  in
  function
  | Sexplib.Sexp.Atom ("less"|"Less") -> Less
  | Sexplib.Sexp.Atom ("equal"|"Equal") -> Equal
  | Sexplib.Sexp.Atom ("greater"|"Greater") -> Greater
  | Sexplib.Sexp.List ((Sexplib.Sexp.Atom ("less"|"Less"))::_) as sexp ->
    Sexplib.Conv_error.stag_no_args _tp_loc sexp
  | Sexplib.Sexp.List ((Sexplib.Sexp.Atom ("equal"|"Equal"))::_) as sexp ->
    Sexplib.Conv_error.stag_no_args _tp_loc sexp
  | Sexplib.Sexp.List ((Sexplib.Sexp.Atom ("greater"|"Greater"))::_) as sexp
    -> Sexplib.Conv_error.stag_no_args _tp_loc sexp
  | Sexplib.Sexp.List ((Sexplib.Sexp.List _)::_) as sexp ->
    Sexplib.Conv_error.nested_list_invalid_sum _tp_loc sexp
  | Sexplib.Sexp.List [] as sexp ->
    Sexplib.Conv_error.empty_list_invalid_sum _tp_loc sexp
  | sexp -> Sexplib.Conv_error.unexpected_stag _tp_loc sexp
let sexp_of_t : t -> Sexplib.Sexp.t =
  function
  | Less  -> Sexplib.Sexp.Atom "Less"
  | Equal  -> Sexplib.Sexp.Atom "Equal"
  | Greater  -> Sexplib.Sexp.Atom "Greater"
[@@@end]

let equal a b = compare a b = 0

module Export = struct
  type _ordering = t =
    | Less
    | Equal
    | Greater
end

let of_int n =
  if n < 0
  then Less
  else if n = 0
  then Equal
  else Greater
;;

let to_int = function
  | Less    -> -1
  | Equal   -> 0
  | Greater -> 1
;;
