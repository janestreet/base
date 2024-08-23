open! Import
open Modes.Export

include (
struct
  type 'a t = 'a option
  [@@deriving_inline compare ~localize, globalize, hash, sexp, sexp_grammar]

  let compare__local
    : 'a. (local_ 'a -> local_ 'a -> int) -> local_ 'a t -> local_ 'a t -> int
    =
    compare_option__local
  ;;

  let compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int = compare_option

  let globalize : 'a. (local_ 'a -> 'a) -> local_ 'a t -> 'a t =
    fun (type a__009_) : ((local_ a__009_ -> a__009_) -> local_ a__009_ t -> a__009_ t) ->
    globalize_option
  ;;

  let hash_fold_t
    : 'a.
    (Ppx_hash_lib.Std.Hash.state -> 'a -> Ppx_hash_lib.Std.Hash.state)
    -> Ppx_hash_lib.Std.Hash.state
    -> 'a t
    -> Ppx_hash_lib.Std.Hash.state
    =
    hash_fold_option
  ;;

  let t_of_sexp : 'a. (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t = option_of_sexp
  let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t = sexp_of_option

  let t_sexp_grammar : 'a. 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t =
    fun _'a_sexp_grammar -> option_sexp_grammar _'a_sexp_grammar
  ;;

  [@@@end]
end :
sig
  type 'a t = 'a option
  [@@deriving_inline compare ~localize, globalize, hash, sexp, sexp_grammar]

  include Ppx_compare_lib.Comparable.S1 with type 'a t := 'a t
  include Ppx_compare_lib.Comparable.S_local1 with type 'a t := 'a t

  val globalize : (local_ 'a -> 'a) -> local_ 'a t -> 'a t

  include Ppx_hash_lib.Hashable.S1 with type 'a t := 'a t
  include Sexplib0.Sexpable.S1 with type 'a t := 'a t

  val t_sexp_grammar : 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t

  [@@@end]
end)

type 'a t = 'a option =
  | None
  | Some of 'a

let is_none = function
  | None -> true
  | _ -> false
;;

let is_some = function
  | Some _ -> true
  | _ -> false
;;

let value_map_local o ~default ~f = exclave_
  match o with
  | Some x -> f x
  | None -> default
;;

let[@inline] value_map o ~default ~f =
  (value_map_local
     (Modes.Global.wrap_option o)
     ~default:{ global = default }
     ~f:(Modes.Global.map ~f))
    .global
;;

let iter_local o ~f =
  match o with
  | None -> ()
  | Some a -> f a
;;

let[@inline] iter o ~f =
  iter_local (Modes.Global.wrap_option o) ~f:(fun x -> f x.global) [@nontail]
;;

let invariant f t = iter t ~f

let call x ~f =
  match f with
  | None -> ()
  | Some f -> f x
;;

let value_local t ~default = exclave_
  match t with
  | None -> default
  | Some x -> x
;;

let[@inline] value t ~default =
  (value_local (Modes.Global.wrap_option t) ~default:{ global = default }).global
;;

let value_local_exn ?here ?error ?message t = exclave_
  match t with
  | Some x -> x
  | None ->
    let error =
      match here, error, message with
      | None, None, None -> Error.of_string "Option.value_exn None"
      | None, None, Some m -> Error.of_string m
      | None, Some e, None -> e
      | None, Some e, Some m -> Error.tag e ~tag:m
      | Some p, None, None ->
        Error.create "Option.value_exn" p Source_code_position0.sexp_of_t
      | Some p, None, Some m -> Error.create m p Source_code_position0.sexp_of_t
      | Some p, Some e, _ ->
        Error.create
          (value message ~default:"")
          (e, p)
          (sexp_of_pair Error.sexp_of_t Source_code_position0.sexp_of_t)
    in
    Error.raise error
;;

let[@inline] value_exn ?here ?error ?message t =
  (value_local_exn ?here ?error ?message (Modes.Global.wrap_option t)).global
;;

let value_or_thunk_local o ~default = exclave_
  match o with
  | Some x -> x
  | None -> default ()
;;

let[@inline] value_or_thunk o ~default =
  (value_or_thunk_local (Modes.Global.wrap_option o) ~default:(fun () ->
     exclave_ { global = default () }))
    .global
;;

let to_array t =
  match t with
  | None -> [||]
  | Some x -> [| x |]
;;

let to_list_local t = exclave_
  match t with
  | None -> []
  | Some x -> [ x ]
;;

let to_list t =
  match t with
  | None -> []
  | Some x -> [ x ]
;;

let for_all t ~f =
  match t with
  | None -> true
  | Some x -> f x
;;

let exists t ~f =
  match t with
  | None -> false
  | Some x -> f x
;;

let mem t a ~equal =
  match t with
  | None -> false
  | Some a' -> equal a a'
;;

let length t =
  match t with
  | None -> 0
  | Some _ -> 1
;;

let fold t ~init ~f =
  match t with
  | None -> init
  | Some x -> f init x
;;

let find t ~f =
  match t with
  | None -> None
  | Some x -> if f x then t else None
;;

let find_map t ~f =
  match t with
  | None -> None
  | Some a -> f a
;;

let equal f t t' =
  match t, t' with
  | None, None -> true
  | Some x, Some x' -> f x x'
  | _ -> false
;;

let equal__local f t t' =
  match t, t' with
  | None, None -> true
  | Some x, Some x' -> f x x'
  | _ -> false
;;

let some x = Some x
let some_local x = exclave_ Some x

let first_some_local x y = exclave_
  match x with
  | Some _ -> x
  | None -> y
;;

let first_some x y =
  match x with
  | Some _ -> x
  | None -> y
;;

let first_some_thunk x y =
  match x with
  | Some _ -> x
  | None -> y ()
;;

let first_some_thunk_local x y = exclave_
  match x with
  | Some _ -> x
  | None -> y ()
;;

let some_if_local cond x = exclave_ if cond then Some x else None
let some_if cond x = if cond then Some x else None
let some_if_thunk cond thunk = if cond then Some (thunk ()) else None
let some_if_thunk_local cond thunk = exclave_ if cond then Some (thunk ()) else None

let merge a b ~f =
  match a, b with
  | None, x | x, None -> x
  | Some a, Some b -> Some (f a b)
;;

let filter t ~f =
  match t with
  | Some v as o when f v -> o
  | _ -> None
;;

let try_with f =
  match f () with
  | x -> Some x
  | exception _ -> None
;;

let try_with_join f =
  match f () with
  | x -> x
  | exception _ -> None
;;

let map_local t ~f = exclave_
  match t with
  | None -> None
  | Some a -> Some (f a)
;;

let map t ~f =
  match t with
  | None -> None
  | Some a -> Some (f a)
;;

module Monad_arg = struct
  type 'a t = 'a option

  let return x = Some x
  let map = `Custom map

  let bind o ~f =
    match o with
    | None -> None
    | Some x -> f x
  ;;
end

include Monad.Make_local (Monad_arg)

module Applicative_arg = struct
  type 'a t = 'a option

  let return x = Some x
  let map = `Custom map

  let map2 x y ~f =
    match x, y with
    | None, _ | _, None -> None
    | Some x, Some y -> Some (f x y)
  ;;
end

include Applicative.Make_using_map2_local (Applicative_arg)
