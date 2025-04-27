open! Import

module%template Constructors = struct
  type nonrec ('a : k) t =
    | None
    | Some of 'a
  [@@kind k = (float64, bits32, bits64, word)]
  [@@deriving sexp ~localize, compare ~localize]

  type 'a t = 'a option =
    | None
    | Some of 'a
end

include Constructors

include (
struct
  type 'a t = 'a option
  [@@deriving compare ~localize, globalize, hash, sexp ~localize, sexp_grammar]
end :
sig
@@ portable
  type 'a t = 'a option
  [@@deriving compare ~localize, globalize, hash, sexp ~localize, sexp_grammar]
end)

type 'a t = 'a option =
  | None
  | Some of 'a

[%%template
[@@@kind.default k = (value, float64, bits32, bits64, word)]

open struct
  type nonrec ('a : k) t = ('a t[@kind k]) =
    | None
    | Some of 'a
end

let is_none = function
  | None -> true
  | _ -> false
;;

let is_some = function
  | Some _ -> true
  | _ -> false
;;

[@@@mode.default m = (global, local)]

let value_map t ~default ~(local_ f) =
  match t with
  | Some x -> f x [@exclave_if_local m]
  | None -> default
[@@kind ki = k, ko = (value, float64, bits32, bits64, word)]
;;

let iter o ~f =
  match o with
  | None -> ()
  | Some a -> f a
;;]

let invariant f t = iter t ~f

let call x ~f =
  match f with
  | None -> ()
  | Some f -> f x
;;

let%template value t ~default =
  match t with
  | None -> default
  | Some x -> x
[@@mode m = (global, local)]
;;

let%template value_exn ~(here : [%call_pos]) ?error ?message t =
  match t with
  | Some x -> x
  | None ->
    let error =
      match error, message with
      | None, None ->
        if Source_code_position.is_dummy here
        then Error.of_string "Option.value_exn None"
        else Error.create "Option.value_exn None" here Source_code_position0.sexp_of_t
      | Some e, None -> e
      | None, Some m -> Error.of_string m
      | Some e, Some m -> Error.tag e ~tag:m
    in
    Error.raise error
[@@mode m = (global, local)]
;;

let%template value_or_thunk o ~default =
  match o with
  | Some x -> x
  | None -> default () [@exclave_if_local m]
[@@mode m = (global, local)]
;;

let to_array t =
  match t with
  | None -> [||]
  | Some x -> [| x |]
;;

let%template to_list t =
  match t with
  | None -> []
  | Some x -> [ x ] [@exclave_if_local m]
[@@mode m = (global, local)]
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

[%%template
[@@@mode.default m = (global, local)]

let equal f (t : (_ t[@kind k])) (t' : (_ t[@kind k])) =
  match t, t' with
  | None, None -> true
  | Some x, Some x' -> f x x'
  | _ -> false
[@@kind k = (value, float64, bits32, bits64, word)]
;;

let equal_t = (equal [@kind k] [@mode m]) [@@kind k = (float64, bits32, bits64, word)]
let some x = Some x [@exclave_if_local m]

let first_some x y =
  match x with
  | Some _ -> x
  | None -> y
;;

let first_some_thunk x y =
  match x with
  | Some _ -> x
  | None -> y () [@exclave_if_local m]
;;

let some_if cond x = if cond then Some x [@exclave_if_local m] else None]

let%template[@mode global] some_if_thunk cond thunk =
  if cond then Some (thunk ()) else None
;;

let%template[@mode local] some_if_thunk cond thunk = exclave_
  if cond then Some (thunk ()) else None
;;

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

let%template[@mode local] map t ~f = exclave_
  match t with
  | None -> None
  | Some a -> Some (f a)
;;

let%template[@mode global] map t ~f =
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

include%template Monad.Make [@mode local] [@modality portable] (Monad_arg)

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

include%template
  Applicative.Make_using_map2 [@mode local] [@modality portable] (Applicative_arg)
