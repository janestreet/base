open! Import
module Constructors = Option0
include Constructors

type 'a t = 'a option =
  | None
  | Some of 'a

[%%rederive.portable
  type 'a t = 'a option [@@deriving compare ~localize, globalize, hash, sexp_grammar]]

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

include struct
  [@@@kind.default k]

  let t_of_sexp a__of_sexp (sexp : Sexplib0.Sexp.t) : _ t =
    if Dynamic.get read_old_option_format
    then (
      match sexp with
      | List [] | Atom ("none" | "None") -> None
      | List [ el ] | List [ Atom ("some" | "Some"); el ] -> Some (a__of_sexp el)
      | List _ -> of_sexp_error "option_of_sexp: list must represent optional value" sexp
      | Atom _ -> of_sexp_error "option_of_sexp: only none can be atom" sexp)
    else (
      match sexp with
      | Atom ("none" | "None") -> None
      | List [ Atom ("some" | "Some"); el ] -> Some (a__of_sexp el)
      | Atom _ -> of_sexp_error "option_of_sexp: only none can be atom" sexp
      | List _ -> of_sexp_error "option_of_sexp: list must be (some el)" sexp)
  [@@kind k]
  ;;

  [@@@alloc a @ m = (heap_global, stack_local)]
  [@@@mode.default m]

  (* Copied and templated from [Sexplib0] *)

  let sexp_of_t sexp_of__a option : Sexplib0.Sexp.t =
    let write_old_option_format = Dynamic.get write_old_option_format in
    match[@exclave_if_stack a] option with
    | Some x when write_old_option_format -> List [ sexp_of__a x ]
    | Some x -> List [ Atom "some"; sexp_of__a x ]
    | None when write_old_option_format -> List []
    | None -> Atom "none"
  [@@kind k] [@@mode m]
  ;;
end

[@@@mode.default m = (global, local)]

let value t ~default =
  match t with
  | None -> default
  | Some x -> x
;;

let value_exn ~(here : [%call_pos]) ?error ?message t =
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
    (match Error.raise error with
     | (_ : Nothing.t) -> .)
;;

let value_or_thunk o ~default =
  match o with
  | Some x -> x
  | None -> default () [@exclave_if_local m]
;;

let iter o ~f =
  match o with
  | None -> ()
  | Some a -> f a
;;

let value_map t ~default ~(local_ f) =
  match t with
  | Some x -> f x [@exclave_if_local m]
  | None -> default
[@@kind ki = k, ko = (value, float64, bits32, bits64, word)]
;;]

let t__float64_of_sexp = t_of_sexp__float64
let t__bits32_of_sexp = t_of_sexp__bits32
let t__bits64_of_sexp = t_of_sexp__bits64
let t__word_of_sexp = t_of_sexp__word
let invariant f t = iter t ~f

let call x ~f =
  match f with
  | None -> ()
  | Some f -> f x
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

[%%template
[@@@kind.default
  ki = (value, float64, bits32, bits64, word), ko = (value, float64, bits32, bits64, word)]

let[@mode local] map (t : (_ t[@kind ki])) ~f : (_ t[@kind ko]) = exclave_
  match t with
  | None -> None
  | Some a -> Some (f a)
;;

let[@mode global] map (t : (_ t[@kind ki])) ~f : (_ t[@kind ko]) =
  match t with
  | None -> None
  | Some a -> Some (f a)
;;]

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
