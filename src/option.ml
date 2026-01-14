open! Import
module Constructors = Option0
include Constructors

[@@@warning "-incompatible-with-upstream"]

type ('a : value_or_null) t = 'a option =
  | None
  | Some of 'a

[%%template
[@@@kind kr1 = (value & value)]
[@@@kind kr2 = (value & value & value)]
[@@@kind kr3 = (value & value & value & value)]

[%%rederive.portable
  type ('a : value_or_null) t = 'a option
  [@@deriving compare ~localize, globalize, hash, sexp_grammar]]

include struct
  [@@@kind.default k = (base_or_null, value & (base, kr1, kr2, kr3))]

  open struct
    type nonrec ('a : any) t = ('a t[@kind k]) =
      | None
      | Some of 'a
  end

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
  ;;

  [@@@alloc.default a = (heap, stack)]

  (* Copied and templated from [Sexplib0] *)

  let sexp_of_t sexp_of__a option : Sexplib0.Sexp.t =
    let write_old_option_format = Dynamic.get write_old_option_format in
    match[@exclave_if_stack a] option with
    | Some x when write_old_option_format -> List [ sexp_of__a x ]
    | Some x -> List [ Atom "some"; sexp_of__a x ]
    | None when write_old_option_format -> List []
    | None -> Atom "none"
  ;;
end

[%%template
[@@@kind.default k = (base_or_null, value & (base, kr1, kr2, kr3))]

open struct
  type nonrec ('a : any) t = ('a t[@kind k]) =
    | None
    | Some of 'a
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
[@@kind ki = k, ko = (base_or_null, value & (base, kr1, kr2, kr3))]
;;]

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
[@@kind k = (base_or_null, value & (base, kr1, kr2, kr3))]
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
  ki = (base_or_null, value & (base, kr1, kr2, kr3))
  , ko = (base_or_null, value & (base, kr1, kr2, kr3))]

let[@mode local] map (t : (_ t[@kind ki])) ~f : (_ t[@kind ko]) = exclave_
  match t with
  | None -> None
  | Some a -> Some (f a)
;;

let[@mode global] map (t : (_ t[@kind ki])) ~f : (_ t[@kind ko]) =
  match t with
  | None -> None
  | Some a -> Some (f a)
;;

[@@@mode.default m = (global, local)]

let bind (t : (_ t[@kind ki])) ~f : (_ t[@kind ko]) =
  match t with
  | None -> None
  | Some a -> f a [@exclave_if_local m]
;;]

[%%template
[@@@mode.default m = (global, local)]

let return = (some [@mode m])]

let%template[@mode local] both x y =
  match x, y with
  | None, _ | _, None -> None
  | Some x, Some y -> exclave_ Some (x, y)
;;

module Monad_arg = struct
  type ('a : value_or_null) t = 'a option

  let return = return
  let map = `Custom map
  let bind = bind
end

include%template
  Monad.Make [@kind value_or_null mod maybe_null] [@mode local] [@modality portable]
    (Monad_arg)

module Applicative_arg = struct
  type ('a : value_or_null) t = 'a option

  let return x = Some x
  let map = `Custom map

  let map2 x y ~f =
    match x, y with
    | None, _ | _, None -> None
    | Some x, Some y -> Some (f x y)
  ;;
end

include%template
  Applicative.Make_using_map2
    [@kind value_or_null mod maybe_null]
    [@mode local]
    [@modality portable]
    (Applicative_arg)

module%template Local = struct
  module Let_syntax = struct
    let return = (return [@mode local])
    let ( >>| ) x f = exclave_ (map [@mode local]) x ~f
    let ( >>= ) x f = exclave_ (bind [@mode local]) x ~f

    module Let_syntax = struct
      let return = (return [@mode local])
      let map = (map [@mode local])
      let bind = (bind [@mode local])
      let both = (both [@mode local])

      module Open_on_rhs = struct end
    end
  end
end]
