open! Import
module Constructors = Option0
module Error = Error0
module Iarray = Iarray0
module Nothing = Nothing0
module Source_code_position = Source_code_position0
include Constructors

[%%rederive.portable
  type 'a t = 'a option [@@deriving compare, globalize, hash, sexp_grammar]]

include struct
  open struct
    type nonrec 'a t = 'a t =
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

  (* Copied and templated from [Sexplib0] *)

  let sexp_of_t sexp_of__a option : Sexplib0.Sexp.t =
    let write_old_option_format = Dynamic.get write_old_option_format in
    match option with
    | Some x when write_old_option_format -> List [ sexp_of__a x ]
    | Some x -> List [ Atom "some"; sexp_of__a x ]
    | None when write_old_option_format -> List []
    | None -> Atom "none"
  ;;
end

include struct
  open struct
    type nonrec 'a t = 'a t =
      | None
      | Some of 'a
  end

  let value t ~default =
    match t with
    | None -> default
    | Some x -> x
  ;;

  let value_exn ?(here = Stdlib.Lexing.dummy_pos) ?error ?message t =
    match t with
    | Some x -> x
    | None ->
      let error =
        match error, message with
        | None, None ->
          if Source_code_position.is_dummy here
          then Error.of_string "Option.value_exn None"
          else Error.create "Option.value_exn None" here Source_code_position.sexp_of_t
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
    | None -> default ()
  ;;

  let iter o ~f =
    match o with
    | None -> ()
    | Some a -> f a
  ;;

  let value_map t ~default ~f =
    match t with
    | Some x -> f x
    | None -> default
  ;;
end

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

let to_iarray t =
  match t with
  | None -> Iarray.unsafe_of_array__promise_no_mutation [||]
  | Some x -> Iarray.unsafe_of_array__promise_no_mutation [| x |]
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

let equal f (t : _ t) (t' : _ t) =
  match t, t' with
  | None, None -> true
  | Some x, Some x' -> f x x'
  | _ -> false
;;

let some x = Some x

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

let some_if cond x = if cond then Some x else None
let some_if_thunk cond thunk = if cond then Some (thunk ()) else None

let merge a b ~f =
  match a, b with
  | None, x | x, None -> x
  | Some a, Some b -> Some (f a b)
;;

let transpose_result t =
  match t with
  | Some (Ok v) -> Ok (Some v)
  | Some (Error e) -> Error e
  | None -> Ok None
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

let return a : _ t = Some a

let map (t : _ t) ~f : _ t =
  match t with
  | None -> None
  | Some a -> Some (f a)
;;

let bind (t : _ t) ~f : _ t =
  match t with
  | None -> None
  | Some a -> f a
;;

module Monad_arg = struct
  type 'a t = 'a option

  let return = return
  let custom_map = map
  let map = `Custom custom_map
  let bind = bind
end

include Monad.Make (Monad_arg)

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

include Applicative.Make_using_map2 (Applicative_arg)

module Local = struct
  module Let_syntax = struct
    let return = return
    let ( >>| ) x f = map x ~f
    let ( >>= ) x f = bind x ~f

    module Let_syntax = struct
      let return = return
      let map = map
      let bind = bind
      let both = both

      module Open_on_rhs = struct end
    end
  end
end

include struct
  let map = Monad_arg.custom_map
  let bind = Monad_arg.bind
end
