open! Import
module Error = Error0
module Nothing = Nothing0

type nonrec 'a t = ('a, Error.t) Result.t
[@@deriving compare, equal, globalize, hash, sexp, sexp_grammar]

type 'a or_error = 'a t

let ( >>= ) = Result.( >>= )
let ( >>| ) = Result.( >>| )
let ignore_m = Result.ignore_m
let join = Result.join
let return a = Result.return a
let bind = Result.bind
let map = Result.map

module Monad_infix = Result.Monad_infix

let invariant invariant_a t =
  match t with
  | Ok a -> invariant_a a
  | Error error -> Error.invariant error
;;

let map2 a b ~f =
  match a, b with
  | Ok x, Ok y -> Ok (f x y)
  | Ok _, (Error _ as e) | (Error _ as e), Ok _ -> e
  | Error e1, Error e2 -> Error (Error.of_list [ e1; e2 ])
;;

module For_applicative = Applicative.Make_using_map2 (struct
    type nonrec 'a t = 'a t

    let return = return
    let map = `Custom map
    let map2 = map2
  end)

let ( *> ) = For_applicative.( *> )
let ( <* ) = For_applicative.( <* )
let ( <*> ) = For_applicative.( <*> )
let apply = For_applicative.apply
let both = For_applicative.both
let map3 = For_applicative.map3

module Applicative_infix = For_applicative.Applicative_infix

module Let_syntax = struct
  let return = return

  include Monad_infix

  module Let_syntax = struct
    let return = return
    let map = map
    let bind = bind
    let both = both

    (* from Applicative.Make *)
    module Open_on_rhs = struct end
  end
end

let ok = Result.ok
let[@zero_alloc] ok_or_null = [%eta1 Result.ok_or_null]
let is_ok = Result.is_ok
let is_error = Result.is_error

let try_with (type a) ?(backtrace = false) f : a t =
  try Ok (f ()) with
  | exn -> Error (Error.of_exn exn ?backtrace:(if backtrace then Some `Get else None))
;;

let try_with_join ?backtrace f = join (try_with ?backtrace f)

let ok_exn : 'a. 'a t -> 'a = function
  | Ok x -> x
  | Error err ->
    (match Error.raise err with
     | (_ : Nothing.t) -> .)
;;

let of_exn ?backtrace exn = Error (Error.of_exn ?backtrace exn)

let of_exn_result ?backtrace = function
  | Ok _ as z -> z
  | Error exn -> of_exn ?backtrace exn
;;

let of_option = Result.of_option
let of_option_lazy_string t ~error = of_option t ~error:(Error.of_lazy error)
let of_option_lazy_sexp t ~error = of_option t ~error:(Error.of_lazy_sexp error)
let of_option_lazy t ~error = of_option t ~error:(Error.of_lazy_t error)
let of_or_null = Result.of_or_null
let of_or_null_lazy_string t ~error = of_or_null t ~error:(Error.of_lazy error)
let of_or_null_lazy_sexp t ~error = of_or_null t ~error:(Error.of_lazy_sexp error)
let of_or_null_lazy t ~error = of_or_null t ~error:(Error.of_lazy_t error)

let error ?here ?strict message a sexp_of_a =
  Error (Error.create ?here ?strict message a sexp_of_a)
;;

let error_s sexp : _ t = Error (Error.create_s sexp)
let error_string message : _ t = Error (Error.of_string message)
let errorf format = Printf.ksprintf error_string format
let errorf_portable format = Printf.ksprintf (fun string () -> error_string string) format

let tag t ~tag =
  match t with
  | Ok _ as ok -> ok
  | Error err -> Error (Error.tag err ~tag)
;;

let tag_s (t : _ t) ~tag =
  match t with
  | Ok _ as ok -> ok
  | Error err -> Error (Error.tag_s err ~tag)
;;

let tag_lazy t ~tag = Result.map_error t ~f:(Error.tag_lazy ~tag)
let tag_s_lazy t ~tag = Result.map_error t ~f:(Error.tag_s_lazy ~tag)

let tag_arg t message a sexp_of_a =
  match t with
  | Ok _ as ok -> ok
  | Error e ->
    let e = Error.tag_arg e message a sexp_of_a in
    Error e
;;

let unimplemented s = error "unimplemented" s [%eta1 sexp_of_string]

let combine_internal list ~on_ok ~on_error =
  match Result.combine_errors list with
  | Ok x -> Ok (on_ok x)
  | Error errs -> Error (on_error errs)
;;

let ignore_unit_list (_ : unit list) = ()

let error_of_list_if_necessary = function
  | [ e ] -> e
  | list -> Error.of_list list
;;

let all list = combine_internal list ~on_ok:Fn.id ~on_error:error_of_list_if_necessary

let all_unit list =
  combine_internal list ~on_ok:ignore_unit_list ~on_error:error_of_list_if_necessary
;;

let combine_errors list = combine_internal list ~on_ok:Fn.id ~on_error:Error.of_list

module Modes = struct
  include Modes

  module Portable = struct
    include Portable

    external wrap_result_list
      : 'a 'b.
      (('a, 'b) Result.t list[@local_opt]) -> (('a t, 'b t) Result.t list[@local_opt])
      = "%identity"

    external unwrap_list
      : 'a.
      ('a t list[@local_opt]) -> ('a list[@local_opt])
      = "%identity"
  end
end

let combine_errors_unit list =
  combine_internal list ~on_ok:ignore_unit_list ~on_error:Error.of_list
;;

let filter_ok_at_least_one l =
  match l with
  | [] -> error_string "filter_ok_at_least_one called on empty list"
  | l ->
    let ok, errs = List.partition_result l in
    (match ok with
     | [] -> Error (Error.of_list errs)
     | _ -> Ok ok)
;;

let find_ok l =
  match List.find_map l ~f:Result.ok with
  | Some x -> Ok x
  | None ->
    Error
      (Error.of_list
         (List.map l ~f:(function
           | Ok _ -> assert false
           | Error err -> err)))
;;

let find_map_ok l ~f =
  match l with
  | [] -> error_string "find_map_ok called on empty list"
  | l ->
    With_return.with_return (fun { return } ->
      Error
        (Error.of_list
           (List.map l ~f:(fun elt ->
              match f elt with
              | Ok _ as x -> return x
              | Error err -> err))))
    [@nontail]
;;

let map = Result.map
let iter = Result.iter
let iter_error = Result.iter_error

(* Portability *)

let of_portable : ('a, Error.Portable.t) Result.t -> 'a t = Modes.Portable.unwrap_error
let to_portable : 'a t -> ('a, Error.Portable.t) Result.t = Modes.Portable.wrap_error

let portabilize : 'a t -> ('a, Error.Portable.t) Result.t = function
  | Ok _ as ok -> ok
  | Error error -> Error (Error.to_portable_portabilize error)
;;

module Portable = struct
  type 'a t = ('a, Error.Portable.t) Result.t [@@deriving compare, equal, globalize, sexp]
  type 'a portable = 'a t

  let ( >>= ) = Result.( >>= )
  let ( >>| ) = Result.( >>| )
  let bind = Result.bind
  let ignore_m = Result.ignore_m
  let join = Result.join
  let map = Result.map
  let return = Result.return

  module Monad_infix = Result.Monad_infix

  let map2 a b ~f =
    match a, b with
    | Ok x, Ok y -> Ok (f x y)
    | Ok _, (Error _ as e) | (Error _ as e), Ok _ -> e
    | Error e1, Error e2 -> Error (Error.Portable.of_list [ e1; e2 ])
  ;;

  module For_applicative = Applicative.Make_using_map2 (struct
      type nonrec 'a t = 'a t

      let return = return
      let map = `Custom map
      let map2 = map2
    end)

  let ( *> ) = For_applicative.( *> )
  let ( <* ) = For_applicative.( <* )
  let ( <*> ) = For_applicative.( <*> )
  let apply = For_applicative.apply
  let both = For_applicative.both
  let map3 = For_applicative.map3

  module Applicative_infix = For_applicative.Applicative_infix

  module Let_syntax = struct
    let return = return

    include Monad_infix

    module Let_syntax = struct
      let return = return
      let map = map
      let bind = bind
      let both = both

      (* from Applicative.Make *)
      module Open_on_rhs = struct end
    end
  end

  (* [Portable_ok_and_err] allows us to treat ['a Or_error.Portable.t] and
     ['a Modes.Portable_via_contended.t Or_error.t @ portable] interchangeably. This is
     nice because it allows us to use [Or_error] functions that are templated over
     portability to implement [Or_error.Portable] functions.
  *)
  module Portable_ok_and_err = struct
    type 'a t = 'a Modes.Portable_via_contended.t or_error

    let _wrap_is_no_op : type a. a portable -> a t = function
      | Ok ok -> Ok (Modes.Portable_via_contended.wrap ok)
      | Error _ as error ->
        Modes.Portable.unwrap_error
          (error : (_ Modes.Portable_via_contended.t, Error.Portable.t) Result.t)
    ;;

    let _unwrap_is_no_op : type a. a t -> a portable = function
      | Ok ok -> Ok (Modes.Portable_via_contended.unwrap ok)
      | Error _ as error -> Modes.Portable.wrap_error error
    ;;

    (* The safety of the below functions hinge critically on the fact that
       [_wrap_is_no_op] and [_unwrap_is_no_op] (which have the same types) are in fact
       no-ops.
    *)
    external wrap : 'a portable -> 'a t = "%identity"
    external unwrap : 'a t -> 'a portable = "%identity"

    (* A generalization of [wrap] for lists. *)
    external wrap_list : 'a portable list -> 'a t list = "%identity"
  end

  let ok = Result.ok
  let ok_exn t = ok_exn (of_portable t)

  let error ?here ?strict message a sexp_of_a =
    error ?here ?strict message a sexp_of_a |> to_portable
  ;;

  let error_s sexp = error_s sexp |> to_portable
  let error_string message = error_string message |> to_portable
  let errorf format = Printf.ksprintf error_string format
  let of_exn ?backtrace exn = Error (Error.of_exn ?backtrace exn |> Error.to_portable)

  let of_exn_result ?backtrace = function
    | Ok _ as ok -> ok
    | Error exn -> of_exn ?backtrace exn
  ;;

  let try_with ?backtrace f =
    try_with ?backtrace (fun () -> Modes.Portable_via_contended.wrap (f ()))
    |> Portable_ok_and_err.unwrap
  ;;

  let try_with_join ?backtrace f = join (try_with ?backtrace f)

  let tag t ~tag:tag_string =
    tag (Portable_ok_and_err.wrap t) ~tag:tag_string |> Portable_ok_and_err.unwrap
  ;;

  let tag_s t ~tag = tag_s (Portable_ok_and_err.wrap t) ~tag |> Portable_ok_and_err.unwrap

  let tag_arg t message b sexp_of_b =
    tag_arg (Portable_ok_and_err.wrap t) message b sexp_of_b |> Portable_ok_and_err.unwrap
  ;;

  let all ts =
    all (Portable_ok_and_err.wrap_list ts)
    |> Modes.Portable.wrap_error
    |> Result.map ~f:Modes.Portable_via_contended.unwrap_list
  ;;

  let all_unit ts =
    all_unit (Modes.Portable.unwrap_error_list ts) |> Modes.Portable.wrap_error
  ;;

  let combine_errors ts =
    combine_errors (Portable_ok_and_err.wrap_list ts)
    |> Modes.Portable.wrap_error
    |> Result.map ~f:Modes.Portable_via_contended.unwrap_list
  ;;

  let combine_errors_unit ts =
    combine_errors_unit (Modes.Portable.unwrap_error_list ts) |> Modes.Portable.wrap_error
  ;;
end
