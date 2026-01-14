open! Import

[@@@warning "-incompatible-with-upstream"]

[%%template
  type nonrec ('a : k) t = (('a, Error.t) Result.t[@kind k])
  [@@deriving compare ~localize, equal ~localize, globalize, sexp]
  [@@kind k = base_non_value]]

type nonrec ('a : value_or_null) t = ('a, Error.t) Result.t
[@@deriving compare ~localize, equal ~localize, globalize, hash, sexp, sexp_grammar]

let ( >>= ) = Result.( >>= )
let ( >>| ) = Result.( >>| )
let bind = Result.bind
let ignore_m = Result.ignore_m
let join = Result.join

let%template map = (Result.map [@kind ki ko])
[@@kind ki = base_or_null, ko = base_or_null]
;;

let return = Result.return

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

module%template For_applicative =
Applicative.Make_using_map2
  [@kind value_or_null mod maybe_null]
  [@mode local]
  [@modality portable]
  (struct
    type nonrec ('a : value_or_null) t = 'a t

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
let is_ok = Result.is_ok
let is_error = Result.is_error

let%template try_with ?(backtrace = false) f =
  try Ok (f ()) with
  | exn -> Error (Error.of_exn exn ?backtrace:(if backtrace then Some `Get else None))
[@@mode p = (nonportable, portable)]
;;

let try_with_join ?backtrace f = join (try_with ?backtrace f)

[%%template
[@@@kind.default k = base_or_null]

let ok_exn : ('a : k). ('a t[@kind k]) -> 'a = function
  | Ok x -> x
  | Error err -> (Error.raise [@kind k]) err
;;]

let of_exn ?backtrace exn = Error (Error.of_exn ?backtrace exn)

let of_exn_result ?backtrace = function
  | Ok _ as z -> z
  | Error exn -> of_exn ?backtrace exn
;;

let of_option = Result.of_option
let of_option_lazy_string t ~error = of_option t ~error:(Error.of_lazy error)
let of_option_lazy_sexp t ~error = of_option t ~error:(Error.of_lazy_sexp error)
let of_option_lazy t ~error = of_option t ~error:(Error.of_lazy_t error)

let%template error ?here ?strict message a sexp_of_a =
  Error ((Error.create [@mode m]) ?here ?strict message a sexp_of_a)
[@@mode m = (portable, nonportable)]
;;

let error_s sexp = Error (Error.create_s sexp)
let error_string message = Error (Error.of_string message)
let errorf format = Printf.ksprintf error_string format
let errorf_portable format = Printf.ksprintf (fun string () -> error_string string) format

let%template tag t ~tag =
  match t with
  | Ok _ as ok -> ok
  | Error err -> Error ((Error.tag [@mode p]) err ~tag)
[@@mode p = (portable, nonportable)]
;;

let%template tag_s t ~tag =
  match t with
  | Ok _ as ok -> ok
  | Error err -> Error ((Error.tag_s [@mode p]) err ~tag)
[@@mode p = (portable, nonportable)]
;;

let tag_s_lazy t ~tag = Result.map_error t ~f:(Error.tag_s_lazy ~tag)

let tag_arg t message a sexp_of_a =
  Result.map_error t ~f:(fun e -> Error.tag_arg e message a sexp_of_a)
;;

let unimplemented s =
  [%template error [@mode portable]] "unimplemented" s [%eta1 sexp_of_string]
;;

let%template combine_internal (list @ p) ~on_ok ~on_error =
  match Result.combine_errors list with
  | Ok x -> Ok (on_ok x)
  | Error errs -> Error (on_error errs)
[@@mode p = (portable, nonportable)]
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
      : ('a : value_or_null) ('b : value_or_null).
      (('a, 'b) Result.t list[@local_opt]) @ portable
      -> (('a t, 'b t) Result.t list[@local_opt]) @ portable
      @@ portable
      = "%identity"

    external unwrap_list
      : ('a : value_or_null).
      ('a t list[@local_opt]) -> ('a list[@local_opt]) @ portable
      @@ portable
      = "%identity"
  end
end

let%template combine_errors list =
  (combine_internal [@mode portable])
    (Modes.Portable.wrap_result_list list)
    ~on_ok:Modes.Portable.unwrap_list
    ~on_error:(fun errs ->
      (Error.of_list [@mode portable]) (Modes.Portable.unwrap_list errs))
[@@mode portable]
;;

let combine_errors_unit list =
  combine_internal list ~on_ok:ignore_unit_list ~on_error:Error.of_list
;;

let%template combine_errors_unit list =
  (combine_internal [@mode portable])
    (Modes.Portable.wrap_result_list list)
    ~on_ok:(fun xs -> Modes.Portable.unwrap_list xs |> ignore_unit_list)
    ~on_error:(fun errs ->
      (Error.of_list [@mode portable]) (Modes.Portable.unwrap_list errs))
[@@mode portable]
;;

let filter_ok_at_least_one l =
  match l with
  | [] -> error_string "filter_ok_at_least_one called on empty list"
  | l ->
    let ok, errs = List.partition_map l ~f:Result.to_either in
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
