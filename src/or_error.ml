open! Import

type 'a t = ('a, Error.t) Result.t
[@@deriving_inline
  compare ~localize, equal ~localize, globalize, hash, sexp, sexp_grammar]

let compare__local : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int =
  fun _cmp__a a__007_ b__008_ ->
  Result.compare__local _cmp__a Error.compare__local a__007_ b__008_
;;

let compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int =
  fun _cmp__a a__001_ b__002_ -> Result.compare _cmp__a Error.compare a__001_ b__002_
;;

let equal__local : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool =
  fun _cmp__a a__019_ b__020_ ->
  Result.equal__local _cmp__a Error.equal__local a__019_ b__020_
;;

let equal : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool =
  fun _cmp__a a__013_ b__014_ -> Result.equal _cmp__a Error.equal a__013_ b__014_
;;

let globalize : 'a. ('a -> 'a) -> 'a t -> 'a t =
  fun (type a__025_) : ((a__025_ -> a__025_) -> a__025_ t -> a__025_ t) ->
  fun _globalize_a__026_ x__027_ ->
  Result.globalize _globalize_a__026_ Error.globalize x__027_
;;

let hash_fold_t :
      'a.
      (Ppx_hash_lib.Std.Hash.state -> 'a -> Ppx_hash_lib.Std.Hash.state)
      -> Ppx_hash_lib.Std.Hash.state
      -> 'a t
      -> Ppx_hash_lib.Std.Hash.state
  =
  fun _hash_fold_a hsv arg -> Result.hash_fold_t _hash_fold_a Error.hash_fold_t hsv arg
;;

let t_of_sexp : 'a. (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t =
  fun _of_a__030_ x__032_ -> Result.t_of_sexp _of_a__030_ Error.t_of_sexp x__032_
;;

let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
  fun _of_a__033_ x__034_ -> Result.sexp_of_t _of_a__033_ Error.sexp_of_t x__034_
;;

let t_sexp_grammar : 'a. 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t =
  fun _'a_sexp_grammar -> Result.t_sexp_grammar _'a_sexp_grammar Error.t_sexp_grammar
;;

[@@@end]

let ( >>= ) = Result.( >>= )
let ( >>| ) = Result.( >>| )
let bind = Result.bind
let ignore_m = Result.ignore_m
let join = Result.join
let map = Result.map
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

module For_applicative = Applicative.Make_using_map2_local (struct
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
let is_ok = Result.is_ok
let is_error = Result.is_error

let try_with ?(backtrace = false) f =
  try Ok (f ()) with
  | exn -> Error (Error.of_exn exn ?backtrace:(if backtrace then Some `Get else None))
;;

let try_with_join ?backtrace f = join (try_with ?backtrace f)

let ok_exn = function
  | Ok x -> x
  | Error err -> Error.raise err
;;

let of_exn ?backtrace exn = Error (Error.of_exn ?backtrace exn)

let of_exn_result ?backtrace = function
  | Ok _ as z -> z
  | Error exn -> of_exn ?backtrace exn
;;

let of_option = Result.of_option

let error ?here ?strict message a sexp_of_a =
  Error (Error.create ?here ?strict message a sexp_of_a)
;;

let error_s sexp = Error (Error.create_s sexp)
let error_string message = Error (Error.of_string message)
let errorf format = Printf.ksprintf error_string format
let tag t ~tag = Result.map_error t ~f:(Error.tag ~tag)
let tag_s t ~tag = Result.map_error t ~f:(Error.tag_s ~tag)
let tag_s_lazy t ~tag = Result.map_error t ~f:(Error.tag_s_lazy ~tag)

let tag_arg t message a sexp_of_a =
  Result.map_error t ~f:(fun e -> Error.tag_arg e message a sexp_of_a)
;;

let unimplemented s = error "unimplemented" s sexp_of_string

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

let combine_errors_unit list =
  combine_internal list ~on_ok:ignore_unit_list ~on_error:Error.of_list
;;

let filter_ok_at_least_one l =
  let ok, errs = List.partition_map l ~f:Result.to_either in
  match ok with
  | [] -> Error (Error.of_list errs)
  | _ -> Ok ok
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
  With_return.with_return (fun { return } ->
    Error
      (Error.of_list
         (List.map l ~f:(fun elt ->
            match f elt with
            | Ok _ as x -> return x
            | Error err -> err)))) [@nontail]
;;

let map = Result.map
let iter = Result.iter
let iter_error = Result.iter_error
