open! Import
module Error = Error0
module Nothing = Nothing0

[%%template
[@@@kind_set.define all_ks_non_value = base_non_value]
[@@@kind_set.define all_ks = (all_ks_non_value, value_or_null)]

[%%template
type nonrec ('a : k) t = (('a, Error.t) Result.t[@kind k])
[@@deriving compare ~localize, equal ~localize, globalize, sexp ~stackify]
[@@kind k = all_ks_non_value]

type nonrec ('a : value_or_null) t = ('a, Error.t) Result.t
[@@deriving
  compare ~localize, equal ~localize, globalize, hash, sexp ~stackify, sexp_grammar]
[@@kind k = value_or_null]]

type 'a or_error = 'a t

let ( >>= ) = Result.( >>= )
let ( >>| ) = Result.( >>| )
let ignore_m = Result.ignore_m
let join = Result.join

[%%template
[@@@mode.default m = (global, local)]
[@@@kind ko = all_ks]

let[@kind ko] return a = (Result.return [@kind ko] [@mode m]) a [@exclave_if_local m]

[@@@kind.default ki = all_ks, ko = ko]

let bind = (Result.bind [@kind ki ko] [@mode m])
let map = (Result.map [@kind ki ko] [@mode m])]

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
let[@zero_alloc] ok_or_null = [%eta1 Result.ok_or_null]
let is_ok = Result.is_ok
let is_error = Result.is_error

let%template try_with (type a : k) ?(backtrace = false) f : (a t[@kind k]) =
  try[@exclave_if_stack a] Ok (f ()) with
  | exn -> Error (Error.of_exn exn ?backtrace:(if backtrace then Some `Get else None))
[@@mode p = (nonportable, portable)] [@@kind k = base_or_null] [@@alloc a = (heap, stack)]
;;

let try_with_join ?backtrace f = join (try_with ?backtrace f)

[%%template
[@@@kind.default k = all_ks]

let ok_exn : ('a : k). ('a t[@kind k]) -> 'a = function
  | Ok x -> x
  | Error err ->
    (match Error.raise err with
     | (_ : Nothing.t) -> .)
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
let of_or_null = Result.of_or_null
let of_or_null_lazy_string t ~error = of_or_null t ~error:(Error.of_lazy error)
let of_or_null_lazy_sexp t ~error = of_or_null t ~error:(Error.of_lazy_sexp error)
let of_or_null_lazy t ~error = of_or_null t ~error:(Error.of_lazy_t error)

let%template error ?here ?strict message a sexp_of_a =
  Error ((Error.create [@mode m]) ?here ?strict message a sexp_of_a)
[@@mode m = (portable, nonportable)]
;;

[%%template
[@@@kind.default k = all_ks]

let error_s sexp : (_ t[@kind k]) = Error (Error.create_s sexp)
let error_string message : (_ t[@kind k]) = Error (Error.of_string message)
let errorf format = Printf.ksprintf (error_string [@kind k]) format]

let errorf_portable format = Printf.ksprintf (fun string () -> error_string string) format

let%template tag t ~tag =
  match t with
  | Ok _ as ok -> ok
  | Error err -> Error ((Error.tag [@mode p]) err ~tag)
[@@mode p = (portable, nonportable)]
;;

let%template tag_s (t : (_ t[@kind k])) ~tag =
  match t with
  | Ok _ as ok -> ok
  | Error err -> Error ((Error.tag_s [@mode p]) err ~tag)
[@@mode p = (portable, nonportable)] [@@kind k = base_or_null]
;;

let tag_lazy t ~tag = Result.map_error t ~f:(Error.tag_lazy ~tag)
let tag_s_lazy t ~tag = Result.map_error t ~f:(Error.tag_s_lazy ~tag)

let%template tag_arg t message a sexp_of_a =
  match t with
  | Ok _ as ok -> ok
  | Error e ->
    let e = (Error.tag_arg [@mode p]) e message a sexp_of_a in
    Error e
[@@mode p = (portable, nonportable)]
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

let%template error_of_list_if_necessary = function
  | [ e ] -> e
  | list -> (Error.of_list [@mode p]) list
[@@mode p = (portable, nonportable)]
;;

let all list = combine_internal list ~on_ok:Fn.id ~on_error:error_of_list_if_necessary

let%template all list =
  (combine_internal [@mode portable])
    (Modes.Portable.wrap_result_list list)
    ~on_ok:Modes.Portable.unwrap_list
    ~on_error:(fun errs ->
      (error_of_list_if_necessary [@mode portable]) (Modes.Portable.unwrap_list errs))
[@@mode portable]
;;

let all_unit list =
  combine_internal list ~on_ok:ignore_unit_list ~on_error:error_of_list_if_necessary
;;

let%template all_unit list =
  (combine_internal [@mode portable])
    (Modes.Portable.wrap_result_list list)
    ~on_ok:(fun list -> ignore_unit_list (Modes.Portable.unwrap_list list))
    ~on_error:(fun errs ->
      (error_of_list_if_necessary [@mode portable]) (Modes.Portable.unwrap_list errs))
[@@mode portable]
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

let%template of_portable : ('a, Error.Portable.t) Result.t @ p -> 'a t @ p =
  (Modes.Portable.unwrap_error [@mode p])
[@@mode p = (portable, nonportable)]
;;

let to_portable : 'a t @ portable -> ('a, Error.Portable.t) Result.t @ portable =
  Modes.Portable.wrap_error
;;

let portabilize : 'a t -> ('a, Error.Portable.t) Result.t = function
  | Ok _ as ok -> ok
  | Error error -> Error (Error.to_portable_portabilize error)
;;

module Portable = struct
  type 'a t = ('a, Error.Portable.t) Result.t
  [@@deriving compare ~localize, equal ~localize, globalize, sexp]

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

  module%template For_applicative =
  Applicative.Make_using_map2 [@mode local] [@modality portable] (struct
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

    let _wrap_is_no_op : type a. a portable -> a t @ portable = function
      | Ok ok -> Ok (Modes.Portable_via_contended.wrap ok)
      | Error _ as error ->
        [%template Modes.Portable.unwrap_error [@mode portable]]
          (error : (_ Modes.Portable_via_contended.t, Error.Portable.t) Result.t)
    ;;

    let _unwrap_is_no_op : type a. a t @ portable -> a portable = function
      | Ok ok -> Ok (Modes.Portable_via_contended.unwrap ok)
      | Error _ as error -> Modes.Portable.wrap_error error
    ;;

    (* The safety of the below functions hinge critically on the fact that
       [_wrap_is_no_op] and [_unwrap_is_no_op] (which have the same types) are in fact
       no-ops.
    *)
    external wrap : 'a portable -> 'a t @ portable @@ portable = "%identity"
    external unwrap : 'a t @ portable -> 'a portable @@ portable = "%identity"

    (* A generalization of [wrap] for lists. *)
    external wrap_list
      :  'a portable list
      -> 'a t list @ portable
      @@ portable
      = "%identity"
  end

  let ok = Result.ok
  let ok_exn t = ok_exn (of_portable t)

  let error ?here ?strict message a sexp_of_a =
    [%template error [@mode portable]] ?here ?strict message a sexp_of_a |> to_portable
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
    [%template try_with [@mode portable]] ?backtrace (fun () ->
      Modes.Portable_via_contended.wrap (f ()))
    |> Portable_ok_and_err.unwrap
  ;;

  let try_with_join ?backtrace f = join (try_with ?backtrace f)

  let tag t ~tag:tag_string =
    [%template tag [@mode portable]] (Portable_ok_and_err.wrap t) ~tag:tag_string
    |> Portable_ok_and_err.unwrap
  ;;

  let tag_s t ~tag =
    [%template tag_s [@mode portable]] (Portable_ok_and_err.wrap t) ~tag
    |> Portable_ok_and_err.unwrap
  ;;

  let tag_arg t message b sexp_of_b =
    [%template tag_arg [@mode portable]] (Portable_ok_and_err.wrap t) message b sexp_of_b
    |> Portable_ok_and_err.unwrap
  ;;

  let all ts =
    [%template all [@mode portable]] (Portable_ok_and_err.wrap_list ts)
    |> Modes.Portable.wrap_error
    |> Result.map ~f:Modes.Portable_via_contended.unwrap_list
  ;;

  let all_unit ts =
    [%template all_unit [@mode portable]]
      ([%template Modes.Portable.unwrap_error_list [@mode portable]] ts)
    |> Modes.Portable.wrap_error
  ;;

  let combine_errors ts =
    [%template combine_errors [@mode portable]] (Portable_ok_and_err.wrap_list ts)
    |> Modes.Portable.wrap_error
    |> Result.map ~f:Modes.Portable_via_contended.unwrap_list
  ;;

  let combine_errors_unit ts =
    [%template combine_errors_unit [@mode portable]]
      ([%template Modes.Portable.unwrap_error_list [@mode portable]] ts)
    |> Modes.Portable.wrap_error
  ;;
end]
