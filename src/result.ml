open! Import
module Either = Either0
include Result0

[%%template
[@@@kind_set.define all_ks_non_value = base_non_value]
[@@@kind_set.define all_ks = (all_ks_non_value, value_or_null_with_imm)]

[%%template
[@@@mode.default m = (global, local)]
[@@@kind ko = all_ks]

let[@kind ko] return (x @ m) : (_ t[@kind ko]) @ m = Ok x [@exclave_if_local m]

[@@@kind.default ki = all_ks, ko = ko]

let bind (x @ m) ~f : (_ t[@kind ko]) @ m =
  match (x : (_ t[@kind ki])) with
  | Error err -> Error err [@exclave_if_local m]
  | Ok x -> f x [@exclave_if_local m]
;;

let map x ~f : (_ t[@kind ko]) =
  match[@exclave_if_local m ~reasons:[ Will_return_unboxed ]] (x : (_ t[@kind ki])) with
  | Error err -> Error err
  | Ok x -> Ok (f x)
;;]

include%template
  Monad.Make2 [@kind value_or_null mod maybe_null] [@mode local] [@modality portable] (struct
    type nonrec ('a : value_or_null, 'b) t = ('a, 'b) t

    let bind = bind
    let map = `Custom map
    let return = return
  end)

let invariant check_ok check_error t =
  match t with
  | Ok ok -> check_ok ok
  | Error error -> check_error error
;;

let fail x = Error x
let failf format = Printf.ksprintf fail format

let%template map_error (type a : k) (t : ((a, _) t[@kind k])) ~f : ((a, _) t[@kind k]) =
  match[@exclave_if_stack a] t with
  | Ok _ as x -> x
  | Error x -> Error (f x)
[@@kind k = all_ks] [@@alloc a @ m = (heap_global, stack_local)]
;;

module%template Error =
Monad.Make2 [@kind value_or_null mod maybe_null] [@mode local] [@modality portable] (struct
    type nonrec ('a : value_or_null, 'b) t = ('b, 'a) t

    let bind x ~f =
      match x with
      | Ok _ as ok -> ok
      | Error e -> f e
    ;;

    let map = `Custom map_error
    let return e = Error e
  end)

[%%template
[@@@kind.default k = all_ks]

let is_ok : (_ t[@kind k]) @ local -> bool = function
  | Ok _ -> true
  | Error _ -> false
;;

let is_error : (_ t[@kind k]) @ local -> bool = function
  | Ok _ -> false
  | Error _ -> true
;;]

let ok = function
  | Ok x -> Some x
  | Error _ -> None
;;

let ok_or_null = function
  | Ok x -> This x
  | Error _ -> Null
;;

let error = function
  | Ok _ -> None
  | Error x -> Some x
;;

let error_or_null = function
  | Ok _ -> Null
  | Error x -> This x
;;

let of_option opt ~error =
  match opt with
  | Some x -> Ok x
  | None -> Error error
;;

let of_or_null or_null ~error =
  match or_null with
  | This x -> Ok x
  | Null -> Error error
;;

let of_option_or_thunk opt ~error =
  match opt with
  | Some x -> Ok x
  | None -> Error (error ())
;;

let iter v ~f =
  match v with
  | Ok x -> f x
  | Error _ -> ()
;;

let iter_error v ~f =
  match v with
  | Ok _ -> ()
  | Error x -> f x
;;

[%%template
[@@@mode.default m = (global, local)]

let to_either : _ t @ m -> _ Either.t @ m = function
  | Ok x -> First x [@exclave_if_local m]
  | Error x -> Second x [@exclave_if_local m]
;;

let of_either : _ Either.t @ m -> _ t @ m = function
  | First x -> Ok x [@exclave_if_local m]
  | Second x -> Error x [@exclave_if_local m]
;;]

let ok_if_true bool ~error = if bool then Ok () else Error error

let transpose_opt t =
  match[@exclave_if_stack a] t with
  | Ok None -> None
  | Ok (Some v) -> Some (Ok v)
  | Error e -> Some (Error e)
[@@alloc a = (stack, heap)]
;;

let try_with f =
  try Ok (f ()) with
  | exn -> Error exn
;;

let ok_exn = function
  | Ok x -> x
  | Error exn -> raise exn
;;

let ok_or_failwith = function
  | Ok x -> x
  | Error str -> failwith str
;;

module Export = struct
  type ('ok : value_or_null, 'err : value_or_null) _result = ('ok, 'err) t =
    | Ok of 'ok
    | Error of 'err

  [%%template
  [@@@kind.default k = all_ks]

  let is_error = (is_error [@kind k])
  let is_ok = (is_ok [@kind k])]
end

let combine t1 t2 ~ok ~err =
  match t1, t2 with
  | Ok _, Error e | Error e, Ok _ -> Error e
  | Ok ok1, Ok ok2 -> Ok (ok ok1 ok2)
  | Error err1, Error err2 -> Error (err err1 err2)
;;

[%%template
[@@@alloc.default a @ m = (heap_global, stack_local)]

let combine_errors l =
  (let ok, errs = (List0.partition_map [@mode m] [@alloc a]) l ~f:(to_either [@mode m]) in
   match errs with
   | [] -> Ok ok
   | _ :: _ -> Error errs)
  [@exclave_if_stack a]
;;]

let combine_errors_unit l = map (combine_errors l) ~f:(fun (_ : unit list) -> ())]
