open! Import0
module Either = Either0
module List = List0
include Result0

let return x : _ t = Ok x

let bind x ~f : _ t =
  match (x : _ t) with
  | Error err -> Error err
  | Ok x -> f x
;;

let map x ~f : _ t =
  match (x : _ t) with
  | Error err -> Error err
  | Ok x -> Ok (f x)
;;

include Monad.Make2 (struct
    type nonrec ('a, 'b) t = ('a, 'b) t

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

let map_error (type a) (t : (a, _) t) ~f : (a, _) t =
  match t with
  | Ok _ as x -> x
  | Error x -> Error (f x)
;;

module Error = Monad.Make2 (struct
    type nonrec ('a, 'b) t = ('b, 'a) t

    let bind x ~f =
      match x with
      | Ok _ as ok -> ok
      | Error e -> f e
    ;;

    let map = `Custom map_error
    let return e = Error e
  end)

let is_ok : _ t -> bool = function
  | Ok _ -> true
  | Error _ -> false
;;

let is_error : _ t -> bool = function
  | Ok _ -> false
  | Error _ -> true
;;

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

let to_either : _ t -> _ Either.t = function
  | Ok x -> First x
  | Error x -> Second x
;;

let of_either : _ Either.t -> _ t = function
  | First x -> Ok x
  | Second x -> Error x
;;

let ok_if_true bool ~error = if bool then Ok () else Error error

let transpose_opt t =
  match t with
  | Ok None -> None
  | Ok (Some v) -> Some (Ok v)
  | Error e -> Some (Error e)
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
  type ('ok, 'err) _result = ('ok, 'err) t =
    | Ok of 'ok
    | Error of 'err

  let is_error = is_error
  let is_ok = is_ok
end

let combine t1 t2 ~ok ~err =
  match t1, t2 with
  | Ok _, Error e | Error e, Ok _ -> Error e
  | Ok ok1, Ok ok2 -> Ok (ok ok1 ok2)
  | Error err1, Error err2 -> Error (err err1 err2)
;;

let combine_errors l =
  let ok, errs = List.partition_map l ~f:to_either in
  match errs with
  | [] -> Ok ok
  | _ :: _ -> Error errs
;;

let combine_errors_unit l = map (combine_errors l) ~f:(fun (_ : unit list) -> ())
