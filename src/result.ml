open! Import
module Either = Either0
include Result0

[%%template
let map x ~f : (_ t[@kind ko]) =
  match (x : (_ t[@kind ki])) with
  | Error err -> Error err
  | Ok x -> Ok (f x)
[@@kind
  ki = (value, immediate, immediate64, float64, bits32, bits64, word)
  , ko = (value, immediate, immediate64, float64, bits32, bits64, word)]
;;

include Monad.Make2 [@mode local] [@modality portable] (struct
    type nonrec ('a, 'b) t = ('a, 'b) t

    let bind x ~f =
      match x with
      | Error _ as x -> x
      | Ok x -> f x
    ;;

    let map = `Custom map
    let return x = Ok x
  end)]

let invariant check_ok check_error t =
  match t with
  | Ok ok -> check_ok ok
  | Error error -> check_error error
;;

let fail x = Error x
let failf format = Printf.ksprintf fail format

let map_error t ~f =
  match t with
  | Ok _ as x -> x
  | Error x -> Error (f x)
;;

module%template Error = Monad.Make2 [@mode local] [@modality portable] (struct
    type nonrec ('a, 'b) t = ('b, 'a) t

    let bind x ~f =
      match x with
      | Ok _ as ok -> ok
      | Error e -> f e
    ;;

    let map = `Custom map_error
    let return e = Error e
  end)

[%%template
[@@@kind.default k = (value, immediate, immediate64, float64, bits32, bits64, word)]

let is_ok : (_ t[@kind k]) -> bool = function
  | Ok _ -> true
  | Error _ -> false
;;

let is_error : (_ t[@kind k]) -> bool = function
  | Ok _ -> false
  | Error _ -> true
;;]

let ok = function
  | Ok x -> Some x
  | Error _ -> None
;;

let error = function
  | Ok _ -> None
  | Error x -> Some x
;;

let of_option opt ~error =
  match opt with
  | Some x -> Ok x
  | None -> Error error
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

  [%%template
  [@@@kind.default k = (value, immediate, immediate64, float64, bits32, bits64, word)]

  let is_error = (is_error [@kind k])
  let is_ok = (is_ok [@kind k])]
end

let combine t1 t2 ~ok ~err =
  match t1, t2 with
  | Ok _, Error e | Error e, Ok _ -> Error e
  | Ok ok1, Ok ok2 -> Ok (ok ok1 ok2)
  | Error err1, Error err2 -> Error (err err1 err2)
;;

let combine_errors l =
  let ok, errs = List1.partition_map l ~f:to_either in
  match errs with
  | [] -> Ok ok
  | _ :: _ -> Error errs
;;

let combine_errors_unit l = map (combine_errors l) ~f:(fun (_ : unit list) -> ())
