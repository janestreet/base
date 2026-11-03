open! Import0
module Either = Either0
module Result = Result0
include Nothing0

include Identifiable.Make (struct
    include Nothing0

    let module_name = "Base.Nothing"
  end)

let of_string = Nothing0.of_string

let must_be_none : t option -> unit = function
  | None -> ()
  | Some _ -> .
;;

let must_be_empty : t list -> unit = function
  | [] -> ()
  | _ :: _ -> .
;;

let must_be_ok : ('ok, t) Result.t -> 'ok = function
  | Ok ok -> ok
  | Error _ -> .
;;

let must_be_error : (t, 'err) Result.t -> 'err = function
  | Ok _ -> .
  | Error error -> error
;;

let must_be_first : ('first, t) Either.t -> 'first = function
  | First first -> first
  | Second _ -> .
;;

let must_be_second : (t, 'second) Either.t -> 'second = function
  | First _ -> .
  | Second second -> second
;;
