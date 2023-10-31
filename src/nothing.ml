open! Import

module T = struct
  type t = |

  let unreachable_code_local = function
    | (_ : t) -> .
  ;;

  let unreachable_code x = unreachable_code_local x
  let all = []
  let hash_fold_t _ t = unreachable_code t
  let hash = unreachable_code
  let compare a _ = unreachable_code a
  let compare__local a _ = unreachable_code a
  let equal__local a _ = unreachable_code a
  let sexp_of_t = unreachable_code
  let t_of_sexp sexp = Sexplib0.Sexp_conv_error.empty_type "Base.Nothing.t" sexp
  let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) = { untyped = Union [] }
  let to_string = unreachable_code
  let of_string (_ : string) = failwith "Base.Nothing.of_string: not supported"
  let globalize = unreachable_code
end

include T

include Identifiable.Make (struct
  include T

  let module_name = "Base.Nothing"
end)

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
