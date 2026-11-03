(** [Result] is often used to handle error messages. *)

open! Import0
module Invariant := Invariant_intf.Definitions
module Either := Either0

(** ['ok] is the return type, and ['err] is often an error message string.

    {[
      type nat =
        | Zero
        | Succ of nat

      let pred = function
        | Succ n -> Ok n
        | Zero -> Error "Zero does not have a predecessor"
      ;;
    ]}

    The return type of [pred] could be [nat option], but [(nat, string) Result.t] gives
    more control over the error message. *)
type ('ok, 'err) t = ('ok, 'err) Stdlib.result =
  | Ok of 'ok
  | Error of 'err
[@@deriving sexp, sexp_grammar, compare, equal, hash, globalize]

include Monad.S2 with type ('a, 'err) t := ('a, 'err) t
module Error : Monad.S2 with type ('err, 'a) t := ('a, 'err) t
include Invariant.S2 with type ('ok, 'err) t := ('ok, 'err) t

val fail : 'err -> (_, 'err) t

(** e.g., [failf "Couldn't find bloogle %s" (Bloogle.to_string b)]. *)
val failf : ('a, unit, string, (_, string) t) format4 -> 'a

val is_ok : 'ok 'err. ('ok, 'err) t -> bool
val is_error : 'ok 'err. ('ok, 'err) t -> bool
val ok : 'ok 'err. ('ok, 'err) t -> 'ok option
val ok_or_null : 'ok 'err. ('ok, 'err) t -> 'ok or_null [@@zero_alloc]
val ok_exn : 'ok. ('ok, exn) t -> 'ok
val ok_or_failwith : 'ok. ('ok, string) t -> 'ok
val error : 'ok 'err. ('ok, 'err) t -> 'err option
val error_or_null : 'ok 'err. ('ok, 'err) t -> 'err or_null [@@zero_alloc]
val of_option : 'ok 'err. 'ok option -> error:'err -> ('ok, 'err) t
val of_or_null : 'ok 'err. 'ok or_null -> error:'err -> ('ok, 'err) t
val of_option_or_thunk : 'ok 'err. 'ok option -> error:(unit -> 'err) -> ('ok, 'err) t
val iter : 'ok 'err. ('ok, 'err) t -> f:('ok -> unit) -> unit
val iter_error : 'ok 'err. ('ok, 'err) t -> f:('err -> unit) -> unit
val return : 'ok 'err. 'ok -> ('ok, 'err) t
val bind : 'a 'b 'err. ('a, 'err) t -> f:('a -> ('b, 'err) t) -> ('b, 'err) t
val map : 'a 'b 'err. ('a, 'err) t -> f:('a -> 'b) -> ('b, 'err) t
val map_error : 'ok 'err 'c. ('ok, 'err) t -> f:('err -> 'c) -> ('ok, 'c) t

(** Returns [Ok] if both are [Ok] and [Error] otherwise. *)
val combine
  : 'ok1 'ok2 'ok3 'err.
  ('ok1, 'err) t
  -> ('ok2, 'err) t
  -> ok:('ok1 -> 'ok2 -> 'ok3)
  -> err:('err -> 'err -> 'err)
  -> ('ok3, 'err) t

(** [combine_errors ts] returns [Ok] if every element in [ts] is [Ok], else it returns
    [Error] with all the errors in [ts].

    This is similar to [all] from [Monad.S2], with the difference that [all] only returns
    the first error. *)
val combine_errors : 'ok 'err. ('ok, 'err) t list -> ('ok list, 'err list) t

(** [combine_errors_unit] returns [Ok] if every element in [ts] is [Ok ()], else it
    returns [Error] with all the errors in [ts], like [combine_errors]. *)
val combine_errors_unit : 'err. (unit, 'err) t list -> (unit, 'err list) t

(** [to_either] is useful with [List.partition_map]. For example:

    {[
      let ints, exns =
        List.partition_map [ "1"; "two"; "three"; "4" ] ~f:(fun string ->
          Result.to_either (Result.try_with (fun () -> Int.of_string string)))
      ;;
    ]} *)
val to_either : 'ok 'err. ('ok, 'err) t -> ('ok, 'err) Either.t

val of_either : 'ok 'err. ('ok, 'err) Either.t -> ('ok, 'err) t

(** [ok_if_true] returns [Ok ()] if [bool] is true, and [Error error] if it is false. *)
val ok_if_true : 'err. bool -> error:'err -> (unit, 'err) t

(** [transpose_opt x] transposes a [result] of an [option] into an [option] of a [result]

    Concretely, [Ok None] maps to [None], [Ok (Some x)] maps to [Some (Ok x)], and
    [Error e] maps to [Some (Error e)].

    Inverse of {!Option.transpose_result}. *)
val transpose_opt : 'ok 'err. ('ok option, 'err) t -> ('ok, 'err) t option

val try_with : 'a. (unit -> 'a) -> ('a, exn) t

module Export : sig
  type ('ok, 'err) _result = ('ok, 'err) t =
    | Ok of 'ok
    | Error of 'err

  val is_ok : 'ok 'err. ('ok, 'err) t -> bool
  val is_error : 'ok 'err. ('ok, 'err) t -> bool
end
