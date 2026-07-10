(** [Result] is often used to handle error messages. *)

open! Import
module Invariant := Invariant_intf.Definitions

[%%template:
[@@@kind_set.define all_ks_non_value = base_non_value]
[@@@kind_set.define all_ks = (all_ks_non_value, value_or_null_with_imm)]

[%%template:
type ('ok, 'err) t =
  | Ok of 'ok
  | Error of 'err
[@@deriving sexp ~stackify, compare ~localize, equal ~localize, globalize]
[@@kind k = all_ks_non_value]

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
[@@deriving
  sexp ~stackify, sexp_grammar, compare ~localize, equal ~localize, hash, globalize]
[@@kind k = value_or_null_with_imm]]

include%template
  Monad.S2
  [@kind value_or_null mod maybe_null] [@mode local]
  with type ('a, 'err) t := ('a, 'err) t

module%template Error :
  Monad.S2
  [@kind value_or_null mod maybe_null] [@mode local]
  with type ('err, 'a) t := ('a, 'err) t

include Invariant.S2 with type ('ok, 'err) t := ('ok, 'err) t

val fail : 'err -> (_, 'err) t

(** e.g., [failf "Couldn't find bloogle %s" (Bloogle.to_string b)]. *)
val failf : ('a, unit, string, (_, string) t) format4 -> 'a

[%%template:
[@@@kind.default k = all_ks]

val is_ok : 'ok 'err. (('ok, 'err) t[@kind k]) -> bool
val is_error : 'ok 'err. (('ok, 'err) t[@kind k]) -> bool]

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

[%%template:
[@@@mode.default m = (global, local)]
[@@@kind ko = all_ks]

val return : 'ok 'err. 'ok -> (('ok, 'err) t[@kind ko])
[@@zero_alloc_if_local m] [@@kind ko]

[@@@kind.default ki = all_ks, ko = ko]

val bind
  : 'a 'b 'err.
  (('a, 'err) t[@kind ki])
  -> f:('a -> (('b, 'err) t[@kind ko]))
  -> (('b, 'err) t[@kind ko])

val map : 'a 'b 'err. (('a, 'err) t[@kind ki]) -> f:('a -> 'b) -> (('b, 'err) t[@kind ko])]

val%template map_error
  : 'ok 'err 'c.
  (('ok, 'err) t[@kind k]) -> f:('err -> 'c) -> (('ok, 'c) t[@kind k])
[@@kind k = all_ks] [@@alloc __ @ m = (heap_global, stack_local)]

(** Returns [Ok] if both are [Ok] and [Error] otherwise. *)
val combine
  : 'ok1 'ok2 'ok3 'err.
  ('ok1, 'err) t
  -> ('ok2, 'err) t
  -> ok:('ok1 -> 'ok2 -> 'ok3)
  -> err:('err -> 'err -> 'err)
  -> ('ok3, 'err) t

[%%template:
[@@@alloc.default __ @ m = (heap_global, stack_local)]

(** [combine_errors ts] returns [Ok] if every element in [ts] is [Ok], else it returns
    [Error] with all the errors in [ts].

    This is similar to [all] from [Monad.S2], with the difference that [all] only returns
    the first error. *)
val combine_errors : 'ok 'err. ('ok, 'err) t list -> ('ok list, 'err list) t]

(** [combine_errors_unit] returns [Ok] if every element in [ts] is [Ok ()], else it
    returns [Error] with all the errors in [ts], like [combine_errors]. *)
val combine_errors_unit : 'err. (unit, 'err) t list -> (unit, 'err list) t

[%%template:
[@@@mode.default m = (global, local)]

(** [to_either] is useful with [List.partition_map]. For example:

    {[
      let ints, exns =
        List.partition_map [ "1"; "two"; "three"; "4" ] ~f:(fun string ->
          Result.to_either (Result.try_with (fun () -> Int.of_string string)))
      ;;
    ]} *)
val to_either : 'ok 'err. ('ok, 'err) t -> ('ok, 'err) Either0.t
[@@zero_alloc_if_local m]

val of_either : 'ok 'err. ('ok, 'err) Either0.t -> ('ok, 'err) t [@@zero_alloc_if_local m]]

(** [ok_if_true] returns [Ok ()] if [bool] is true, and [Error error] if it is false. *)
val ok_if_true : 'err. bool -> error:'err -> (unit, 'err) t

(** [transpose_opt x] transposes a [result] of an [option] into an [option] of a [result]

    Concretely, [Ok None] maps to [None], [Ok (Some x)] maps to [Some (Ok x)], and
    [Error e] maps to [Some (Error e)].

    Inverse of {!Option.transpose_result}. *)
val transpose_opt : 'ok 'err. ('ok option, 'err) t -> ('ok, 'err) t option
[@@alloc __ @ m = (stack_local, heap_global)]

val try_with : 'a. (unit -> 'a) -> ('a, exn) t

module Export : sig
  type ('ok, 'err) _result = ('ok, 'err) t =
    | Ok of 'ok
    | Error of 'err

  [%%template:
  [@@@kind.default k = all_ks]

  val is_ok : 'ok 'err. (('ok, 'err) t[@kind k]) -> bool
  val is_error : 'ok 'err. (('ok, 'err) t[@kind k]) -> bool]
end]
