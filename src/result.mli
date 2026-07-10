@@ portable

(** [Result] is often used to handle error messages. *)

open! Import
module Invariant := Invariant_intf.Definitions

[%%template:
[@@@kind_set.define all_ks_non_value = base_non_value]
[@@@kind_set.define all_ks = (all_ks_non_value, value_or_null_with_imm)]

[%%template:
type ('ok : k, 'err : value_or_null) t =
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
type ('ok : value_or_null, 'err : value_or_null) t = ('ok, 'err) Stdlib.result =
  | Ok of 'ok
  | Error of 'err
[@@deriving
  sexp ~stackify, sexp_grammar, compare ~localize, equal ~localize, hash, globalize]
[@@kind k = value_or_null_with_imm]]

include%template
  Monad.S2
  [@kind value_or_null mod maybe_null] [@mode local]
  with type ('a : value_or_null, 'err) t := ('a, 'err) t

module%template Error :
  Monad.S2
  [@kind value_or_null mod maybe_null] [@mode local]
  with type ('err : value_or_null, 'a) t := ('a, 'err) t

include Invariant.S2 with type ('ok, 'err) t := ('ok, 'err) t

val fail : 'err -> (_, 'err) t

(** e.g., [failf "Couldn't find bloogle %s" (Bloogle.to_string b)]. *)
val failf : ('a, unit, string, (_, string) t) format4 -> 'a

[%%template:
[@@@kind.default k = all_ks]

val is_ok : ('ok : k) ('err : value_or_null). (('ok, 'err) t[@kind k]) @ local -> bool
val is_error : ('ok : k) ('err : value_or_null). (('ok, 'err) t[@kind k]) @ local -> bool]

val ok : ('ok : value_or_null) ('err : value_or_null). ('ok, 'err) t -> 'ok option
val ok_or_null : 'ok ('err : value_or_null). ('ok, 'err) t -> 'ok or_null [@@zero_alloc]
val ok_exn : ('ok : value_or_null). ('ok, exn) t -> 'ok
val ok_or_failwith : ('ok : value_or_null). ('ok, string) t -> 'ok
val error : ('ok : value_or_null) ('err : value_or_null). ('ok, 'err) t -> 'err option

val error_or_null : ('ok : value_or_null) 'err. ('ok, 'err) t -> 'err or_null
[@@zero_alloc]

val of_option
  : ('ok : value_or_null) ('err : value_or_null).
  'ok option -> error:'err -> ('ok, 'err) t

val of_or_null : 'ok ('err : value_or_null). 'ok or_null -> error:'err -> ('ok, 'err) t

val of_option_or_thunk
  : ('ok : value_or_null) ('err : value_or_null).
  'ok option -> error:(unit -> 'err) @ local once -> ('ok, 'err) t

val iter
  : ('ok : value_or_null) ('err : value_or_null).
  ('ok, 'err) t -> f:('ok -> unit) @ local once -> unit

val iter_error
  : ('ok : value_or_null) ('err : value_or_null).
  ('ok, 'err) t -> f:('err -> unit) @ local once -> unit

[%%template:
[@@@mode.default m = (global, local)]
[@@@kind ko = all_ks]

val return : ('ok : ko) 'err. 'ok @ m -> (('ok, 'err) t[@kind ko]) @ m
[@@zero_alloc_if_local m] [@@kind ko]

[@@@kind.default ki = all_ks, ko = ko]

val bind
  : ('a : ki) ('b : ko) 'err.
  (('a, 'err) t[@kind ki]) @ m
  -> f:('a @ m -> (('b, 'err) t[@kind ko]) @ m) @ local
  -> (('b, 'err) t[@kind ko]) @ m

val map
  : ('a : ki) ('b : ko) 'err.
  (('a, 'err) t[@kind ki]) @ m
  -> f:('a @ m -> 'b @ m) @ local
  -> (('b, 'err) t[@kind ko]) @ m]

val%template map_error
  : ('ok : k) ('err : value_or_null) ('c : value_or_null).
  (('ok, 'err) t[@kind k]) @ m
  -> f:('err @ m -> 'c @ m) @ local once
  -> (('ok, 'c) t[@kind k]) @ m
[@@kind k = all_ks] [@@alloc __ @ m = (heap_global, stack_local)]

(** Returns [Ok] if both are [Ok] and [Error] otherwise. *)
val combine
  : ('ok1 : value_or_null) ('ok2 : value_or_null) ('ok3 : value_or_null)
    ('err : value_or_null).
  ('ok1, 'err) t
  -> ('ok2, 'err) t
  -> ok:('ok1 -> 'ok2 -> 'ok3) @ local once
  -> err:('err -> 'err -> 'err) @ local once
  -> ('ok3, 'err) t

[%%template:
[@@@alloc.default __ @ m = (heap_global, stack_local)]

(** [combine_errors ts] returns [Ok] if every element in [ts] is [Ok], else it returns
    [Error] with all the errors in [ts].

    This is similar to [all] from [Monad.S2], with the difference that [all] only returns
    the first error. *)
val combine_errors
  : ('ok : value_or_null) ('err : value_or_null).
  ('ok, 'err) t list @ m -> ('ok list, 'err list) t @ m]

(** [combine_errors_unit] returns [Ok] if every element in [ts] is [Ok ()], else it
    returns [Error] with all the errors in [ts], like [combine_errors]. *)
val combine_errors_unit
  : ('err : value_or_null).
  (unit, 'err) t list -> (unit, 'err list) t

[%%template:
[@@@mode.default m = (global, local)]

(** [to_either] is useful with [List.partition_map]. For example:

    {[
      let ints, exns =
        List.partition_map [ "1"; "two"; "three"; "4" ] ~f:(fun string ->
          Result.to_either (Result.try_with (fun () -> Int.of_string string)))
      ;;
    ]} *)
val to_either
  : ('ok : value_or_null) ('err : value_or_null).
  ('ok, 'err) t @ m -> ('ok, 'err) Either0.t @ m
[@@zero_alloc_if_local m]

val of_either
  : ('ok : value_or_null) ('err : value_or_null).
  ('ok, 'err) Either0.t @ m -> ('ok, 'err) t @ m
[@@zero_alloc_if_local m]]

(** [ok_if_true] returns [Ok ()] if [bool] is true, and [Error error] if it is false. *)
val ok_if_true : ('err : value_or_null). bool -> error:'err -> (unit, 'err) t

(** [transpose_opt x] transposes a [result] of an [option] into an [option] of a [result]

    Concretely, [Ok None] maps to [None], [Ok (Some x)] maps to [Some (Ok x)], and
    [Error e] maps to [Some (Error e)].

    Inverse of {!Option.transpose_result}. *)
val transpose_opt
  : ('ok : value_or_null) ('err : value_or_null).
  ('ok option, 'err) t @ m -> ('ok, 'err) t option @ m
[@@alloc __ @ m = (stack_local, heap_global)]

val try_with : ('a : value_or_null). (unit -> 'a) @ local once -> ('a, exn) t

module Export : sig
  type ('ok : value_or_null, 'err : value_or_null) _result = ('ok, 'err) t =
    | Ok of 'ok
    | Error of 'err

  [%%template:
  [@@@kind.default k = all_ks]

  val is_ok : ('ok : k) ('err : value_or_null). (('ok, 'err) t[@kind k]) -> bool
  val is_error : ('ok : k) ('err : value_or_null). (('ok, 'err) t[@kind k]) -> bool]
end]
