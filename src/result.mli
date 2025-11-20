@@ portable

(** [Result] is often used to handle error messages. *)

open! Import
module Invariant := Invariant_intf.Definitions

[@@@warning "-incompatible-with-upstream"]

[%%template:
type ('ok : k, 'err : value_or_null) t =
  | Ok of 'ok
  | Error of 'err
[@@deriving sexp ~stackify, compare ~localize, equal ~localize, globalize]
[@@kind k = base_non_value]

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
[@@@kind.default k = base_or_null_with_imm]

val is_ok : ('ok : k) ('err : value_or_null). (('ok, 'err) t[@kind k]) -> bool
val is_error : ('ok : k) ('err : value_or_null). (('ok, 'err) t[@kind k]) -> bool]

val ok : ('ok : value_or_null) ('err : value_or_null). ('ok, 'err) t -> 'ok option
val ok_exn : ('ok : value_or_null). ('ok, exn) t -> 'ok
val ok_or_failwith : ('ok : value_or_null). ('ok, string) t -> 'ok
val error : ('ok : value_or_null) ('err : value_or_null). ('ok, 'err) t -> 'err option

val of_option
  : ('ok : value_or_null) ('err : value_or_null).
  'ok option -> error:'err -> ('ok, 'err) t

val of_option_or_thunk
  : ('ok : value_or_null) ('err : value_or_null).
  'ok option -> error:(unit -> 'err) -> ('ok, 'err) t

val iter
  : ('ok : value_or_null) ('err : value_or_null).
  ('ok, 'err) t -> f:local_ ('ok -> unit) -> unit

val iter_error
  : ('ok : value_or_null) ('err : value_or_null).
  ('ok, 'err) t -> f:local_ ('err -> unit) -> unit

val%template map
  : ('a : ki) ('b : ko) 'err.
  (('a, 'err) t[@kind ki]) -> f:local_ ('a -> 'b) -> (('b, 'err) t[@kind ko])
[@@kind ki = base_or_null_with_imm, ko = base_or_null_with_imm]

val map_error
  : ('ok : value_or_null) ('err : value_or_null) ('c : value_or_null).
  ('ok, 'err) t -> f:local_ ('err -> 'c) -> ('ok, 'c) t

(** Returns [Ok] if both are [Ok] and [Error] otherwise. *)
val combine
  : ('ok1 : value_or_null) ('ok2 : value_or_null) ('ok3 : value_or_null)
    ('err : value_or_null).
  ('ok1, 'err) t
  -> ('ok2, 'err) t
  -> ok:local_ ('ok1 -> 'ok2 -> 'ok3)
  -> err:local_ ('err -> 'err -> 'err)
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

val of_either
  : ('ok : value_or_null) ('err : value_or_null).
  ('ok, 'err) Either0.t @ m -> ('ok, 'err) t @ m]

(** [ok_if_true] returns [Ok ()] if [bool] is true, and [Error error] if it is false. *)
val ok_if_true : ('err : value_or_null). bool -> error:'err -> (unit, 'err) t

val try_with : ('a : value_or_null). local_ (unit -> 'a) -> ('a, exn) t

module Export : sig
  type ('ok : value_or_null, 'err : value_or_null) _result = ('ok, 'err) t =
    | Ok of 'ok
    | Error of 'err

  [%%template:
  [@@@kind.default k = base_or_null_with_imm]

  val is_ok : ('ok : k) ('err : value_or_null). (('ok, 'err) t[@kind k]) -> bool
  val is_error : ('ok : k) ('err : value_or_null). (('ok, 'err) t[@kind k]) -> bool]
end
