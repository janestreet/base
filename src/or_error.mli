@@ portable

(** Type for tracking errors in an [Error.t]. This is a specialization of the [Result]
    type, where the [Error] constructor carries an [Error.t].

    A common idiom is to wrap a function that is not implemented on all platforms, e.g.,

    {[
      val do_something_linux_specific : (unit -> unit) Or_error.t
    ]} *)

open! Import
module Sexp := Sexp0

[@@@warning "-incompatible-with-upstream"]

[%%template:
  type ('a : k) t = (('a, Error.t) Result.t[@kind k])
  [@@deriving compare ~localize, equal ~localize, globalize, sexp]
  [@@kind k = base_non_value]]

(** Serialization and comparison of an [Error] force the error's lazy message. *)
type ('a : value_or_null) t = ('a, Error.t) Result.t
[@@deriving compare ~localize, equal ~localize, globalize, hash, sexp, sexp_grammar]

(** [Applicative] functions don't have quite the same semantics as
    [Applicative.Of_Monad(Or_error)] would give -- [apply (Error e1) (Error e2)] returns
    the combination of [e1] and [e2], whereas it would only return [e1] if it were defined
    using [bind]. *)
include%template
  Applicative.S
  [@kind value_or_null mod maybe_null] [@mode local]
  with type ('a : value_or_null) t := 'a t

include Invariant.S1 with type 'a t := 'a t

include%template
  Monad.S
  [@kind value_or_null mod maybe_null] [@mode local]
  with type ('a : value_or_null) t := 'a t

val is_ok : ('a : value_or_null). 'a t -> bool
val is_error : ('a : value_or_null). 'a t -> bool

(** [try_with f] catches exceptions thrown by [f] and returns them in the [Result.t] as an
    [Error.t]. [try_with_join] is like [try_with], except that [f] can throw exceptions or
    return an [Error] directly, without ending up with a nested error; it is equivalent to
    [Result.join (try_with f)]. *)
val%template try_with
  : ('a : value_or_null).
  ?backtrace:bool (** defaults to [false] *) -> local_ (unit -> 'a @ p) -> 'a t @ p
[@@mode p = (nonportable, portable)]

val try_with_join
  : ('a : value_or_null).
  ?backtrace:bool (** defaults to [false] *) -> local_ (unit -> 'a t) -> 'a t

(** [ok t] returns [None] if [t] is an [Error], and otherwise returns the contents of the
    [Ok] constructor. *)
val ok : ('ok : value_or_null). 'ok t -> 'ok option

(** [ok_exn t] throws an exception if [t] is an [Error], and otherwise returns the
    contents of the [Ok] constructor. *)
val%template ok_exn : ('a : k). ('a t[@kind k]) -> 'a
[@@kind k = base_or_null]

(** [of_exn ?backtrace exn] is [Error (Error.of_exn ?backtrace exn)]. *)
val of_exn : ('a : value_or_null). ?backtrace:[ `Get | `This of string ] -> exn -> 'a t

(** [of_exn_result ?backtrace (Ok a) = Ok a]

    [of_exn_result ?backtrace (Error exn) = of_exn ?backtrace exn] *)
val of_exn_result
  : ('a : value_or_null).
  ?backtrace:[ `Get | `This of string ] -> ('a, exn) Result.t -> 'a t

(** [of_option t] returns [Ok 'a] if [t] is [Some 'a], and otherwise returns the supplied
    [error] as [Error error] *)
val of_option : ('a : value_or_null). 'a option -> error:Error.t -> 'a t

(** Calls [of_option ~error:(Error.of_lazy_t error)]. *)
val of_option_lazy : ('a : value_or_null). 'a option -> error:Error.t Lazy.t -> 'a t

(** Calls [of_option ~error:(Error.of_lazy_sexp error)]. *)
val of_option_lazy_sexp : ('a : value_or_null). 'a option -> error:Sexp.t Lazy.t -> 'a t

(** Calls [of_option ~error:(Error.of_lazy error)]. *)
val of_option_lazy_string : ('a : value_or_null). 'a option -> error:string Lazy.t -> 'a t

(** [error] is a wrapper around [Error.create]:

    {[
      error ?strict message a sexp_of_a = Error (Error.create ?strict message a sexp_of_a)
    ]}

    As with [Error.create], [sexp_of_a a] is lazily computed when the info is converted to
    a sexp. So, if [a] is mutated in the time between the call to [create] and the sexp
    conversion, those mutations will be reflected in the sexp. Use [~strict:()] to force
    [sexp_of_a a] to be computed immediately. *)
val%template error
  : ('a : value_or_null mod c) ('b : value_or_null).
  ?here:Source_code_position0.t
  -> ?strict:unit
  -> string
  -> 'a @ p
  -> ('a -> Sexp.t) @ p
  -> 'b t @ p
[@@mode (p, c) = ((nonportable, uncontended), (portable, contended))]

val error_s : ('a : value_or_null). Sexp.t -> 'a t @ portable

(** [error_string message] is [Error (Error.of_string message)]. *)
val error_string : ('a : value_or_null). string -> 'a t @ portable

(** [errorf format arg1 arg2 ...] is [Error (sprintf format arg1 arg2 ...)]. Note that it
    calculates the string eagerly, so when performance matters you may want to use [error]
    instead. *)
val errorf : 'a ('b : value_or_null). ('a, unit, string, 'b t) format4 -> 'a

(** [errorf_portable format arg1 arg2 ... ()] is like [errorf format arg1 arg2 ...] but
    constructing a portable error. *)
val errorf_portable
  : 'a ('b : value_or_null).
  ('a, unit, string, unit -> 'b t @ portable) format4 -> 'a

(** [tag t ~tag] is [Result.map_error t ~f:(Error.tag ~tag)]. *)
val%template tag : ('a : value_or_null). 'a t @ p -> tag:string -> 'a t @ p
[@@mode p = (portable, nonportable)]

(** [tag_s] is like [tag] with a sexp tag. *)
val%template tag_s : ('a : value_or_null). 'a t @ p -> tag:Sexp.t -> 'a t @ p
[@@mode p = (portable, nonportable)]

(** [tag_s_lazy] is like [tag] with a lazy sexp tag. *)
val tag_s_lazy : ('a : value_or_null). 'a t -> tag:Sexp.t Lazy.t -> 'a t

(** [tag_arg] is like [tag], with a tag that has a sexpable argument. *)
val tag_arg
  : ('a : value_or_null) ('b : value_or_null).
  'a t -> string -> 'b -> ('b -> Sexp.t) -> 'a t

(** For marking a given value as unimplemented. Typically combined with conditional
    compilation, where on some platforms the function is defined normally, and on some
    platforms it is defined as unimplemented. The supplied string should be the name of
    the function that is unimplemented. *)
val unimplemented : ('a : value_or_null). string -> 'a t @ portable

val%template map
  : ('a : ki) ('b : ko).
  ('a t[@kind ki]) -> f:local_ ('a -> 'b) -> ('b t[@kind ko])
[@@kind ki = base_or_null, ko = base_or_null]

val iter : ('a : value_or_null). 'a t -> f:local_ ('a -> unit) -> unit
val iter_error : ('a : value_or_null). 'a t -> f:local_ (Error.t -> unit) -> unit

(** [combine_errors ts] returns [Ok] if every element in [ts] is [Ok], else it returns
    [Error] with all the errors in [ts]. More precisely:

    - [combine_errors [Ok a1; ...; Ok an] = Ok [a1; ...; an]]
    - {[
        combine_errors [...; Error e1; ...; Error en; ...]
        = Error (Error.of_list [e1; ...; en])
      ]} *)
val%template combine_errors : ('a : value_or_null). 'a t list @ p -> 'a list t @ p
[@@mode p = (portable, nonportable)]

(** [combine_errors_unit ts] returns [Ok] if every element in [ts] is [Ok ()], else it
    returns [Error] with all the errors in [ts], like [combine_errors]. *)
val%template combine_errors_unit : unit t list @ p -> unit t @ p
[@@mode p = (portable, nonportable)]

(** [filter_ok_at_least_one ts] returns all values in [ts] that are [Ok] if there is at
    least one, otherwise it returns the same error as [combine_errors ts]. Returns a
    bespoke error when passed an empty list. *)
val filter_ok_at_least_one : ('a : value_or_null). 'a t list -> 'a list t

(** [find_ok ts] returns the first value in [ts] that is [Ok], otherwise it returns the
    same error as [combine_errors ts]. *)
val find_ok : ('a : value_or_null). 'a t list -> 'a t

(** [find_map_ok l ~f] returns the first value in [l] for which [f] returns [Ok],
    otherwise it returns the same error as [combine_errors (List.map l ~f)]. Returns a
    bespoke error when passed an empty list. *)
val find_map_ok
  : ('a : value_or_null) ('b : value_or_null).
  'a list -> f:local_ ('a -> 'b t) -> 'b t
