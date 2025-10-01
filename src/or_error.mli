(** Type for tracking errors in an [Error.t]. This is a specialization of the [Result]
    type, where the [Error] constructor carries an [Error.t].

    A common idiom is to wrap a function that is not implemented on all platforms, e.g.,

    {[
      val do_something_linux_specific : (unit -> unit) Or_error.t
    ]} *)

open! Import
module Sexp := Sexp0

[%%template:
type 'a t = (('a, Error.t) Result.t[@kind k])
[@@deriving compare ~localize, equal ~localize, globalize, sexp]
[@@kind k = (float64, bits32, bits64, word)]

(** Serialization and comparison of an [Error] force the error's lazy message. *)
type 'a t = (('a, Error.t) Result.t[@kind k])
[@@deriving compare ~localize, equal ~localize, globalize, hash, sexp, sexp_grammar]
[@@kind k = (value, immediate, immediate64)]]

(** [Applicative] functions don't have quite the same semantics as
    [Applicative.Of_Monad(Or_error)] would give -- [apply (Error e1) (Error e2)] returns
    the combination of [e1] and [e2], whereas it would only return [e1] if it were defined
    using [bind]. *)
include Applicative.S__local with type 'a t := 'a t

include Invariant.S1 with type 'a t := 'a t
include Monad.S__local with type 'a t := 'a t

val is_ok : _ t -> bool
val is_error : _ t -> bool

(** [try_with f] catches exceptions thrown by [f] and returns them in the [Result.t] as an
    [Error.t]. [try_with_join] is like [try_with], except that [f] can throw exceptions or
    return an [Error] directly, without ending up with a nested error; it is equivalent to
    [Result.join (try_with f)]. *)
val%template try_with : ?backtrace:bool (** defaults to [false] *) -> (unit -> 'a) -> 'a t
[@@mode p = (nonportable, portable)]

val try_with_join : ?backtrace:bool (** defaults to [false] *) -> (unit -> 'a t) -> 'a t

(** [ok t] returns [None] if [t] is an [Error], and otherwise returns the contents of the
    [Ok] constructor. *)
val ok : 'ok t -> 'ok option

(** [ok_exn t] throws an exception if [t] is an [Error], and otherwise returns the
    contents of the [Ok] constructor. *)
val%template ok_exn : 'a. ('a t[@kind k]) -> 'a
[@@kind k = (value_or_null, immediate, immediate64, float64, bits32, bits64, word)]

(** [of_exn ?backtrace exn] is [Error (Error.of_exn ?backtrace exn)]. *)
val of_exn : ?backtrace:[ `Get | `This of string ] -> exn -> _ t

(** [of_exn_result ?backtrace (Ok a) = Ok a]

    [of_exn_result ?backtrace (Error exn) = of_exn ?backtrace exn] *)
val of_exn_result : ?backtrace:[ `Get | `This of string ] -> ('a, exn) Result.t -> 'a t

(** [of_option t] returns [Ok 'a] if [t] is [Some 'a], and otherwise returns the supplied
    [error] as [Error error] *)
val of_option : 'a option -> error:Error.t -> 'a t

(** Calls [of_option ~error:(Error.of_lazy_t error)]. *)
val of_option_lazy : 'a option -> error:Error.t Lazy.t -> 'a t

(** Calls [of_option ~error:(Error.of_lazy_sexp error)]. *)
val of_option_lazy_sexp : 'a option -> error:Sexp.t Lazy.t -> 'a t

(** Calls [of_option ~error:(Error.of_lazy error)]. *)
val of_option_lazy_string : 'a option -> error:string Lazy.t -> 'a t

(** [error] is a wrapper around [Error.create]:

    {[
      error ?strict message a sexp_of_a = Error (Error.create ?strict message a sexp_of_a)
    ]}

    As with [Error.create], [sexp_of_a a] is lazily computed when the info is converted to
    a sexp. So, if [a] is mutated in the time between the call to [create] and the sexp
    conversion, those mutations will be reflected in the sexp. Use [~strict:()] to force
    [sexp_of_a a] to be computed immediately. *)
val error
  :  ?here:Source_code_position0.t
  -> ?strict:unit
  -> string
  -> 'a
  -> ('a -> Sexp.t)
  -> _ t

val error_s : Sexp.t -> _ t

(** [error_string message] is [Error (Error.of_string message)]. *)
val error_string : string -> _ t

(** [errorf format arg1 arg2 ...] is [Error (sprintf format arg1 arg2 ...)]. Note that it
    calculates the string eagerly, so when performance matters you may want to use [error]
    instead. *)
val errorf : ('a, unit, string, _ t) format4 -> 'a

(** [errorf_portable format arg1 arg2 ... ()] is like [errorf format arg1 arg2 ...] but
    constructing a portable error. *)
val errorf_portable : ('a, unit, string, unit -> _ t) format4 -> 'a

(** [tag t ~tag] is [Result.map_error t ~f:(Error.tag ~tag)]. *)
val%template tag : 'a t -> tag:string -> 'a t
[@@mode p = (portable, nonportable)]

(** [tag_s] is like [tag] with a sexp tag. *)
val%template tag_s : 'a t -> tag:Sexp.t -> 'a t
[@@mode p = (portable, nonportable)]

(** [tag_s_lazy] is like [tag] with a lazy sexp tag. *)
val tag_s_lazy : 'a t -> tag:Sexp.t Lazy.t -> 'a t

(** [tag_arg] is like [tag], with a tag that has a sexpable argument. *)
val tag_arg : 'a t -> string -> 'b -> ('b -> Sexp.t) -> 'a t

(** For marking a given value as unimplemented. Typically combined with conditional
    compilation, where on some platforms the function is defined normally, and on some
    platforms it is defined as unimplemented. The supplied string should be the name of
    the function that is unimplemented. *)
val unimplemented : string -> _ t

val%template map : 'a 'b. ('a t[@kind ki]) -> f:('a -> 'b) -> ('b t[@kind ko])
[@@kind
  ki = (value_or_null, immediate, immediate64, float64, bits32, bits64, word)
  , ko = (value_or_null, immediate, immediate64, float64, bits32, bits64, word)]

val iter : 'a t -> f:('a -> unit) -> unit
val iter_error : _ t -> f:(Error.t -> unit) -> unit

(** [combine_errors ts] returns [Ok] if every element in [ts] is [Ok], else it returns
    [Error] with all the errors in [ts]. More precisely:

    - [combine_errors [Ok a1; ...; Ok an] = Ok [a1; ...; an]]
    - {[
        combine_errors [...; Error e1; ...; Error en; ...]
        = Error (Error.of_list [e1; ...; en])
      ]} *)
val%template combine_errors : 'a t list -> 'a list t
[@@mode p = (portable, nonportable)]

(** [combine_errors_unit ts] returns [Ok] if every element in [ts] is [Ok ()], else it
    returns [Error] with all the errors in [ts], like [combine_errors]. *)
val%template combine_errors_unit : unit t list -> unit t
[@@mode p = (portable, nonportable)]

(** [filter_ok_at_least_one ts] returns all values in [ts] that are [Ok] if there is at
    least one, otherwise it returns the same error as [combine_errors ts]. Returns a
    bespoke error when passed an empty list. *)
val filter_ok_at_least_one : 'a t list -> 'a list t

(** [find_ok ts] returns the first value in [ts] that is [Ok], otherwise it returns the
    same error as [combine_errors ts]. *)
val find_ok : 'a t list -> 'a t

(** [find_map_ok l ~f] returns the first value in [l] for which [f] returns [Ok],
    otherwise it returns the same error as [combine_errors (List.map l ~f)]. Returns a
    bespoke error when passed an empty list. *)
val find_map_ok : 'a list -> f:('a -> 'b t) -> 'b t
