(** Type for tracking errors in an [Error.t]. This is a specialization of the [Result]
    type, where the [Error] constructor carries an [Error.t].

    A common idiom is to wrap a function that is not implemented on all platforms, e.g.,

    {[
      val do_something_linux_specific : (unit -> unit) Or_error.t
    ]} *)

open! Import0
module Error := Error0
module Sexp := Sexp0
module Source_code_position := Source_code_position0

(** Serialization and comparison of an [Error] force the error's lazy message. *)
type 'a t = ('a, Error.t) Result.t
[@@deriving compare, equal, globalize, hash, sexp, sexp_grammar]

(** [Applicative] functions don't have quite the same semantics as
    [Applicative.Of_Monad(Or_error)] would give -- [apply (Error e1) (Error e2)] returns
    the combination of [e1] and [e2], whereas it would only return [e1] if it were defined
    using [bind]. *)
include Applicative.S with type 'a t := 'a t

include Invariant.S1 with type 'a t := 'a t
include Monad.S with type 'a t := 'a t

val is_ok : 'a. 'a t -> bool
val is_error : 'a. 'a t -> bool

(** [try_with f] catches exceptions thrown by [f] and returns them in the [Result.t] as an
    [Error.t]. [try_with_join] is like [try_with], except that [f] can throw exceptions or
    return an [Error] directly, without ending up with a nested error; it is equivalent to
    [Result.join (try_with f)]. *)
val try_with : 'a. ?backtrace:bool (** defaults to [false] *) -> (unit -> 'a) -> 'a t

val try_with_join
  : 'a.
  ?backtrace:bool (** defaults to [false] *) -> (unit -> 'a t) -> 'a t

(** [ok t] returns [None] if [t] is an [Error], and otherwise returns the contents of the
    [Ok] constructor. *)
val ok : 'ok. 'ok t -> 'ok option

(** [ok_or_null t] returns [Null] if [t] is an [Error], and otherwise returns the contents
    of the [Ok] constructor wrapped in [This]. *)
val ok_or_null : 'ok t -> 'ok or_null
[@@zero_alloc]

(** [ok_exn t] throws an exception if [t] is an [Error], and otherwise returns the
    contents of the [Ok] constructor. *)
val ok_exn : 'a. 'a t -> 'a

(** [of_exn ?backtrace exn] is [Error (Error.of_exn ?backtrace exn)]. *)
val of_exn : 'a. ?backtrace:[ `Get | `This of string ] -> exn -> 'a t

(** [of_exn_result ?backtrace (Ok a) = Ok a]

    [of_exn_result ?backtrace (Error exn) = of_exn ?backtrace exn] *)
val of_exn_result
  : 'a.
  ?backtrace:[ `Get | `This of string ] -> ('a, exn) Result.t -> 'a t

(** [of_option t] returns [Ok 'a] if [t] is [Some 'a], and otherwise returns the supplied
    [error] as [Error error] *)
val of_option : 'a. 'a option -> error:Error.t -> 'a t

(** Calls [of_option ~error:(Error.of_lazy_t error)]. *)
val of_option_lazy : 'a. 'a option -> error:Error.t lazy_t -> 'a t

(** Calls [of_option ~error:(Error.of_lazy_sexp error)]. *)
val of_option_lazy_sexp : 'a. 'a option -> error:Sexp.t lazy_t -> 'a t

(** Calls [of_option ~error:(Error.of_lazy error)]. *)
val of_option_lazy_string : 'a. 'a option -> error:string lazy_t -> 'a t

(** [of_or_null t ~error] returns [Ok 'a] if [t] is [This 'a], and otherwise returns the
    supplied [error] as [Error error]. *)
val of_or_null : 'a or_null -> error:Error.t -> 'a t

(** Calls [of_or_null ~error:(Error.of_lazy_t error)]. *)
val of_or_null_lazy : 'a or_null -> error:Error.t lazy_t -> 'a t

(** Calls [of_or_null ~error:(Error.of_lazy_sexp error)]. *)
val of_or_null_lazy_sexp : 'a or_null -> error:Sexp.t lazy_t -> 'a t

(** Calls [of_or_null ~error:(Error.of_lazy error)]. *)
val of_or_null_lazy_string : 'a or_null -> error:string lazy_t -> 'a t

(** [error] is a wrapper around [Error.create]:

    {[
      error ?strict message a sexp_of_a = Error (Error.create ?strict message a sexp_of_a)
    ]}

    As with [Error.create], [sexp_of_a a] is lazily computed when the info is converted to
    a sexp. So, if [a] is mutated in the time between the call to [create] and the sexp
    conversion, those mutations will be reflected in the sexp. Use [~strict:()] to force
    [sexp_of_a a] to be computed immediately. *)
val error
  : 'a 'b.
  ?here:Source_code_position.t -> ?strict:unit -> string -> 'a -> ('a -> Sexp.t) -> 'b t

val error_s : 'a. Sexp.t -> 'a t

(** [error_string message] is [Error (Error.of_string message)]. *)
val error_string : 'a. string -> 'a t

(** [errorf format arg1 arg2 ...] is [Error (sprintf format arg1 arg2 ...)]. Note that it
    calculates the string eagerly, so when performance matters you may want to use [error]
    instead. *)
val errorf : 'a 'b. ('a, unit, string, 'b t) format4 -> 'a

(** [errorf_portable format arg1 arg2 ... ()] is like [errorf format arg1 arg2 ...] but
    constructing a portable error. *)
val errorf_portable : 'a 'b. ('a, unit, string, unit -> 'b t) format4 -> 'a

(** [tag t ~tag] is [Result.map_error t ~f:(Error.tag ~tag)]. *)
val tag : 'a. 'a t -> tag:string -> 'a t

(** [tag_s] is like [tag] with a sexp tag. *)
val tag_s : 'a. 'a t -> tag:Sexp.t -> 'a t

(** [tag_lazy] is like [tag] with a lazy tag. *)
val tag_lazy : 'a. 'a t -> tag:string lazy_t -> 'a t

(** [tag_s_lazy] is like [tag] with a lazy sexp tag. *)
val tag_s_lazy : 'a. 'a t -> tag:Sexp.t lazy_t -> 'a t

(** [tag_arg] is like [tag], with a tag that has a sexpable argument. *)
val tag_arg : 'a 'b. 'a t -> string -> 'b -> ('b -> Sexp.t) -> 'a t

(** For marking a given value as unimplemented. Typically combined with conditional
    compilation, where on some platforms the function is defined normally, and on some
    platforms it is defined as unimplemented. The supplied string should be the name of
    the function that is unimplemented. *)
val unimplemented : 'a. string -> 'a t

val return : 'a. 'a -> 'a t
val bind : 'a 'b. 'a t -> f:('a -> 'b t) -> 'b t
val map : 'a 'b. 'a t -> f:('a -> 'b) -> 'b t
val iter : 'a. 'a t -> f:('a -> unit) -> unit
val iter_error : 'a. 'a t -> f:(Error.t -> unit) -> unit

(** [combine_errors ts] returns [Ok] if every element in [ts] is [Ok], else it returns
    [Error] with all the errors in [ts]. More precisely:

    - [combine_errors [Ok a1; ...; Ok an] = Ok [a1; ...; an]]
    - {[
        combine_errors [...; Error e1; ...; Error en; ...]
        = Error (Error.of_list [e1; ...; en])
      ]} *)
val combine_errors : 'a. 'a t list -> 'a list t

(** [combine_errors_unit ts] returns [Ok] if every element in [ts] is [Ok ()], else it
    returns [Error] with all the errors in [ts], like [combine_errors]. *)
val combine_errors_unit : unit t list -> unit t

(** [filter_ok_at_least_one ts] returns all values in [ts] that are [Ok] if there is at
    least one, otherwise it returns the same error as [combine_errors ts]. Returns a
    bespoke error when passed an empty list. *)
val filter_ok_at_least_one : 'a. 'a t list -> 'a list t

(** [find_ok ts] returns the first value in [ts] that is [Ok], otherwise it returns the
    same error as [combine_errors ts]. *)
val find_ok : 'a. 'a t list -> 'a t

(** [find_map_ok l ~f] returns the first value in [l] for which [f] returns [Ok],
    otherwise it returns the same error as [combine_errors (List.map l ~f)]. Returns a
    bespoke error when passed an empty list. *)
val find_map_ok : 'a 'b. 'a list -> f:('a -> 'b t) -> 'b t

(** [('a, Error.Portable.t) Result.t] with some common ppxes derived on it. It also
    re-exports some functions of [Or_error] so users don't need to frequently wrap and
    unwrap [Error.Portable.t]. *)
module Portable : sig
  module Sexp := Sexp0

  type 'a t = ('a, Error.Portable.t) Result.t [@@deriving compare, equal, globalize, sexp]

  include Applicative.S with type 'a t := 'a t
  include Monad.S with type 'a t := 'a t

  val ok : 'ok t -> 'ok option
  val ok_exn : 'a t -> 'a

  val error
    : 'a 'b.
    ?here:Source_code_position0.t
    -> ?strict:unit
    -> string
    -> 'b
    -> ('b -> Sexp.t)
    -> 'a t

  val error_s : Sexp.t -> _ t
  val error_string : string -> _ t
  val tag : 'a t -> tag:string -> 'a t
  val tag_s : 'a t -> tag:Sexp.t -> 'a t
  val tag_arg : 'a 'b. 'a t -> string -> 'b -> ('b -> Sexp.t) -> 'a t
  val combine_errors : 'a t list -> 'a list t
  val combine_errors_unit : unit t list -> unit t

  (** [try_with f] catches exceptions thrown by [f] and returns them as portable errors.
      [try_with_join] is like [try_with], except that [f] may return an [Error] directly. *)
  val try_with : ?backtrace:bool -> (unit -> 'a) -> 'a t

  val try_with_join : ?backtrace:bool -> (unit -> 'a t) -> 'a t

  (** [errorf format arg1 arg2 ...] is [error_string (sprintf format arg1 arg2 ...)]. *)
  val errorf : ('a, unit, string, 'b t) format4 -> 'a

  (** [of_exn ?backtrace exn] is
      [Error (Error.to_portable (Error.of_exn ?backtrace exn))]. *)
  val of_exn : ?backtrace:[ `Get | `This of string ] -> exn -> 'a t

  val of_exn_result : ?backtrace:[ `Get | `This of string ] -> ('a, exn) Result.t -> 'a t
end

val of_portable : 'a Portable.t -> 'a t
val to_portable : 'a t -> 'a Portable.t

(** Constructs a [Portable.t] out of a possibly non-portable [t].

    This operation is a no-op for [Ok], but it's not a no-op because it needs to force any
    non-portable computation of the input error. *)
val portabilize : 'a t -> 'a Portable.t
