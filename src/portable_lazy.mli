@@ portable

open! Import0
module Sexp := Sexp0

(** A value of type ['a Portable_lazy.t] is a deferred computation that has a result of
    type ['a].

    It is comparable to ['a Lazy.t], except that it can be shared between threads. If
    multiple threads force a ['a Portable_lazy.t] simultaneously, the computation will
    only be executed once, and the same result will be returned by all threads.

    A thread that forces a ['a Portable_lazy.t] while it is already being computed by
    another thread will block until the computation finishes; it parks on a mutex and
    condition variable rather than spinning. *)

type ('a : value_or_null) t : value mod contended portable = 'a Basement.Portable_lazy.t

(** [from_val v] returns an already-forced suspension of [v]. *)
val from_val : ('a : value_or_null). 'a @ contended portable -> 'a t

(** [from_fun f] returns a suspension of the function [f]. Note that [f] must be
    [portable], as it may be run on any thread. *)
val from_fun
  : ('a : value_or_null).
  (unit -> 'a @ contended portable) @ once portable -> 'a t

(** [from_fun_fixed f] returns a suspension of the {i fix-point} function [f], which takes
    the lazy value itself as an argument. This can be used in places where one might
    normally use [let rec] to make a [lazy_t] that refers to itself. *)
val from_fun_fixed
  : ('a : value_or_null).
  ('a t -> 'a @ contended portable) @ once portable -> 'a t

(** [force t] forces the suspension [t] and returns its result. If [t] has already been
    forced, [force t] returns the same value again without recomputing it. If multiple
    threads force the same lazy simultaneously, only one will execute the computation, and
    the rest will block until the computation has finished executing.

    If the suspension raises an exception, the call to [force] and all subsequent calls to
    [force] on the same suspension will raise that exception.

    Note that unlike the [lazy] in the standard library, [Portable_lazy] does {i not}
    raise an error if a lazy calls [force] from within its own suspension - instead, this
    will cause a deadlock. *)
val force : ('a : value_or_null). 'a t @ local -> 'a @ contended portable

(** [map t ~f] returns a new suspension that, when forced, will first force [t], then call
    [f] on the result. Note that [f] must be [portable], as it may be run by any thread. *)
val map
  : ('a : value_or_null) ('b : value_or_null).
  'a t -> f:('a @ contended portable -> 'b @ contended portable) @ once portable -> 'b t

(** [bind t ~f] returns a new suspension that when forced will first force [t], then call
    [f] on the result, then force the suspension returned by [f]. Note that [f] must be
    [portable], as it may be run by any thread. *)
val bind
  : ('a : value_or_null) ('b : value_or_null).
  'a t -> f:('a @ contended portable -> 'b t) @ once portable -> 'b t

(** {6 Let syntax} *)

module Let_syntax : sig
  val return : ('a : value_or_null). 'a @ contended portable -> 'a t

  (** Pronounced "map". Infix form of [map]. *)
  val ( >>| )
    : ('a : value_or_null) ('b : value_or_null).
    'a t -> ('a @ contended portable -> 'b @ contended portable) @ once portable -> 'b t

  (** Pronounced "bind". Infix form of [bind]. *)
  val ( >>= )
    : ('a : value_or_null) ('b : value_or_null).
    'a t -> ('a @ contended portable -> 'b t) @ once portable -> 'b t

  module Let_syntax : sig
    val return : ('a : value_or_null). 'a @ contended portable -> 'a t

    val map
      : ('a : value_or_null) ('b : value_or_null).
      'a t
      -> f:('a @ contended portable -> 'b @ contended portable) @ once portable
      -> 'b t

    val bind
      : ('a : value_or_null) ('b : value_or_null).
      'a t -> f:('a @ contended portable -> 'b t) @ once portable -> 'b t

    val both : ('a : value_or_null) ('b : value_or_null). 'a t -> 'b t -> ('a * 'b) t

    module Open_on_rhs : sig end
  end
end

[%%template:
[@@@mode.default m = (global, local)]

(** [compare compare_a t1 t2] forces both suspensions [t1] and [t2], and then returns the
    result of [compare_a] called on the two results. *)
val compare
  : ('a : value_or_null mod contended).
  ('a @ m -> 'a @ m -> int) -> 'a t @ m -> 'a t @ m -> int

(** [equal equal_a t1 t2] forces both suspensions [t1] and [t2], and then returns the
    result of [equal_a] called on the two results. *)
val equal
  : ('a : value_or_null mod contended).
  ('a @ m -> 'a @ m -> bool) -> 'a t @ m -> 'a t @ m -> bool]

val sexp_of_t : ('a : value_or_null mod contended). ('a -> Sexp.t) -> 'a t -> Sexp.t

(** [t_of_sexp a_of_sexp sexp] eagerly evaluates [a_of_sexp sexp]. *)
val t_of_sexp : ('a : value_or_null mod portable). (Sexp.t -> 'a) -> Sexp.t -> 'a t

val t_sexp_grammar
  : ('a : value_or_null).
  'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t

val hash_fold_t
  : ('a : value_or_null mod contended).
  (Hash.state -> 'a -> Hash.state) -> Hash.state -> 'a t -> Hash.state

(** [globalize _ t] is a noop since all ['a t] values are created on the heap. *)
val globalize : ('a : value_or_null) ('b : value_or_null). 'b -> 'a t @ local -> 'a t

(** [is_val x] returns [true] if [x] has already been forced and did not raise an
    exception. Returns [false] if [x] has not been forced, or is currently being forced. *)
val is_val : ('a : value_or_null). 'a t -> bool

(** [peek x] returns [Null] if [x] has never been forced, raised an exception, or is
    currently being forced by any thread, or [This v] if [x] was forced to value [v]. *)
val peek : ('a : value). 'a t -> 'a or_null @ contended portable

(** [peek x] returns [None] if [x] has never been forced, raised an exception, or is
    currently being forced by any thread, or [Some v] if [x] was forced to value [v]. *)
val peek_opt : ('a : value_or_null). 'a t -> 'a option @ contended portable
