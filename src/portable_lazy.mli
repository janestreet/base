open! Import

(** A value of type ['a Portable_lazy.t] is a deferred computation that has a result of
    type ['a].

    It is comparable to ['a Lazy.t], except that it can be shared between threads. If
    multiple threads force a ['a Portable_lazy.t] simultaneously, the computation will
    only be executed once, and the same result will be returned by all threads.

    Threads that force a ['a Portable_lazy.t] while it is already being computed by
    another thread will block via spinning and calls to [Domain.cpu_relax], without use of
    synchronization primitives or thread parking. This means that [Portable_lazy] may not
    be suitable for long-running computations which are likely to be forced by many
    threads simultaneously, and where prompt wakeup of blocked threads is important. *)

type 'a t = 'a Basement.Portable_lazy.t

(** [from_val v] returns an already-forced suspension of [v]. *)
val from_val : 'a. 'a -> 'a t

(** [from_fun f] returns a suspension of the function [f]. Note that [f] must be
    [portable], as it may be run by any domain. *)
val from_fun : 'a. (unit -> 'a) -> 'a t

(** [from_fun_fixed f] returns a suspension of the [{ifix-point}] function [f], which
    takes the lazy value itself as an argument. This can be used in places where one might
    normally use [let rec] to make a [lazy_t] that refers to itself. *)
val from_fun_fixed : 'a. ('a t -> 'a) -> 'a t

(** [force t] forces the suspension [t] and returns its result. If [t] has already been
    forced, [force t] returns the same value again without recomputing it. If multiple
    threads force the same lazy simultaneously, only one will execute the computation, and
    the rest will block until the computation has finished executing.

    If the suspension raises an exception, that exception will be converted to a string
    (to guarantee that it's safe to share between threads) and [force] will raise an
    exception. The raised exception is intentionally opaque and cannot be matched on in
    order to preserve forward compatibility; if you want the suspension to possibly return
    an exception, use [Result.t] or another similar type.

    Note that unlike the [lazy] in the standard library, [Portable_lazy] does {i not}
    raise an error if a lazy calls [force] from within its own suspension - instead, this
    will cause a deadlock. *)
val force : 'a. 'a t -> 'a

(** [map t ~f] returns a new suspension that, when forced, will first force [t], then call
    [f] on the result. Note that [f] must be [portable], as it may be run by any thread. *)
val map : 'a 'b. 'a t -> f:('a -> 'b) -> 'b t

(** [bind t ~f] returns a new suspension that when forced will first force [t], then call
    [f] on the result, then force the suspension returned by [f]. Note that [f] must be
    [portable], as it may be run by any thread. *)
val bind : 'a 'b. 'a t -> f:('a -> 'b t) -> 'b t

[%%template:
[@@@mode.default m = (global, local)]

(** [compare compare_a t1 t2] forces both suspensions [t1] and [t2], and then returns the
    result of [compare_a] called on the two results. *)
val compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int

(** [equal equal_a t1 t2] forces both suspensions [t1] and [t2], and then returns the
    result of [equal_a] called on the two results. *)
val equal : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool]

val sexp_of_t : 'a. ('a -> Sexp0.t) -> 'a t -> Sexp0.t

(** [t_of_sexp a_of_sexp sexp] eagerly evaluates [a_of_sexp sexp]. *)
val t_of_sexp : 'a. (Sexp0.t -> 'a) -> Sexp0.t -> 'a t

val t_sexp_grammar : 'a. 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t
val hash_fold_t : 'a. (Hash.state -> 'a -> Hash.state) -> Hash.state -> 'a t -> Hash.state

(** [globalize _ t] is a noop since all ['a t] values are created on the heap. *)
val globalize : 'a 'b. 'b -> 'a t -> 'a t

(** [is_val x] returns [true] if [x] has already been forced and did not raise an
    exception. Returns [false] if [x] has not been forced, or is currently being forced. *)
val is_val : 'a. 'a t -> bool

(** [peek x] returns [Null] if [x] has never been forced, raised an exception, or is
    currently being forced by any thread, or [This v] if [x] was forced to value [v]. *)
val peek : 'a. 'a t -> 'a or_null

(** [peek x] returns [None] if [x] has never been forced, raised an exception, or is
    currently being forced by any thread, or [Some v] if [x] was forced to value [v]. *)
val peek_opt : 'a. 'a t -> 'a option
