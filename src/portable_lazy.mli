@@ portable

open! Import

(** A value of type ['a Portable_lazy.t] is a deferred computation that has a result of
    type ['a].

    It is comparable to ['a Lazy.t], except that it can be shared between domains. If
    multiple domains force a ['a Portable_lazy.t] simultaneously, the computation will
    only be executed once, and the same result will be returned by all domains.

    Domains that force a ['a Portable_lazy.t] while it is already being computed by
    another domain will block via spinning and calls to [Domain.cpu_relax], without use of
    synchronization primitives or thread parking. This means that [Portable_lazy] may not
    be suitable for long-running computations which are likely to be forced by many
    domains simultaneously, and where prompt wakeup of blocked domains is important. *)

type 'a t : value mod contended portable = 'a Basement.Portable_lazy.t

(** [from_val v] returns an already-forced suspension of [v]. *)
val from_val : 'a @ contended portable -> 'a t

(** [from_fun f] returns a suspension of the function [f]. Note that [f] must be
    [portable], as it may be run by any domain. *)
val from_fun : (unit -> 'a @ contended portable) @ once portable -> 'a t

(** [from_fun_fixed f] returns a suspension of the [{ifix-point}] function [f], which
    takes the lazy value itself as an argument. This can be used in places where one might
    normally use [let rec] to make a [lazy_t] that refers to itself. *)
val from_fun_fixed : ('a t -> 'a @ contended portable) @ once portable -> 'a t

exception Undefined

(** [force t] forces the suspension [t] and returns its result. If [t] has already been
    forced, [force t] returns the same value again without recomputing it. If multiple
    domains force the same lazy simultaneously, only one will execute the computation, and
    the rest will block until the computation has finished executing.

    If the suspension raises an exception, that exception will be converted to a string
    (to guarantee that it's safe to share between threads) and [force] will raise an
    exception. The raised exception is intentionally opaque and cannot be matched on in
    order to preserve forward compatibility; if you want the suspension to possibly return
    an exception, use [Result.t] or another similar type.

    If the computation calls [force t] on the lazy itself recursively from the same
    domain, [force] will raise [Undefined]. *)
val force : 'a t -> 'a @ contended portable

(** [map t ~f] returns a new suspension that, when forced, will first force [t], then call
    [f] on the result. Note that [f] must be [portable], as it may be run by any domain. *)
val map
  :  'a t
  -> f:('a @ contended portable -> 'b @ contended portable) @ once portable
  -> 'b t

(** [bind t ~f] returns a new suspension that when forced will first force [t], then call
    [f] on the result, then force the suspension returned by [f]. Note that [f] must be
    [portable], as it may be run by any domain. *)
val bind : 'a t -> f:('a @ contended portable -> 'b t) @ once portable -> 'b t

(** [compare compare_a t1 t2] forces both suspensions [t1] and [t2], and then returns the
    result of [compare_a] called on the two results. *)
val compare : ('a : value mod contended). ('a -> 'a -> int) -> 'a t -> 'a t -> int

val compare__local
  : ('a : value mod contended).
  ('a @ local -> 'a @ local -> int) -> 'a t @ local -> 'a t @ local -> int

(** [equal equal_a t1 t2] forces both suspensions [t1] and [t2], and then returns the
    result of [equal_a] called on the two results. *)
val equal : ('a : value mod contended). ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val equal__local
  : ('a : value mod contended).
  ('a @ local -> 'a @ local -> bool) -> 'a t @ local -> 'a t @ local -> bool

val sexp_of_t : ('a : value mod contended). ('a -> Sexp0.t) -> 'a t -> Sexp0.t

(** [t_of_sexp a_of_sexp sexp] eagerly evaluates [a_of_sexp sexp]. *)
val t_of_sexp : ('a : value mod portable). (Sexp0.t -> 'a) -> Sexp0.t -> 'a t

val t_sexp_grammar : 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t

val hash_fold_t
  : ('a : value mod contended).
  (Hash.state -> 'a -> Hash.state) -> Hash.state -> 'a t -> Hash.state

(** [globalize _ t] is a noop since all ['a t] values are created on the heap. *)
val globalize : _ -> 'a t @ local -> 'a t

(** [is_val x] returns [true] if [x] has already been forced and did not raise an
    exception. Returns [false] if [x] has not been forced, or is currently being forced. *)
val is_val : 'a t -> bool

(** [peek x] returns [None] if [x] has never been forced, raised an exception, or is
    currently being forced by any domain, or [Some v] if [x] was forced to value [v]. *)
val peek : 'a t -> 'a option @ contended portable
