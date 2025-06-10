@@ portable

(** Exceptions.

    [sexp_of_t] uses a global table of sexp converters. To register a converter for a new
    exception, add [[@@deriving sexp]] to its definition. If no suitable converter is
    found, the standard converter in [Printexc] will be used to generate an atomic
    S-expression. *)

open! Import
module Sexp := Sexp0

type t = exn [@@deriving sexp_of]

include Pretty_printer.S with type t := t

(** Raised when finalization after an exception failed, too. The first exception argument
    is the one raised by the initial function, the second exception the one raised by the
    finalizer. *)
exception Finally of t * t

exception Reraised of string * t

(** [create_s sexp] returns an exception [t] such that [phys_equal (sexp_of_t t) sexp].
    This is useful when one wants to create an exception that serves as a message and the
    particular exn constructor doesn't matter. *)
val create_s : Sexp.t -> t

(** [create_s_lazy lazy_sexp] is like [create_s], but takes a lazily generated sexp. *)
val create_s_lazy : Sexp.t Lazy.t -> t

(** Same as [raise], except that the backtrace is not recorded. *)
val raise_without_backtrace : t -> _ @ portable unique

(** [raise_with_original_backtrace t bt] raises the exception [exn], recording [bt] as the
    backtrace it was originally raised at. This is useful to re-raise exceptions annotated
    with extra information. *)
val raise_with_original_backtrace
  :  t
  -> Stdlib.Printexc.raw_backtrace
  -> _ @ portable unique

val reraise : t -> string -> _ @ portable unique

(** Types with [format4] are hard to read, so here's an example.

    {[
      let foobar str =
        try
          ...
        with exn ->
          Exn.reraisef exn "Foobar is buggy on: %s" str ()
    ]} *)
val reraisef : t -> ('a, unit, string, unit -> _) format4 -> 'a

(** Human-readable, multi-line. *)
val to_string : t -> string

(** Machine format, single-line. *)
val to_string_mach : t -> string

(** Executes [f] and afterwards executes [finally], whether [f] throws an exception or
    not. *)
val protectx : f:local_ ('a -> 'b) -> 'a -> finally:local_ ('a -> unit) -> 'b

val protect : f:local_ (unit -> 'a) -> finally:local_ (unit -> unit) -> 'a

(** [handle_uncaught ~exit f] catches an exception escaping [f] and prints an error
    message to stderr. Exits with return code 1 if [exit] is [true], and returns unit
    otherwise.

    Note that since OCaml 4.02.0, you don't need to use this at the entry point of your
    program, as the OCaml runtime will do better than this function. *)
val handle_uncaught : exit:bool -> local_ (unit -> unit) -> unit @@ nonportable

(** [handle_uncaught_and_exit f] returns [f ()], unless that raises, in which case it
    prints the exception and exits nonzero. *)
val handle_uncaught_and_exit : local_ (unit -> 'a) -> 'a @@ nonportable

(** Traces exceptions passing through. Useful because in practice, backtraces still don't
    seem to work.

    Example:
    {[
      let rogue_function () = if Random.bool () then failwith "foo" else 3

      let traced_function () =
        Exn.reraise_uncaught "rogue_function" rogue_function traced_function ()
      ;;
    ]}
    {v : Program died with Reraised("rogue_function", Failure "foo") v} *)
val reraise_uncaught : string -> local_ (unit -> 'a) -> 'a

(** [does_raise f] returns [true] iff [f ()] raises, which is often useful in unit tests. *)
val does_raise : local_ (unit -> _) -> bool

(** Returns [true] if this exception is physically equal to the most recently raised one.
    If so, then [Backtrace.Exn.most_recent ()] is a backtrace corresponding to this
    exception.

    Note that, confusingly, exceptions can be physically equal even if the caller was not
    involved in handling of the last-raised exception. See the documentation of
    [Backtrace.Exn.most_recent_for_exn] for further discussion. *)
val is_phys_equal_most_recent : t -> bool

(** User code never calls this. It is called in [base.ml] as a top-level side effect to
    change the display of exceptions and install an uncaught-exception printer. *)
val initialize_module : unit -> unit

(**/**)

(*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

  https://opensource.janestreet.com/standards/#private-submodules *)
module Private : sig
  val clear_backtrace : unit -> unit
end
