(** An uninhabited type. This is useful when interfaces require that a type be specified,
    but the implementer knows this type will not be used in their implementation of the
    interface.

    For instance, [Async.Rpc.Pipe_rpc.t] is parameterized by an error type, but a user may
    want to define a Pipe RPC that can't fail. *)

open! Import

(** Having [[@@deriving enumerate]] may seem strange due to the fact that generated
    [val all : t list] is the empty list, so it seems like it could be of no use.

    This may be true if you always expect your type to be [Nothing.t], but
    [[@@deriving enumerate]] can be useful if you have a type which you expect to change
    over time. For example, you may have a program which has to interact with multiple
    servers which are possibly at different versions. It may be useful in this program to
    have a variant type which enumerates the ways in which the servers may differ. When
    all the servers are at the same version, you can change this type to [Nothing.t] and
    code which uses an enumeration of the type will continue to work correctly.

    This is a similar issue to the identifiability of [Nothing.t]. As discussed below,
    another case where [[@deriving enumerate]] could be useful is when this type is part
    of some larger type.

    Similar arguments apply for other derivers, like [globalize] and [sexp_grammar]. *)
type t = | [@@deriving enumerate, globalize, sexp ~stackify, sexp_grammar]

(** Because there are no values of type [Nothing.t], a piece of code that has a value of
    type [Nothing.t] must be unreachable. In such an unreachable piece of code, one can
    use [unreachable_code] to give the code whatever type one needs. For example:

    {[
      let f (r : (int, Nothing.t) Result.t) : int =
        match r with
        | Ok i -> i
        | Error n -> Nothing.unreachable_code n
      ;;
    ]}

    Note that the compiler knows that [Nothing.t] is uninhabited, hence this will type
    without warning:

    {[
      let f (Ok i : (int, Nothing.t) Result.t) = i
    ]} *)
val unreachable_code : t -> _

(** The same as [unreachable_code], but for local [t]s. *)
val unreachable_code_local : t -> _

(** It may seem weird that this is identifiable, but we're just trying to anticipate all
    the contexts in which people may need this. It would be a crying shame if you had some
    variant type involving [Nothing.t] that you wished to make identifiable, but were
    prevented for lack of [Identifiable.S] here.

    Obviously, [of_string] and [t_of_sexp] will raise an exception. *)
include%template Identifiable.S [@mode local] [@modality portable] with type t := t

(** Ignores [None] and guarantees there is no [Some _]. A better replacement for [ignore]. *)
val must_be_none : t option -> unit

(** Ignores [ [] ] and guarantees there is no [_ :: _]. A better replacement for [ignore]. *)
val must_be_empty : t list -> unit

(** Returns [ok] from [Ok ok] and guarantees there is no [Error _]. *)
val must_be_ok : ('ok, t) Result.t -> 'ok

(** Returns [err] from [Error err] and guarantees there is no [Ok _]. *)
val must_be_error : (t, 'err) Result.t -> 'err

(** Returns [fst] from [First fst] and guarantees there is no [Second _]. *)
val must_be_first : ('fst, t) Either.t -> 'fst

(** Returns [snd] from [Second snd] and guarantees there is no [First _]. *)
val must_be_second : (t, 'snd) Either.t -> 'snd
