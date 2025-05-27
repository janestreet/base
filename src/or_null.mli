(** ['a or_null], or the unboxed option type, represents a value of type ['a] or its
    absence. Similar to [option], it has two constructors, [Null] and [This a].

    In OxCaml, [Null] is represented in memory as a null pointer, while [This a] has the
    same representation as [a]. Since [This Null] would be indistinguishable from [Null],
    it is forbidden: the kind system prevents nested ['a or_null or_null] types.

    The ['a] in ['a or_null] must have a [non_null] kind, like vanilla OCaml types, while
    ['a or_null] has a new kind called [immediate_or_null with 'a], inheriting everything
    but the nullability from the kind of ['a]. In the most general case, the kind of
    ['a or_null] is called [value_or_null].

    This module largely mirrors {!Option}. One major difference is that ['a or_null],
    despite admitting [bind], is not a [Monad] (or even an [Applicative]), since it's not
    an endofunctor taking types to types of the same kind. *)

open! Import

(** {2 Type and Interfaces} *)

type 'a t = 'a or_null =
  | Null
  | This of 'a

[%%rederive:
  type nonrec 'a t = 'a t
  [@@deriving compare ~localize, equal ~localize, globalize, sexp ~localize]]

(** {3 Accessors} *)

[%%template:
[@@@mode.default m = (global, local)]

(** Extracts the underlying value if present, otherwise returns [default]. *)
val value : 'a t -> default:'a -> 'a

(** Extracts the underlying value, or raises if there is no value present. The raised
    error will include the provided location. *)
val value_exn : ?here:Stdlib.Lexing.position -> 'a t -> 'a

(** Extracts the underlying value if present, otherwise executes and returns the result of
    [default]. [default] is only executed if the underlying value is absent. *)
val value_or_thunk : 'a t -> default:(unit -> 'a) -> 'a

val iter : 'a t -> f:('a -> unit) -> unit

(** Extracts the underlying value and applies [f] to it if present, otherwise returns
    [default]. *)
val value_map : 'a t -> default:'b -> f:('a -> 'b) -> 'b

val map : 'a t -> f:('a -> 'b) -> 'b t
val both : 'a t -> 'b t -> ('a * 'b) t
val bind : 'a t -> f:('a -> 'b t) -> 'b t
val to_list : 'a t -> 'a list

val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
[@@mode m = m, n = (global, local)]

val mem : 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool

(** On [Null], returns [false]. On [This x], returns [f x]. *)
val exists : 'a t -> f:('a -> bool) -> bool

(** On [Null], returns [true]. On [This x], returns [f x]. *)
val for_all : 'a t -> f:('a -> bool) -> bool

(** [find t ~f] returns [t] if [t = This x] and [f x = true]; otherwise, [find] returns
    [Null]. *)
val find : 'a t -> f:('a -> bool) -> 'a t

val to_array : 'a t -> 'a array

(** [call x f] runs an optional function [~f] on the argument. *)
val call : 'a -> f:('a -> unit) t -> unit

(** [merge a b ~f] merges together the values from [a] and [b] using [f]. If both [a] and
    [b] are [None], returns [None]. If only one is [Some], returns that one, and if both
    are [Some], returns [Some] of the result of applying [f] to the contents of [a] and
    [b]. *)
val merge : 'a t -> 'a t -> f:('a -> 'a -> 'a) -> 'a t]

val length : 'a t -> int

(** {4 Constructors} *)

[%%template:
[@@@mode.default m = (global, local)]

(** [try_with f] returns [This x] if [f] returns [x] and [Null] if [f] raises an
    exception. See [Result.try_with] if you'd like to know which exception. *)
val try_with : (unit -> 'a) -> 'a t

(** [try_with_join f] returns the optional value returned by [f] if it exits normally, and
    [Null] if [f] raises an exception. *)
val try_with_join : (unit -> 'a t) -> 'a t

(** [this x = This x]. *)
val this : 'a -> 'a t

(** [first_this t1 t2] returns [t1] if it has an underlying value, or [t2] otherwise. *)
val first_this : 'a t -> 'a t -> 'a t

(** [first_this_thunk a b] is like [first_this], but it only computes [b ()] if [a] is
    [None] *)
val first_this_thunk : 'a t -> (unit -> 'a t) -> 'a t

(** [this_if b x] converts a value [x] to [This x] if [b], and [Null] otherwise. *)
val this_if : bool -> 'a -> 'a t

(** Like [this_if], but only computes [x ()] if [b] is true. *)
val this_if_thunk : bool -> (unit -> 'a) -> 'a t

(** [to_option (This x) = Some x] and [to_option Null = None]. *)
val to_option : 'a t -> 'a option

(** [of_option (Some x) = This x] and [of_option None = Null]. *)
val of_option : 'a option -> 'a t]

(** [map_to_option t ~f] is the same as [map t ~f |> to_option] *)
val%template map_to_option : 'a t -> f:('a -> 'b) -> 'b option
[@@alloc __ @ m = (heap_global, stack_local)]

(** {5 Predicates} *)

(** [is_null t] returns true iff [t = Null]. *)
val is_null : 'a t -> bool

(** [is_this t] returns true iff [t = This x]. *)
val is_this : 'a t -> bool

(** {6 Let syntax} *)

module Let_syntax : sig
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  module Let_syntax : sig
    val return : 'a -> 'a t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t

    module Open_on_rhs : sig end
  end
end
