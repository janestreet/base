@@ portable

(** ['a or_null], or the unboxed option type, represents a value of type ['a] or its
    absence. Similar to [option], it has two constructors, [Null] and [This a].

    In OxCaml, [Null] is represented in memory as a null pointer, while [This a] has the
    same representation as [a]. Since [This Null] would be indistinguishable from [Null],
    it is forbidden: the kind system prevents nested ['a or_null or_null] types.

    The ['a] in ['a or_null] must have a [non_null] kind, like vanilla OCaml types, while
    ['a or_null] has a new kind called [value_or_null mod everything with 'a], inheriting
    everything but the nullability from the kind of ['a]. In the most general case, the
    kind of ['a or_null] is called [value_or_null].

    This module largely mirrors {!Option}. One major difference is that ['a or_null],
    despite admitting [bind], is not a [Monad] (or even an [Applicative]), since it's not
    an endofunctor taking types to types of the same kind. *)

open! Import

(** {2 Type and Interfaces} *)

type 'a t = 'a or_null [@@or_null_reexport]

[%%rederive:
  type nonrec ('a : value mod non_null) t : value_or_null mod everything with 'a = 'a t
  [@@deriving compare ~localize, equal ~localize, globalize, hash, sexp ~stackify]]

(** {3 Accessors} *)

[%%template:
[@@@mode.default m = (global, local)]

(** Extracts the underlying value if present, otherwise returns [default]. *)
val value : 'a t @ m -> default:'a @ m -> 'a @ m
[@@zero_alloc]

(** Extracts the underlying value, or raises if there is no value present. The raised
    error will include the provided location. *)
val value_exn : here:[%call_pos] -> 'a t @ m -> 'a @ m
[@@zero_alloc]

(** Unsafely casts ['a t] to ['a]. Equivalent to [Obj.magic]. *)
val unsafe_value : 'a t @ m -> 'a @ m

(** Extracts the underlying value if present, otherwise executes and returns the result of
    [default]. [default] is only executed if the underlying value is absent. *)
val value_or_thunk : 'a t @ m -> default:(unit -> 'a @ m) @ local once -> 'a @ m

val iter : 'a t @ m -> f:('a @ m -> unit) @ local once -> unit

(** Extracts the underlying value and applies [f] to it if present, otherwise returns
    [default]. *)
val value_map : 'a t @ m -> default:'b @ m -> f:('a @ m -> 'b @ m) @ local once -> 'b @ m

val map : 'a t @ m -> f:('a @ m -> 'b @ m) @ local once -> 'b t @ m
val both : 'a t @ m -> 'b t @ m -> ('a * 'b) t @ m
val bind : 'a t @ m -> f:('a @ m -> 'b t @ m) @ local once -> 'b t @ m
val to_list : 'a t @ m -> 'a list @ m [@@zero_alloc_if_local m]

val fold
  :  'a t @ m
  -> init:'acc @ n
  -> f:('acc @ n -> 'a @ m -> 'acc @ n) @ local once
  -> 'acc @ n
[@@mode m = m, n = (global, local)]

val mem : 'a t @ m -> 'a @ m -> equal:('a @ m -> 'a @ m -> bool) @ local once -> bool

(** On [Null], returns [false]. On [This x], returns [f x]. *)
val exists : 'a t @ m -> f:('a @ m -> bool) @ local once -> bool

(** On [Null], returns [true]. On [This x], returns [f x]. *)
val for_all : 'a t @ m -> f:('a @ m -> bool) @ local once -> bool

(** [find t ~f] returns [t] if [t = This x] and [f x = true]; otherwise, [find] returns
    [Null]. *)
val find : 'a t @ m -> f:('a @ m -> bool) @ local once -> 'a t @ m

val to_array : 'a t -> 'a array @ m

(** [call x f] runs an optional function [~f] on the argument. *)
val call : 'a @ m -> f:('a @ m -> unit) t @ local once -> unit

(** [merge a b ~f] merges together the values from [a] and [b] using [f]. If both [a] and
    [b] are [None], returns [None]. If only one is [Some], returns that one, and if both
    are [Some], returns [Some] of the result of applying [f] to the contents of [a] and
    [b]. *)
val merge
  :  'a t @ m
  -> 'a t @ m
  -> f:('a @ m -> 'a @ m -> 'a @ m) @ local once
  -> 'a t @ m]

val length : 'a t @ local -> int

(** {4 Constructors} *)

[%%template:
[@@@mode.default m = (global, local)]

(** [try_with f] returns [This x] if [f] returns [x] and [Null] if [f] raises an
    exception. See [Result.try_with] if you'd like to know which exception. *)
val try_with : (unit -> 'a @ m) @ local once -> 'a t @ m

(** [try_with_join f] returns the optional value returned by [f] if it exits normally, and
    [Null] if [f] raises an exception. *)
val try_with_join : (unit -> 'a t @ m) @ local once -> 'a t @ m

(** [this x = This x]. *)
val this : 'a @ m -> 'a t @ m [@@zero_alloc]

(** [first_this t1 t2] returns [t1] if it has an underlying value, or [t2] otherwise. *)
val first_this : 'a t @ m -> 'a t @ m -> 'a t @ m
[@@zero_alloc]

(** [first_this_thunk a b] is like [first_this], but it only computes [b ()] if [a] is
    [None] *)
val first_this_thunk : 'a t @ m -> (unit -> 'a t @ m) @ local once -> 'a t @ m

(** [this_if b x] converts a value [x] to [This x] if [b], and [Null] otherwise. *)
val this_if : bool -> 'a @ m -> 'a t @ m
[@@zero_alloc]

(** Like [this_if], but only computes [x ()] if [b] is true. *)
val this_if_thunk : bool -> (unit -> 'a @ m) @ local once -> 'a t @ m

(** [to_option (This x) = Some x] and [to_option Null = None]. *)
val to_option : 'a t @ m -> 'a option @ m
[@@zero_alloc_if_local m]

(** [of_option (Some x) = This x] and [of_option None = Null]. *)
val of_option : 'a option @ m -> 'a t @ m
[@@zero_alloc]]

(** [map_to_option t ~f] is the same as [map t ~f |> to_option] *)
val%template map_to_option
  :  'a t @ m
  -> f:('a @ m -> 'b @ m) @ local once
  -> 'b option @ m
[@@alloc __ @ m = (heap_global, stack_local)]

(** {5 Predicates} *)

(** [is_null t] returns true iff [t = Null]. *)
val is_null : 'a t @ immutable local -> bool
[@@zero_alloc]

(** [is_this t] returns true iff [t = This x]. *)
val is_this : 'a t @ immutable local -> bool
[@@zero_alloc]

(** {6 Let syntax} *)

module Let_syntax : sig
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) @ local once -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) @ local once -> 'b t

  module Let_syntax : sig
    val return : 'a -> 'a t
    val bind : 'a t -> f:('a -> 'b t) @ local once -> 'b t
    val map : 'a t -> f:('a -> 'b) @ local once -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t

    module Open_on_rhs : sig end
  end
end

(** Let syntax for use with locally-allocated values. *)
module Local : sig
  module Let_syntax : sig
    val return : 'a @ local -> 'a t @ local

    module Let_syntax : sig
      val return : 'a @ local -> 'a t @ local
      val map : 'a t @ local -> f:('a @ local -> 'b @ local) @ local once -> 'b t @ local

      val bind
        :  'a t @ local
        -> f:('a @ local -> 'b t @ local) @ local once
        -> 'b t @ local

      val both : 'a t @ local -> 'b t @ local -> ('a * 'b) t @ local

      module Open_on_rhs : sig end
    end
  end
end

(** Be very careful -- [unsafe_value] is particularly unsafe. This should only be used in
    [match%optional] syntax. *)
module Optional_syntax : sig
  module Optional_syntax : sig
    val is_none : _ t @ local -> bool [@@zero_alloc]
    val unsafe_value : 'a t -> 'a [@@zero_alloc]
  end
end
