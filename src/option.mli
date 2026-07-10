@@ portable

(** The option type indicates whether a meaningful value is present. It is frequently used
    to represent success or failure, using [None] for failure. To be more descriptive
    about why a function failed, see the {!Or_error} module.

    Usage example from a utop session follows. Hash table lookups use the option type to
    indicate success or failure when looking up a key.

    {[
      # let h = Hashtbl.of_alist_exn (module String) [ ("Bar", "Value") ];;
      val h : (string, string) Base.Hashtbl.t = <abstr>
      # (Hashtbl.find h "Foo" : _ option);;
      - : string option = Base.Option.None
      # (Hashtbl.find h "Bar" : _ option);;
      - : string option = Base.Option.Some "Value"
    ]} *)

open! Import

(** {2 Type and Interfaces} *)

module Constructors : module type of Option0

[%%template:
[@@@kind kr1 = (value_or_null & value_or_null)]
[@@@kind kr2 = (value_or_null & kr1)]

[@@@kind_set.define
  all_ks_non_value = (base_non_value, value_or_null & (base_or_null, kr2))]

[@@@kind_set.define all_ks = (all_ks_non_value, value_or_null)]

type ('a : k) t = ('a Constructors.t[@kind k])
[@@deriving compare ~localize, equal ~localize, sexp ~stackify]
[@@kind k = all_ks_non_value]

type ('a : value_or_null) t = 'a option =
  | None
  | Some of 'a

[%%rederive:
  type nonrec ('a : value_or_null) t = 'a t
  [@@deriving
    compare ~localize, equal ~localize, globalize, hash, sexp ~stackify, sexp_grammar]]

include Invariant.S1 with type 'a t := 'a t

(** {3 Applicative interface}

    Options form an applicative, where:

    - [return x = Some x]
    - [None <*> x = None]
    - [Some f <*> None = None]
    - [Some f <*> Some x = Some (f x)] *)

include
  Applicative.S
  [@kind value_or_null mod maybe_null] [@mode local]
  with type ('a : value_or_null) t := 'a t

(** {3 Monadic interface}

    Options form a monad, where:

    - [return x = Some x]
    - [(None >>= f) = None]
    - [(Some x >>= f) = f x] *)

include
  Monad.S
  [@kind value_or_null mod maybe_null] [@mode local]
  with type ('a : value_or_null) t := 'a t

(** {2 Extracting Underlying Values} *)

include sig
  [@@@mode.default m = (global, local)]
  [@@@kind.default k = all_ks]

  val return : ('a : k). 'a @ m -> ('a t[@kind k]) @ m [@@zero_alloc_if_local m]

  (** Extracts the underlying value if present, otherwise returns [default]. *)
  val value : ('a : k). ('a t[@kind k]) @ m -> default:'a @ m -> 'a @ m
  [@@zero_alloc_if_local m]

  (** Extracts the underlying value, or raises if there is no value present. The raised
      error can be augmented using the [~error] and [~message] optional arguments. If
      neither is provided, the raised error will include the provided location. *)
  val value_exn
    : ('a : k).
    here:[%call_pos] -> ?error:Error.t -> ?message:string -> ('a t[@kind k]) @ m -> 'a @ m
  [@@zero_alloc_if_local m]

  (** Extracts the underlying value if present, otherwise executes and returns the result
      of [default]. [default] is only executed if the underlying value is absent. *)
  val value_or_thunk
    : ('a : k).
    ('a t[@kind k]) @ m -> default:(unit -> 'a @ m) @ local once -> 'a @ m

  val iter : ('a : k). ('a t[@kind k]) @ m -> f:('a @ m -> unit) @ local once -> unit

  [@@@kind ki = k]
  [@@@kind.default ko = all_ks]

  (** Extracts the underlying value and applies [f] to it if present, otherwise returns
      [default]. *)
  val value_map
    : ('a : ki) ('b : ko).
    ('a t[@kind ki]) @ m -> default:'b @ m -> f:('a @ m -> 'b @ m) @ local once -> 'b @ m

  val map
    : ('a : ki) ('b : ko).
    ('a t[@kind ki]) @ m -> f:('a @ m -> 'b @ m) @ local once -> ('b t[@kind ko]) @ m

  val bind
    : ('a : ki) ('b : ko).
    ('a t[@kind ki]) @ m
    -> f:('a @ m -> ('b t[@kind ko]) @ m) @ local once
    -> ('b t[@kind ko]) @ m
end

(** On [None], returns [init]. On [Some x], returns [f init x]. *)
val fold
  : ('a : value_or_null) ('acc : value_or_null).
  'a t -> init:'acc -> f:('acc -> 'a -> 'acc) @ local once -> 'acc

(** Checks whether the provided element is there, using [equal]. *)
val mem
  : ('a : value_or_null).
  'a t -> 'a -> equal:('a -> 'a -> bool) @ local once -> bool

val length : ('a : value_or_null). 'a t -> int

(** On [None], returns [false]. On [Some x], returns [f x]. *)
val exists : ('a : value_or_null). 'a t -> f:('a -> bool) @ local once -> bool

(** On [None], returns [true]. On [Some x], returns [f x]. *)
val for_all : ('a : value_or_null). 'a t -> f:('a -> bool) @ local once -> bool

(** [find t ~f] returns [t] if [t = Some x] and [f x = true]; otherwise, [find] returns
    [None]. *)
val find : ('a : value_or_null). 'a t -> f:('a -> bool) @ local once -> 'a option

(** On [None], returns [None]. On [Some x], returns [f x]. *)
val find_map
  : ('a : value_or_null) ('b : value_or_null).
  'a t -> f:('a -> 'b option) @ local once -> 'b option

val to_list : ('a : value_or_null). 'a t @ m -> 'a list @ m [@@mode m = (global, local)]
val to_array : ('a : value_or_null mod separable). 'a t -> 'a array
val to_iarray : ('a : value_or_null mod separable). 'a t -> 'a iarray

(** [call x f] runs an optional function [~f] on the argument. *)
val call : ('a : value_or_null). 'a -> f:('a -> unit) t @ local once -> unit

(** [merge a b ~f] merges together the values from [a] and [b] using [f]. If both [a] and
    [b] are [None], returns [None]. If only one is [Some], returns that one, and if both
    are [Some], returns [Some] of the result of applying [f] to the contents of [a] and
    [b]. *)
val merge : ('a : value_or_null). 'a t -> 'a t -> f:('a -> 'a -> 'a) @ local once -> 'a t

(** [transpose_result x] transposes an [option] of a [result] into a [result] of an
    [option].

    Concretely, [None] maps to [Ok None], [Some (Ok x)] maps to [Ok (Some x)], and
    [Some (Error e)] maps to [Error e].

    Inverse of {!Result.transpose_opt}. *)
val transpose_result
  : ('ok : value_or_null) ('err : value_or_null).
  ('ok, 'err) Result.t t @ m -> ('ok t, 'err) Result.t @ m
[@@alloc __ @ m = (stack_local, heap_global)]

val filter : ('a : value_or_null). 'a t -> f:('a -> bool) @ local once -> 'a t

(** {2 Constructors} *)

(** [try_with f] returns [Some x] if [f] returns [x] and [None] if [f] raises an
    exception. See [Result.try_with] if you'd like to know which exception. *)
val try_with : ('a : value_or_null). (unit -> 'a) @ local once -> 'a t

(** [try_with_join f] returns the optional value returned by [f] if it exits normally, and
    [None] if [f] raises an exception. *)
val try_with_join : ('a : value_or_null). (unit -> 'a t) @ local once -> 'a t

include sig
  [@@@mode.default m = (global, local)]

  (** Wraps the [Some] constructor as a function. *)
  val some : ('a : value_or_null). 'a @ m -> 'a t @ m

  (** [first_some t1 t2] returns [t1] if it has an underlying value, or [t2] otherwise. *)
  val first_some : ('a : value_or_null). 'a t @ m -> 'a t @ m -> 'a t @ m

  (** [first_some_thunk a b] is like [first_some], but it only computes [b ()] if [a] is
      [None] *)
  val first_some_thunk
    : ('a : value_or_null).
    'a t @ m -> (unit -> 'a t @ m) @ local once -> 'a t @ m

  (** [some_if b x] converts a value [x] to [Some x] if [b], and [None] otherwise. *)
  val some_if : ('a : value_or_null). bool -> 'a @ m -> 'a t @ m

  (** Like [some_if], but only computes [x ()] if [b] is true. *)
  val some_if_thunk
    : ('a : value_or_null).
    bool -> (unit -> 'a @ m) @ local once -> 'a t @ m
end

(** {2 Predicates} *)

include sig
  [@@@kind.default k = all_ks]

  (** [is_none t] returns true iff [t = None]. *)
  val is_none : ('a : k). ('a t[@kind k]) @ contended local -> bool

  (** [is_some t] returns true iff [t = Some x]. *)
  val is_some : ('a : k). ('a t[@kind k]) @ contended local -> bool
end

module Local : sig
  module Let_syntax : sig
    val return : ('a : value_or_null). 'a @ local -> 'a t @ local

    (** Pronounced "map". Infix form of [map]. *)
    val ( >>| )
      : ('a : value_or_null) ('b : value_or_null).
      'a t @ local -> ('a @ local -> 'b @ local) @ local once -> 'b t @ local

    (** Pronounced "bind". Infix form of [bind]. *)
    val ( >>= )
      : ('a : value_or_null) ('b : value_or_null).
      'a t @ local -> ('a @ local -> 'b t @ local) @ local once -> 'b t @ local

    module Let_syntax : sig
      val return : ('a : value_or_null). 'a @ local -> 'a t @ local

      val map
        : ('a : value_or_null) ('b : value_or_null).
        'a t @ local -> f:('a @ local -> 'b @ local) @ local once -> 'b t @ local

      val bind
        : ('a : value_or_null) ('b : value_or_null).
        'a t @ local -> f:('a @ local -> 'b t @ local) @ local once -> 'b t @ local

      val both
        : ('a : value_or_null) ('b : value_or_null).
        'a t @ local -> 'b t @ local -> ('a * 'b) t @ local

      module Open_on_rhs : sig end
    end
  end
end]
