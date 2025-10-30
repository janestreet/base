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
[@@@kind kr1 = (value & value)]
[@@@kind kr2 = (value & value & value)]
[@@@kind kr3 = (value & value & value & value)]

type 'a t = ('a Constructors.t[@kind k])
[@@deriving compare ~localize, equal ~localize, sexp ~stackify]
[@@kind k = (base_non_value, value & (base_with_imm, kr1, kr2, kr3))]

type 'a t = 'a option =
  | None
  | Some of 'a

type 'a t = 'a t
[@@kind __ = (immediate, immediate64, value mod external_, value mod external64)]

[%%template:
[@@@kind
  k = (value_or_null, immediate, immediate64, value mod external_, value mod external64)]

[%%rederive:
  type nonrec 'a t = ('a t[@kind k])
  [@@kind k]
  [@@deriving
    compare ~localize, equal ~localize, globalize, hash, sexp ~stackify, sexp_grammar]]]

include Invariant.S1 with type 'a t := 'a t

(** {3 Applicative interface}

    Options form an applicative, where:

    - [return x = Some x]
    - [None <*> x = None]
    - [Some f <*> None = None]
    - [Some f <*> Some x = Some (f x)] *)

include%template
  Applicative.S [@kind value_or_null mod maybe_null] [@mode local] with type 'a t := 'a t

(** {3 Monadic interface}

    Options form a monad, where:

    - [return x = Some x]
    - [(None >>= f) = None]
    - [(Some x >>= f) = f x] *)

include%template
  Monad.S [@kind value_or_null mod maybe_null] [@mode local] with type 'a t := 'a t

(** {2 Extracting Underlying Values} *)

[%%template:
[@@@mode.default m = (global, local)]
[@@@kind.default k = (base_or_null_with_imm, value & (base_with_imm, kr1, kr2, kr3))]

(** Extracts the underlying value if present, otherwise returns [default]. *)
val value : 'a. ('a t[@kind k]) -> default:'a -> 'a

(** Extracts the underlying value, or raises if there is no value present. The raised
    error can be augmented using the [~error] and [~message] optional arguments. If
    neither is provided, the raised error will include the provided location. *)
val value_exn
  : 'a.
  ?here:Stdlib.Lexing.position
  -> ?error:Error.t
  -> ?message:string
  -> ('a t[@kind k])
  -> 'a

(** Extracts the underlying value if present, otherwise executes and returns the result of
    [default]. [default] is only executed if the underlying value is absent. *)
val value_or_thunk : 'a. ('a t[@kind k]) -> default:(unit -> 'a) -> 'a

val iter : 'a. ('a t[@kind k]) -> f:('a -> unit) -> unit

[@@@kind ki = k]
[@@@kind.default ko = (base_or_null_with_imm, value & (base_with_imm, kr1, kr2, kr3))]

(** Extracts the underlying value and applies [f] to it if present, otherwise returns
    [default]. *)
val value_map : 'a 'b. ('a t[@kind ki]) -> default:'b -> f:('a -> 'b) -> 'b

val map : 'a 'b. ('a t[@kind ki]) -> f:('a -> 'b) -> ('b t[@kind ko])
val bind : 'a 'b. ('a t[@kind ki]) -> f:('a -> ('b t[@kind ko])) -> ('b t[@kind ko])]

(** On [None], returns [init]. On [Some x], returns [f init x]. *)
val fold : 'a 'acc. 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc

(** Checks whether the provided element is there, using [equal]. *)
val mem : 'a. 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool

val length : 'a. 'a t -> int

(** On [None], returns [false]. On [Some x], returns [f x]. *)
val exists : 'a. 'a t -> f:('a -> bool) -> bool

(** On [None], returns [true]. On [Some x], returns [f x]. *)
val for_all : 'a. 'a t -> f:('a -> bool) -> bool

(** [find t ~f] returns [t] if [t = Some x] and [f x = true]; otherwise, [find] returns
    [None]. *)
val find : 'a. 'a t -> f:('a -> bool) -> 'a option

(** On [None], returns [None]. On [Some x], returns [f x]. *)
val find_map : 'a 'b. 'a t -> f:('a -> 'b option) -> 'b option

val%template to_list : 'a. 'a t -> 'a list [@@mode m = (global, local)]

val to_array : 'a. 'a t -> 'a array

(** [call x f] runs an optional function [~f] on the argument. *)
val call : 'a. 'a -> f:('a -> unit) t -> unit

(** [merge a b ~f] merges together the values from [a] and [b] using [f]. If both [a] and
    [b] are [None], returns [None]. If only one is [Some], returns that one, and if both
    are [Some], returns [Some] of the result of applying [f] to the contents of [a] and
    [b]. *)
val merge : 'a. 'a t -> 'a t -> f:('a -> 'a -> 'a) -> 'a t

val filter : 'a. 'a t -> f:('a -> bool) -> 'a t

(** {2 Constructors} *)

(** [try_with f] returns [Some x] if [f] returns [x] and [None] if [f] raises an
    exception. See [Result.try_with] if you'd like to know which exception. *)
val try_with : 'a. (unit -> 'a) -> 'a t

(** [try_with_join f] returns the optional value returned by [f] if it exits normally, and
    [None] if [f] raises an exception. *)
val try_with_join : 'a. (unit -> 'a t) -> 'a t

[%%template:
[@@@mode.default m = (global, local)]

(** Wraps the [Some] constructor as a function. *)
val some : 'a. 'a -> 'a t

val return : 'a. 'a -> 'a t

(** [first_some t1 t2] returns [t1] if it has an underlying value, or [t2] otherwise. *)
val first_some : 'a. 'a t -> 'a t -> 'a t

(** [first_some_thunk a b] is like [first_some], but it only computes [b ()] if [a] is
    [None] *)
val first_some_thunk : 'a. 'a t -> (unit -> 'a t) -> 'a t

(** [some_if b x] converts a value [x] to [Some x] if [b], and [None] otherwise. *)
val some_if : 'a. bool -> 'a -> 'a t

(** Like [some_if], but only computes [x ()] if [b] is true. *)
val some_if_thunk : 'a. bool -> (unit -> 'a) -> 'a t]

(** {2 Predicates} *)

[%%template:
[@@@kind.default k = (base_or_null_with_imm, value & (base_with_imm, kr1, kr2, kr3))]

(** [is_none t] returns true iff [t = None]. *)
val is_none : 'a. ('a t[@kind k]) -> bool

(** [is_some t] returns true iff [t = Some x]. *)
val is_some : 'a. ('a t[@kind k]) -> bool]]

module Local : sig
  module Let_syntax : sig
    val return : 'a. 'a -> 'a t

    (** Pronounced "map". Infix form of [map]. *)
    val ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t

    (** Pronounced "bind". Infix form of [bind]. *)
    val ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t

    module Let_syntax : sig
      val return : 'a. 'a -> 'a t
      val map : 'a 'b. 'a t -> f:('a -> 'b) -> 'b t
      val bind : 'a 'b. 'a t -> f:('a -> 'b t) -> 'b t
      val both : 'a 'b. 'a t -> 'b t -> ('a * 'b) t

      module Open_on_rhs : sig end
    end
  end
end
