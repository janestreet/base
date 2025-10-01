@@ portable

[@@@warning "-incompatible-with-upstream"]

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

type ('a : k) t = ('a Constructors.t[@kind k])
[@@deriving compare ~localize, equal ~localize, sexp ~stackify]
[@@kind
  k
  = ( float64
    , bits32
    , bits64
    , word
    , immediate
    , immediate64
    , value & float64
    , value & bits32
    , value & bits64
    , value & word
    , value & immediate
    , value & immediate64
    , value & value
    , value & kr1
    , value & kr2
    , value & kr3 )]]

type ('a : value_or_null) t = 'a option =
  | None
  | Some of 'a
[@@deriving
  compare ~localize, equal ~localize, globalize, hash, sexp ~stackify, sexp_grammar]

include Invariant.S1 with type 'a t := 'a t

(** {3 Applicative interface}

    Options form an applicative, where:

    - [return x = Some x]
    - [None <*> x = None]
    - [Some f <*> None = None]
    - [Some f <*> Some x = Some (f x)] *)

include%template
  Applicative.S
  [@kind value_or_null mod maybe_null] [@mode local]
  with type ('a : value_or_null) t := 'a t

(** {3 Monadic interface}

    Options form a monad, where:

    - [return x = Some x]
    - [(None >>= f) = None]
    - [(Some x >>= f) = f x] *)

include%template
  Monad.S
  [@kind value_or_null mod maybe_null] [@mode local]
  with type ('a : value_or_null) t := 'a t

(** {2 Extracting Underlying Values} *)

[%%template:
[@@@mode.default m = (global, local)]
[@@@kind kr1 = (value & value)]
[@@@kind kr2 = (value & value & value)]
[@@@kind kr3 = (value & value & value & value)]

[@@@kind.default
  k
  = ( value_or_null
    , float64
    , bits32
    , bits64
    , word
    , immediate
    , immediate64
    , value & float64
    , value & bits32
    , value & bits64
    , value & word
    , value & immediate
    , value & immediate64
    , value & value
    , value & kr1
    , value & kr2
    , value & kr3 )]

(** Extracts the underlying value if present, otherwise returns [default]. *)
val value : ('a t[@kind k]) @ m -> default:'a @ m -> 'a @ m

(** Extracts the underlying value, or raises if there is no value present. The raised
    error can be augmented using the [~error] and [~message] optional arguments. If
    neither is provided, the raised error will include the provided location. *)
val value_exn
  :  here:[%call_pos]
  -> ?error:Error.t
  -> ?message:string
  -> ('a t[@kind k]) @ m
  -> 'a @ m

(** Extracts the underlying value if present, otherwise executes and returns the result of
    [default]. [default] is only executed if the underlying value is absent. *)
val value_or_thunk : ('a t[@kind k]) @ m -> default:(unit -> 'a @ m) @ local -> 'a @ m

val iter : ('a t[@kind k]) @ m -> f:('a @ m -> unit) @ local -> unit

[@@@kind ki = k]

[@@@kind.default
  ko
  = ( value_or_null
    , float64
    , bits32
    , bits64
    , word
    , immediate
    , immediate64
    , value & float64
    , value & bits32
    , value & bits64
    , value & word
    , value & immediate
    , value & immediate64
    , value & value
    , value & kr1
    , value & kr2
    , value & kr3 )]

(** Extracts the underlying value and applies [f] to it if present, otherwise returns
    [default]. *)
val value_map
  : ('a : ki) ('b : ko).
  ('a t[@kind ki]) @ m -> default:'b @ m -> f:('a @ m -> 'b @ m) @ local -> 'b @ m

val map
  : ('a : ki) ('b : ko).
  ('a t[@kind ki]) @ m -> f:('a @ m -> 'b @ m) @ local -> ('b t[@kind ko]) @ m

val bind
  : ('a : ki) ('b : ko).
  ('a t[@kind ki]) @ m
  -> f:('a @ m -> ('b t[@kind ko]) @ m) @ local
  -> ('b t[@kind ko]) @ m]

(** On [None], returns [init]. On [Some x], returns [f init x]. *)
val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) @ local -> 'acc

(** Checks whether the provided element is there, using [equal]. *)
val mem : 'a t -> 'a -> equal:('a -> 'a -> bool) @ local -> bool

val length : 'a t -> int

(** On [None], returns [false]. On [Some x], returns [f x]. *)
val exists : 'a t -> f:('a -> bool) @ local -> bool

(** On [None], returns [true]. On [Some x], returns [f x]. *)
val for_all : 'a t -> f:('a -> bool) @ local -> bool

(** [find t ~f] returns [t] if [t = Some x] and [f x = true]; otherwise, [find] returns
    [None]. *)
val find : 'a t -> f:('a -> bool) @ local -> 'a option

(** On [None], returns [None]. On [Some x], returns [f x]. *)
val find_map : 'a t -> f:('a -> 'b option) @ local -> 'b option

val%template to_list : 'a t @ m -> 'a list @ m [@@mode m = (global, local)]

val to_array : 'a t -> 'a array

(** [call x f] runs an optional function [~f] on the argument. *)
val call : 'a -> f:('a -> unit) t @ local -> unit

(** [merge a b ~f] merges together the values from [a] and [b] using [f]. If both [a] and
    [b] are [None], returns [None]. If only one is [Some], returns that one, and if both
    are [Some], returns [Some] of the result of applying [f] to the contents of [a] and
    [b]. *)
val merge : 'a t -> 'a t -> f:('a -> 'a -> 'a) @ local -> 'a t

val filter : 'a t -> f:('a -> bool) @ local -> 'a t

(** {2 Constructors} *)

(** [try_with f] returns [Some x] if [f] returns [x] and [None] if [f] raises an
    exception. See [Result.try_with] if you'd like to know which exception. *)
val try_with : (unit -> 'a) @ local -> 'a t

(** [try_with_join f] returns the optional value returned by [f] if it exits normally, and
    [None] if [f] raises an exception. *)
val try_with_join : (unit -> 'a t) @ local -> 'a t

[%%template:
[@@@mode.default m = (global, local)]

(** Wraps the [Some] constructor as a function. *)
val some : 'a @ m -> 'a t @ m

val return : 'a @ m -> 'a t @ m

(** [first_some t1 t2] returns [t1] if it has an underlying value, or [t2] otherwise. *)
val first_some : 'a t @ m -> 'a t @ m -> 'a t @ m

(** [first_some_thunk a b] is like [first_some], but it only computes [b ()] if [a] is
    [None] *)
val first_some_thunk : 'a t @ m -> (unit -> 'a t @ m) @ local -> 'a t @ m

(** [some_if b x] converts a value [x] to [Some x] if [b], and [None] otherwise. *)
val some_if : bool -> 'a @ m -> 'a t @ m

(** Like [some_if], but only computes [x ()] if [b] is true. *)
val some_if_thunk : bool -> (unit -> 'a @ m) @ local -> 'a t @ m]

(** {2 Predicates} *)

[%%template:
[@@@kind kr1 = (value & value)]
[@@@kind kr2 = (value & value & value)]
[@@@kind kr3 = (value & value & value & value)]

[@@@kind.default
  k
  = ( value_or_null
    , float64
    , bits32
    , bits64
    , word
    , immediate
    , immediate64
    , value & float64
    , value & bits32
    , value & bits64
    , value & word
    , value & immediate
    , value & immediate64
    , value & value
    , value & kr1
    , value & kr2
    , value & kr3 )]

(** [is_none t] returns true iff [t = None]. *)
val is_none : ('a t[@kind k]) @ contended local -> bool

(** [is_some t] returns true iff [t = Some x]. *)
val is_some : ('a t[@kind k]) @ contended local -> bool]

module Local : sig
  module Let_syntax : sig
    val return : 'a @ local -> 'a t @ local

    (** Pronounced "map". Infix form of [map]. *)
    val ( >>| ) : 'a t @ local -> ('a @ local -> 'b @ local) @ local -> 'b t @ local

    (** Pronounced "bind". Infix form of [bind]. *)
    val ( >>= ) : 'a t @ local -> ('a @ local -> 'b t @ local) @ local -> 'b t @ local

    module Let_syntax : sig
      val return : 'a @ local -> 'a t @ local
      val map : 'a t @ local -> f:('a @ local -> 'b @ local) @ local -> 'b t @ local
      val bind : 'a t @ local -> f:('a @ local -> 'b t @ local) @ local -> 'b t @ local
      val both : 'a t @ local -> 'b t @ local -> ('a * 'b) t @ local

      module Open_on_rhs : sig end
    end
  end
end
