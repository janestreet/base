(** The option type indicates whether a meaningful value is present. It is frequently used
    to represent success or failure, using [None] for failure. To be more descriptive
    about why a function failed, see the {!Or_error} module.

    Usage example from a utop session follows. Hash table lookups use the option type to
    indicate success or failure when looking up a key.

    {v
  # let h = Hashtbl.of_alist (module String) [ ("Bar", "Value") ];;
  val h : (string, string) Hashtbl.t = <abstr>;;
  - : (string, string) Hashtbl.t = <abstr>
  # Hashtbl.find h "Foo";;
  - : string option = None
  # Hashtbl.find h "Bar";;
  - : string option = Some "Value"
    v} *)

open! Import

(** {2 Type and Interfaces} *)

type 'a t = 'a option =
  | None
  | Some of 'a
[@@deriving_inline compare ~localize, equal ~localize, globalize, hash, sexp_grammar]

include Ppx_compare_lib.Comparable.S1 with type 'a t := 'a t
include Ppx_compare_lib.Comparable.S_local1 with type 'a t := 'a t
include Ppx_compare_lib.Equal.S1 with type 'a t := 'a t
include Ppx_compare_lib.Equal.S_local1 with type 'a t := 'a t

val globalize : ('a -> 'a) -> 'a t -> 'a t

include Ppx_hash_lib.Hashable.S1 with type 'a t := 'a t

val t_sexp_grammar : 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t

[@@@end]

include Invariant.S1 with type 'a t := 'a t
include Sexpable.S1 with type 'a t := 'a t

(** {3 Applicative interface}

    Options form an applicative, where:

    {ul
    {- [return x = Some x] }
    {- [None <*> x = None] }
    {- [Some f <*> None = None] }
    {- [Some f <*> Some x = Some (f x)] }}
*)

include Applicative.S_local with type 'a t := 'a t

(** {3 Monadic interface}

    Options form a monad, where:

    {ul
    {- [return x = Some x]}
    {- [(None >>= f) = None]}
    {- [(Some x >>= f) = f x]}}
*)

include Monad.S_local with type 'a t := 'a t

(** {2 Extracting Underlying Values} *)

(** Extracts the underlying value if present, otherwise returns [default]. *)
val value : 'a t -> default:'a -> 'a

(** Like [value], but over a local [t]. *)
val value_local : 'a t -> default:'a -> 'a

(** Extracts the underlying value, or raises if there is no value present. The
    raised error can be augmented using the [~error] and [~message] optional arguments.
    If neither is provided, the raised error will include the provided location. *)
val value_exn
  :  ?here:Stdlib.Lexing.position
  -> ?error:Error.t
  -> ?message:string
  -> 'a t
  -> 'a

(** Like [value_exn], but over a local [t]. *)
val value_local_exn
  :  ?here:Stdlib.Lexing.position
  -> ?error:Error.t
  -> ?message:string
  -> 'a t
  -> 'a

(** Extracts the underlying value and applies [f] to it if present, otherwise returns
    [default]. *)
val value_map : 'a t -> default:'b -> f:('a -> 'b) -> 'b

(** Like [value_map], but over a local [t]. *)
val value_map_local : 'a t -> default:'b -> f:('a -> 'b) -> 'b

(** Extracts the underlying value if present, otherwise executes and returns the result of
    [default]. [default] is only executed if the underlying value is absent. *)
val value_or_thunk : 'a t -> default:(unit -> 'a) -> 'a

(** Like [value_or_thunk], but over a local [t]. *)
val value_or_thunk_local : 'a t -> default:(unit -> 'a) -> 'a

(** Like [map], but over a local [t]. *)
val map_local : 'a t -> f:('a -> 'b) -> 'b t

(** On [None], returns [init]. On [Some x], returns [f init x]. *)
val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc

(** Checks whether the provided element is there, using [equal]. *)
val mem : 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool

val length : 'a t -> int
val iter : 'a t -> f:('a -> unit) -> unit

(** Like [iter], but over a local [t]. *)
val iter_local : 'a t -> f:('a -> unit) -> unit

(** On [None], returns [false]. On [Some x], returns [f x]. *)
val exists : 'a t -> f:('a -> bool) -> bool

(** On [None], returns [true]. On [Some x], returns [f x]. *)
val for_all : 'a t -> f:('a -> bool) -> bool

(** [find t ~f] returns [t] if [t = Some x] and [f x = true]; otherwise, [find] returns
    [None]. *)
val find : 'a t -> f:('a -> bool) -> 'a option

(** On [None], returns [None]. On [Some x], returns [f x]. *)
val find_map : 'a t -> f:('a -> 'b option) -> 'b option

val to_list : 'a t -> 'a list
val to_list_local : 'a t -> 'a list
val to_array : 'a t -> 'a array

(** [call x f] runs an optional function [~f] on the argument. *)
val call : 'a -> f:('a -> unit) t -> unit

(** [merge a b ~f] merges together the values from [a] and [b] using [f].  If both [a] and
    [b] are [None], returns [None].  If only one is [Some], returns that one, and if both
    are [Some], returns [Some] of the result of applying [f] to the contents of [a] and
    [b]. *)
val merge : 'a t -> 'a t -> f:('a -> 'a -> 'a) -> 'a t

val filter : 'a t -> f:('a -> bool) -> 'a t

(** {2 Constructors} *)

(** [try_with f] returns [Some x] if [f] returns [x] and [None] if [f] raises an
    exception.  See [Result.try_with] if you'd like to know which exception. *)
val try_with : (unit -> 'a) -> 'a t

(** [try_with_join f] returns the optional value returned by [f] if it exits normally, and
    [None] if [f] raises an exception. *)
val try_with_join : (unit -> 'a t) -> 'a t

(** Wraps the [Some] constructor as a function. *)
val some : 'a -> 'a t

(** Like [some], but over a local [t]. *)
val some_local : 'a -> 'a t

(** [first_some t1 t2] returns [t1] if it has an underlying value, or [t2]
    otherwise. *)
val first_some : 'a t -> 'a t -> 'a t

(** Like [first_some], but over local [t]s. *)
val first_some_local : 'a t -> 'a t -> 'a t

(** [first_some_thunk a b] is like [first_some], but it only computes [b ()] if [a] is
    [None] *)
val first_some_thunk : 'a t -> (unit -> 'a t) -> 'a t

(** Like [first_some_thunk], but over local [t]s. *)
val first_some_thunk_local : 'a t -> (unit -> 'a t) -> 'a t

(** [some_if b x] converts a value [x] to [Some x] if [b], and [None]
    otherwise. *)
val some_if : bool -> 'a -> 'a t

(** Like [some_if], but over a local [t]. *)
val some_if_local : bool -> 'a -> 'a t

(** Like [some_if], but only computes [x ()] if [b] is true. *)
val some_if_thunk : bool -> (unit -> 'a) -> 'a t

(** Like [some_if_thunk], but over a local [t] *)
val some_if_thunk_local : bool -> (unit -> 'a) -> 'a t

(** {2 Predicates} *)

(** [is_none t] returns true iff [t = None]. *)
val is_none : 'a t -> bool

(** [is_some t] returns true iff [t = Some x]. *)
val is_some : 'a t -> bool
