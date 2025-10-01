(** Various combinators for functions. *)

open! Import

(** A "pipe" operator. [x |> f] is equivalent to [f x].

    See {{:https://github.com/janestreet/ppx_pipebang} ppx_pipebang} for further details. *)
external ( |> ) : 'a 'b. 'a -> (('a -> 'b)[@local_opt]) -> 'b = "%revapply"
[@@layout_poly]

(** Produces a function that just returns its first argument. *)
val const : 'a -> _ -> 'a

(** Ignores its argument and returns [()]. *)
external ignore : 'a. ('a[@local_opt]) -> unit = "%ignore"
[@@layout_poly]

(** Negates a boolean function. *)
val non : ('a -> bool) -> 'a -> bool

(** [forever f] runs [f ()] until it throws an exception and returns the exception. This
    function is useful for read_line loops, etc. *)
val forever : (unit -> unit) -> exn

(** [apply_n_times ~n f x] is the [n]-fold application of [f] to [x]. *)
val apply_n_times : n:int -> ('a -> 'a) -> 'a -> 'a

(** The identity function.

    See also: {!Sys.opaque_identity}. *)
external id : 'a. ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
[@@layout_poly]

(** [compose f g x] is [f (g x)]. *)
val%template compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
[@@modality p = (portable, nonportable)]

(** Reverses the order of arguments for a binary function. *)
val%template flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
[@@modality p = (portable, nonportable)]
