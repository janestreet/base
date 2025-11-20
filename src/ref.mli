@@ portable

(** Module for the type [ref], mutable indirection cells [r] containing a value of type
    ['a], accessed with [!r] and set by [r := a]. *)

open! Import

type ('a : value_or_null) t = 'a Stdlib.ref = { mutable contents : 'a }
[@@deriving compare ~localize, equal ~localize, globalize, sexp ~stackify, sexp_grammar]

(*_ defined as externals to avoid breaking the inliner *)

external create : ('a : value_or_null). 'a -> ('a t[@local_opt]) = "%makemutable"
external ( ! ) : ('a : value_or_null). ('a t[@local_opt]) -> 'a = "%field0"
external ( := ) : ('a : value_or_null). ('a t[@local_opt]) -> 'a -> unit = "%setfield0"

(** [swap t1 t2] swaps the values in [t1] and [t2]. *)
val swap : ('a : value_or_null). 'a t -> 'a t -> unit

(** [replace t f] is [t := f !t] *)
val replace : ('a : value_or_null). 'a t -> local_ ('a -> 'a) -> unit

(** [set_temporarily t a ~f] sets [t] to [a], calls [f ()], and then restores [t] to its
    value prior to [set_temporarily] being called, whether [f] returns or raises. *)
val set_temporarily : ('a : value_or_null) 'b. 'a t -> 'a -> f:local_ (unit -> 'b) -> 'b

module And_value : sig
  type t = T : 'a ref * 'a -> t [@@deriving sexp_of ~stackify]

  (** [set (T (r, x))] is equivalent to [r := x]. *)
  val set : t -> unit

  (** [sets ts = List.iter ts ~f:set] *)
  val sets : t list -> unit

  (** [snapshot (T (r, _))] returns [T (r, !r)]. *)
  val snapshot : t -> t
end

(** [sets_temporarily [ ...; T (ti, ai); ... ] ~f] sets each [ti] to [ai], calls [f ()],
    and then restores all [ti] to their value prior to [sets_temporarily] being called,
    whether [f] returns or raises. *)
val sets_temporarily : And_value.t list -> f:local_ (unit -> 'a) -> 'a
