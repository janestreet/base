
open! Import

type 'a t = Incl of 'a | Excl of 'a | Unbounded [@@deriving sexp]
(** Used for specifying a bound (either upper or lower) as inclusive, exclusive, or
    unbounded. *)

val map : 'a t -> f:('a -> 'b) -> 'b t

val is_lower_bound : 'a t -> of_:'a -> compare:('a -> 'a -> int) -> bool
val is_upper_bound : 'a t -> of_:'a -> compare:('a -> 'a -> int) -> bool

(** [interval_contains_exn ~lower ~upper x ~compare] raises if [lower] and [upper] are
    crossed. *)
val interval_contains_exn
  :  lower   : 'a t
  -> upper   : 'a t
  -> 'a
  -> compare : ('a -> 'a -> int)
  -> bool

type interval_comparison =
  | Below_lower_bound
  | In_range
  | Above_upper_bound
[@@deriving sexp, compare, hash]

(** [compare_to_interval_exn ~lower ~upper x ~compare] raises if [lower] and [upper] are
    crossed. *)
val compare_to_interval_exn
  :  lower   : 'a t
  -> upper   : 'a t
  -> 'a
  -> compare : ('a -> 'a -> int)
  -> interval_comparison
