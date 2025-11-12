@@ portable

open! Import

type (+'a : any mod separable) t = 'a iarray

[%%rederive:
  type ('a : value_or_null mod separable) t = 'a iarray
  [@@deriving equal ~localize, sexp_of ~stackify]]

[%%template:
[@@@mode.default c = (uncontended, shared)]

external unsafe_to_array__promise_no_mutation
  : ('a : any mod separable).
  'a t @ c -> 'a array @ c
  = "%array_of_iarray"

external unsafe_of_array__promise_no_mutation
  : ('a : any mod separable).
  ('a array[@local_opt]) @ c -> ('a t[@local_opt]) @ c
  = "%array_to_iarray"]

module O : sig
  external ( .:() )
    : ('a : any mod separable).
    ('a t[@local_opt]) -> int -> ('a[@local_opt])
    = "%array_safe_get"
  [@@layout_poly]
end

[%%template:
[@@@mode.default c = (uncontended, shared, contended), p = (portable, nonportable)]

external get
  : ('a : any mod separable).
  ('a t[@local_opt]) @ c p -> int -> ('a[@local_opt]) @ c p
  = "%array_safe_get"
[@@layout_poly]

external unsafe_get
  : ('a : any mod separable).
  ('a t[@local_opt]) @ c p -> int -> ('a[@local_opt]) @ c p
  = "%array_unsafe_get"
[@@layout_poly]]

external length
  : ('a : any mod separable).
  ('a t[@local_opt]) @ immutable -> int
  = "%array_length"
[@@layout_poly]

val init : int -> f:(int -> 'a) @ local -> 'a t
val map : 'a t -> f:('a -> 'b) @ local -> 'b t
