open! Import

type +'a t = 'a iarray

[%%rederive: type 'a t = 'a iarray [@@deriving equal ~localize, sexp_of ~stackify]]

external unsafe_to_array__promise_no_mutation : 'a. 'a t -> 'a array = "%identity"

external unsafe_of_array__promise_no_mutation
  : 'a.
  ('a array[@local_opt]) -> ('a t[@local_opt])
  = "%identity"

module O : sig
  external ( .:() ) : ('a t[@local_opt]) -> int -> ('a[@local_opt]) = "%array_safe_get"
end

[%%template:
[@@@mode.default c = (uncontended, shared, contended), p = (portable, nonportable)]

external get : ('a t[@local_opt]) -> int -> ('a[@local_opt]) = "%array_safe_get"
external unsafe_get : ('a t[@local_opt]) -> int -> ('a[@local_opt]) = "%array_unsafe_get"]

external length : 'a. ('a t[@local_opt]) -> int = "%array_length" [@@layout_poly]
val init : int -> f:(int -> 'a) -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t
