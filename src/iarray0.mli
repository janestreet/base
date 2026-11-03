open! Import0

type +'a t = 'a iarray

[%%rederive: type nonrec 'a t = 'a t [@@deriving equal, compare, sexp_of]]

external unsafe_to_array__promise_no_mutation : 'a. 'a t -> 'a array = "%identity"

external unsafe_of_array__promise_no_mutation
  : 'a.
  ('a array[@local_opt]) -> ('a t[@local_opt])
  = "%identity"

module O : sig
  external ( .:() )
    : 'a.
    ('a t[@local_opt]) -> int -> ('a[@local_opt])
    = "%array_safe_get"
  [@@layout_poly]
end

external get : 'a. ('a t[@local_opt]) -> int -> ('a[@local_opt]) = "%array_safe_get"
[@@layout_poly]

external unsafe_get
  : 'a.
  ('a t[@local_opt]) -> int -> ('a[@local_opt])
  = "%array_unsafe_get"
[@@layout_poly]

external length : 'a. ('a t[@local_opt]) -> int = "%array_length" [@@layout_poly]
val init : int -> f:(int -> 'a) -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t
