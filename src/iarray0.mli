@@ portable

open! Import

type +'a t = 'a iarray

external unsafe_to_array__promise_no_mutation
  : 'a.
  'a t -> 'a array
  @@ portable
  = "%array_of_iarray"

external unsafe_of_array__promise_no_mutation
  : 'a.
  ('a array[@local_opt]) -> ('a t[@local_opt])
  @@ portable
  = "%array_to_iarray"

module O : sig
  external ( .:() )
    :  ('a t[@local_opt])
    -> int
    -> ('a[@local_opt])
    @@ portable
    = "%array_safe_get"
end

external get
  :  ('a t[@local_opt])
  -> int
  -> ('a[@local_opt])
  @@ portable
  = "%array_safe_get"

external unsafe_get
  :  ('a t[@local_opt])
  -> int
  -> ('a[@local_opt])
  @@ portable
  = "%array_unsafe_get"

external length : ('a t[@local_opt]) @ contended -> int @@ portable = "%array_length"
val init : int -> f:(int -> 'a) @ local -> 'a t
val map : 'a t -> f:('a -> 'b) @ local -> 'b t
