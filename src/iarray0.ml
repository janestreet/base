open! Import0

(** Abstract type and unsafe casts *)

type +'a t = 'a iarray

(* This one should not operate on local arrays, because that would be more unsafe:
   extraction from a local array gets *global* elements. So if this function worked
   on local arrays it could be used to forget that a value was local by storing it
   in a local iarray, converting, and then extracting from the local array. *)
external unsafe_to_array__promise_no_mutation
  : 'a.
  'a t -> 'a array
  @@ portable
  = "%array_of_iarray"

(* In contrast to the function above, this one is safe to work on locals. Well, just
   as safe as it is on globals. *)
external unsafe_of_array__promise_no_mutation
  : 'a.
  ('a array[@local_opt]) -> ('a t[@local_opt])
  @@ portable
  = "%array_to_iarray"

(** Operators *)

module O = struct
  external ( .:() )
    :  ('a t[@local_opt])
    -> int
    -> ('a[@local_opt])
    @@ portable
    = "%array_safe_get"
end

(** Indexing and length *)

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

(** Constructors *)

let init len ~f = unsafe_of_array__promise_no_mutation (Array.init len ~f)

(** Transformations *)

let map t ~f = init (length t) ~f:(fun i -> f (unsafe_get t i)) [@nontail]
