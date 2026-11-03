open! Import0
module Array = Array0
module Sexp = Sexp0
open Ppx_compare_lib.Builtin

(** Abstract type and unsafe casts *)

type +'a t = 'a iarray

let equal = equal_iarray
let compare = compare_iarray

(* This one should not operate on local arrays, because that would be more unsafe:
   extraction from a local array gets *global* elements. So if this function worked on
   local arrays it could be used to forget that a value was local by storing it in a local
   iarray, converting, and then extracting from the local array. *)
external unsafe_to_array__promise_no_mutation : 'a. 'a t -> 'a array = "%identity"

(* In contrast to the function above, this one is safe to work on locals. Well, just as
   safe as it is on globals. *)
external unsafe_of_array__promise_no_mutation
  : 'a.
  ('a array[@local_opt]) -> ('a t[@local_opt])
  = "%identity"

(** Operators *)

module O = struct
  external ( .:() )
    : 'a.
    ('a t[@local_opt]) -> int -> ('a[@local_opt])
    = "%array_safe_get"
  [@@layout_poly]
end

open O

(** Indexing and length *)

external get : 'a. ('a t[@local_opt]) -> int -> ('a[@local_opt]) = "%array_safe_get"
[@@layout_poly]

external unsafe_get
  : 'a.
  ('a t[@local_opt]) -> int -> ('a[@local_opt])
  = "%array_unsafe_get"
[@@layout_poly]

external length : 'a. ('a t[@local_opt]) -> int = "%array_length" [@@layout_poly]

(** Constructors *)

let init len ~f = unsafe_of_array__promise_no_mutation (Array.init len ~f)

(** Transformations *)

let map t ~f = init (length t) ~f:(fun i -> f (unsafe_get t i)) [@nontail]

(* sexp serialization is copied from that of [array] in [Sexplib0] *)

let sexp_of_t sexp_of__a (ar : _ t) =
  let lst_ref = ref [] in
  for i = length ar - 1 downto 0 do
    lst_ref := sexp_of__a ar.:(i) :: !lst_ref
  done;
  Sexp.List !lst_ref
;;
