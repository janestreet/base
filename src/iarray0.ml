open! Import0

(** Abstract type and unsafe casts *)

type (+'a : any mod separable) t = 'a iarray

[%%template
[@@@mode.default c = (uncontended, shared)]

(* This one should not operate on local arrays, because that would be more unsafe:
   extraction from a local array gets *global* elements. So if this function worked on
   local arrays it could be used to forget that a value was local by storing it in a local
   iarray, converting, and then extracting from the local array. *)
external unsafe_to_array__promise_no_mutation
  : ('a : any mod separable).
  'a t @ c -> 'a array @ c
  @@ portable
  = "%array_of_iarray"

(* In contrast to the function above, this one is safe to work on locals. Well, just as
   safe as it is on globals. *)
external unsafe_of_array__promise_no_mutation
  : ('a : any mod separable).
  ('a array[@local_opt]) @ c -> ('a t[@local_opt]) @ c
  @@ portable
  = "%array_to_iarray"]

(** Operators *)

module O = struct
  external ( .:() )
    : ('a : any mod separable).
    ('a t[@local_opt]) -> int -> ('a[@local_opt])
    @@ portable
    = "%array_safe_get"
  [@@layout_poly]
end

open O

(** Indexing and length *)

[%%template
[@@@mode.default c = (uncontended, shared, contended), p = (portable, nonportable)]

external get
  : ('a : any mod separable).
  ('a t[@local_opt]) @ c p -> int -> ('a[@local_opt]) @ c p
  @@ portable
  = "%array_safe_get"
[@@layout_poly]

external unsafe_get
  : ('a : any mod separable).
  ('a t[@local_opt]) @ c p -> int -> ('a[@local_opt]) @ c p
  @@ portable
  = "%array_unsafe_get"
[@@layout_poly]]

external length
  : ('a : any mod separable).
  ('a t[@local_opt]) @ immutable -> int
  @@ portable
  = "%array_length"
[@@layout_poly]

(** Constructors *)

let init len ~f = unsafe_of_array__promise_no_mutation (Array.init len ~f)

(** Transformations *)

let map t ~f = init (length t) ~f:(fun i -> f (unsafe_get t i)) [@nontail]

let%template equal equal_elt ta tb =
  if phys_equal ta tb
  then true
  else (
    let na = length ta in
    let nb = length tb in
    match na = nb with
    | false -> false
    | true ->
      let rec local_ loop pos =
        if pos = na
        then true
        else equal_elt (unsafe_get ta pos) (unsafe_get tb pos) && loop (pos + 1)
      in
      loop 0 [@nontail])
[@@mode __ = (local, global)]
;;

(* sexp serialization is copied from that of [array] in [Sexplib0] *)

let sexp_of_t sexp_of__a ar =
  let lst_ref = ref [] in
  for i = length ar - 1 downto 0 do
    lst_ref := sexp_of__a ar.:(i) :: !lst_ref
  done;
  Sexp0.List !lst_ref
;;

let%template[@alloc stack] sexp_of_t sexp_of__a ar = exclave_
  let rec loop i acc = exclave_
    if i < 0 then Sexp0.List acc else loop (i - 1) (sexp_of__a (get ar i) :: acc)
  in
  loop (length ar - 1) []
;;
