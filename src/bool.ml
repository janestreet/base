open! Import

let invalid_argf = Base_printf.invalid_argf

module T = struct
  type t = bool [@@deriving hash, sexp]
  let compare (t : t) t' = compare t t'

  (* we use physical equality here because for bools it is the same *)
  let equal (t : t) t' = t == t'

  (* This shadows the [hash] definition coming from [@@deriving hash] because that
     function is significantly slower.

     Unfortunately, this means [Bool.hash] and [%hash: Bool.t] produce different hash
     values, but this is unavoidable if we want predictable name-based behavior from the
     syntax extension.
  *)
  let hash x = if x then 1 else 0
end

include T
include Comparator.Make(T)

let of_string = function
  | "true" -> true
  | "false" -> false
  | s -> invalid_argf "Bool.of_string: expected true or false but got %s" s ()
;;

let to_string = string_of_bool

module Replace_polymorphic_compare = struct
  let min (x : t) y = if x < y then x else y
  let max (x : t) y = if x > y then x else y
  let compare = compare
  let ascending = compare
  let descending x y = compare y x
  let ( >= ) (x : t) y = x >= y
  let ( <= ) (x : t) y = x <= y
  let ( = ) = equal
  let equal = equal
  let ( > ) (x : t) y = x > y
  let ( < ) (x : t) y = x < y
  let ( <> ) (x : t) y = x != y
  let between t ~low ~high = low <= t && t <= high
  let clamp_unchecked t ~min ~max =
    if t < min then min else if t <= max then t else max

  let clamp_exn t ~min ~max =
    assert (min <= max);
    clamp_unchecked t ~min ~max

  let clamp t ~min ~max =
    if min > max then
      Or_error.error "clamp requires [min <= max]"
        (`Min min, `Max max) [%sexp_of: [`Min of T.t] * [`Max of T.t]]
    else
      Ok (clamp_unchecked t ~min ~max)
end

include Replace_polymorphic_compare

(* Making bool hashable may seem frivolous, but consider an aggregate type with
   a bool in it that needs a custom hash function. *)
include Hashable.Make (T)

include Comparable.Validate (T)

(* We use [Obj.magic] here as other implementations generate a conditional jump and the
   performance difference is noticeable. *)
let to_int (x : bool) = (Obj.magic x : int)

(* We do this as a direct assert on the theory that it's a cheap thing to test and a
   really core invariant that we never expect to break, and we should be happy for a
   program to fail immediately if this is violated. *)
let () =
  assert (Pervasives.(=) (to_int true ) 1 &&
          Pervasives.(=) (to_int false) 0);
;;
