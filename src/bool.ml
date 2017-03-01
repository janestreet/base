open! Import

let invalid_argf = Printf.invalid_argf

module T = struct
  type t = bool [@@deriving_inline compare, enumerate, hash, sexp]
  let t_of_sexp : Sexplib.Sexp.t -> t =
    let _tp_loc = "src/bool.ml.T.t"  in fun t  -> bool_of_sexp t
  let sexp_of_t : t -> Sexplib.Sexp.t = fun v  -> sexp_of_bool v
  let (hash_fold_t :
         Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
    fun hsv  -> fun arg  -> hash_fold_bool hsv arg
  let (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
    fun arg  ->
      Ppx_hash_lib.Std.Hash.get_hash_value
        (hash_fold_t (Ppx_hash_lib.Std.Hash.create ()) arg)

  let all : t list = [false; true]
  let compare : t -> t -> int =
    fun a__001_  -> fun b__002_  -> compare_bool a__001_ b__002_
  [@@@end]

  (* we use physical equality here because for bools it is the same *)
  let equal (t : t) t' = phys_equal t t'

  (* This shadows the [hash] definition coming from [@@deriving_inline hash][@@@end] because that
     function is significantly slower.

     Unfortunately, this means [Bool.hash] and [%hash: Bool.t] produce different hash
     values, but this is unavoidable if we want predictable name-based behavior from the
     syntax extension. *)
  let _ = hash
  let hash x = if x then 1 else 0
end

include T
include Comparator.Make(T)

let of_string = function
  | "true" -> true
  | "false" -> false
  | s -> invalid_argf "Bool.of_string: expected true or false but got %s" s ()
;;

let to_string = Caml.string_of_bool

module Replace_polymorphic_compare = struct
  let min (x : t) y = if Poly.( < ) x y then x else y
  let max (x : t) y = if Poly.( > ) x y then x else y
  let compare = compare
  let ascending = compare
  let descending x y = compare y x
  let ( >= ) (x : t) y = Poly.( >= ) x y
  let ( <= ) (x : t) y = Poly.( <= ) x y
  let ( = ) = equal
  let equal = equal
  let ( > ) (x : t) y = Poly.( > ) x y
  let ( < ) (x : t) y = Poly.( < ) x y
  let ( <> ) (x : t) y = x != y
  let between t ~low ~high = low <= t && t <= high
  let clamp_unchecked t ~min ~max =
    if t < min then min else if t <= max then t else max

  let clamp_exn t ~min ~max =
    assert (min <= max);
    clamp_unchecked t ~min ~max

  let clamp t ~min ~max =
    if min > max then
      Or_error.error_s
        (Sexp.message "clamp requires [min <= max]"
           [ "min", T.sexp_of_t min
           ; "max", T.sexp_of_t max
           ])
    else
      Ok (clamp_unchecked t ~min ~max)
end

include Replace_polymorphic_compare

include Comparable.Validate (T)

(* We use [Obj.magic] here as other implementations generate a conditional jump and the
   performance difference is noticeable. *)
let to_int (x : bool) = (Caml.Obj.magic x : int)

(* We do this as a direct assert on the theory that it's a cheap thing to test and a
   really core invariant that we never expect to break, and we should be happy for a
   program to fail immediately if this is violated. *)
let () =
  assert (Pervasives.(=) (to_int true ) 1 &&
          Pervasives.(=) (to_int false) 0);
;;
