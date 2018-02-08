open! Import

let invalid_argf = Printf.invalid_argf

module T = struct
  type t = bool [@@deriving_inline compare, enumerate, hash, sexp]
  let compare : t -> t -> int = compare_bool
  let all : t list = [false; true]
  let (hash_fold_t :
         Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
    hash_fold_bool

  and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = hash_bool  in fun x  -> func x

  let t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t = bool_of_sexp
  let sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t = sexp_of_bool
  [@@@end]
end

include T
include Comparator.Make(T)

let of_string = function
  | "true" -> true
  | "false" -> false
  | s -> invalid_argf "Bool.of_string: expected true or false but got %s" s ()
;;

let to_string = Caml.string_of_bool

include Bool_replace_polymorphic_compare

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
