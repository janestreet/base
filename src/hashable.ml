open! Import
module Sexp = Sexp0
include Hashable_intf.Definitions

type ('a : any) t =
  { hash : 'a -> int
  ; compare : 'a -> 'a -> int
  ; sexp_of_t : 'a -> Sexp.t
  }

(** This function is sound but not complete, meaning that if it returns [true] then it's
    safe to use the two interchangeably. If it's [false], you have no guarantees. For
    example:

    {[
      > utop
      open Core;;
      let equal (a : 'a Hashtbl_intf.Hashable.t) b =
        phys_equal a b
        || (phys_equal a.hash b.hash
            && phys_equal a.compare b.compare
            && phys_equal a.sexp_of_t b.sexp_of_t)
      ;;
      let a = Hashtbl_intf.Hashable.{ hash; compare; sexp_of_t = Int.sexp_of_t };;
      let b = Hashtbl_intf.Hashable.{ hash; compare; sexp_of_t = Int.sexp_of_t };;
      equal a b;;  (* false?! *)
    ]} *)
let equal a b =
  phys_equal a b
  || (phys_equal a.hash b.hash
      && phys_equal a.compare b.compare
      && phys_equal a.sexp_of_t b.sexp_of_t)
;;

let hash_param = Stdlib.Hashtbl.hash_param
let hash = Stdlib.Hashtbl.hash
let poly = { hash; compare = Poly.compare; sexp_of_t = (fun _ -> Sexp.Atom "_") }

let%template[@kind k = (value, float64, bits64)] of_key
  : type (a : k). ((module Key with type t = a)[@kind k] [@modality p]) -> a t @ p
  =
  fun (module Key) ->
  { hash = Key.hash; compare = Key.compare; sexp_of_t = Key.sexp_of_t }
[@@modality p = (portable, nonportable)]
;;

let%template[@kind k = (value, float64, bits64)] to_key
  : type (a : k). a t @ p -> ((module Key with type t = a)[@kind k] [@modality p]) @ p
  =
  fun { hash; compare; sexp_of_t } ->
  (module struct
    type t = a

    let hash = hash
    let compare = compare
    let sexp_of_t = sexp_of_t
  end : Key
    with type t = a[@kind k] [@modality p])
[@@modality p = (portable, nonportable)]
;;
