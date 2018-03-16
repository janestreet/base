open! Import
open Container_intf.Export

(* In the definition of [t], we do not have [[@@deriving_inline compare, sexp][@@@end]] because
   in general, syntax extensions tend to use the implementation when available rather than
   using the alias.  Here that would lead to use the record representation [ { mutable
   contents : 'a } ] which would result in different (and unwanted) behavior.  *)
type 'a t = 'a ref = { mutable contents : 'a }

include (struct
  type 'a t = 'a ref [@@deriving_inline compare, sexp]
  let compare : 'a . ('a -> 'a -> int) -> 'a t -> 'a t -> int = compare_ref
  let t_of_sexp :
    'a . (Ppx_sexp_conv_lib.Sexp.t -> 'a) -> Ppx_sexp_conv_lib.Sexp.t -> 'a t =
    ref_of_sexp
  let sexp_of_t :
    'a . ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t =
    sexp_of_ref
  [@@@end]
end : sig
           type 'a t = 'a ref [@@deriving_inline compare, sexp]
           include
           sig
             [@@@ocaml.warning "-32"]
             val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
             val t_of_sexp :
               (Ppx_sexp_conv_lib.Sexp.t -> 'a) -> Ppx_sexp_conv_lib.Sexp.t -> 'a t
             val sexp_of_t :
               ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t
           end
           [@@@end]
         end with type 'a t := 'a t)

external create : 'a   -> 'a t       = "%makemutable"
external ( ! )  : 'a t -> 'a         = "%field0"
external ( := ) : 'a t -> 'a -> unit = "%setfield0"

let swap t1 t2 =
  let tmp = !t1 in
  t1 := !t2;
  t2 := tmp

let replace t f = t := f !t

(* container functions below *)
let length _ = 1

let is_empty _ = false

let iter t ~f = f !t

let fold t ~init ~f = f init !t

let fold_result t ~init ~f = f init !t
let fold_until  t ~init ~f ~finish =
  match (f init !t : ('a, 'b) Continue_or_stop.t) with
  | Stop     x -> x
  | Continue x -> finish x

let count t ~f = if f !t then 1 else 0
let sum _ t ~f = f !t

let exists t ~f = f !t

let for_all t ~f = f !t

let mem t a ~equal = equal a !t

let find t ~f = let a = !t in if f a then Some a else None

let find_map t ~f = f !t

let to_list t = [ !t ]

let to_array t = [| !t |]

let min_elt t ~compare:_ = Some !t
let max_elt t ~compare:_ = Some !t

let set_temporarily t a ~f =
  let restore_to = !t in
  t := a;
  Exn.protect ~f ~finally:(fun () -> t := restore_to);
;;
