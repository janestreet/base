open! Import

module Compare_failed_or_set_here = struct
  type t =
    | Compare_failed
    | Set_here
  [@@deriving sexp_of ~localize]
end

type 'a t = 'a Stdlib.Atomic.t

let make = Stdlib.Atomic.make
let make_alone = Stdlib.Atomic.make_contended

external get : ('a t[@local_opt]) -> 'a = "%atomic_load"
external exchange : ('a t[@local_opt]) -> 'a -> 'a = "%atomic_exchange"
external set : ('a t[@local_opt]) -> 'a -> unit = "caml_atomic_set_stub"

external compare_and_set
  :  ('a t[@local_opt])
  -> if_phys_equal_to:'a
  -> replace_with:'a
  -> Compare_failed_or_set_here.t
  = "%atomic_cas"

external compare_exchange
  :  ('a t[@local_opt])
  -> if_phys_equal_to:'a
  -> replace_with:'a
  -> 'a
  = "caml_atomic_compare_exchange_stub"

let rec update_and_return t ~pure_f =
  let old = get t in
  let new_ = pure_f old in
  match compare_and_set t ~if_phys_equal_to:old ~replace_with:new_ with
  | Set_here -> old
  | Compare_failed ->
    Stdlib.Domain.cpu_relax ();
    update_and_return t ~pure_f
;;

let update (type a) (t : a t) ~pure_f = ignore_contended (update_and_return t ~pure_f : a)

external fetch_and_add : (int t[@local_opt]) -> int -> int = "%atomic_fetch_add"
external add : (int t[@local_opt]) -> int -> unit = "caml_atomic_add_stub"
external sub : (int t[@local_opt]) -> int -> unit = "caml_atomic_sub_stub"
external logand : (int t[@local_opt]) -> int -> unit = "caml_atomic_land_stub"
external logor : (int t[@local_opt]) -> int -> unit = "caml_atomic_lor_stub"
external logxor : (int t[@local_opt]) -> int -> unit = "caml_atomic_lxor_stub"

let incr r = add r 1
let decr r = sub r 1
let sexp_of_t sexp_of_a t = sexp_of_a (get t)
let t_of_sexp a_of_sexp sexp = make (a_of_sexp sexp)

module Expert = struct
  (* This is subject to CSE. *)
  external fenceless_get_cse : ('a t[@local_opt]) -> 'a = "%field0"

  let[@inline] fenceless_get t =
    (* We use [Sys.opaque_identity] to prevent CSE. *)
    fenceless_get_cse (Sys.opaque_identity t)
  ;;
end
