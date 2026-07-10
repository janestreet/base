open! Import
module Int = Int0
module Sexp = Sexp0
module String = String0

module T = struct
  type t = Stdlib.Lexing.position =
    { pos_fname : string
    ; pos_lnum : int
    ; pos_bol : int
    ; pos_cnum : int
    }
  [@@deriving compare ~localize, hash, sexp_of]
end

include T

include%template Comparator.Make [@modality portable] (T)

(* This is the same function as Ppx_here.lift_position_as_string. *)
let%template[@alloc a = (heap, stack)] make_location_string
  ~pos_fname
  ~pos_lnum
  ~pos_cnum
  ~pos_bol
  =
  (String.concat [@alloc a])
    [ pos_fname
    ; ":"
    ; (Int.to_string [@alloc a]) pos_lnum
    ; ":"
    ; (Int.to_string [@alloc a]) (pos_cnum - pos_bol)
    ] [@exclave_if_stack a]
;;

let%template[@alloc a = (heap, stack)] to_string
  { Stdlib.Lexing.pos_fname; pos_lnum; pos_cnum; pos_bol }
  =
  (make_location_string [@alloc a])
    ~pos_fname
    ~pos_lnum
    ~pos_cnum
    ~pos_bol [@exclave_if_stack a]
;;

let%template[@alloc a = (heap, stack)] sexp_of_t t =
  Sexp.Atom ((to_string [@alloc a]) t) [@exclave_if_stack a]
;;

let%template[@mode local] equal (local_ a) (local_ b) =
  equal_int ((compare [@mode local]) a b) 0
;;

let%template equal = [%eta2 equal [@mode local]]

let of_pos (pos_fname, pos_lnum, pos_cnum, _) =
  { pos_fname; pos_lnum; pos_cnum; pos_bol = 0 }
;;

let here_or_there ~(here : [%call_pos]) there =
  match there with
  | None -> here
  | Some there -> there
;;

let%template is_dummy (local_ t) = (equal [@mode local]) Stdlib.Lexing.dummy_pos t
