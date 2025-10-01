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
  [@@deriving compare ~localize, hash, sexp_of ~stackify]
end

include T

include%template Comparator.Make [@modality portable] (T)

(* This is the same function as Ppx_here.lift_position_as_string. *)
let make_location_string ~pos_fname ~pos_lnum ~pos_cnum ~pos_bol =
  String.concat
    [ pos_fname; ":"; Int.to_string pos_lnum; ":"; Int.to_string (pos_cnum - pos_bol) ]
;;

let to_string { Stdlib.Lexing.pos_fname; pos_lnum; pos_cnum; pos_bol } =
  make_location_string ~pos_fname ~pos_lnum ~pos_cnum ~pos_bol
;;

let sexp_of_t t = Sexp.Atom (to_string t)
let equal__local (local_ a) (local_ b) = equal_int (compare__local a b) 0
let equal = [%eta2 equal__local]

let of_pos (pos_fname, pos_lnum, pos_cnum, _) =
  { pos_fname; pos_lnum; pos_cnum; pos_bol = 0 }
;;

let here_or_there ~(here : [%call_pos]) there =
  match there with
  | None -> here
  | Some there -> there
;;

let is_dummy (local_ t) = equal__local Stdlib.Lexing.dummy_pos t
