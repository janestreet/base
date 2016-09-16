open! Import

module T = struct
  type t = Lexing.position =
    { pos_fname : string;
      pos_lnum : int;
      pos_bol : int;
      pos_cnum : int;
  }
  [@@deriving compare, hash, sexp]
end

include T
include Comparator.Make(T)

(* This is the same function as Ppx_here.lift_position_as_string. *)
let make_location_string ~pos_fname ~pos_lnum ~pos_cnum ~pos_bol =
  String.concat ""
    [ pos_fname
    ; ":"; string_of_int pos_lnum
    ; ":"; string_of_int (pos_cnum - pos_bol)
    ]

let to_string {Lexing.pos_fname; pos_lnum; pos_cnum; pos_bol} =
  make_location_string ~pos_fname ~pos_lnum ~pos_cnum ~pos_bol

let sexp_of_t t = Sexp.Atom (to_string t)
