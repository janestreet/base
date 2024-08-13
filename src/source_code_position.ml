open! Import
module M = Source_code_position0
include M
include Comparable.Make_using_comparator (M)

let equal__local a b = equal_int (compare__local a b) 0

let of_pos (pos_fname, pos_lnum, pos_cnum, _) =
  { pos_fname; pos_lnum; pos_cnum; pos_bol = 0 }
;;

let here_or_there ?(here = Stdlib.Lexing.dummy_pos) there =
  match there with
  | None -> here
  | Some there -> there
;;

let is_dummy t = equal__local Stdlib.Lexing.dummy_pos t
