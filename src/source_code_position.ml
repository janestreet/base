open! Import

module M = struct
  include Source_code_position0

  let hash { Caml.Lexing. pos_fname; pos_lnum; pos_bol; pos_cnum } =
    String.hash pos_fname
    lxor Int.hash pos_lnum
    lxor Int.hash pos_bol
    lxor Int.hash pos_cnum
  ;;
end

include M
include Comparable.Make_using_comparator(M)
