open! Import
module Int = Int0
module String = String0

module T = struct
  type t = Caml.Lexing.position =
    { pos_fname : string
    ; pos_lnum : int
    ; pos_bol : int
    ; pos_cnum : int
    }
  [@@deriving_inline compare, hash, sexp_of]

  let compare =
    (fun a__001_ b__002_ ->
       if Ppx_compare_lib.phys_equal a__001_ b__002_
       then 0
       else (
         match compare_string a__001_.pos_fname b__002_.pos_fname with
         | 0 ->
           (match compare_int a__001_.pos_lnum b__002_.pos_lnum with
            | 0 ->
              (match compare_int a__001_.pos_bol b__002_.pos_bol with
               | 0 -> compare_int a__001_.pos_cnum b__002_.pos_cnum
               | n -> n)
            | n -> n)
         | n -> n)
         : t -> t -> int)
  ;;

  let (hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
    fun hsv arg ->
      let hsv =
        let hsv =
          let hsv =
            let hsv = hsv in
            hash_fold_string hsv arg.pos_fname
          in
          hash_fold_int hsv arg.pos_lnum
        in
        hash_fold_int hsv arg.pos_bol
      in
      hash_fold_int hsv arg.pos_cnum
  ;;

  let (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func arg =
      Ppx_hash_lib.Std.Hash.get_hash_value
        (let hsv = Ppx_hash_lib.Std.Hash.create () in
         hash_fold_t hsv arg)
    in
    fun x -> func x
  ;;

  let sexp_of_t =
    (fun { pos_fname = v_pos_fname
         ; pos_lnum = v_pos_lnum
         ; pos_bol = v_pos_bol
         ; pos_cnum = v_pos_cnum
         } ->
      let bnds = [] in
      let bnds =
        let arg = sexp_of_int v_pos_cnum in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "pos_cnum"; arg ] :: bnds
      in
      let bnds =
        let arg = sexp_of_int v_pos_bol in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "pos_bol"; arg ] :: bnds
      in
      let bnds =
        let arg = sexp_of_int v_pos_lnum in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "pos_lnum"; arg ] :: bnds
      in
      let bnds =
        let arg = sexp_of_string v_pos_fname in
        Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "pos_fname"; arg ] :: bnds
      in
      Sexplib0.Sexp.List bnds
      : t -> Sexplib0.Sexp.t)
  ;;

  [@@@end]
end

include T
include Comparator.Make (T)

(* This is the same function as Ppx_here.lift_position_as_string. *)
let make_location_string ~pos_fname ~pos_lnum ~pos_cnum ~pos_bol =
  String.concat
    [ pos_fname; ":"; Int.to_string pos_lnum; ":"; Int.to_string (pos_cnum - pos_bol) ]
;;

let to_string { Caml.Lexing.pos_fname; pos_lnum; pos_cnum; pos_bol } =
  make_location_string ~pos_fname ~pos_lnum ~pos_cnum ~pos_bol
;;

let sexp_of_t t = Sexp.Atom (to_string t)
