open! Import

module T = struct
  type t = unit [@@deriving_inline enumerate, hash, sexp]
  let all : t list = [()]
  let (hash_fold_t :
         Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
    hash_fold_unit
  let (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
    fun arg  ->
      Ppx_hash_lib.Std.Hash.get_hash_value
        (let hsv = Ppx_hash_lib.Std.Hash.create ()  in hash_fold_t hsv arg)

  let t_of_sexp : Sexplib.Sexp.t -> t = unit_of_sexp
  let sexp_of_t : t -> Sexplib.Sexp.t = sexp_of_unit
  [@@@end]

  let compare _ _ = 0

  let of_string = function
    | "()" -> ()
    | _    -> failwith "Base.Unit.of_string: () expected"

  let to_string () = "()"

  let module_name = "Base.Unit"
end

include T
include Identifiable.Make (T)

let invariant () = ()
