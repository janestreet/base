open! Import

module T = struct
  type t = unit [@@deriving_inline hash, sexp]
  let t_of_sexp : Sexplib.Sexp.t -> t =
    let _tp_loc = "src/unit.ml.T.t"  in fun t  -> unit_of_sexp t
  let sexp_of_t : t -> Sexplib.Sexp.t = fun v  -> sexp_of_unit v
  let (hash_fold_t :
         Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
    fun hsv  -> fun arg  -> hash_fold_unit hsv arg
  let (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
    fun arg  ->
      Ppx_hash_lib.Std.Hash.get_hash_value
        (hash_fold_t (Ppx_hash_lib.Std.Hash.create ()) arg)

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
