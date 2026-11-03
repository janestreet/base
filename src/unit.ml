open! Import

module T = struct
  type t = unit [@@deriving enumerate, globalize, hash, sexp ~unboxed, sexp_grammar]

  let compare _ _ = 0

  let of_string = function
    | "()" -> ()
    | _ -> failwith "Base.Unit.of_string: () expected"
  ;;

  let to_string () = "()"
  let module_name = "Base.Unit"
end

include T
include Identifiable.Make (T)

let invariant () = ()
