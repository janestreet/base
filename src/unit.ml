open! Import

module T = struct
  type t = unit
  [@@deriving enumerate, globalize, hash, sexp ~stackify ~unboxed, sexp_grammar]

  let%template[@mode __ = (global, local)] compare _ _ = 0

  let of_string = function
    | "()" -> ()
    | _ -> failwith "Base.Unit.of_string: () expected"
  ;;

  let to_string () = "()"
  let module_name = "Base.Unit"
end

include T

include%template Identifiable.Make [@mode local] [@modality portable] (T)

let invariant () = ()
