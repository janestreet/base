open! Import

module T = struct
  type t = unit [@@deriving enumerate, globalize, hash, sexp ~stackify, sexp_grammar]

  let compare _ _ = 0
  let compare__local _ _ = 0
  let equal__local _ _ = true

  let of_string = function
    | "()" -> ()
    | _ -> failwith "Base.Unit.of_string: () expected"
  ;;

  let to_string () = "()"
  let module_name = "Base.Unit"
end

include T

include%template Identifiable.Make [@modality portable] (T)

let invariant () = ()
