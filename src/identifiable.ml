open! Import0
include Identifiable_intf.Definitions

[%%template
[@@@mode.default m = (global, local)]
[@@@alloc.default a = (heap, stack)]

module%template.portable [@modality p] Make (T : Arg [@mode m] [@alloc a]) = struct
  include T
  include Comparable.Make [@mode m] [@modality p] (T)
  include Pretty_printer.Register [@modality p] (T)

  let hashable : t Hashable.t = { hash; compare; sexp_of_t }
end

module%template.portable
  [@modality p]
  Make_using_comparator
    (T : Arg_with_comparator
  [@mode m] [@modality p] [@alloc a]) =
struct
  include T
  include Comparable.Make_using_comparator [@mode m] [@modality p] (T)
  include Pretty_printer.Register [@modality p] (T)

  let hashable : t Hashable.t = { hash; compare; sexp_of_t }
end]
