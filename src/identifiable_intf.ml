(** A signature combining functionality that is commonly used for types that are intended
    to act as names or identifiers.

    Modules that satisfy [Identifiable] can be printed and parsed (both through string and
    s-expression converters) and can be used in hash-based and comparison-based containers
    (e.g., hashtables and maps).

    This module also provides functors for conveniently constructing identifiable modules. *)

open! Import

module Definitions = struct
  [%%template
  [@@@mode.default m = (global, local)]

  module type Arg = sig
    type t [@@deriving (compare [@mode m]), hash, sexp]

    include Stringable.S with type t := t

    (** For registering the pretty printer. *)
    val module_name : string
  end

  [@@@modality.default p = (portable, nonportable)]

  module type Arg_with_comparator = sig
    include Arg [@mode m]
    include Comparator.S [@modality p] with type t := t
  end

  module type S = sig
    type t [@@deriving hash, sexp]

    include Stringable.S with type t := t
    include Comparable.S [@modality p] [@mode m] with type t := t
    include Pretty_printer.S with type t := t

    val hashable : t Hashable.t
  end]
end

module type%template Identifiable = sig @@ portable
  include module type of struct
    include Definitions
  end

  [@@@mode.default m = (global, local)]

  (** Used for making an Identifiable module. Here's an example.

      {[
        module Id = struct
          module T = struct
            type t =
              | A
              | B
            [@@deriving compare, hash, sexp]

            let of_string s = t_of_sexp (sexp_of_string s)
            let to_string t = string_of_sexp (sexp_of_t t)
            let module_name = "My_library.Id"
          end

          include T
          include Identifiable.Make (T)
        end
      ]} *)
  module%template.portable [@modality p] Make (M : Arg [@mode m]) :
    S [@mode m] [@modality p] with type t := M.t

  module%template.portable
    [@modality p] Make_using_comparator
      (M : Arg_with_comparator
    [@mode m] [@modality p]) :
    S
    [@mode m] [@modality p]
    with type t := M.t
    with type comparator_witness := M.comparator_witness
end
