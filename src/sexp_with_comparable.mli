(*_ This module is separated from Sexp to avoid circular dependencies as many things use
  s-expressions *)

(** @inline *)
include module type of struct
  include Sexp
end

include Comparable.S with type t := t
include Ppx_compare_lib.Comparable.S_local with type t := t
include Ppx_compare_lib.Equal.S_local with type t := t
