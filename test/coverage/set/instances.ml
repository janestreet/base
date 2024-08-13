open! Base
open Base_quickcheck
open Base_test_coverage_helpers
open Overrides
include Instances_intf.Definitions

module Make_elt
    (Types : Functor.Types with type 'elt elt = 'elt)
    (Cmp : sig
       val comparator : (int, Int.comparator_witness Types.cmp) Comparator.t
     end) : Functor.Elt with module Types := Types = struct
  type t = int [@@deriving compare, equal, quickcheck, sexp_of]
  type comparator_witness = Int.comparator_witness Types.cmp

  let comparator = Cmp.comparator

  include (Int : Comparable.Infix with type t := t)

  let of_int = Fn.id
  let to_int = Fn.id
end

module Poly = struct
  module Types = Types.Poly
  module Elt = Make_elt (Types) (Comparator.Poly)

  type t = Set.M(Elt).t [@@deriving compare, equal, sexp_of]

  let create x = x
  let access x = x
end

module Toplevel = struct
  module Types = Types.Toplevel
  module Elt = Make_elt (Types) (Int)

  type t = Set.M(Elt).t [@@deriving compare, equal, sexp_of]

  let create f = f ((module Int) : _ Comparator.Module.t)
  let access x = x
end

module Tree = struct
  module Types = Types.Tree
  module Elt = Make_elt (Types) (Int)

  type t = (int, Int.comparator_witness) Types.t

  let compare = Comparable.lift Toplevel.compare ~f:(Set.of_tree (module Elt))
  let equal = Comparable.lift Toplevel.equal ~f:(Set.of_tree (module Elt))
  let sexp_of_t t = Toplevel.sexp_of_t (Set.of_tree (module Elt) t)
  let create f = f ~comparator:Int.comparator
  let access f = f ~comparator:Int.comparator
end

module Using_comparator = struct
  module Types = Types.Using_comparator
  module Elt = Make_elt (Types) (Int)

  type t = Set.M(Elt).t [@@deriving compare, equal, sexp_of]

  let create f = f ~comparator:Int.comparator
  let access x = x
end
