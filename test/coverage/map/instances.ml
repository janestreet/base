open! Base
open Base_quickcheck
open Base_test_coverage_helpers
open Overrides
include Instances_intf.Definitions

module Make_key
    (Types : Functor.Types with type 'key key = 'key)
    (Cmp : sig
       val comparator : (int, Int.comparator_witness Types.cmp) Comparator.t
     end) : Functor.Key with module Types := Types = struct
  type t = int [@@deriving compare, equal, quickcheck, sexp_of]
  type comparator_witness = Int.comparator_witness Types.cmp

  let comparator = Cmp.comparator

  include (Int : Comparable.Infix with type t := t)

  let of_int = Fn.id
  let to_int = Fn.id
end

module Poly = struct
  module Types = Types.Poly
  module Key = Make_key (Types) (Comparator.Poly)

  type 'a t = 'a Map.M(Key).t [@@deriving compare, equal, sexp_of]

  let create x = x
  let access x = x
end

module Toplevel = struct
  module Types = Types.Toplevel
  module Key = Make_key (Types) (Int)

  type 'a t = 'a Map.M(Key).t [@@deriving compare, equal, sexp_of]

  let create f = f ((module Int) : _ Comparator.Module.t)
  let access x = x
end

module Tree = struct
  module Types = Types.Tree
  module Key = Make_key (Types) (Int)

  type 'a t = (int, 'a, Int.comparator_witness) Types.t

  let compare f = Comparable.lift (Toplevel.compare f) ~f:(Map.of_tree (module Key))
  let equal f = Comparable.lift (Toplevel.equal f) ~f:(Map.of_tree (module Key))
  let sexp_of_t f t = Toplevel.sexp_of_t f (Map.of_tree (module Key) t)
  let create f = f ~comparator:Int.comparator
  let access f = f ~comparator:Int.comparator
end

module Using_comparator = struct
  module Types = Types.Using_comparator
  module Key = Make_key (Types) (Int)

  type 'a t = 'a Map.M(Key).t [@@deriving compare, equal, sexp_of]

  let create f = f ~comparator:Int.comparator
  let access x = x
end
