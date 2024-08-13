open! Base

module Definitions = struct
  module Types = struct
    module Poly = struct
      type 'elt elt = 'elt
      type 'cmp cmp = Comparator.Poly.comparator_witness
      type ('elt, 'cmp) set = ('elt, 'cmp) Set.t
      type ('elt, 'cmp) t = 'elt Set.Poly.t

      type ('elt, 'cmp) tree =
        ('elt, Comparator.Poly.comparator_witness) Set.Using_comparator.Tree.t

      type ('elt, 'cmp, 'fn) create_options = 'fn
      type ('elt, 'cmp, 'fn) access_options = 'fn
    end

    module Toplevel = struct
      type 'elt elt = 'elt
      type 'cmp cmp = 'cmp
      type ('elt, 'cmp) set = ('elt, 'cmp) Set.t
      type ('elt, 'cmp) t = ('elt, 'cmp) Set.t
      type ('elt, 'cmp) tree = ('elt, 'cmp) Set.Using_comparator.Tree.t
      type ('elt, 'cmp, 'fn) create_options = ('elt, 'cmp) Comparator.Module.t -> 'fn
      type ('elt, 'cmp, 'fn) access_options = 'fn
    end

    module Tree = struct
      type 'elt elt = 'elt
      type 'cmp cmp = 'cmp
      type ('elt, 'cmp) set = ('elt, 'cmp) Set.Using_comparator.Tree.t
      type ('elt, 'cmp) t = ('elt, 'cmp) Set.Using_comparator.Tree.t
      type ('elt, 'cmp) tree = ('elt, 'cmp) Set.Using_comparator.Tree.t
      type ('elt, 'cmp, 'fn) create_options = comparator:('elt, 'cmp) Comparator.t -> 'fn
      type ('elt, 'cmp, 'fn) access_options = comparator:('elt, 'cmp) Comparator.t -> 'fn
    end

    module Using_comparator = struct
      type 'elt elt = 'elt
      type 'cmp cmp = 'cmp
      type ('elt, 'cmp) set = ('elt, 'cmp) Set.Using_comparator.t
      type ('elt, 'cmp) t = ('elt, 'cmp) Set.Using_comparator.t
      type ('elt, 'cmp) tree = ('elt, 'cmp) Set.Using_comparator.Tree.t
      type ('elt, 'cmp, 'fn) create_options = comparator:('elt, 'cmp) Comparator.t -> 'fn
      type ('elt, 'cmp, 'fn) access_options = 'fn
    end
  end
end

module type Instances = sig
  include module type of struct
    include Definitions
  end

  module Poly : Functor.Instance with module Types = Types.Poly
  module Toplevel : Functor.Instance with module Types = Types.Toplevel
  module Tree : Functor.Instance with module Types = Types.Tree
  module Using_comparator : Functor.Instance with module Types = Types.Using_comparator
end
