open! Base

module Definitions = struct
  module Types = struct
    module Poly = struct
      type 'key key = 'key
      type 'cmp cmp = Comparator.Poly.comparator_witness
      type ('key, 'data, 'cmp) t = ('key, 'data) Map.Poly.t
      type ('key, 'data, 'cmp) tree = ('key, 'data) Map.Poly.tree
      type ('key, 'cmp, 'fn) create_options = 'fn
      type ('key, 'cmp, 'fn) access_options = 'fn
    end

    module Toplevel = struct
      type 'key key = 'key
      type 'cmp cmp = 'cmp
      type ('key, 'data, 'cmp) t = ('key, 'data, 'cmp) Map.t
      type ('key, 'data, 'cmp) tree = ('key, 'data, 'cmp) Map.Using_comparator.Tree.t
      type ('key, 'cmp, 'fn) create_options = ('key, 'cmp) Comparator.Module.t -> 'fn
      type ('key, 'cmp, 'fn) access_options = 'fn
    end

    module Tree = struct
      type 'key key = 'key
      type 'cmp cmp = 'cmp
      type ('key, 'data, 'cmp) t = ('key, 'data, 'cmp) Map.Using_comparator.Tree.t
      type ('key, 'data, 'cmp) tree = ('key, 'data, 'cmp) Map.Using_comparator.Tree.t
      type ('key, 'cmp, 'fn) create_options = comparator:('key, 'cmp) Comparator.t -> 'fn
      type ('key, 'cmp, 'fn) access_options = comparator:('key, 'cmp) Comparator.t -> 'fn
    end

    module Using_comparator = struct
      type 'key key = 'key
      type 'cmp cmp = 'cmp
      type ('key, 'data, 'cmp) t = ('key, 'data, 'cmp) Map.Using_comparator.t
      type ('key, 'data, 'cmp) tree = ('key, 'data, 'cmp) Map.Using_comparator.Tree.t
      type ('key, 'cmp, 'fn) create_options = comparator:('key, 'cmp) Comparator.t -> 'fn
      type ('key, 'cmp, 'fn) access_options = 'fn
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
