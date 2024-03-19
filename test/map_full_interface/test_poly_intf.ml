open! Base

module Definitions = struct
  module Types = struct
    type 'key key = 'key
    type 'cmp cmp = Comparator.Poly.comparator_witness
    type ('key, 'data, 'cmp) t = ('key, 'data) Map.Poly.t
    type ('key, 'data, 'cmp) tree = ('key, 'data) Map.Poly.tree
    type ('key, 'cmp, 'fn) create_options = 'fn
    type ('key, 'cmp, 'fn) access_options = 'fn
  end

  module type S = Functor.S with module Types := Types
end

module type Test_poly = sig
  include module type of struct
    include Definitions
  end

  include S
end
