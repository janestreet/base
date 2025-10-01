open! Base
open Base_test_coverage_helpers

module Definitions = struct
  (** The types that distinguish instances of
      [Map.Creators_and_accessors_and_transformers_generic]. *)
  module type Types = sig
    type 'k key
    type 'c cmp
    type ('k, 'v, 'c) t
    type ('k, 'v, 'c) tree
    type ('k, 'c, 'a) create_options
    type ('k, 'c, 'a) access_options
  end

  (** Like [Map.Creators_generic], but based on [Types] for easier instantiation. *)
  module type Creators = sig @@ portable
    module Types : Types

    include
      Map.Creators_generic
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) Types.t
      with type ('a, 'b, 'c) map := ('a, 'b, 'c) Types.t
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Types.tree
      with type 'a key := 'a Types.key
      with type 'a map_key := 'a Types.key
      with type 'a cmp := 'a Types.cmp
      with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) Types.create_options
      with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) Types.access_options
  end

  (** Like [Map.Accessors_generic], but based on [Types] for easier instantiation. *)
  module type Accessors = sig @@ portable
    module Types : Types

    include
      Map.Accessors_generic
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) Types.t
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Types.tree
      with type 'a key := 'a Types.key
      with type 'a cmp := 'a Types.cmp
      with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) Types.access_options
  end

  (** Like [Map.Transformers_generic], but based on [Types] for easier instantiation. *)
  module type Transformers = sig @@ portable
    module Types : Types

    include
      Map.Transformers_generic
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) Types.t
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Types.tree
      with type 'a key := 'a Types.key
      with type 'a cmp := 'a Types.cmp
      with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) Types.access_options
  end

  (** Like [Map.Creators_and_accessors_and_transformers_generic], but based on [Types] for
      easier instantiation. *)
  module type Impl = sig @@ portable
    module Types : Types
    include Creators with module Types := Types
    include Accessors with module Types := Types
    include Transformers with module Types := Types
  end

  module type Key = sig
    module Types : Types

    type t = int Types.key [@@deriving compare, equal, quickcheck, sexp_of]

    include Comparable.Infix with type t := t

    include
      Comparator.S
      with type t := t
       and type comparator_witness = Int.comparator_witness Types.cmp

    val of_int : int -> t
    val to_int : t -> int
  end

  (** Helpers for testing a tree or map type that is an instance of [S]. *)
  module type Instance = sig
    module Types : Types
    module Key : Key with module Types := Types

    type 'a t = (int, 'a, Int.comparator_witness) Types.t
    [@@deriving compare, equal, sexp_of]

    (** Pass a comparator to a creator function, if necessary. *)
    val create : (int, Int.comparator_witness, 'a) Types.create_options -> 'a

    (** Pass a comparator to an accessor function, if necessary *)
    val access : (int, Int.comparator_witness, 'a) Types.access_options -> 'a
  end

  module type Generate = sig
    module Instance : Instance
    module Data : Data.S

    type t [@@deriving equal, quickcheck, sexp_of]

    module Value : sig
      type t = Data.t Instance.t [@@deriving compare, equal, sexp_of]
    end

    val value : t -> Value.t
  end
end

module type Functor = sig
  include module type of struct
    include Definitions
  end

  (** Expect tests for everything exported from [Map.Creators_generic]. *)
  module Test_creators
      (Instance : Instance)
      (Impl : Impl with module Types := Instance.Types) :
    Creators with module Types := Instance.Types

  (** Expect tests for everything exported from [Map.Accessors_generic]. *)
  module Test_accessors
      (Instance : Instance)
      (Impl : Impl with module Types := Instance.Types) :
    Accessors with module Types := Instance.Types

  (** Expect tests for everything exported from [Map.Transformers_generic]. *)
  module Test_transformers
      (Instance : Instance)
      (Impl : Impl with module Types := Instance.Types) :
    Transformers with module Types := Instance.Types

  module Generate
      (Instance : Instance)
      (Impl : Impl with module Types := Instance.Types)
      (Data : Data.S) : Generate with module Instance := Instance and module Data := Data
end
