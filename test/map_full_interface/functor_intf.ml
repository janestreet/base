open! Base

module Definitions = struct
  (** The types that distinguish instances of [Map.Creators_and_accessors_generic]. *)
  module type Types = sig
    type 'k key
    type 'c cmp
    type ('k, 'v, 'c) t
    type ('k, 'v, 'c) tree
    type ('k, 'c, 'a) create_options
    type ('k, 'c, 'a) access_options
  end

  (** Like [Map.Creators_and_accessors_generic], but based on [Types] for easier
      instantiation. *)
  module type S = sig
    module Types : Types

    include
      Map.Creators_and_accessors_generic
        with type ('a, 'b, 'c) t := ('a, 'b, 'c) Types.t
        with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Types.tree
        with type 'a key := 'a Types.key
        with type 'a cmp := 'a Types.cmp
        with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) Types.create_options
        with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) Types.access_options
  end

  (** Helpers for testing a tree or map type that is an instance of [S]. *)
  module type Instance = sig
    module Types : Types

    module Key : sig
      type t = int Types.key [@@deriving compare, equal, quickcheck, sexp_of]

      include Comparable.Infix with type t := t
    end

    type 'a t = (int, 'a, Int.comparator_witness) Types.t
    [@@deriving equal, quickcheck, sexp_of]

    (** Construct a [Key.t]. *)
    val key : int -> Key.t

    (** Extract an int from a [Key.t]. *)
    val int : Key.t -> int

    (** Extract a tree (without a comparator) from [t]. *)
    val tree
      :  (Key.t, 'a, Int.comparator_witness) Types.tree
      -> (Key.t, 'a, Int.comparator_witness Types.cmp) Map.Using_comparator.Tree.t

    (** Pass a comparator to a creator function, if necessary. *)
    val create : (int, Int.comparator_witness, 'a) Types.create_options -> 'a

    (** Pass a comparator to an accessor function, if necessary *)
    val access : (int, Int.comparator_witness, 'a) Types.access_options -> 'a
  end
end

module type Functor = sig
  include module type of struct
    include Definitions
  end

  (** Expect tests for everything exported from [Map.Creators_and_accessors_generic]. *)
  module Test_creators_and_accessors
    (Types : Types)
    (Impl : S with module Types := Types)
    (Instance : Instance with module Types := Types) : S with module Types := Types

  (** A functor to generate all of [Instance] but [create] and [access] for a map type. *)
  module Instance (Cmp : sig
    type comparator_witness

    val comparator : (int, comparator_witness) Comparator.t
  end) : sig
    module Key : sig
      type t = int [@@deriving compare, equal, quickcheck, sexp_of]

      include
        Comparator.S with type t := t and type comparator_witness = Cmp.comparator_witness

      include Comparable.Infix with type t := t
    end

    type 'a t = 'a Map.M(Key).t [@@deriving equal, quickcheck, sexp_of]

    val key : 'a -> 'a
    val int : 'a -> 'a
    val tree : 'a -> 'a
  end

  (** A functor like [Instance], but for tree types. *)
  module Instance_tree (Cmp : sig
    type comparator_witness

    val comparator : (int, comparator_witness) Comparator.t
  end) : sig
    module Key : sig
      type t = int [@@deriving compare, equal, quickcheck, sexp_of]

      include
        Comparator.S
          with type t := int
           and type comparator_witness = Cmp.comparator_witness

      include Comparable.Infix with type t := t
    end

    type 'a t = (int, 'a, Cmp.comparator_witness) Map.Using_comparator.Tree.t
    [@@deriving equal, quickcheck, sexp_of]

    val key : 'a -> 'a
    val int : 'a -> 'a
    val tree : 'a -> 'a
  end

  module Ok (T : sig
    type t [@@deriving equal, sexp_of]
  end) : sig
    type t = T.t Or_error.t [@@deriving equal, sexp_of]
  end

  module Pair (T : sig
    type t [@@deriving equal, quickcheck, sexp_of]
  end) : sig
    type t = T.t * T.t [@@deriving equal, quickcheck, sexp_of]
  end
end
