open! Base

module Definitions = struct
  module type S = sig
    type t [@@deriving compare, equal, quickcheck, sexp_of]

    val to_int : t -> int
    val of_int : int -> t
    val combine_non_commutative : t -> t -> t
  end

  module type With_equal = sig
    type t [@@deriving equal, sexp_of]
  end

  module type With_quickcheck = sig
    type t [@@deriving equal, quickcheck, sexp_of]
  end
end

module type Data = sig
  include module type of struct
    include Definitions
  end

  module Int : S with type t = int

  (** Functor for [List] *)
  module List (T : With_equal) : With_equal with type t = T.t list

  (** Functor for [Or_error], ignoring error contents when comparing. *)
  module Or_error (T : With_equal) : With_equal with type t = T.t Or_error.t

  (** Functor for [Option] *)
  module Option (T : With_equal) : With_equal with type t = T.t option

  (** Functor for pairs of the same data, with quickcheck generation. *)
  module Pair (T : With_quickcheck) : With_quickcheck with type t = T.t * T.t
end
