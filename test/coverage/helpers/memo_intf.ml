open! Base
open Base_quickcheck
open Expect_test_helpers_base

module Definitions = struct
  module type Memoized = sig
    type t [@@deriving quickcheck ~shrinker, sexp_of]

    val sample : t list Lazy.t
  end

  module type Unmemoized = sig
    type t [@@deriving quickcheck ~generator ~shrinker, sexp_of]
  end
end

module type Memo = sig
  include module type of struct
    include Definitions
  end

  val memoize : 'a Generator.t -> 'a list Lazy.t

  val quickcheck_m
    :  here:[%call_pos]
    -> ?cr:CR.t
    -> (module Memoized with type t = 'a)
    -> f:('a -> unit)
    -> unit
end
