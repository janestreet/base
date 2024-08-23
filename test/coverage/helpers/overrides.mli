open! Base
open Base_quickcheck
open Expect_test_helpers_base
open Memo_intf.Definitions

module Either : sig
  include module type of struct
    include Base.Either
  end

  type ('a, 'b) t = ('a, 'b) Either.t [@@deriving quickcheck]
end

module Maybe_bound : sig
  include module type of struct
    include Base.Maybe_bound
  end

  type 'a t = 'a Maybe_bound.t [@@deriving equal, quickcheck]

  val to_list : 'a t -> 'a list
end

val quickcheck_generator_int : int Generator.t
val quickcheck_generator_list : 'a Generator.t -> 'a list Generator.t

val quickcheck_m
  :  here:[%call_pos]
  -> ?cr:CR.t
  -> (module Memoized with type t = 'a)
  -> f:('a -> unit)
  -> unit
