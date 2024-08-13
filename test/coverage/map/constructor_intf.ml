(** Creates types for explicit trees of map constructors. This can be used to test maps
    with coverage over most functions from Map that construct them. *)

open! Base
open Base_test_coverage_helpers
open Functor_intf.Definitions

module type Constructor = sig
  (** Create a constructor type for maps with [int Types.key] keys and [Data.t] data.
      Excludes functions that operate on restricted types of data, like [of_list_multi] or
      [unzip]. *)
  module Make
      (Instance : Instance)
      (Impl : Impl with module Types := Instance.Types)
      (Data : Data.S) : sig
    type t [@@deriving equal, quickcheck, sexp_of]

    (** Get the actual value the constructor represents. *)
    val value : t -> Data.t Instance.t
  end
end
