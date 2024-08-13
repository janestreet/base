(** Creates types for explicit trees of map constructors. This can be used to test maps
    with coverage over most functions from Map that construct them. *)

open! Base
open Functor_intf.Definitions

module type Constructor = sig
  (** Create a constructor type for maps with [int Types.key] keys and [Data.t] data. *)
  module Make
      (Instance : Instance)
      (Impl : Impl with module Types := Instance.Types) : sig
    type t [@@deriving equal, quickcheck, sexp_of]

    (** Get the actual value the constructor represents. *)
    val value : t -> Instance.t
  end
end
