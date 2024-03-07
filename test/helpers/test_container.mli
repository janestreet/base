open! Base
open! Container

module Test_S1_allow_skipping_tests (Container : sig
  type 'a t [@@deriving sexp]

  include Container.S1 with type 'a t := 'a t

  val of_list : 'a list -> [ `Ok of 'a t | `Skip_test ]
end) : sig
  type 'a t [@@deriving sexp]

  include Generic with type ('a, _, _) t := 'a t

  val mem : 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool
end
with type 'a t := 'a Container.t
with type 'a elt := 'a

module Test_S1 (Container : sig
  type 'a t [@@deriving sexp]

  include Container.S1 with type 'a t := 'a t

  val of_list : 'a list -> 'a t
end) : sig
  type 'a t [@@deriving sexp]

  include Generic with type ('a, _, _) t := 'a t

  val mem : 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool
end
with type 'a t := 'a Container.t
with type 'a elt := 'a

module Test_S0 (Container : sig
  module Elt : sig
    type t [@@deriving sexp]

    val of_int : int -> t
    val to_int : t -> int
  end

  type t [@@deriving sexp]

  include Container.S0 with type t := t and type elt := Elt.t

  val of_list : Elt.t list -> t
end) : sig
  type 'a t [@@deriving sexp]

  include Generic with type ('a, _, _) t := 'a t

  val mem : 'a t -> 'a elt -> bool
end
with type 'a t := Container.t
with type 'a elt := Container.Elt.t
