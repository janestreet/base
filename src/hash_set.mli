
open! Import
open! Hash_set_intf

type 'a t [@@deriving_inline sexp_of]
include
sig
  [@@@ocaml.warning "-32"]
  val sexp_of_t : ('a -> Sexplib.Sexp.t) -> 'a t -> Sexplib.Sexp.t
end
[@@@end]

(** We use [[@@deriving_inline sexp_of][@@@end]] but not [[@@deriving sexp]] because we want people to be
    explicit about the hash and comparison functions used when creating hashtables.  One
    can use [Hash_set.Poly.t], which does have [[@@deriving_inline sexp][@@@end]], to use polymorphic
    comparison and hashing. *)

include Creators
  with type 'a t := 'a t
  with type 'a elt = 'a
  with type ('key, 'z) create_options := ('key, 'z) create_options_with_first_class_module

include Accessors with type 'a t := 'a t with type 'a elt := 'a elt

val hashable : 'key t -> 'key Hashtbl_intf.Hashable.t

(** A hash set that uses polymorphic comparison *)
module Poly : sig

  type nonrec 'a t = 'a t [@@deriving_inline sexp]
  include
  sig
    [@@@ocaml.warning "-32"]
    val t_of_sexp : (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a t
    val sexp_of_t : ('a -> Sexplib.Sexp.t) -> 'a t -> Sexplib.Sexp.t
  end
  [@@@end]

  include Creators
    with type 'a t := 'a t
    with type 'a elt = 'a
    with type ('key, 'z) create_options := ('key, 'z) create_options_without_hashable

  include Accessors with type 'a t := 'a t with type 'a elt := 'a elt

end

(** [M] is meant to be used in combination with OCaml applicative functor types:

    {[
      type string_hash_set = Hash_set.M(String).t
    ]}

    which stands for:

    {[
      type string_hash_set = (String.t, int) Hash_set.t
    ]}

    The point is that [Hash_set.M(String).t] supports deriving, whereas the second
    syntax doesn't (because [t_of_sexp] doesn't know what comparison/hash function to
    use). *)
module M (Elt : T.T) : sig
  type nonrec t = Elt.t t
end
module type Sexp_of_m = sig
  type t [@@deriving_inline sexp_of]
  include sig [@@@ocaml.warning "-32"] val sexp_of_t : t -> Sexplib.Sexp.t end
  [@@@end]
end
module type M_of_sexp = sig
  type t [@@deriving_inline of_sexp]
  include sig [@@@ocaml.warning "-32"] val t_of_sexp : Sexplib.Sexp.t -> t end
  [@@@end]
  include Hashtbl_intf.Key with type t := t
end
val sexp_of_m__t : (module Sexp_of_m with type t = 'elt) -> 'elt t -> Sexp.t
val m__t_of_sexp : (module M_of_sexp with type t = 'elt) -> Sexp.t -> 'elt t

module Creators (Elt : sig
    type 'a t
    val hashable : 'a t Hashtbl_intf.Hashable.t
  end) : sig
  type 'a t_ = 'a Elt.t t
  val t_of_sexp : (Sexp.t -> 'a Elt.t) -> Sexp.t -> 'a t_
  include Creators
    with type 'a t := 'a t_
    with type 'a elt := 'a Elt.t
    with type ('elt, 'z) create_options := ('elt, 'z) create_options_without_hashable
end

module Using_hashable : sig
  include Accessors with type 'a t = 'a t with type 'a elt := 'a elt
  include Creators
    with type 'a t := 'a t
    with type 'a elt = 'a
    with type ('key, 'z) create_options := ('key, 'z) create_options_with_hashable_required
end
