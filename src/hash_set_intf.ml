
open! Import

module type Accessors = sig
  include Container.Generic

  val mem : 'a t -> 'a -> bool (** override [Container.Generic.mem] *)

  val copy : 'a t -> 'a t      (** preserves the equality function *)

  val add               : 'a t -> 'a -> unit
  val strict_add        : 'a t -> 'a -> unit Or_error.t
  val strict_add_exn    : 'a t -> 'a -> unit
  val remove            : 'a t -> 'a -> unit
  val strict_remove     : 'a t -> 'a -> unit Or_error.t
  val strict_remove_exn : 'a t -> 'a -> unit
  val clear : 'a t -> unit
  val equal : 'a t -> 'a t -> bool
  val filter : 'a t -> f:('a -> bool) -> 'a t
  val filter_inplace : 'a t -> f:('a -> bool) -> unit

  (** [inter t1 t2] computes the set intersection of [t1] and [t2].  Runs in O(max(length
      t1, length t2)).  Behavior is undefined if [t1] and [t2] don't have the same
      equality function. *)
  val inter : 'key t -> 'key t -> 'key t
  val diff  : 'a   t -> 'a   t -> 'a   t

  val of_hashtbl_keys : ('a, _) Hashtbl.t -> 'a t
  val to_hashtbl : 'key t -> f:('key -> 'data) -> ('key, 'data) Hashtbl.t
end

type ('key, 'z) create_options_without_hashable =
  ('key, unit, 'z) Hashtbl_intf.create_options_without_hashable

type ('key, 'z) create_options_with_hashable_required =
  ('key, unit, 'z) Hashtbl_intf.create_options_with_hashable

type ('key, 'z) create_options_with_first_class_module =
  ('key, unit, 'z) Hashtbl_intf.create_options_with_first_class_module

module type Creators = sig
  type 'a t
  type 'a elt
  type ('a, 'z) create_options

  val create  : ('a, unit        -> 'a t) create_options
  val of_list : ('a, 'a elt list -> 'a t) create_options
end
