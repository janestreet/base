open! Base

(* Typechecking this code is a compile-time check that the specific interfaces have not
   drifted apart from each other. *)

module _ : sig
  open Set

  type ('a, 'b) t

  include
    Creators_and_accessors_generic
      with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) Without_comparator.t
      with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) With_first_class_module.t
      with type ('a, 'b) set := ('a, 'b) t
      with type ('a, 'b) t := ('a, 'b) t
      with type ('a, 'b) tree := ('a, 'b) Set.Using_comparator.Tree.t
      with type 'a elt := 'a
      with type 'c cmp := 'c
end =
  Set
