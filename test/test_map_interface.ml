open! Base

(* Typechecking this code is a compile-time check that the specific interfaces have not
   drifted apart from each other. *)

module _ : sig
  open Map

  type ('a, 'b, 'c) t

  include
    Creators_and_accessors_generic
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Map.Using_comparator.Tree.t
      with type 'k key := 'k
      with type 'c cmp := 'c
      with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) Without_comparator.t
      with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) With_first_class_module.t
end =
  Map
