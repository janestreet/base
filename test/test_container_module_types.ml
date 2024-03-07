(** This file tests the consistency of [Container] and [Indexed_container] module types.

    We compare each module type S to the most generic version G that exports the same set
    of values. We create a module type I by instantiating G to mimic S, such as by
    dropping a type parameter. We then test that S = I by writing two identity functors,
    one from S to I and one from I to S. *)

open! Base

module _ : module type of Container = struct
  (* The most generic interface that everything else implements. *)
  module type Generic = Container.Generic

  (* The generic interface with creator functions. Ensure it implements Generic. *)
  module type Generic_with_creators = Container.Generic_with_creators

  module _ (M : Container.Generic_with_creators) : Container.Generic = M

  (* Ensure that S0 is Generic with no type arguments. *)
  module type S0 = Container.S0

  open struct
    module type Generic0 = sig
      type elt
      type t

      include Generic with type _ elt := elt and type (_, _, _) t := t

      val mem : t -> elt -> bool
    end
  end

  module _ (M : S0) : Generic0 = M
  module _ (M : Generic0) : S0 = M

  (* Ensure that S0_phantom is Generic with a fixed element type. *)
  module type S0_phantom = Container.S0_phantom

  open struct
    module type Generic0_phantom = sig
      type elt
      type _ t

      include Container.Generic with type _ elt := elt and type (_, 'p, _) t := 'p t

      val mem : _ t -> elt -> bool
    end
  end

  module _ (M : S0_phantom) : Generic0_phantom = M
  module _ (M : Generic0_phantom) : S0_phantom = M

  (* Ensure that S0_with_creators is Generic_with_creators with no type arguments. *)
  module type S0_with_creators = Container.S0_with_creators

  open struct
    module type Generic0_with_creators = sig
      type elt
      type t

      include
        Generic_with_creators
          with type _ elt := elt
           and type (_, _, _) t := t
           and type ('a, _, _) concat := 'a list

      val mem : t -> elt -> bool
    end
  end

  module _ (M : S0_with_creators) : Generic0_with_creators = M
  module _ (M : Generic0_with_creators) : S0_with_creators = M

  (* Ensure that S1 is Generic with no phantom type. *)
  module type S1 = Container.S1

  open struct
    module type Generic1 = sig
      type _ t

      include Container.Generic with type 'a elt := 'a and type ('a, _, _) t := 'a t
    end
  end

  module _ (M : S1) : Generic1 = M
  module _ (M : Generic1) : S1 = M

  (* Ensure that S1_phantom is Generic with a covariant phantom type. *)
  module type S1_phantom = Container.S1_phantom

  open struct
    module type Generic1_phantom = sig
      type (_, _) t

      include Generic with type 'a elt := 'a and type ('a, 'p, _) t := ('a, 'p) t
    end
  end

  module _ (M : S1_phantom) : Generic1_phantom = M
  module _ (M : Generic1_phantom) : S1_phantom = M

  (* Ensure that S1_with_creators is Generic_with_creators with no phantom type. *)
  module type S1_with_creators = Container.S1_with_creators

  open struct
    module type Generic1_with_creators = sig
      type 'a t

      include
        Generic_with_creators
          with type 'a elt := 'a
           and type ('a, _, _) t := 'a t
           and type ('a, _, _) concat := 'a t
    end
  end

  module _ (M : S1_with_creators) : Generic1_with_creators = M
  module _ (M : Generic1_with_creators) : S1_with_creators = M

  (* Other definitions that we are not testing: *)

  module Continue_or_stop = Container.Continue_or_stop
  module Make = Container.Make
  module Make0 = Container.Make0
  module Make_gen = Container.Make_gen
  module Make_with_creators = Container.Make_with_creators
  module Make0_with_creators = Container.Make0_with_creators
  module Make_gen_with_creators = Container.Make_gen_with_creators

  module type Derived = Container.Derived
  module type Summable = Container.Summable

  include (Container : Derived)
end

module _ : module type of Indexed_container = struct
  (* The generic interface everything else implements. *)
  module type Generic = Indexed_container.Generic

  (* Ensure that S0 is Generic without type parameters. *)
  module type S0 = Indexed_container.S0

  open struct
    module type Generic0 = sig
      type elt
      type t

      include Generic with type _ elt := elt and type (_, _, _) t := t

      val mem : t -> elt -> bool
    end
  end

  module _ (M : S0) : Generic0 = M
  module _ (M : Generic0) : S0 = M

  (* Ensure that S1 is Generic without an abstract element type. *)
  module type S1 = Indexed_container.S1

  open struct
    module type Generic1 = sig
      type 'a t

      include Generic with type 'a elt := 'a and type ('a, _, _) t := 'a t
    end
  end

  module _ (M : S1) : Generic1 = M
  module _ (M : Generic1) : S1 = M

  (* Ensure that Generic_with_creators includes Generic. *)
  module type Generic_with_creators = Indexed_container.Generic_with_creators

  module _ (M : Indexed_container.Generic_with_creators) : Indexed_container.Generic = M

  (* Ensure that S0_with_creators is Generic_with_creators with no type arguments. *)
  module type S0_with_creators = Indexed_container.S0_with_creators

  open struct
    module type Generic0_with_creators = sig
      type elt
      type t

      include
        Generic_with_creators
          with type _ elt := elt
           and type (_, _, _) t := t
           and type ('a, _, _) concat := 'a list

      val mem : t -> elt -> bool
    end
  end

  module _ (M : S0_with_creators) : Generic0_with_creators = M
  module _ (M : Generic0_with_creators) : S0_with_creators = M

  (* Ensure that S1_with_creators is Generic_with_creators with no phantom type. *)
  module type S1_with_creators = Indexed_container.S1_with_creators

  open struct
    module type Generic1_with_creators = sig
      type 'a t

      include
        Generic_with_creators
          with type 'a elt := 'a
           and type ('a, _, _) t := 'a t
           and type ('a, _, _) concat := 'a t
    end
  end

  module _ (M : S1_with_creators) : Generic1_with_creators = M
  module _ (M : Generic1_with_creators) : S1_with_creators = M

  (* Other definitions that we are not testing: *)

  module Make = Indexed_container.Make
  module Make0 = Indexed_container.Make0
  module Make_gen = Indexed_container.Make_gen
  module Make_with_creators = Indexed_container.Make_with_creators
  module Make0_with_creators = Indexed_container.Make0_with_creators
  module Make_gen_with_creators = Indexed_container.Make_gen_with_creators

  module type Derived = Indexed_container.Derived

  include (Indexed_container : Derived)
end
