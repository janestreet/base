(** This file tests the consistency of [Dictionary_immutable] module types.

    We compare each module type S to the most generic version G that exports the same set
    of values. We create a module type I by instantiating G to mimic S, such as by
    dropping a type parameter. We then test that S = I by writing two identity functors,
    one from S to I and one from I to S. *)

open! Base

module _ : module type of Dictionary_immutable = struct
  (* The generic interface for accessors. *)
  module type Accessors = Dictionary_immutable.Accessors

  (* Ensure that Accessors1 is Accessors with only a data type argument. *)
  module type Accessors1 = Dictionary_immutable.Accessors1

  open struct
    module type Accessors_instance1 = sig
      type key
      type 'data t

      include
        Accessors
          with type _ key := key
           and type (_, 'data, _) t := 'data t
           and type ('fn, _, _, _) accessor := 'fn
    end
  end

  module _ (M : Accessors1) : Accessors_instance1 = M
  module _ (M : Accessors_instance1) : Accessors1 = M

  (* Ensure that Accessors2 is Accessors with no phantom type argument. *)
  module type Accessors2 = Dictionary_immutable.Accessors2

  open struct
    module type Accessors_instance2 = sig
      type ('key, 'data) t
      type ('fn, 'key, 'data) accessor

      include
        Accessors
          with type 'key key := 'key
           and type ('key, 'data, _) t := ('key, 'data) t
           and type ('fn, 'key, 'data, _) accessor := ('fn, 'key, 'data) accessor
    end
  end

  module _ (M : Accessors2) : Accessors_instance2 = M
  module _ (M : Accessors_instance2) : Accessors2 = M

  (* Ensure that Accessors3 is Accessors with no [key] type. *)
  module type Accessors3 = Dictionary_immutable.Accessors3

  open struct
    module type Accessors_instance3 = sig
      type ('key, 'data, 'phantom) t
      type ('fn, 'key, 'data, 'phantom) accessor

      include
        Accessors
          with type 'key key := 'key
           and type ('key, 'data, 'phantom) t := ('key, 'data, 'phantom) t
           and type ('fn, 'key, 'data, 'phantom) accessor :=
            ('fn, 'key, 'data, 'phantom) accessor
    end
  end

  module _ (M : Accessors3) : Accessors_instance3 = M
  module _ (M : Accessors_instance3) : Accessors3 = M

  (* The generic interface for creators. *)
  module type Creators = Dictionary_immutable.Creators

  (* Ensure that Creators1 is Creators with only a data type argument. *)
  module type Creators1 = Dictionary_immutable.Creators1

  open struct
    module type Creators_instance1 = sig
      type key
      type 'data t

      include
        Creators
          with type _ key := key
           and type (_, 'data, _) t := 'data t
           and type ('fn, _, _, _) creator := 'fn
    end
  end

  module _ (M : Creators1) : Creators_instance1 = M
  module _ (M : Creators_instance1) : Creators1 = M

  (* Ensure that Creators2 is Creators with no phantom type argument. *)
  module type Creators2 = Dictionary_immutable.Creators2

  open struct
    module type Creators_instance2 = sig
      type ('key, 'data) t
      type ('fn, 'key, 'data) creator

      include
        Creators
          with type 'key key := 'key
           and type ('key, 'data, _) t := ('key, 'data) t
           and type ('fn, 'key, 'data, _) creator := ('fn, 'key, 'data) creator
    end
  end

  module _ (M : Creators2) : Creators_instance2 = M
  module _ (M : Creators_instance2) : Creators2 = M

  (* Ensure that Creators3 is Creators with no [key] type. *)
  module type Creators3 = Dictionary_immutable.Creators3

  open struct
    module type Creators_instance3 = sig
      type ('key, 'data, 'phantom) t
      type ('fn, 'key, 'data, 'phantom) creator

      include
        Creators
          with type 'key key := 'key
           and type ('key, 'data, 'phantom) t := ('key, 'data, 'phantom) t
           and type ('fn, 'key, 'data, 'phantom) creator :=
            ('fn, 'key, 'data, 'phantom) creator
    end
  end

  module _ (M : Creators3) : Creators_instance3 = M
  module _ (M : Creators_instance3) : Creators3 = M

  (* The generic type for creators + accessors. *)
  module type S = Dictionary_immutable.S

  open struct
    module type Creators_and_accessors = sig
      type 'key key
      type ('key, 'data, 'phantom) t
      type ('fn, 'key, 'data, 'phantom) accessor
      type ('fn, 'key, 'data, 'phantom) creator

      include
        Accessors
          with type 'key key := 'key key
          with type ('key, 'data, 'phantom) t := ('key, 'data, 'phantom) t
          with type ('fn, 'key, 'data, 'phantom) accessor :=
            ('fn, 'key, 'data, 'phantom) accessor

      include
        Creators
          with type 'key key := 'key key
          with type ('key, 'data, 'phantom) t := ('key, 'data, 'phantom) t
          with type ('fn, 'key, 'data, 'phantom) creator :=
            ('fn, 'key, 'data, 'phantom) creator
    end
  end

  module _ (M : S) : Creators_and_accessors = M
  module _ (M : Creators_and_accessors) : S = M

  (* Ensure that S1 is S with only a data type argument. *)
  module type S1 = Dictionary_immutable.S1

  open struct
    module type S_instance1 = sig
      type key
      type 'data t

      include
        S
          with type _ key := key
           and type (_, 'data, _) t := 'data t
           and type ('fn, _, _, _) accessor := 'fn
           and type ('fn, _, _, _) creator := 'fn
    end
  end

  module _ (M : S1) : S_instance1 = M
  module _ (M : S_instance1) : S1 = M

  (* Ensure that S2 is S with no phantom type argument. *)
  module type S2 = Dictionary_immutable.S2

  open struct
    module type S_instance2 = sig
      type ('key, 'data) t
      type ('fn, 'key, 'data) accessor
      type ('fn, 'key, 'data) creator

      include
        S
          with type 'key key := 'key
           and type ('key, 'data, _) t := ('key, 'data) t
           and type ('fn, 'key, 'data, _) accessor := ('fn, 'key, 'data) accessor
           and type ('fn, 'key, 'data, _) creator := ('fn, 'key, 'data) creator
    end
  end

  module _ (M : S2) : S_instance2 = M
  module _ (M : S_instance2) : S2 = M

  (* Ensure that S3 is S with no [key] type. *)
  module type S3 = Dictionary_immutable.S3

  open struct
    module type S_instance3 = sig
      type ('key, 'data, 'phantom) t
      type ('fn, 'key, 'data, 'phantom) accessor
      type ('fn, 'key, 'data, 'phantom) creator

      include
        S
          with type 'key key := 'key
           and type ('key, 'data, 'phantom) t := ('key, 'data, 'phantom) t
           and type ('fn, 'key, 'data, 'phantom) accessor :=
            ('fn, 'key, 'data, 'phantom) accessor
           and type ('fn, 'key, 'data, 'phantom) creator :=
            ('fn, 'key, 'data, 'phantom) creator
    end
  end

  module _ (M : S3) : S_instance3 = M
  module _ (M : S_instance3) : S3 = M
end
