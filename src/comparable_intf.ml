open! Import0
module Error = Error0
module Sign = Sign0

module Definitions = struct
  module type Infix = Comparisons.Infix
  module type Infix_with_zero_alloc = Comparisons.Infix_with_zero_alloc
  module type Comparisons = Comparisons.S
  module type Comparisons_with_zero_alloc = Comparisons.S_with_zero_alloc

  (** Various combinators for [compare] and [equal] functions. *)
  module type With_compare = sig
    (** {!reversed} is the identity type but its associated compare function is the same
        as the {!reverse} function below. It allows you to get reversed comparisons with
        [ppx_compare], writing, for example, [[%compare: string Comparable.reversed]] to
        have strings ordered in the reverse order. *)
    type 'a reversed = 'a

    type ('a, 'b) compare_fn := 'a -> 'a -> 'b
    type ('a, 'b) fn := 'a -> 'b
    type 'a select_fn := 'a -> 'a -> 'a

    (** [lexicographic cmps x y] compares [x] and [y] lexicographically using functions in
        the list [cmps]. *)
    val lexicographic : 'a. ('a, int) compare_fn list -> ('a, int) compare_fn

    (** [lift cmp ~f x y] compares [x] and [y] by comparing [f x] and [f y] via [cmp]. *)
    val lift
      : 'a 'b 'result.
      ('a, 'result) compare_fn -> f:('b, 'a) fn -> ('b, 'result) compare_fn

    (** [reverse cmp x y = cmp y x]

        Reverses the direction of asymmetric relations by swapping their arguments.
        Useful, e.g., for relations implementing "is a subset of" or "is a descendant of".

        Where reversed relations are already provided, use them directly. For example,
        [Comparable.S] provides [ascending] and [descending], which are more readable as a
        pair than [compare] and [reverse compare]. Similarly, [<=] is more idiomatic than
        [reverse (>=)]. *)
    val reverse : 'a 'result. ('a, 'result) compare_fn -> ('a, 'result) compare_fn

    val compare_reversed : 'a. ('a, int) compare_fn -> ('a reversed, int) compare_fn

    (** The functions below are analogues of the type-specific functions exported by the
        [Comparable.S] interface. *)

    val equal : 'a. ('a, int) compare_fn -> ('a, bool) compare_fn
    val max : 'a. ('a, int) compare_fn -> 'a select_fn
    val min : 'a. ('a, int) compare_fn -> 'a select_fn
  end

  module type Using_comparator_arg = sig
    type t

    include Comparator.S with type t := t
  end

  module type With_zero = sig
    type t

    val is_positive : t -> bool
    val is_non_negative : t -> bool
    val is_negative : t -> bool
    val is_non_positive : t -> bool

    (** Returns [Neg], [Zero], or [Pos] in a way consistent with the above functions. *)
    val sign : t -> Sign.t
  end

  module type S = sig
    type t

    include Comparisons with type t := t

    (** [ascending] is identical to [compare]. [descending x y = ascending y x]. These are
        intended to be mnemonic when used like [List.sort ~compare:ascending] and
        [List.sort ~cmp:descending], since they cause the list to be sorted in ascending
        or descending order, respectively. *)
    val ascending : t -> t -> int

    val descending : t -> t -> int

    (** [between t ~low ~high] means [low <= t <= high] *)
    val between : t -> low:t -> high:t -> bool

    (** [clamp_exn t ~min ~max] returns [t'], the closest value to [t] such that
        [between t' ~low:min ~high:max] is true.

        Raises if [not (min <= max)]. *)
    val clamp_exn : t -> min:t -> max:t -> t

    (*_ We avoid a dependency on [Or_error] by writing [(_, Error.t) Result.t]. *)
    val clamp : t -> min:t -> max:t -> (t, Error.t) Result.t

    include Comparator.S with type t := t
  end

  (** Usage example:

      {[
        module Foo : sig
          type t = ...
          include Comparable.S with type t := t
        end
      ]}

      Then use [Comparable.Make] in the struct (see comparable.mli for an example). *)
end

module type Comparable = sig
  include module type of struct
    include Definitions
  end

  (** Defines functors for making modules comparable. *)

  (** Usage example:

      {[
        module Foo = struct
          module T = struct
            type t = ... [@@deriving compare, sexp]
          end
          include T
          include Comparable.Make (T)
        end
      ]}

      Then include [Comparable.S] in the signature

      {[
        module Foo : sig
          type t = ...
          include Comparable.S with type t := t
        end
      ]}

      To add an [Infix] submodule:

      {[
        module C = Comparable.Make (T)
        include C
        module Infix : Comparable.Infix with type t := t = C
      ]}

      A common pattern is to define a module [O] with a restricted signature. It aims to
      be (locally) opened to bring useful operators into scope without shadowing
      unexpected variable names. E.g., in the [Date] module:

      {[
        module O = struct
          include (C : Comparable.Infix with type t := t)
          let to_string t = ..
        end
      ]}

      Opening [Date] would shadow [now], but opening [Date.O] doesn't:

      {[
        let now = .. in
        let someday = .. in
        Date.O.(now > someday)
      ]}

      {2 Mode-crossing comparators}

      To make the [Comparator.t] for a type (and hence maps built with that comparator)
      cross portability in OxCaml, add the [[@portable]] template attribute to
      [Comparable.Make] and [Comparable.S]. For example:

      {[
        module Foo : sig
          type t [@@deriving compare, sexp]

          include%template Comparable.S [@modality portable] with type t := t
        end = struct
          type t = ... [@@deriving compare, sexp]

          include%template functor Comparable.Make [@modality portable]
        end
      ]} *)

  include With_compare

  (** Derive [Infix] or [Comparisons] functions from just [[@@deriving compare]], without
      need for the [sexp_of_t] required by [Make*] (see below). *)

  module Infix (T : sig
      type t [@@deriving compare]
    end) : Infix with type t := T.t

  module Infix_with_zero_alloc (T : sig
      type t [@@deriving compare ~zero_alloc]
    end) : Infix_with_zero_alloc with type t := T.t

  module Comparisons (T : sig
      type t [@@deriving compare]
    end) : Comparisons with type t := T.t

  module Comparisons_with_zero_alloc (T : sig
      type t [@@deriving compare ~zero_alloc]
    end) : Comparisons_with_zero_alloc with type t := T.t

  (** Inherit comparability from a component. *)
  module Inherit
      (C : sig
         type t [@@deriving compare]
       end)
      (T : sig
         type t [@@deriving sexp_of]

         val component : t -> C.t
       end) : S with type t := T.t

  (** Note that the template parameter of the [Make] functor controls not just the
      portability of the argument and return modules, but also the {i mode crossing} of
      the created [Comparator.t] (and hence the mode crossing of {!Map.t} with this type
      as a key). See {!Map} for more information *)
  module Make (T : sig
      type t [@@deriving compare, sexp_of]
    end) : S with type t := T.t

  module Make_using_comparator (T : sig
      type t [@@deriving sexp_of]

      include Using_comparator_arg with type t := t
    end) : S with type t := T.t with type comparator_witness := T.comparator_witness

  module Poly (T : sig
      type t [@@deriving sexp_of]
    end) : S with type t := T.t

  module With_zero (T : sig
      type t [@@deriving compare, sexp_of]

      val zero : t
    end) : sig
    include With_zero with type t := T.t
  end
end
