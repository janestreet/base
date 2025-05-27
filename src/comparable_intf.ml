open! Import

module Definitions = struct
  module type Infix = Comparisons.Infix
  module type Infix_with_zero_alloc = Comparisons.Infix_with_zero_alloc

  [%%template
  [@@@mode.default m = (global, local)]

  module type Comparisons = Comparisons.S [@mode m]
  module type Comparisons_with_zero_alloc = Comparisons.S_with_zero_alloc [@mode m]]

  (** Various combinators for [compare] and [equal] functions. *)
  module type%template With_compare = sig
    (** {!reversed} is the identity type but its associated compare function is the same
        as the {!reverse} function below. It allows you to get reversed comparisons with
        [ppx_compare], writing, for example, [[%compare: string Comparable.reversed]] to
        have strings ordered in the reverse order. *)
    type 'a reversed = 'a

    [@@@mode m = (global, local)]

    type ('a, 'b) compare_fn := 'a -> 'a -> 'b
    type ('a, 'b) fn := 'a -> 'b
    type 'a select_fn := 'a -> 'a -> 'a

    [@@@mode.default m]
    [@@@kind.default k = (value, float64, bits32, bits64, word)]

    (** [lexicographic cmps x y] compares [x] and [y] lexicographically using functions in
        the list [cmps]. *)
    val lexicographic : ('a, int) compare_fn list -> ('a, int) compare_fn

    (** [lift cmp ~f x y] compares [x] and [y] by comparing [f x] and [f y] via [cmp]. *)
    val lift : ('a, 'result) compare_fn -> f:('b, 'a) fn -> ('b, 'result) compare_fn

    (** [reverse cmp x y = cmp y x]

        Reverses the direction of asymmetric relations by swapping their arguments.
        Useful, e.g., for relations implementing "is a subset of" or "is a descendant of".

        Where reversed relations are already provided, use them directly. For example,
        [Comparable.S] provides [ascending] and [descending], which are more readable as a
        pair than [compare] and [reverse compare]. Similarly, [<=] is more idiomatic than
        [reverse (>=)]. *)
    val reverse : ('a, 'result) compare_fn -> ('a, 'result) compare_fn

    val compare_reversed : ('a, int) compare_fn -> ('a reversed, int) compare_fn

    (** The functions below are analogues of the type-specific functions exported by the
        [Comparable.S] interface. *)

    val equal : ('a, int) compare_fn -> ('a, bool) compare_fn
    val max : ('a, int) compare_fn -> 'a select_fn
    val min : ('a, int) compare_fn -> 'a select_fn
  end

  module type%template
    [@mode global] [@modality p = (portable, nonportable)] Using_comparator_arg = sig
    type t

    include Comparator.S [@modality p] with type t := t
  end

  module type%template
    [@mode local] [@modality p = (portable, nonportable)] Using_comparator_arg = sig
    type t [@@deriving compare [@mode local]]

    include Comparator.S [@modality p] with type t := t
  end

  module type%template With_zero = sig
    type t

    val is_positive : t -> bool
    val is_non_negative : t -> bool
    val is_negative : t -> bool
    val is_non_positive : t -> bool

    (** Returns [Neg], [Zero], or [Pos] in a way consistent with the above functions. *)
    val sign : t -> Sign0.t
  end
  [@@mode m = (global, local)]

  module type%template [@modality p = (portable, nonportable)] S = sig
    type t

    include Comparisons [@mode m] with type t := t

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

    val clamp : t -> min:t -> max:t -> t Or_error.t

    include Comparator.S [@modality p] with type t := t
  end
  [@@mode m = (global, local)]

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
      ]} *)

  include With_compare

  (** Derive [Infix] or [Comparisons] functions from just [[@@deriving compare]], without
      need for the [sexp_of_t] required by [Make*] (see below). *)

  [%%template:
  [@@@mode.default m = (local, global)]

  module%template.portable Infix (T : sig
      type t [@@deriving compare [@mode m]]
    end) : Infix with type t := T.t

  module%template.portable Comparisons (T : sig
      type t [@@deriving compare [@mode m]]
    end) : Comparisons [@mode m] with type t := T.t

  (** Inherit comparability from a component. *)
  module%template.portable Inherit
      (C : sig
         type t [@@deriving compare [@mode m]]
       end)
      (T : sig
         type t [@@deriving sexp_of]

         val component : t -> C.t
       end) : S [@mode m] with type t := T.t

  module%template.portable
    [@modality p] Make (T : sig
      type t [@@deriving (compare [@mode m]), sexp_of]
    end) : S [@modality p] [@mode m] with type t := T.t

  module%template.portable Make_using_comparator (T : sig
      type t [@@deriving sexp_of]

      include Using_comparator_arg [@mode m] with type t := t
    end) :
    S [@mode m] with type t := T.t with type comparator_witness := T.comparator_witness

  module%template.portable
    [@modality p] Poly (T : sig
      type t [@@deriving sexp_of]
    end) : S [@modality p] [@mode m] with type t := T.t

  module With_zero (T : sig
      type t [@@deriving (compare [@mode m]), sexp_of]

      val zero : t
    end) : sig
    include With_zero [@mode m] with type t := T.t
  end

  module With_zero (T : sig
      type t [@@deriving (compare [@mode m]), sexp_of]

      val zero : t
    end) : sig
    include With_zero [@mode m] with type t := T.t
  end
  [@@modality portable]]
end
