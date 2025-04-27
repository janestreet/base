open! Import

module type Infix = Comparisons.Infix
module type Infix_with_zero_alloc = Comparisons.Infix_with_zero_alloc
module type Comparisons = Comparisons.S
module type Comparisons_with_zero_alloc = Comparisons.S_with_zero_alloc

module Sign = Sign0 (** @canonical Base.Sign *)

(** Various combinators for [compare] and [equal] functions. *)
module type%template With_compare = sig
  (** {!reversed} is the identity type but its associated compare function is the same as
      the {!reverse} function below. It allows you to get reversed comparisons with
      [ppx_compare], writing, for example, [[%compare: string Comparable.reversed]] to
      have strings ordered in the reverse order. *)
  type ('a : any) reversed = 'a

  [@@@mode m = (global, local)]

  type ('a : any, 'b) compare_fn := 'a @ m -> 'a @ m -> 'b
  type ('a, 'b : any) fn := 'a @ m -> 'b @ m
  type ('a : any) select_fn := 'a @ m -> 'a @ m -> 'a @ m

  [@@@mode.default m]
  [@@@kind.default k = (value, float64, bits32, bits64, word)]

  (** [lexicographic cmps x y] compares [x] and [y] lexicographically using functions in
      the list [cmps]. *)
  val lexicographic : ('a : k, int) compare_fn list -> ('a, int) compare_fn

  (** [lift cmp ~f x y] compares [x] and [y] by comparing [f x] and [f y] via [cmp]. *)
  val lift : ('a : k, 'result) compare_fn -> f:('b, 'a) fn -> ('b, 'result) compare_fn

  (** [reverse cmp x y = cmp y x]

      Reverses the direction of asymmetric relations by swapping their arguments. Useful,
      e.g., for relations implementing "is a subset of" or "is a descendant of".

      Where reversed relations are already provided, use them directly. For example,
      [Comparable.S] provides [ascending] and [descending], which are more readable as a
      pair than [compare] and [reverse compare]. Similarly, [<=] is more idiomatic than
      [reverse (>=)]. *)
  val reverse : ('a : k, 'result) compare_fn -> ('a, 'result) compare_fn

  val compare_reversed : ('a : k, int) compare_fn -> ('a reversed, int) compare_fn

  (** The functions below are analogues of the type-specific functions exported by the
      [Comparable.S] interface. *)

  val equal : ('a : k, int) compare_fn -> ('a, bool) compare_fn
  val max : ('a : k, int) compare_fn -> 'a select_fn
  val min : ('a : k, int) compare_fn -> 'a select_fn
end

module type%template With_zero = sig
  type t

  val is_positive : t @ m -> bool
  val is_non_negative : t @ m -> bool
  val is_negative : t @ m -> bool
  val is_non_positive : t @ m -> bool

  (** Returns [Neg], [Zero], or [Pos] in a way consistent with the above functions. *)
  val sign : t @ m -> Sign.t
end
[@@mode m = (global, local)]

module type S = sig
  type t

  include Comparisons with type t := t

  (** [ascending] is identical to [compare]. [descending x y = ascending y x]. These are
      intended to be mnemonic when used like [List.sort ~compare:ascending] and
      [List.sort ~cmp:descending], since they cause the list to be sorted in ascending or
      descending order, respectively. *)
  val ascending : t -> t -> int

  val descending : t -> t -> int

  (** [between t ~low ~high] means [low <= t <= high] *)
  val between : t -> low:t -> high:t -> bool

  (** [clamp_exn t ~min ~max] returns [t'], the closest value to [t] such that
      [between t' ~low:min ~high:max] is true.

      Raises if [not (min <= max)]. *)
  val clamp_exn : t -> min:t -> max:t -> t

  val clamp : t -> min:t -> max:t -> t Or_error.t

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

module type Comparable = sig @@ portable
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

  module type Infix = Infix
  module type Infix_with_zero_alloc = Infix_with_zero_alloc
  module type S = S
  module type Comparisons = Comparisons
  module type Comparisons_with_zero_alloc = Comparisons_with_zero_alloc
  module type With_compare = With_compare
  module type%template With_zero = With_zero [@mode m] [@@mode m = (global, local)]

  include With_compare

  (** Derive [Infix] or [Comparisons] functions from just [[@@deriving compare]], without
      need for the [sexp_of_t] required by [Make*] (see below). *)

  module%template.portable Infix (T : sig
      type t [@@deriving compare]
    end) : Infix with type t := T.t

  module%template.portable Comparisons (T : sig
      type t [@@deriving compare]
    end) : Comparisons with type t := T.t

  (** Inherit comparability from a component. *)
  module%template.portable Inherit
      (C : sig
         type t [@@deriving compare]
       end)
      (T : sig
         type t [@@deriving sexp_of]

         val component : t -> C.t
       end) : S with type t := T.t

  module%template.portable Make (T : sig
      type t [@@deriving compare, sexp_of]

      include Ppx_compare_lib.Comparable.S with type t := t
    end) : S with type t := T.t

  module%template.portable Make_using_comparator (T : sig
      type t [@@deriving sexp_of]

      include Comparator.S with type t := t
    end) : S with type t := T.t with type comparator_witness := T.comparator_witness

  module%template.portable Poly (T : sig
      type t [@@deriving sexp_of]
    end) : S with type t := T.t

  module%template With_zero (T : sig
      type t [@@deriving (compare [@mode m]), sexp_of]

      val zero : t
    end) : sig
    include With_zero [@mode m] with type t := T.t
  end
  [@@mode m = (global, local)]

  module With_zero__portable (T : sig
    @@ portable
      type t : value mod contended [@@deriving compare, sexp_of]

      val zero : t
    end) : sig
    @@ portable
    include With_zero with type t := T.t
  end
end
