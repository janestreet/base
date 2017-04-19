open! Import
open! Comparable_intf

module type Infix     = Infix
module type S         = S
module type Validate  = Validate
module type With_zero = With_zero

(** [lexicographic cmps x y] compares [x] and [y] lexicographically using functions in the
    list [cmps]. *)
val lexicographic : ('a -> 'a -> int) list -> 'a -> 'a -> int

(** [lift cmp ~f x y] compares [x] and [y] by comparing [f x] and [f y] via [cmp]. *)
val lift : ('a -> 'a -> 'int_or_bool) -> f:('b -> 'a) -> ('b -> 'b -> 'int_or_bool)

(** Inherit comparability from a component. *)
module Inherit
    (C : sig type t [@@deriving_inline compare]
       include sig [@@@ocaml.warning "-32"] val compare : t -> t -> int end
       [@@@end] end)
    (T : sig
       type t [@@deriving_inline sexp_of]
       include sig [@@@ocaml.warning "-32"] val sexp_of_t : t -> Sexplib.Sexp.t end
       [@@@end]
       val component : t -> C.t
     end) : S with type t := T.t

(** Usage example:

    {[
      module Foo = struct
        module T = struct
          type t = ... [@@deriving_inline compare, sexp][@@@end]
        end
        include T
        include Comparable.Make (T)
      end
    ]}

    Then include [Comparable.S] in the signature (see comparable_intf.mli for an
    example).

    To add an [Infix] submodule:

    {[
      module C = Comparable.Make (T)
      include C
      module Infix = (C : Comparable.Infix with type t := t)
    ]}

    Common pattern: Define a module [O] with a restricted signature.  It aims to be
    (locally) opened to bring useful operators into scope without shadowing unexpected
    variable names.  E.g. in the [Date] module:

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

module Make (T : sig
    type t [@@deriving_inline compare, sexp_of]
    include
    sig
      [@@@ocaml.warning "-32"]
      val compare : t -> t -> int
      val sexp_of_t : t -> Sexplib.Sexp.t
    end
    [@@@end]
  end) : S with type t := T.t

module Make_using_comparator (T : sig
    type t [@@deriving_inline sexp_of]
    include sig [@@@ocaml.warning "-32"] val sexp_of_t : t -> Sexplib.Sexp.t end
    [@@@end]
    include Comparator.S with type t := t
  end) : S
  with type t := T.t
  with type comparator_witness := T.comparator_witness

module Poly (T : sig type t [@@deriving_inline sexp_of]
    include sig [@@@ocaml.warning "-32"] val sexp_of_t : t -> Sexplib.Sexp.t end
    [@@@end] end) : S with type t := T.t

module Validate (T : sig type t [@@deriving_inline compare, sexp_of]
    include
    sig
      [@@@ocaml.warning "-32"]
      val compare : t -> t -> int
      val sexp_of_t : t -> Sexplib.Sexp.t
    end
    [@@@end] end)
  : Validate with type t := T.t

module With_zero
    (T : sig
       type t [@@deriving_inline compare, sexp_of]
       include
       sig
         [@@@ocaml.warning "-32"]
         val compare : t -> t -> int
         val sexp_of_t : t -> Sexplib.Sexp.t
       end
       [@@@end]
       val zero : t
       include Validate with type t := t
     end) : With_zero with type t := T.t

module Validate_with_zero
    (T : sig
       type t [@@deriving_inline compare, sexp_of]
       include
       sig
         [@@@ocaml.warning "-32"]
         val compare : t -> t -> int
         val sexp_of_t : t -> Sexplib.Sexp.t
       end
       [@@@end]
       val zero : t
     end)
  : sig
    include Validate  with type t := T.t
    include With_zero with type t := T.t
  end
