open! Import

module type S = sig
  type t [@@deriving_inline sexp]
  include
  sig
    [@@@ocaml.warning "-32"]
    val t_of_sexp : Sexplib.Sexp.t -> t
    val sexp_of_t : t -> Sexplib.Sexp.t
  end
  [@@@end]
  include Stringable    .S with type t := t
  include Comparable    .S with type t := t
  include Hashable      .S with type t := t
  include Pretty_printer.S with type t := t
end

module Make (T : sig
    type t [@@deriving_inline compare, sexp]
    include
    sig
      [@@@ocaml.warning "-32"]
      val t_of_sexp : Sexplib.Sexp.t -> t
      val sexp_of_t : t -> Sexplib.Sexp.t
      val compare : t -> t -> int
    end
    [@@@end]
    include Stringable.S with type t := t
    val hash : t -> int
    val module_name : string
  end) = struct
  include T
  include Comparable    .Make     (T)
  include Hashable      .Make     (T)
  include Pretty_printer.Register (T)
end

module Make_using_comparator (T : sig
    type t [@@deriving_inline compare, sexp]
    include
    sig
      [@@@ocaml.warning "-32"]
      val t_of_sexp : Sexplib.Sexp.t -> t
      val sexp_of_t : t -> Sexplib.Sexp.t
      val compare : t -> t -> int
    end
    [@@@end]
    include Comparator.S with type t := t
    include Stringable.S with type t := t
    val hash : t -> int
    val module_name : string
  end) = struct
  include T
  include Comparable    .Make_using_comparator (T)
  include Hashable      .Make                  (T)
  include Pretty_printer.Register              (T)
end
