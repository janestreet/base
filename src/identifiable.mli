(** a signature for identifier types. *)

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
  include Stringable.S     with type t := t
  include Comparable.S     with type t := t
  include Hashable.S       with type t := t
  include Pretty_printer.S with type t := t
end

(** Used for making an Identifiable module.  Here's an example.

    {[
      module Id = struct
        module T = struct
          type t = A | B [@@deriving_inline compare, hash, sexp][@@@end]
          let of_string s = t_of_sexp (sexp_of_string s)
          let to_string t = string_of_sexp (sexp_of_t t)
          let module_name = "My_library.Std.Id"
        end
        include T
        include Identifiable.Make (T)
      end
    ]} *)
module Make (M : sig
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
    val module_name : string  (** for registering the pretty printer *)
  end) : S
  with type t := M.t

module Make_using_comparator (M : sig
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
  end) : S
  with type t := M.t
  with type comparator_witness := M.comparator_witness
