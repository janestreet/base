open! Import

module type S = sig
  type t [@@deriving sexp]
  include Stringable    .S with type t := t
  include Comparable    .S with type t := t
  include Hashable      .S with type t := t
  include Pretty_printer.S with type t := t
end

module Make (T : sig
  type t [@@deriving compare, sexp]
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
  type t [@@deriving compare, sexp]
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
