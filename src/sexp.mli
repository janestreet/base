@@ portable
   (*_ This module is separated from Sexp to avoid circular dependencies as many things use
  s-expressions *)

(** @inline *)
include module type of struct
  include Sexp0
end

include%template Comparable.S [@mode local] [@modality portable] with type t := t

(** A witness that [Sexp.t] can safely cross portability. *)
val cross_portable : t Basement.Portability_hacks.Cross.Portable.t

(** A witness that [Sexp.t] can safely cross contention. *)
val cross_contended : t Basement.Portability_hacks.Cross.Contended.t

(** Like the printing functions above, but without escaping UTF-8 characters. Still
    escapes ASCII control codes, but does not escape other non-graphical UTF-8 characters.
    The [to_string*] functions return [String.Utf8.t], which is a subtype of [string]. *)
module Utf8 : sig
  val to_string : t -> String.Utf8.t
  val to_string_mach : t -> String.Utf8.t
  val to_string_hum : ?indent:int -> t -> String.Utf8.t
  val pp_hum : Format.formatter -> t -> unit
  val pp_hum_indent : int -> Format.formatter -> t -> unit
  val pp_mach : Format.formatter -> t -> unit
  val pp : Format.formatter -> t -> unit
  val must_escape : string -> bool
  val mach_maybe_esc_str : string -> string
  val esc_str : string -> string
end

module Private : sig
  module Utf8 : sig
    val escaped : string -> quoted:bool -> string
  end
end
[@@alert
  base_sexp_private "This module is intended only for use in testing [Base.Sexp] itself."]
