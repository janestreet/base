@@ portable
   (*_ This module is separated from Sexp to avoid circular dependencies as many things
       use s-expressions *)

(** @inline *)
include module type of struct
  include Sexp0
end

include%template Comparable.S [@mode local] [@modality portable] with type t := t

(** A witness that [Sexp.t] can safely cross portability. *)
val cross_portable : t Basement.Portability_hacks.Cross.Portable.t

(** A witness that [Sexp.t] can safely cross contention. *)
val cross_contended : t Basement.Portability_hacks.Cross.Contended.t

module type Pretty_printing = Sexplib0.Sexp.Pretty_printing (** @inline *)

(** Like {!Pretty_printing}, but specialized to [string] output. Useful for making packed
    modules since [(module Pretty_printing with type output := string)] is not valid
    syntax. *)
module type Pretty_printing_to_string = Pretty_printing with type output := string

include Pretty_printing_to_string

(** Like the printing functions above, but without escaping UTF-8 characters. Still
    escapes ASCII control codes, but does not escape other non-graphical UTF-8 characters.

    The [to_string*] functions return [String.Utf8.t], which is a subtype of [string]. *)
module Utf8 : Pretty_printing with type output := String.Utf8.t
(** inline *)

(** Like {!Utf8}, but prints to [string]s rather than [String.Utf8.t]. This mostly exists
    to make it easier to migrate to utf8 based printing, e.g. with
    {[
      module Sexp = struct
        include Sexp
        include Utf8_as_string
      end
    ]} *)
module Utf8_as_string : Pretty_printing_to_string
(** inline *)

module Private : sig
  module Utf8 : sig
    val escaped : string -> quoted:bool -> string
  end
end
[@@alert
  base_sexp_private "This module is intended only for use in testing [Base.Sexp] itself."]
