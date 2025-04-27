@@ portable

(** Type of S-expressions *)
type t = Sexplib0.Sexp.t =
  | Atom of string
  | List of t list
[@@deriving globalize, hash]

include
  module type of Sexplib0.Sexp
  with type t := Sexplib0.Sexp.t
   and module Private := Sexplib0.Sexp.Private

include Ppx_compare_lib.Equal.S with type t := t
include Ppx_compare_lib.Equal.S__local with type t := t
include Ppx_compare_lib.Comparable.S with type t := t
include Ppx_compare_lib.Comparable.S__local with type t := t

val t_sexp_grammar : t Sexplib0.Sexp_grammar.t
val invariant : t -> unit

(** Base has never had an [of_string] function. We expose a deprecated [of_string] here so
    that people can find it (e.g. with merlin), and learn what we recommend. This
    [of_string] has type [unit] because we don't want it to be accidentally used. *)
val of_string : unit
[@@deprecated "[since 2018-02] Use [Parsexp.Single.parse_string_exn]"]
