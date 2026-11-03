(** Provides type-specific conversion functions to and from [string]. *)

open! Import0

(** Templated to provide two [to_string] functions. The first may stack allocate, and
    always takes its input at the same mode as its output. The second may take local
    input, and always produces global output. In the default case of global input and heap
    output, they are equivalent and one shadows the other. *)
module type To_stringable = sig
  type t

  val to_string : t -> string
  val to_string : t -> string
end

module type Of_stringable = sig
  type t

  val of_string : string -> t
end

module type S = sig
  type t

  include Of_stringable with type t := t
  include To_stringable with type t := t
end
