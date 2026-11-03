(** Provides type-specific conversion functions to and from [string]. *)

open! Import0

(** Templated to provide two [to_string] functions. The first may stack allocate, and
    always takes its input at the same mode as its output. The second may take local
    input, and always produces global output. In the default case of global input and heap
    output, they are equivalent and one shadows the other. *)
module type%template
  [@mode l = (local, global)] [@alloc a @ m = (heap_global, stack_local)]
  To_stringable = sig
  type t

  val to_string : t @ m -> string @ m [@@alloc a]
  val to_string : t @ l -> string
end

module type%template [@mode l = (local, global)] Of_stringable = sig
  type t

  val of_string : string @ l -> t
end

module type%template [@mode l = (local, global)] [@alloc a = (stack, heap)] S = sig
  type t

  include Of_stringable [@mode l] with type t := t
  include To_stringable [@mode l] [@alloc a] with type t := t
end
