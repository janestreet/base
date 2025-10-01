(** Provides type-specific conversion functions to and from [string]. *)

open! Import

module type%template [@alloc a @ m = (heap_global, stack_local)] S = sig
  type t

  val of_string : string -> t
  val to_string : t @ m -> string @ m [@@alloc a @ m = (a @ m, heap_global)]
end

module type S_local_input = sig
  type t

  val of_string : local_ string -> t
  val to_string : local_ t -> string
end
