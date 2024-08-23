(** Provides type-specific conversion functions to and from [string]. *)

open! Import

module type S = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
end

module type S_local_input = sig
  type t

  val of_string : local_ string -> t
  val to_string : local_ t -> string
end
