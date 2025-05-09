(** Functor that adds integer conversion functions to a module. *)

open! Import

module type S = sig
  type t

  val of_int_exn : int -> t
  val to_int_exn : t -> int
end

module type S_with_zero_alloc = sig
  type t

  val of_int_exn : int -> t [@@zero_alloc]
  val to_int_exn : t -> int [@@zero_alloc]
end
