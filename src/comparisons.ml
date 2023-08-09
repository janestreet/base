(** Interfaces for infix comparison operators and comparison functions. *)

open! Import

(** [Infix] lists the typical infix comparison operators.  These functions are provided by
    [<M>.O] modules, i.e., modules that expose monomorphic infix comparisons over some
    [<M>.t]. *)
module type Infix = sig
  type t

  val ( >= ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( = ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
end

module type S = sig
  include Infix

  val equal : t -> t -> bool

  (** [compare t1 t2] returns 0 if [t1] is equal to [t2], a negative integer if [t1] is
      less than [t2], and a positive integer if [t1] is greater than [t2]. *)
  val compare : t -> t -> int

  val min : t -> t -> t
  val max : t -> t -> t
end

module type S_with_local_opt = sig
  type t

  external ( < ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%lessthan"
  external ( <= ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%lessequal"
  external ( <> ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%notequal"
  external ( = ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%equal"
  external ( > ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%greaterthan"
  external ( >= ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%greaterequal"
  external equal : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%equal"
  external compare : (t[@local_opt]) -> (t[@local_opt]) -> int = "%compare"
  val min : t -> t -> t
  val max : t -> t -> t
end
