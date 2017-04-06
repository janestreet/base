(** Interfaces used for hiding and replacing polymorphic compare.  Including a module with
    interface [S] should hide the majority of functions that use polymorphic compare.  *)

open! Import

module type Infix = sig
  type t
  val ( >= ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( =  ) : t -> t -> bool
  val ( >  ) : t -> t -> bool
  val ( <  ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
end

module type S = sig
  include Infix

  val equal   : t -> t -> bool

  (** [compare t1 t2] returns 0 if [t1] is equal to [t2], a negative integer if [t1] is
      less than [t2], and a positive integer if [t1] is greater than [t2]. *)
  val compare : t -> t -> int

  val min     : t -> t -> t
  val max     : t -> t -> t
end
