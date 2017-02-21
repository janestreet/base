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

  (** -1 means "less than", 0 means "equal", 1 means "greater than", and other values
      should not be returned *)
  val compare : t -> t -> int

  val min     : t -> t -> t
  val max     : t -> t -> t
end
