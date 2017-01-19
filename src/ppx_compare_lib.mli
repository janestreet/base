(** Runtime support for auto-generated comparators.  Users are not intended to use this
    module directly. *)

val phys_equal : 'a -> 'a -> bool
external polymorphic_compare : 'a -> 'a -> int  = "%compare"

type 'a compare = 'a -> 'a -> int

(** Raise when fully applied *)
val compare_abstract : type_name:string -> _ compare

module Builtin : sig
  type 'a t = 'a -> 'a -> int

  val compare_bool      : bool      t
  val compare_char      : char      t
  val compare_float     : float     t
  val compare_int       : int       t
  val compare_int32     : int32     t
  val compare_int64     : int64     t
  val compare_nativeint : nativeint t
  val compare_string    : string    t
  val compare_unit      : unit      t

  val compare_array  : 'a t -> 'a array  t
  val compare_list   : 'a t -> 'a list   t
  val compare_option : 'a t -> 'a option t
  val compare_ref    : 'a t -> 'a ref    t
end with type 'a t := 'a compare
