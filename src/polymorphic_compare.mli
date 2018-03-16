(** A module containing the ad-hoc polymorphic comparison functions.  Useful when
    you want to use polymorphic compare in some small scope of a file within which
    polymorphic compare has been hidden *)

open! Import0

val compare    : 'a -> 'a -> int

(** [ascending] is identical to [compare]. [descending x y = ascending y x].  These are
    intended to be mnemonic when used like [List.sort ~compare:ascending] and [List.sort
    ~cmp:descending], since they cause the list to be sorted in ascending or descending
    order, respectively. *)
val ascending  : 'a -> 'a -> int
val descending : 'a -> 'a -> int

val (<)     : 'a -> 'a -> bool
val (<=)    : 'a -> 'a -> bool
val (>)     : 'a -> 'a -> bool
val (>=)    : 'a -> 'a -> bool
val (=)     : 'a -> 'a -> bool
val (<>)    : 'a -> 'a -> bool
val equal   : 'a -> 'a -> bool
val min     : 'a -> 'a -> 'a
val max     : 'a -> 'a -> 'a

