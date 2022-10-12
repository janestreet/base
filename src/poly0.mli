(** A module containing the ad-hoc polymorphic comparison functions.  Useful when
    you want to use polymorphic compare in some small scope of a file within which
    polymorphic compare has been hidden *)

external compare : ('a[@local_opt]) -> ('a[@local_opt]) -> int = "%compare"

(** [ascending] is identical to [compare].  [descending x y = ascending y x].  These are
    intended to be mnemonic when used like [List.sort ~compare:ascending] and [List.sort
    ~compare:descending], since they cause the list to be sorted in ascending or
    descending order, respectively. *)
val ascending : 'a -> 'a -> int

val descending : 'a -> 'a -> int
external ( < ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%lessthan"
external ( <= ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%lessequal"
external ( <> ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%notequal"
external ( = ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%equal"
external ( > ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%greaterthan"
external ( >= ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%greaterequal"
external equal : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%equal"
val min : 'a -> 'a -> 'a
val max : 'a -> 'a -> 'a
