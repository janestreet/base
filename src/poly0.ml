(** Primitives for polymorphic compare. *)

(*_ Polymorphic compiler primitives can't be aliases as this doesn't play well with
  inlining. (If aliased without a type annotation, the compiler would implement them
  using the generic code doing a C call, and it's this code that would be inlined.) As a
  result we have to copy the [external ...] declaration here. *)
external ( < ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%lessthan"
external ( <= ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%lessequal"
external ( <> ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%notequal"
external ( = ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%equal"
external ( > ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%greaterthan"
external ( >= ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%greaterequal"
external ascending : ('a[@local_opt]) -> ('a[@local_opt]) -> int = "%compare"
external compare : ('a[@local_opt]) -> ('a[@local_opt]) -> int = "%compare"
external equal : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%equal"

let descending x y = compare y x
let max x y = Bool0.select (x >= y) x y
let min x y = Bool0.select (x <= y) x y
