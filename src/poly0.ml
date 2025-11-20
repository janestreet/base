(** Primitives for polymorphic compare. *)

(*_ Polymorphic compiler primitives can't be aliases as this doesn't play well with
    inlining. (If aliased without a type annotation, the compiler would implement them
    using the generic code doing a C call, and it's this code that would be inlined.) As a
    result we have to copy the [external ...] declaration here. *)
external ( < ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool @@ portable = "%lessthan"
external ( <= ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool @@ portable = "%lessequal"
external ( <> ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool @@ portable = "%notequal"
external ( = ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool @@ portable = "%equal"
external ( > ) : ('a[@local_opt]) -> ('a[@local_opt]) -> bool @@ portable = "%greaterthan"

external ( >= )
  :  ('a[@local_opt])
  -> ('a[@local_opt])
  -> bool
  @@ portable
  = "%greaterequal"

external ascending : ('a[@local_opt]) -> ('a[@local_opt]) -> int @@ portable = "%compare"

(** We provide templated names so [include Poly] satisfies relevant interfaces. *)

external%template compare
  :  ('a[@local_opt])
  -> ('a[@local_opt])
  -> int
  @@ portable
  = "%compare"
[@@mode m = (global, local)]

external%template equal
  :  ('a[@local_opt])
  -> ('a[@local_opt])
  -> bool
  @@ portable
  = "%equal"
[@@mode m = (global, local)]

let descending x y = compare y x
let max x y = Bool0.select (x >= y) x y
let min x y = Bool0.select (x <= y) x y
