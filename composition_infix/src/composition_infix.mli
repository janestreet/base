@@ portable

(** Infix composition operators.

    - [ a |> (f >> g) = a |> f |> g ]
    - [ (f << g) a = f (g a) ] *)

val ( >> )
  : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
  ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

val ( << )
  : ('a : value_or_null) ('b : value_or_null) ('c : value_or_null).
  ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
