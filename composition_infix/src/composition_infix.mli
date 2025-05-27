(** Infix composition operators.

    - [ a |> (f >> g) = a |> f |> g ]
    - [ (f << g) a = f (g a) ] *)

val ( >> ) : 'a 'b 'c. ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val ( << ) : 'a 'b 'c. ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
