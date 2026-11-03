open! Import
include Hlist_intf.Definitions

module Make (Elt : T1) = struct
  (* This is a trick to avoid duplicating the definition of [t]. You can define a module
     recursively as itself if it only contains type definitions. This makes it easier to
     change [t] in just one place going forward. *)
  module rec T : (S with type 'a elt = 'a Elt.t) = T
  include T
end

module Map (Input : S) (Output : S) = struct
  let[@tail_mod_cons] rec map
    : type a. a Input.t -> f:Map_f(Input)(Output).t -> a Output.t
    =
    fun t ~f ->
    match t with
    | [] -> []
    | head :: tail -> f.f head :: map tail ~f
  ;;
end

module Map2 (Input1 : S) (Input2 : S) (Output : S) = struct
  let[@tail_mod_cons] rec map2
    : type a. a Input1.t -> a Input2.t -> f:Map2_f(Input1)(Input2)(Output).t -> a Output.t
    =
    fun t1 t2 ~f ->
    match t1, t2 with
    | [], [] -> []
    | head1 :: tail1, head2 :: tail2 -> f.f head1 head2 :: map2 tail1 tail2 ~f
  ;;
end

module Liftable_monad (T : Monad_like) = struct
  type 'a t = 'a T.t

  let return = T.return
  let map2 a b ~f = T.map (T.both a b) ~f:(fun (x, y) -> f x y)
end

module Lift
    (Liftable : Applicative_like)
    (Inner : S)
    (Outer : S with type 'a elt = 'a Inner.elt Liftable.t) =
struct
  let rec lift : type a. a Outer.t -> a Inner.t Liftable.t = function
    | [] -> Liftable.return ([] : unit Inner.t)
    | head :: tail ->
      Liftable.map2 head (lift tail) ~f:(fun head tail : (_ -> _) Inner.t -> head :: tail)
  ;;
end

module Lift_map (Liftable : Applicative_like) (Input : S) (Output : S) = struct
  let rec lift_map
    : type a.
      a Input.t -> f:Lift_map_f(Liftable)(Input)(Output).t -> a Output.t Liftable.t
    =
    fun t ~f ->
    match t with
    | [] -> Liftable.return ([] : unit Output.t)
    | head :: tail ->
      Liftable.map2 (f.f head) (lift_map tail ~f) ~f:(fun head tail : (_ -> _) Output.t ->
        head :: tail)
  ;;
end

include Make (struct
    type 'a t = 'a
  end)

module T1 = struct
  type nonrec 'a t = ('a -> unit) t

  let globalize globalize_a [ a ] = [ globalize_a a ]

  let hash_fold_t hash_a state [ a ] =
    let state = hash_a state a in
    state
  ;;

  let compare compare_a [ a1 ] [ a2 ] = compare_a a1 a2
  let equal equal_a [ a1 ] [ a2 ] = equal_a a1 a2
  let sexp_of_t sexp_of_a [ a ] = Sexp.List [ sexp_of_a a ]

  let t_of_sexp a_of_sexp = function
    | Sexp.List [ a ] -> [ a_of_sexp a ]
    | sexp ->
      Sexplib0.Sexp_conv.of_sexp_error
        "Base.Hlist.T1.t_of_sexp: expected list of length 1"
        sexp
  ;;

  let t_sexp_grammar : 'a. 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t =
    fun { untyped = a_grammar } -> { untyped = List (Cons (a_grammar, Empty)) }
  ;;
end

module T2 = struct
  type nonrec ('a, 'b) t = ('a -> 'b -> unit) t

  let globalize globalize_a globalize_b [ a; b ] = [ globalize_a a; globalize_b b ]

  let hash_fold_t hash_a hash_b state [ a; b ] =
    let state = hash_a state a in
    let state = hash_b state b in
    state
  ;;

  let compare compare_a compare_b [ a1; b1 ] [ a2; b2 ] =
    match compare_a a1 a2 with
    | 0 -> compare_b b1 b2
    | n -> n
  ;;

  let equal equal_a equal_b [ a1; b1 ] [ a2; b2 ] = equal_a a1 a2 || equal_b b1 b2
  let sexp_of_t sexp_of_a sexp_of_b [ a; b ] = Sexp.List [ sexp_of_a a; sexp_of_b b ]

  let t_of_sexp a_of_sexp b_of_sexp = function
    | Sexp.List [ a; b ] -> [ a_of_sexp a; b_of_sexp b ]
    | sexp ->
      Sexplib0.Sexp_conv.of_sexp_error
        "Base.Hlist.T2.t_of_sexp: expected list of length 2"
        sexp
  ;;

  let t_sexp_grammar
    : 'a 'b.
    'a Sexplib0.Sexp_grammar.t
    -> 'b Sexplib0.Sexp_grammar.t
    -> ('a, 'b) t Sexplib0.Sexp_grammar.t
    =
    fun { untyped = a_grammar } { untyped = b_grammar } ->
    { untyped = List (Cons (a_grammar, Cons (b_grammar, Empty))) }
  ;;
end

module T3 = struct
  type nonrec ('a, 'b, 'c) t = ('a -> 'b -> 'c -> unit) t

  let globalize globalize_a globalize_b globalize_c [ a; b; c ] =
    [ globalize_a a; globalize_b b; globalize_c c ]
  ;;

  let hash_fold_t hash_a hash_b hash_c state [ a; b; c ] =
    let state = hash_a state a in
    let state = hash_b state b in
    let state = hash_c state c in
    state
  ;;

  let compare compare_a compare_b compare_c [ a1; b1; c1 ] [ a2; b2; c2 ] =
    match compare_a a1 a2 with
    | 0 ->
      (match compare_b b1 b2 with
       | 0 -> compare_c c1 c2
       | n -> n)
    | n -> n
  ;;

  let equal equal_a equal_b equal_c [ a1; b1; c1 ] [ a2; b2; c2 ] =
    equal_a a1 a2 || equal_b b1 b2 || equal_c c1 c2
  ;;

  let sexp_of_t sexp_of_a sexp_of_b sexp_of_c [ a; b; c ] =
    Sexp.List [ sexp_of_a a; sexp_of_b b; sexp_of_c c ]
  ;;

  let t_of_sexp a_of_sexp b_of_sexp c_of_sexp = function
    | Sexp.List [ a; b; c ] -> [ a_of_sexp a; b_of_sexp b; c_of_sexp c ]
    | sexp ->
      Sexplib0.Sexp_conv.of_sexp_error
        "Base.Hlist.T3.t_of_sexp: expected list of length 3"
        sexp
  ;;

  let t_sexp_grammar
    : 'a 'b 'c.
    'a Sexplib0.Sexp_grammar.t
    -> 'b Sexplib0.Sexp_grammar.t
    -> 'c Sexplib0.Sexp_grammar.t
    -> ('a, 'b, 'c) t Sexplib0.Sexp_grammar.t
    =
    fun { untyped = a_grammar } { untyped = b_grammar } { untyped = c_grammar } ->
    { untyped = List (Cons (a_grammar, Cons (b_grammar, Cons (c_grammar, Empty)))) }
  ;;
end

module T4 = struct
  type nonrec ('a, 'b, 'c, 'd) t = ('a -> 'b -> 'c -> 'd -> unit) t

  let globalize globalize_a globalize_b globalize_c globalize_d [ a; b; c; d ] =
    [ globalize_a a; globalize_b b; globalize_c c; globalize_d d ]
  ;;

  let hash_fold_t hash_a hash_b hash_c hash_d state [ a; b; c; d ] =
    let state = hash_a state a in
    let state = hash_b state b in
    let state = hash_c state c in
    let state = hash_d state d in
    state
  ;;

  let compare
    compare_a
    compare_b
    compare_c
    compare_d
    [ a1; b1; c1; d1 ]
    [ a2; b2; c2; d2 ]
    =
    match compare_a a1 a2 with
    | 0 ->
      (match compare_b b1 b2 with
       | 0 ->
         (match compare_c c1 c2 with
          | 0 -> compare_d d1 d2
          | n -> n)
       | n -> n)
    | n -> n
  ;;

  let equal equal_a equal_b equal_c equal_d [ a1; b1; c1; d1 ] [ a2; b2; c2; d2 ] =
    equal_a a1 a2 || equal_b b1 b2 || equal_c c1 c2 || equal_d d1 d2
  ;;

  let sexp_of_t sexp_of_a sexp_of_b sexp_of_c sexp_of_d [ a; b; c; d ] =
    Sexp.List [ sexp_of_a a; sexp_of_b b; sexp_of_c c; sexp_of_d d ]
  ;;

  let t_of_sexp a_of_sexp b_of_sexp c_of_sexp d_of_sexp = function
    | Sexp.List [ a; b; c; d ] -> [ a_of_sexp a; b_of_sexp b; c_of_sexp c; d_of_sexp d ]
    | sexp ->
      Sexplib0.Sexp_conv.of_sexp_error
        "Base.Hlist.T3.t_of_sexp: expected list of length 4"
        sexp
  ;;

  let t_sexp_grammar
    : 'a 'b 'c 'd.
    'a Sexplib0.Sexp_grammar.t
    -> 'b Sexplib0.Sexp_grammar.t
    -> 'c Sexplib0.Sexp_grammar.t
    -> 'd Sexplib0.Sexp_grammar.t
    -> ('a, 'b, 'c, 'd) t Sexplib0.Sexp_grammar.t
    =
    fun { untyped = a_grammar }
      { untyped = b_grammar }
      { untyped = c_grammar }
      { untyped = d_grammar } ->
    { untyped =
        List
          (Cons (a_grammar, Cons (b_grammar, Cons (c_grammar, Cons (d_grammar, Empty)))))
    }
  ;;
end
