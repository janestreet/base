@@ portable

(** Provides functors for making modules sexpable when you want the sexp representation of
    one type to be the same as that for some other isomorphic type. *)

open! Import
open! Sexplib0.Sexpable

[%%template:
[@@@alloc.default a @ m = (stack_local, heap_global)]

module%template.portable Of_stringable
    (M : Stringable.S
  [@alloc a]) : sig
    type t [@@deriving sexp_grammar]

    include S [@alloc a] with type t := t
  end
  with type t := M.t

module%template.portable Of_sexpable
    (Sexpable : sig
       type t

       include S [@alloc a] with type t := t
     end)
    (M : sig
       type t

       val to_sexpable : t @ m -> Sexpable.t @ m [@@alloc __ @ m = (a @ m, heap_global)]
       val of_sexpable : Sexpable.t -> t
     end) : S [@alloc a] with type t := M.t

[@@@kind.default ka = (value, any)]

module%template.portable Of_sexpable1
    (Sexpable : sig
       type ('a : ka) t

       include S1 [@kind ka] [@alloc a] with type ('a : ka) t := ('a : ka) t
     end)
    (M : sig
       type ('a : ka) t

       val to_sexpable : ('a : ka). 'a t @ m -> 'a Sexpable.t @ m
       [@@alloc __ @ m = (a @ m, heap_global)]

       val of_sexpable : ('a : ka). 'a Sexpable.t -> 'a t
     end) : S1 [@kind ka] [@alloc a] with type ('a : ka) t := 'a M.t

[@@@kind.default kb = (value, any)]

module%template.portable Of_sexpable2
    (Sexpable : sig
       type ('a : ka, 'b : kb) t

       include
         S2
         [@kind ka kb] [@alloc a]
         with type ('a : ka, 'b : kb) t := ('a : ka, 'b : kb) t
     end)
    (M : sig
       type ('a : ka, 'b : kb) t

       val to_sexpable : ('a : ka) ('b : kb). ('a, 'b) t @ m -> ('a, 'b) Sexpable.t @ m
       [@@alloc __ @ m = (a @ m, heap_global)]

       val of_sexpable : ('a : ka) ('b : kb). ('a, 'b) Sexpable.t -> ('a, 'b) t
     end) : S2 [@kind ka kb] [@alloc a] with type ('a : ka, 'b : kb) t := ('a, 'b) M.t

[@@@kind.default kc = (value, any)]

module%template.portable Of_sexpable3
    (Sexpable : sig
       type ('a : ka, 'b : kb, 'c : kc) t

       include
         S3
         [@kind ka kb kc] [@alloc a]
         with type ('a : ka, 'b : kb, 'c : kc) t := ('a : ka, 'b : kb, 'c : kc) t
     end)
    (M : sig
       type ('a : ka, 'b : kb, 'c : kc) t

       val to_sexpable
         : ('a : ka) ('b : kb) ('c : kc).
         ('a, 'b, 'c) t @ m -> ('a, 'b, 'c) Sexpable.t @ m
       [@@alloc __ @ m = (a @ m, heap_global)]

       val of_sexpable
         : ('a : ka) ('b : kb) ('c : kc).
         ('a, 'b, 'c) Sexpable.t -> ('a, 'b, 'c) t
     end) :
  S3
  [@kind ka kb kc] [@alloc a]
  with type ('a : ka, 'b : kb, 'c : kc) t := ('a, 'b, 'c) M.t]

(** New code should use the [[@@deriving sexp]] syntax directly. These module types ([S],
    [S1], [S2], and [S3]) are exported for backwards compatibility only. *)
include module type of Sexplib0.Sexpable
(** @inline *)
