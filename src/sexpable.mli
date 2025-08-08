(** Provides functors for making modules sexpable when you want the sexp representation of
    one type to be the same as that for some other isomorphic type. *)

open! Import
open! Sexplib0.Sexpable

[%%template:
[@@@alloc.default a @ m = (stack_local, heap_global)]

module%template.portable Of_sexpable
    (Sexpable : S
  [@alloc a])
    (M : sig
       type t

       val to_sexpable : t -> Sexpable.t [@@alloc __ @ m = (a @ m, heap_global)]
       val of_sexpable : Sexpable.t -> t
     end) : S [@alloc a] with type t := M.t

module%template.portable Of_sexpable1
    (Sexpable : S1
  [@alloc a])
    (M : sig
       type 'a t

       val to_sexpable : 'a t -> 'a Sexpable.t [@@alloc __ @ m = (a @ m, heap_global)]
       val of_sexpable : 'a Sexpable.t -> 'a t
     end) : S1 [@alloc a] with type 'a t := 'a M.t

module%template.portable Of_sexpable2
    (Sexpable : S2
  [@alloc a])
    (M : sig
       type ('a, 'b) t

       val to_sexpable : ('a, 'b) t -> ('a, 'b) Sexpable.t
       [@@alloc __ @ m = (a @ m, heap_global)]

       val of_sexpable : ('a, 'b) Sexpable.t -> ('a, 'b) t
     end) : S2 [@alloc a] with type ('a, 'b) t := ('a, 'b) M.t

module%template.portable Of_sexpable3
    (Sexpable : S3
  [@alloc a])
    (M : sig
       type ('a, 'b, 'c) t

       val to_sexpable : ('a, 'b, 'c) t -> ('a, 'b, 'c) Sexpable.t
       [@@alloc __ @ m = (a @ m, heap_global)]

       val of_sexpable : ('a, 'b, 'c) Sexpable.t -> ('a, 'b, 'c) t
     end) : S3 [@alloc a] with type ('a, 'b, 'c) t := ('a, 'b, 'c) M.t

module%template.portable Of_stringable
    (M : Stringable.S
  [@alloc a]) : sig
    type t [@@deriving sexp_grammar]

    include S [@alloc a] with type t := t
  end
  with type t := M.t]

(** New code should use the [[@@deriving sexp]] syntax directly. These module types ([S],
    [S1], [S2], and [S3]) are exported for backwards compatibility only. *)
include module type of Sexplib0.Sexpable
(** @inline *)
