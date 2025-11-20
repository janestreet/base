open! Import
module Sexp = Sexp0
include Sexplib0.Sexpable

[%%template
[@@@alloc.default a @ m = (heap_global, stack_local)]

module%template.portable Of_stringable
    (M : Stringable.S
  [@alloc a]) : sig
    type t [@@deriving sexp_grammar]

    include S [@alloc a] with type t := t
  end
  with type t := M.t = struct
  let t_of_sexp sexp =
    match sexp with
    | Sexp.Atom s ->
      (try M.of_string s with
       | exn -> of_sexp_error_exn exn sexp)
    | Sexp.List _ ->
      of_sexp_error
        "Sexpable.Of_stringable.t_of_sexp expected an atom, but got a list"
        sexp
  ;;

  let[@alloc a = (a, heap)] sexp_of_t t =
    Sexp.Atom ((M.to_string [@alloc a]) t) [@exclave_if_stack a]
  ;;

  let t_sexp_grammar : M.t Sexplib0.Sexp_grammar.t =
    Sexplib0.Sexp_grammar.coerce string_sexp_grammar
  ;;
end

module%template.portable Of_sexpable
    (Sexpable : sig
       type t

       include S [@alloc a] with type t := t
     end)
    (M : sig
       type t

       val to_sexpable : t @ m -> Sexpable.t @ m [@@alloc __ @ m = (a @ m, heap_global)]
       val of_sexpable : Sexpable.t -> t
     end) : S [@alloc a] with type t := M.t = struct
  let t_of_sexp sexp =
    let s = Sexpable.t_of_sexp sexp in
    try M.of_sexpable s with
    | exn -> of_sexp_error_exn exn sexp
  ;;

  let[@alloc a = (a, heap)] sexp_of_t t =
    (Sexpable.sexp_of_t [@alloc a]) ((M.to_sexpable [@alloc a]) t) [@exclave_if_stack a]
  ;;
end

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
     end) : S1 [@kind ka] [@alloc a] with type ('a : ka) t := 'a M.t = struct
  let t_of_sexp a_of_sexp sexp =
    let s = Sexpable.t_of_sexp a_of_sexp sexp in
    try M.of_sexpable s with
    | exn -> of_sexp_error_exn exn sexp
  ;;

  let[@alloc a = (a, heap)] sexp_of_t sexp_of_a t =
    (Sexpable.sexp_of_t [@alloc a])
      sexp_of_a
      ((M.to_sexpable [@alloc a]) t) [@exclave_if_stack a]
  ;;
end

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
     end) : S2 [@kind ka kb] [@alloc a] with type ('a : ka, 'b : kb) t := ('a, 'b) M.t =
struct
  let t_of_sexp a_of_sexp b_of_sexp sexp =
    let s = Sexpable.t_of_sexp a_of_sexp b_of_sexp sexp in
    try M.of_sexpable s with
    | exn -> of_sexp_error_exn exn sexp
  ;;

  let[@alloc a = (a, heap)] sexp_of_t sexp_of_a sexp_of_b t =
    (Sexpable.sexp_of_t [@alloc a])
      sexp_of_a
      sexp_of_b
      ((M.to_sexpable [@alloc a]) t) [@exclave_if_stack a]
  ;;
end

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
  with type ('a : ka, 'b : kb, 'c : kc) t := ('a, 'b, 'c) M.t = struct
  let t_of_sexp a_of_sexp b_of_sexp c_of_sexp sexp =
    let s = Sexpable.t_of_sexp a_of_sexp b_of_sexp c_of_sexp sexp in
    try M.of_sexpable s with
    | exn -> of_sexp_error_exn exn sexp
  ;;

  let[@alloc a = (a, heap)] sexp_of_t sexp_of_a sexp_of_b sexp_of_c t =
    (Sexpable.sexp_of_t [@alloc a])
      sexp_of_a
      sexp_of_b
      sexp_of_c
      ((M.to_sexpable [@alloc a]) t) [@exclave_if_stack a]
  ;;
end]
