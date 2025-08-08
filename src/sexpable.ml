open! Import
module Sexp = Sexp0
include Sexplib0.Sexpable

[%%template
[@@@alloc.default a @ m = (heap_global, stack_local)]

module%template.portable Of_sexpable
    (Sexpable : S
  [@alloc a])
    (M : sig
       type t

       val to_sexpable : t -> Sexpable.t [@@alloc __ @ m = (a @ m, heap_global)]
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

module%template.portable Of_sexpable1
    (Sexpable : S1
  [@alloc a])
    (M : sig
       type 'a t

       val to_sexpable : 'a t -> 'a Sexpable.t [@@alloc __ @ m = (a @ m, heap_global)]
       val of_sexpable : 'a Sexpable.t -> 'a t
     end) : S1 [@alloc a] with type 'a t := 'a M.t = struct
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

module%template.portable Of_sexpable2
    (Sexpable : S2
  [@alloc a])
    (M : sig
       type ('a, 'b) t

       val to_sexpable : ('a, 'b) t -> ('a, 'b) Sexpable.t
       [@@alloc __ @ m = (a @ m, heap_global)]

       val of_sexpable : ('a, 'b) Sexpable.t -> ('a, 'b) t
     end) : S2 [@alloc a] with type ('a, 'b) t := ('a, 'b) M.t = struct
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

module%template.portable Of_sexpable3
    (Sexpable : S3
  [@alloc a])
    (M : sig
       type ('a, 'b, 'c) t

       val to_sexpable : ('a, 'b, 'c) t -> ('a, 'b, 'c) Sexpable.t
       [@@alloc __ @ m = (a @ m, heap_global)]

       val of_sexpable : ('a, 'b, 'c) Sexpable.t -> ('a, 'b, 'c) t
     end) : S3 [@alloc a] with type ('a, 'b, 'c) t := ('a, 'b, 'c) M.t = struct
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
end

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
end]
