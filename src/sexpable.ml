open! Import

module type S = sig
  type t [@@deriving_inline sexp]
  include
  sig
    [@@@ocaml.warning "-32"]
    val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  end
  [@@@end]
end

module type S1 = sig
  type 'a t [@@deriving_inline sexp]
  include
  sig
    [@@@ocaml.warning "-32"]
    val t_of_sexp :
      (Ppx_sexp_conv_lib.Sexp.t -> 'a) -> Ppx_sexp_conv_lib.Sexp.t -> 'a t
    val sexp_of_t :
      ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t
  end
  [@@@end]
end

module type S2 = sig
  type ('a, 'b) t [@@deriving_inline sexp]
  include
  sig
    [@@@ocaml.warning "-32"]
    val t_of_sexp :
      (Ppx_sexp_conv_lib.Sexp.t -> 'a) ->
      (Ppx_sexp_conv_lib.Sexp.t -> 'b) ->
      Ppx_sexp_conv_lib.Sexp.t -> ('a,'b) t
    val sexp_of_t :
      ('a -> Ppx_sexp_conv_lib.Sexp.t) ->
      ('b -> Ppx_sexp_conv_lib.Sexp.t) ->
      ('a,'b) t -> Ppx_sexp_conv_lib.Sexp.t
  end
  [@@@end]
end

module type S3 = sig
  type ('a, 'b, 'c) t [@@deriving_inline sexp]
  include
  sig
    [@@@ocaml.warning "-32"]
    val t_of_sexp :
      (Ppx_sexp_conv_lib.Sexp.t -> 'a) ->
      (Ppx_sexp_conv_lib.Sexp.t -> 'b) ->
      (Ppx_sexp_conv_lib.Sexp.t -> 'c) ->
      Ppx_sexp_conv_lib.Sexp.t -> ('a,'b,'c) t
    val sexp_of_t :
      ('a -> Ppx_sexp_conv_lib.Sexp.t) ->
      ('b -> Ppx_sexp_conv_lib.Sexp.t) ->
      ('c -> Ppx_sexp_conv_lib.Sexp.t) ->
      ('a,'b,'c) t -> Ppx_sexp_conv_lib.Sexp.t
  end
  [@@@end]
end

module Of_sexpable
    (Sexpable : S)
    (M : sig
       type t
       val to_sexpable : t -> Sexpable.t
       val of_sexpable : Sexpable.t -> t
     end)
  : S with type t := M.t =
struct
  let t_of_sexp sexp =
    let s = Sexpable.t_of_sexp sexp in
    (try M.of_sexpable s with exn -> of_sexp_error_exn exn sexp)

  let sexp_of_t t = Sexpable.sexp_of_t (M.to_sexpable t)
end

module Of_sexpable1
    (Sexpable : S1)
    (M : sig
       type 'a t
       val to_sexpable : 'a t -> 'a Sexpable.t
       val of_sexpable : 'a Sexpable.t -> 'a t
     end)
  : S1 with type 'a t := 'a M.t =
struct
  let t_of_sexp a_of_sexp sexp =
    let s = Sexpable.t_of_sexp a_of_sexp sexp in
    (try M.of_sexpable s with exn -> of_sexp_error_exn exn sexp)

  let sexp_of_t sexp_of_a t = Sexpable.sexp_of_t sexp_of_a (M.to_sexpable t)
end

module Of_sexpable2 (Sexpable : S2)
    (M : sig
       type ('a, 'b) t
       val to_sexpable : ('a, 'b) t -> ('a, 'b) Sexpable.t
       val of_sexpable : ('a, 'b) Sexpable.t -> ('a, 'b) t
     end)
  : S2 with type ('a, 'b) t := ('a, 'b) M.t =
struct
  let t_of_sexp a_of_sexp b_of_sexp sexp =
    let s = Sexpable.t_of_sexp a_of_sexp b_of_sexp sexp in
    (try M.of_sexpable s with exn -> of_sexp_error_exn exn sexp)

  let sexp_of_t sexp_of_a sexp_of_b t =
    Sexpable.sexp_of_t sexp_of_a sexp_of_b (M.to_sexpable t)
end

module Of_sexpable3 (Sexpable : S3)
    (M : sig
       type ('a, 'b, 'c) t
       val to_sexpable : ('a, 'b, 'c) t -> ('a, 'b, 'c) Sexpable.t
       val of_sexpable : ('a, 'b, 'c) Sexpable.t -> ('a, 'b, 'c) t
     end)
  : S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) M.t =
struct
  let t_of_sexp a_of_sexp b_of_sexp c_of_sexp sexp =
    let s = Sexpable.t_of_sexp a_of_sexp b_of_sexp c_of_sexp sexp in
    (try M.of_sexpable s with exn -> of_sexp_error_exn exn sexp)

  let sexp_of_t sexp_of_a sexp_of_b sexp_of_c t =
    Sexpable.sexp_of_t sexp_of_a sexp_of_b sexp_of_c (M.to_sexpable t)
end

module Of_stringable (M : Stringable.S) : S with type t := M.t = struct
  let t_of_sexp sexp =
    match sexp with
    | Sexp.Atom s ->
      (try M.of_string s with exn -> of_sexp_error_exn exn sexp)
    | Sexp.List _ ->
      of_sexp_error
        "Sexpable.Of_stringable.t_of_sexp expected an atom, but got a list" sexp

  let sexp_of_t t = Sexp.Atom (M.to_string t)
end
