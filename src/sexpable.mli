(** Provides functors for making modules sexpable. New code should use the [[@@deriving
    sexp]] syntax directly.  These module types ([S], [S1], [S2], and [S3]) are exported
    for backwards compatibility only. *)

open! Import

module type S  = sig type              t [@@deriving_inline sexp]
  include
  sig
    [@@@ocaml.warning "-32"]
    val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t
    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  end
  [@@@end] end
module type S1 = sig type 'a           t [@@deriving_inline sexp]
  include
  sig
    [@@@ocaml.warning "-32"]
    val t_of_sexp :
      (Ppx_sexp_conv_lib.Sexp.t -> 'a) -> Ppx_sexp_conv_lib.Sexp.t -> 'a t
    val sexp_of_t :
      ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t
  end
  [@@@end] end
module type S2 = sig type ('a, 'b)     t [@@deriving_inline sexp]
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
  [@@@end] end
module type S3 = sig type ('a, 'b, 'c) t [@@deriving_inline sexp]
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
  [@@@end] end

(** For when you want the sexp representation of one type to be the same as that for
    some other isomorphic type. *)
module Of_sexpable
    (Sexpable : S)
    (M : sig
       type t
       val to_sexpable : t -> Sexpable.t
       val of_sexpable : Sexpable.t -> t
     end)
  : S with type t := M.t

module Of_sexpable1
    (Sexpable : S1)
    (M : sig
       type 'a t
       val to_sexpable : 'a t -> 'a Sexpable.t
       val of_sexpable : 'a Sexpable.t -> 'a t
     end)
  : S1 with type 'a t := 'a M.t

module Of_sexpable2
    (Sexpable : S2)
    (M : sig
       type ('a, 'b) t
       val to_sexpable : ('a, 'b) t -> ('a, 'b) Sexpable.t
       val of_sexpable : ('a, 'b) Sexpable.t -> ('a, 'b) t
     end)
  : S2 with type ('a, 'b) t := ('a, 'b) M.t

module Of_sexpable3
    (Sexpable : S3)
    (M : sig
       type ('a, 'b, 'c) t
       val to_sexpable : ('a, 'b, 'c) t -> ('a, 'b, 'c) Sexpable.t
       val of_sexpable : ('a, 'b, 'c) Sexpable.t -> ('a, 'b, 'c) t
     end)
  : S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) M.t

module Of_stringable (M : Stringable.S) : S with type t := M.t
