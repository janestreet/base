open! Import

module type S = sig
  type t
  val hash : t -> int
  val compare : t -> t -> int
end

module Make (T :
             sig
               type t [@@deriving_inline compare, sexp_of]
               include
               sig
                 [@@@ocaml.warning "-32"]
                 val sexp_of_t : t -> Sexplib.Sexp.t
                 val compare : t -> t -> int
               end
               [@@@end]
               val hash : t -> int
             end) : S with type t := T.t = struct
  include T
end
