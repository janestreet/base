(** [Ordering] is intended to make code that matches on the result of a comparison
    more concise and easier to read.  For example, one would write:

    {[
      match Ordering.of_int (compare x y) with
      | Less -> ...
      | Equal -> ...
      | Greater -> ...
    ]}

    rather than:

    {[
      let r = compare x y in
      if r < 0 then
        ...
      else if r = 0 then
        ...
      else
        ...
    ]} *)

open! Import

type t =
  | Less
  | Equal
  | Greater
[@@deriving_inline compare, enumerate, hash, sexp]
include
sig
  [@@@ocaml.warning "-32"]
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val hash_fold_t :
    Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state
  val hash : t -> Ppx_hash_lib.Std.Hash.hash_value
  val all : t list
  val compare : t -> t -> int
end
[@@@end]

include Equal.S with type t := t

(** [of_int n] is:

    {v
      Less     if n < 0
      Equal    if n = 0
      Greater  if n > 0
    v} *)
val of_int : int -> t

(** [to_int t] is:

    {v
      Less     -> -1
      Equal    -> 0
      Greater  -> 1
    v}

    It can be useful when writing a comparison function to allow one to return
    [Ordering.t] values and transform them to [int]s later. *)
val to_int : t -> int

module Export : sig
  type _ordering = t =
    | Less
    | Equal
    | Greater
end
