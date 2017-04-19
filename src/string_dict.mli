(** Efficient static string dictionaries.  By static, we mean that new key-value pairs
    cannot be added after the dictionary is created.

    This uses the algorithm the OCaml compiler uses for pattern matching on strings. *)

open Import0

type 'a t [@@deriving_inline hash, compare]
include
sig
  [@@@ocaml.warning "-32"]
  val hash_fold_t :
    (Ppx_hash_lib.Std.Hash.state -> 'a -> Ppx_hash_lib.Std.Hash.state) ->
    Ppx_hash_lib.Std.Hash.state -> 'a t -> Ppx_hash_lib.Std.Hash.state
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
end
[@@@end]

(** We don't use [@@deriving_inline sexp][@@@end] to avoid a circular dependency *)
val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t

(** Creates a dictionary from an association list. It is an error for the list to contain
    duplicate keys. *)
val of_alist     : (string * 'a) list -> ('a t, string) Caml.result
val of_alist_exn : (string * 'a) list ->  'a t

val find     : 'a t -> string -> 'a option
val find_exn : 'a t -> string -> 'a

val to_alist : 'a t -> (string * 'a) list
