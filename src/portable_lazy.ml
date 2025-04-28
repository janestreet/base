open! Import
include Basement.Portable_lazy

let hash_fold_t hash_fold_a state t = hash_fold_a state (force t)
let sexp_of_t sexp_of_a t = sexp_of_a (force t)
let t_of_sexp a_of_sexp sexp = from_val (a_of_sexp sexp)

let t_sexp_grammar (a_sexp_grammar : 'a Sexplib0.Sexp_grammar.t)
  : 'a t Sexplib0.Sexp_grammar.t
  =
  Sexplib0.Sexp_grammar.coerce a_sexp_grammar
;;
