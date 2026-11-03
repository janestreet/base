open! Import0
include Basement.Portable_lazy

let both t1 t2 = from_fun (fun () -> force t1, force t2)

module Let_syntax = struct
  let return = from_val
  let ( >>| ) t f = map t ~f
  let ( >>= ) t f = bind t ~f

  module Let_syntax = struct
    let return = from_val
    let map = map
    let bind = bind
    let both = both

    module Open_on_rhs = struct end
  end
end

let hash_fold_t hash_fold_a state t = hash_fold_a state (force t)
let sexp_of_t sexp_of_a t = sexp_of_a (force t)
let t_of_sexp a_of_sexp sexp = from_val (a_of_sexp sexp)

let t_sexp_grammar (a_sexp_grammar : 'a Sexplib0.Sexp_grammar.t)
  : 'a t Sexplib0.Sexp_grammar.t
  =
  Sexplib0.Sexp_grammar.coerce a_sexp_grammar
;;
