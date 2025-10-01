type t = |

let unreachable_code_local = function
  | (_ : t) -> .
;;

let unreachable_code x = unreachable_code_local x
let all = []
let hash_fold_t _ t = unreachable_code t
let hash = unreachable_code
let compare a _ = unreachable_code a
let compare__local a _ = unreachable_code a
let equal a _ = unreachable_code a
let equal__local a _ = unreachable_code a
let sexp_of_t = unreachable_code
let sexp_of_t__stack = unreachable_code_local
let t_of_sexp sexp = Sexplib0.Sexp_conv_error.empty_type "Base.Nothing.t" sexp
let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) = { untyped = Union [] }
let to_string = unreachable_code
let of_string (_ : string) = failwith "Base.Nothing.of_string: not supported"
let globalize = unreachable_code
