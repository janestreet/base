include Comparable.Make (Sexp)
include Sexp
(* we include [sexp] last to ensure we get a faster [equal] than the one
   produced by [Comparable.Make] *)
