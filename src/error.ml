(* This module is trying to minimize dependencies on modules in Core, so as to allow
   [Error] and [Or_error] to be used in various places.  Please avoid adding new
   dependencies. *)

open! Import
include Info

let t_sexp_grammar : t Sexplib0.Sexp_grammar.t = { untyped = Any "Error.t" }
let[@cold] [@inline never] [@local never] [@specialise never] raise t = raise (to_exn t)

let[@cold] [@inline never] [@local never] [@specialise never] raise_s sexp =
  raise (create_s sexp)
;;

(* Tailcalls to raising functions are to be avoided, as the stack traces are much worse.
   Instead, we try really hard to inline wrapper functions that just perform non-tail
   calls to the raising functions. That way, even if a call to [Error.raise] appears
   in tail position, the post-inlining result doesn't perform a tail call.
*)
let[@inline always] raise t = raise t [@nontail]
let[@inline always] raise_s sexp = raise_s sexp [@nontail]
let to_info t = t
let of_info t = t

include Pretty_printer.Register_pp (struct
    type nonrec t = t

    let module_name = "Base.Error"
    let pp = pp
  end)
