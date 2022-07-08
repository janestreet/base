open! Import0
include Caml.Printf

(** failwith, invalid_arg, and exit accepting printf's format. *)

let[@inline never] failwithf fmt = ksprintf (fun s () -> failwith s) fmt
let[@inline never] invalid_argf fmt = ksprintf (fun s () -> invalid_arg s) fmt
