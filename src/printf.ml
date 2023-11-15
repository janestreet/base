open! Import0
include Stdlib.Printf

(** failwith, invalid_arg, and exit accepting printf's format. *)

let[@inline never] [@zero_alloc assume never_returns_normally] failwithf fmt =
  ksprintf (fun s () -> failwith s) fmt
;;

let[@inline never] [@zero_alloc assume never_returns_normally] invalid_argf fmt =
  ksprintf (fun s () -> invalid_arg s) fmt
;;
