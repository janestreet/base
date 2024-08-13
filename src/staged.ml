open! Import

type 'a t = 'a

(* We define these primitives using ['a] instead of ['a t] as proof to ourselves that this
   is a safe use of [%identity], since [external] provides no typechecking. *)
external stage : ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
external unstage : ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
