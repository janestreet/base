open! Import

type 'a t = 'a

[%%template
[@@@mode.default p = (nonportable, portable)]

(* We define these primitives using ['a] instead of ['a t] as proof to ourselves that this
   is a safe use of [%identity], since [external] provides no typechecking. *)
external stage : ('a[@local_opt]) @ p -> ('a[@local_opt]) @ p @@ portable = "%identity"
external unstage : ('a[@local_opt]) @ p -> ('a[@local_opt]) @ p @@ portable = "%identity"]
