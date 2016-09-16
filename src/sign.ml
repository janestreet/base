open! Import

include Sign0
include Identifiable.Make(Sign0)

let to_float = function
  | Neg -> -1.
  | Zero -> 0.
  | Pos -> 1.

let flip = function
  | Neg -> Pos
  | Zero -> Zero
  | Pos -> Neg

let ( * ) t t' = of_int (to_int t * to_int t')

let%test _ = of_int 37 = Pos && of_int (-22) = Neg && of_int 0 = Zero
