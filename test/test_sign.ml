open! Import
open! Sign

let%test _ = of_int 37 = Pos && of_int (-22) = Neg && of_int 0 = Zero
