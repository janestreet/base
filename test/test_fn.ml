open! Import
open! Fn

let%test _ = 1 |> fun x -> x = 1
let%test _ = 1 |> fun x -> x + 1 |> fun y -> y = 2

let%test _ = 0  = apply_n_times ~n:0 (fun _ -> assert false) 0
let%test _ = 0  = apply_n_times ~n:(-3) (fun _ -> assert false) 0
let%test _ = 10 = apply_n_times ~n:10 ((+) 1) 0
