open! Import
open! Option

let f = ( + )

let%test _ = [%compare.equal: int t] (merge None None ~f) None
let%test _ = [%compare.equal: int t] (merge (Some 3) None ~f) (Some 3)
let%test _ = [%compare.equal: int t] (merge None (Some 3) ~f) (Some 3)
let%test _ = [%compare.equal: int t] (merge (Some 1) (Some 3) ~f) (Some 4)

let%test _ = [%compare.equal: int] 1 (with_default 1 None)
let%test _ = [%compare.equal: int] 2 (with_default 1 (Some 2))
