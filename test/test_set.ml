open! Import
open! Set

let%test _ =
  invariants
    (of_increasing_iterator_unchecked (module Int) ~len:20 ~f:Fn.id)
