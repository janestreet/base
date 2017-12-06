open! Import
open! Map

let%test _ =
  invariants (of_increasing_iterator_unchecked (module Int) ~len:20 ~f:(fun x -> x,x))


