open! Import
open! Map

let%test _ =
  invariants (of_increasing_iterator_unchecked (module Int) ~len:20 ~f:(fun x -> x,x))

module M = M

let add12 t = add_exn t ~key:1 ~data:2

type int_map = int Map.M(Int).t [@@deriving sexp_of]

let%expect_test "[add_exn] success" =
  print_s [%sexp (add12 (empty (module Int)) : int_map)];
  [%expect {| ((1 2)) |}]
;;

let%expect_test "[add_exn] failure" =
  show_raise (fun () -> add12 (add12 (empty (module Int))));
  [%expect {| (raised ("[Map.add_exn] got key already present" (key 1))) |}]
;;

let%expect_test "[add] success" =
  print_s [%sexp (
    add (empty (module Int)) ~key:1 ~data:2 : int_map Or_duplicate.t)];
  [%expect {| (Ok ((1 2))) |}]
;;

let%expect_test "[add] duplicate" =
  print_s [%sexp (
    add (add12 (empty (module Int))) ~key:1 ~data:2 : int_map Or_duplicate.t)];
  [%expect {| Duplicate |}]
;;
