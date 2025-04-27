open! Base
open! Import
open Portable_test_helpers

let%expect_test "compare_and_set" =
  let atomic = Atomic.make 1 in
  let compare_failed =
    Atomic.compare_and_set atomic ~if_phys_equal_to:4 ~replace_with:10
  in
  let current_value = Atomic.get atomic in
  print_s
    [%message
      (compare_failed : Atomic.Compare_failed_or_set_here.t) (current_value : int)];
  [%expect
    {|
    ((compare_failed Compare_failed)
     (current_value  1))
    |}];
  let set_here = Atomic.compare_and_set atomic ~if_phys_equal_to:1 ~replace_with:10 in
  let current_value = Atomic.get atomic in
  print_s
    [%message (set_here : Atomic.Compare_failed_or_set_here.t) (current_value : int)];
  [%expect
    {|
    ((set_here      Set_here)
     (current_value 10))
    |}]
;;

let%expect_test "update_and_return" =
  let atomic = Atomic.make 1 in
  let result = Atomic.update_and_return atomic ~pure_f:(fun x -> x + 1) in
  let new_value = Atomic.get atomic in
  print_s [%message (result : int) (new_value : int)];
  [%expect
    {|
    ((result    1)
     (new_value 2))
    |}]
;;

let%expect_test ("update from multiple domains"
  [@tags "runtime5-only", "no-js", "no-wasm"])
  =
  let atomic = Atomic.make [] in
  let num_domains = 5 in
  let barrier = Barrier.create num_domains in
  let domains =
    List.init num_domains ~f:(fun n ->
      (Stdlib.Domain.Safe.spawn [@alert "-unsafe_parallelism"]) (fun () ->
        Barrier.await barrier;
        Atomic.update atomic ~pure_f:(fun l -> n :: l)))
  in
  List.iter domains ~f:Domain.join;
  let final_result =
    Atomic.get atomic
    |> Portability_hacks.Cross.Contended.(cross (list infer))
    |> List.sort ~compare:Int.compare
  in
  print_s [%message (final_result : int list)];
  [%expect {| (final_result (0 1 2 3 4)) |}]
;;
