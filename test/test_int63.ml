open! Import
open! Int63

let%expect_test "hash coherence" [@tags "64-bits-only"] =
  check_int_hash_coherence [%here] (module Int63);
  [%expect {| |}];
;;

let%test_unit _ = [%test_result: t] max_value ~expect:(of_int64_exn 4611686018427387903L)
let%test_unit _ = [%test_result: t] min_value ~expect:(of_int64_exn (-4611686018427387904L))

let%test_unit _ =
  [%test_result: t] (of_int32_exn Int32.min_value) ~expect:(of_int32 Int32.min_value)
let%test_unit _ =
  [%test_result: t] (of_int32_exn Int32.max_value) ~expect:(of_int32 Int32.max_value)

let%test "typical random 0" = Exn.does_raise (fun () -> random zero)

let%test_module "Overflow_exn" =
  (module struct
    open Overflow_exn

    let%test_module "( + )" =
      (module struct
        let test t = Exn.does_raise (fun () -> t + t)
        let%test "max_value / 2 + 1"     = test (succ (max_value / of_int 2))
        let%test "min_value / 2 - 1"     = test (pred (min_value / of_int 2))
        let%test "min_value + min_value" = test min_value
        let%test "max_value + max_value" = test max_value
      end)
    ;;

    let%test_module "( - )" =
      (module struct
        let%test "min_value -  1" = Exn.does_raise (fun () -> min_value -     one)
        let%test "max_value - -1" = Exn.does_raise (fun () -> max_value - neg one)
        let%test "min_value / 2 - max_value / 2 - 2" =
          Exn.does_raise (fun () -> min_value / of_int 2 - max_value / of_int 2 - of_int 2)
        let%test "min_value - max_value" = Exn.does_raise (fun () -> min_value - max_value)
        let%test "max_value - min_value" = Exn.does_raise (fun () -> max_value - min_value)
        let%test "max_value - -max_value" =
          Exn.does_raise (fun () -> max_value - neg max_value)
      end)
    ;;
  end)
