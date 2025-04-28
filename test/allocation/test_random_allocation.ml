open! Base
open Expect_test_helpers_core

let test ?(max_alloc = 0) f = require_allocation_does_not_exceed (Minor_words max_alloc) f
let state = Random.State.make [| 1; 2; 3; 4 |]

module _ : module type of Random = struct
  open Random

  let bool = bool
  let%expect_test "bool" = test (fun () -> ignore (bool () : bool))
  let char = char
  let%expect_test "char" = test (fun () -> ignore (char () : char))
  let ascii = ascii
  let%expect_test "ascii" = test (fun () -> ignore (ascii () : char))
  let int = int
  let%expect_test "int" = test (fun () -> ignore (int 100 : int))
  let int_incl = int_incl
  let%expect_test "int_incl" = test (fun () -> ignore (int_incl 0 100 : int))
  let int32 = int32
  let%expect_test "int32" = test ~max_alloc:3 (fun () -> ignore (int32 100l : int32))

  let%expect_test ("int32 fast-flambda2" [@tags "fast-flambda2"]) =
    test (fun () -> ignore (int32 100l : int32))
  ;;

  let int32_incl = int32_incl

  let%expect_test "int32_incl" =
    test ~max_alloc:3 (fun () -> ignore (int32_incl 0l 100l : int32))
  ;;

  let%expect_test ("int32_incl fast-flambda2" [@tags "fast-flambda2"]) =
    test (fun () -> ignore (int32_incl 0l 100l : int32))
  ;;

  let int64 = int64
  let%expect_test "int64" = test ~max_alloc:3 (fun () -> ignore (int64 100L : int64))

  let%expect_test ("int64 fast-flambda2" [@tags "fast-flambda2"]) =
    test (fun () -> ignore (int64 100L : int64))
  ;;

  let int64_incl = int64_incl

  let%expect_test "int64_incl" =
    test ~max_alloc:3 (fun () -> ignore (int64_incl 0L 100L : int64))
  ;;

  let%expect_test ("int64_incl fast-flambda2" [@tags "fast-flambda2"]) =
    test (fun () -> ignore (int64_incl 0L 100L : int64))
  ;;

  let nativeint = nativeint

  let%expect_test "nativeint" =
    test ~max_alloc:3 (fun () -> ignore (nativeint 100n : nativeint))
  ;;

  let%expect_test ("nativeint fast-flambda2" [@tags "fast-flambda2"]) =
    test (fun () -> ignore (nativeint 100n : nativeint))
  ;;

  let nativeint_incl = nativeint_incl

  let%expect_test "nativeint_incl" =
    test ~max_alloc:3 (fun () -> ignore (nativeint_incl 0n 100n : nativeint))
  ;;

  let%expect_test ("nativeint_incl fast-flambda2" [@tags "fast-flambda2"]) =
    test (fun () -> ignore (nativeint_incl 0n 100n : nativeint))
  ;;

  let bits = bits
  let%expect_test "bits" = test (fun () -> ignore (bits () : int))
  let bits64 = bits64
  let%expect_test "bits64" = test ~max_alloc:3 (fun () -> ignore (bits64 () : int64))

  let%expect_test ("bits64 fast-flambda2" [@tags "fast-flambda2"]) =
    test (fun () -> ignore (bits64 () : int64))
  ;;

  let float = float
  let%expect_test "float" = test (fun () -> ignore (float 1. : float))
  let float_range = float_range
  let%expect_test "float_range" = test (fun () -> ignore (float_range 0. 1. : float))
  let init = init
  let%expect_test "init" = test ~max_alloc:38 (fun () -> init 1)
  let full_init = full_init

  let%expect_test "full_init" =
    let seed = [| 1 |] in
    test ~max_alloc:36 (fun () -> full_init seed);
    let seed = [| 1; 2; 3; 4 |] in
    test ~max_alloc:39 (fun () -> full_init seed)
  ;;

  let self_init = self_init

  let%expect_test "self_init" =
    test ~max_alloc:60 (fun () -> self_init ~allow_in_tests:true ())
  ;;

  let set_state = set_state

  let%expect_test "set_state" =
    let state = State.make [| 1; 2; 3; 4 |] in
    test (fun () -> set_state state)
  ;;

  module State = struct
    open State

    type nonrec t = t

    let default = default
    let get_default = get_default
    let%expect_test "get_default" = test (fun () -> ignore (get_default () : t))
    let make = make

    let%expect_test "make" =
      let seed = [| 1 |] in
      test ~max_alloc:49 (fun () -> ignore (make seed : t));
      let seed = [| 1; 2; 3; 4 |] in
      test ~max_alloc:52 (fun () -> ignore (make seed : t))
    ;;

    let make_self_init = make_self_init

    let%expect_test "make_self_init" =
      test ~max_alloc:9 (fun () -> ignore (make_self_init ~allow_in_tests:true () : t))
    ;;

    let copy = copy
    let%expect_test "copy" = test ~max_alloc:26 (fun () -> ignore (copy state : t))
    let bool = bool
    let%expect_test "bool" = test (fun () -> ignore (bool state : bool))
    let char = char
    let%expect_test "char" = test (fun () -> ignore (char state : char))
    let ascii = ascii
    let%expect_test "ascii" = test (fun () -> ignore (ascii state : char))
    let int = int
    let%expect_test "int" = test (fun () -> ignore (int state 100 : int))
    let int_incl = int_incl
    let%expect_test "int_incl" = test (fun () -> ignore (int_incl state 0 100 : int))
    let int32 = int32

    let%expect_test "int32" =
      test ~max_alloc:3 (fun () -> ignore (int32 state 100l : int32))
    ;;

    let%expect_test ("int32 fast-flambda2" [@tags "fast-flambda2"]) =
      test (fun () -> ignore (int32 state 100l : int32))
    ;;

    let int32_incl = int32_incl

    let%expect_test "int32_incl" =
      test ~max_alloc:3 (fun () -> ignore (int32_incl state 0l 100l : int32))
    ;;

    let%expect_test ("int32_incl fast-flambda2" [@tags "fast-flambda2"]) =
      test (fun () -> ignore (int32_incl state 0l 100l : int32))
    ;;

    let int64 = int64

    let%expect_test "int64" =
      test ~max_alloc:3 (fun () -> ignore (int64 state 100L : int64))
    ;;

    let%expect_test ("int64 fast-flambda2" [@tags "fast-flambda2"]) =
      test (fun () -> ignore (int64 state 100L : int64))
    ;;

    let int64_incl = int64_incl

    let%expect_test "int64_incl" =
      test ~max_alloc:3 (fun () -> ignore (int64_incl state 0L 100L : int64))
    ;;

    let%expect_test ("int64_incl fast-flambda2" [@tags "fast-flambda2"]) =
      test (fun () -> ignore (int64_incl state 0L 100L : int64))
    ;;

    let nativeint = nativeint

    let%expect_test "nativeint" =
      test ~max_alloc:3 (fun () -> ignore (nativeint state 100n : nativeint))
    ;;

    let%expect_test ("nativeint fast-flambda2" [@tags "fast-flambda2"]) =
      test (fun () -> ignore (nativeint state 100n : nativeint))
    ;;

    let nativeint_incl = nativeint_incl

    let%expect_test "nativeint_incl" =
      test ~max_alloc:3 (fun () -> ignore (nativeint_incl state 0n 100n : nativeint))
    ;;

    let%expect_test ("nativeint_incl fast-flambda2" [@tags "fast-flambda2"]) =
      test (fun () -> ignore (nativeint_incl state 0n 100n : nativeint))
    ;;

    let bits = bits
    let%expect_test "bits" = test (fun () -> ignore (bits state : int))
    let bits64 = bits64
    let%expect_test "bits64" = test ~max_alloc:3 (fun () -> ignore (bits64 state : int64))

    let%expect_test ("bits64 fast-flambda2" [@tags "fast-flambda2"]) =
      test (fun () -> ignore (bits64 state : int64))
    ;;

    let float = float
    let%expect_test "float" = test (fun () -> ignore (float state 1. : float))
    let float_range = float_range

    let%expect_test "float_range" =
      test (fun () -> ignore (float_range state 0. 1. : float))
    ;;
  end
end
