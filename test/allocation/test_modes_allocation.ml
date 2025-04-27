open! Base
open Expect_test_helpers_core

module type S = sig
  type t [@@deriving equal, globalize, sexp_of]
end

let test (type t) (module T : S with type t = t) global local =
  let g = require_no_allocation global in
  print_s [%sexp (g : T.t)];
  let l = require_no_allocation_local local in
  require_equal (module T) g ([%globalize: T.t] l)
;;

let%expect_test "Global.Poly_fn1" =
  let open
    Modes.Global.Poly_fn1 (Int) (Int)
      (functor
         (W : Modes.Global.Wrapper)
         ->
         struct
           let fn x = exclave_ W.wrap (Int.globalize (W.unwrap x) + 0)
         end) in
  test (module Int) (fun () -> fn_global 1) (fun () -> exclave_ fn_local 1);
  [%expect {| 1 |}]
;;

let%expect_test "Global.Poly_fn2" =
  let open
    Modes.Global.Poly_fn2 (Int) (Int) (Int)
      (functor
         (W : Modes.Global.Wrapper)
         ->
         struct
           let fn x y = exclave_
             W.wrap (Int.globalize (W.unwrap x) + Int.globalize (W.unwrap y))
           ;;
         end) in
  test (module Int) (fun () -> fn_global 1 2) (fun () -> exclave_ fn_local 1 2);
  [%expect {| 3 |}]
;;

let%expect_test "Global.Poly_fn3" =
  let open
    Modes.Global.Poly_fn3 (Int) (Int) (Int) (Int)
      (functor
         (W : Modes.Global.Wrapper)
         ->
         struct
           let fn x y z = exclave_
             W.wrap
               (Int.globalize (W.unwrap x)
                + Int.globalize (W.unwrap y)
                + Int.globalize (W.unwrap z))
           ;;
         end) in
  test (module Int) (fun () -> fn_global 1 2 3) (fun () -> exclave_ fn_local 1 2 3);
  [%expect {| 6 |}]
;;

let%expect_test ("Global.Poly_fn1" [@tags "fast-flambda2"]) =
  let open
    Modes.Global.Poly_fn1 (Float) (Float)
      (functor
         (W : Modes.Global.Wrapper)
         ->
         struct
           let fn x = exclave_ W.wrap (Float.globalize (W.unwrap x) +. 0.)
         end) in
  test (module Float) (fun () -> fn_global 1.) (fun () -> exclave_ fn_local 1.);
  [%expect {| 1 |}]
;;

let%expect_test ("Global.Poly_fn2" [@tags "fast-flambda2"]) =
  let open
    Modes.Global.Poly_fn2 (Float) (Float) (Float)
      (functor
         (W : Modes.Global.Wrapper)
         ->
         struct
           let fn x y = exclave_
             W.wrap (Float.globalize (W.unwrap x) +. Float.globalize (W.unwrap y))
           ;;
         end) in
  test (module Float) (fun () -> fn_global 1. 2.) (fun () -> exclave_ fn_local 1. 2.);
  [%expect {| 3 |}]
;;

let%expect_test ("Global.Poly_fn3" [@tags "fast-flambda2"]) =
  let open
    Modes.Global.Poly_fn3 (Float) (Float) (Float) (Float)
      (functor
         (W : Modes.Global.Wrapper)
         ->
         struct
           let fn x y z = exclave_
             W.wrap
               (Float.globalize (W.unwrap x)
                +. Float.globalize (W.unwrap y)
                +. Float.globalize (W.unwrap z))
           ;;
         end) in
  test
    (module Float)
    (fun () -> fn_global 1. 2. 3.)
    (fun () -> exclave_ fn_local 1. 2. 3.);
  [%expect {| 6 |}]
;;

let%expect_test "At_locality.(un)wrap" =
  let test x =
    test
      (module Int)
      (fun () ->
        Modes.At_locality.unwrap (Sys.opaque_identity (Modes.At_locality.wrap x)))
      (fun () -> exclave_
        Modes.At_locality.unwrap_local
          (Sys.opaque_identity (Modes.At_locality.wrap_local x)))
  in
  test 0;
  [%expect {| 0 |}];
  test 1;
  [%expect {| 1 |}]
;;

let%expect_test "At_locality.unwrap_global" =
  let test x =
    test
      (module Int)
      (fun () ->
        Modes.At_locality.unwrap_global (Sys.opaque_identity (Modes.At_locality.wrap x)))
      (fun () -> exclave_
        Modes.At_locality.unwrap_local
          (Sys.opaque_identity (Modes.At_locality.wrap_local x)))
  in
  test 0;
  [%expect {| 0 |}];
  test 1;
  [%expect {| 1 |}]
;;
