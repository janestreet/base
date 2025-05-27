open! Import
open! Obj

let native_code =
  match Sys.backend_type with
  | Sys.Native -> true
  | _ -> false
;;

(* immediate *)
let%test_unit _ =
  [%test_result: stack_or_heap]
    (let x = 42 in
     stack_or_heap (repr x))
    ~expect:Immediate
;;

let%test_unit _ =
  [%test_result: uniform_or_mixed]
    (let x = 42 in
     uniform_or_mixed (repr x) |> [%globalize: uniform_or_mixed])
    ~expect:Immediate
;;

(* global*)
let%test_unit _ =
  [%test_result: stack_or_heap]
    (let s = "hello" in
     let _r = ref s in
     stack_or_heap (repr s))
    ~expect:Heap
;;

(* local *)
let%test_unit _ =
  [%test_result: stack_or_heap]
    (let foo x =
       let local_ s = ref x in
       stack_or_heap (repr s) [@nontail]
     in
     foo 42)
    ~expect:(if native_code then Stack else Heap)
;;

(* uniform *)
let%test_unit _ =
  [%test_result: uniform_or_mixed]
    (let s = "hello" in
     let _r = ref s in
     uniform_or_mixed (repr s) |> [%globalize: uniform_or_mixed])
    ~expect:Uniform
;;

(* mixed *)
type t =
  { a : int
  ; b : float#
  }

let%test_unit _ =
  [%test_result: uniform_or_mixed]
    (let foo x =
       let local_ s = { a = x; b = #0.0 } in
       let repr = uniform_or_mixed (repr s) in
       [%globalize: uniform_or_mixed] repr [@nontail]
     in
     foo 42)
    ~expect:(if native_code then Mixed { scannable_prefix_len = 1 } else Uniform)
;;

[%%expect_test
  let ("js failure" [@tags "js-only", "no-wasm"]) =
    Expect_test_helpers_base.require_does_raise (fun () ->
      Obj.uniquely_reachable_words [| Obj.repr () |]);
    [%expect
      {| (Failure "Obj.uniquely_reachable_words is not available in javascript.") |}]
  ;;]

[%%expect_test
  let ("wasm failure" [@tags "wasm-only"]) =
    Expect_test_helpers_base.require_does_raise (fun () ->
      Obj.uniquely_reachable_words [| Obj.repr () |]);
    [%expect {| (Failure "Obj.uniquely_reachable_words is not available in wasm.") |}]
  ;;]
