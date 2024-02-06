open! Import
open! Exported_for_specific_uses.Obj_local

(* immediate *)
let%test_unit _ =
  [%test_result: stack_or_heap]
    (let x = 42 in
     stack_or_heap (repr x))
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

let stack_enabled =
  match Sys.backend_type with
  | Sys.Native -> true
  | _ -> false
;;

(* local *)
let%test_unit _ =
  [%test_result: stack_or_heap]
    (let foo x =
       let s = ref x in
       stack_or_heap (repr s) [@nontail]
     in
     foo 42)
    ~expect:(if stack_enabled then Stack else Heap)
;;
