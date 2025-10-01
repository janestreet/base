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

module%test [@name "[new_mixed_block]"] _ = struct
  module Metadata = struct
    type t =
      { tag : int
      ; total_words : int
      ; scannable_words : int
      }
    [@@deriving equal, sexp_of]

    let of_obj obj ~scannable_words_if_non_native =
      let tag = Obj.tag obj in
      let total_words = Obj.size obj in
      let scannable_words =
        match Sys.backend_type with
        | Native ->
          (match
             Obj.Expert.Uniform_or_mixed.repr (Obj.Expert.Uniform_or_mixed.of_block obj)
           with
           | Uniform -> total_words
           | Mixed { scannable_prefix_len } -> scannable_prefix_len)
        | Bytecode | Other _ ->
          (* Other platforms don't represent mixed blocks, so we simulate them. *)
          scannable_words_if_non_native
      in
      { tag; total_words; scannable_words }
    ;;
  end

  module type S = sig
    type t [@@deriving equal, sexp_of]

    val copy : src:t -> dst:t -> unit
    val examples : t list
    val scannable_words_if_non_native : t -> int
  end

  let test (module M : S) =
    Dynamic.with_temporarily sexp_style Sexp_style.simple_pretty ~f:(fun () ->
      List.iter M.examples ~f:(fun val1 ->
        let scannable_words_if_non_native = M.scannable_words_if_non_native val1 in
        let obj1 = Obj.repr val1 in
        let meta1 = Metadata.of_obj obj1 ~scannable_words_if_non_native in
        print_s [%sexp (meta1 : Metadata.t), (val1 : M.t)];
        let obj2 =
          let%tydi { tag; total_words; scannable_words } = meta1 in
          Expert.new_mixed_block ~tag ~total_words ~scannable_words
        in
        let meta2 = Metadata.of_obj obj2 ~scannable_words_if_non_native in
        require_equal (module Metadata) meta1 meta2;
        let val2 = Obj.Expert.obj obj2 in
        M.copy ~src:val1 ~dst:val2;
        require_equal (module M) val1 val2))
  ;;

  let%expect_test "all scannable" =
    test
      (module struct
        type t =
          { mutable a : float
          ; mutable b : bool
          ; mutable c : string
          }
        [@@deriving equal, sexp_of]

        let scannable_words_if_non_native _ = 3

        let copy ~src:{ a; b; c } ~dst =
          dst.a <- a;
          dst.b <- b;
          dst.c <- c
        ;;

        let examples =
          [ { a = 0.; b = false; c = "" }; { a = 1.; b = true; c = "hello" } ]
        ;;
      end);
    [%expect
      {|
      (((tag 0) (total_words 3) (scannable_words 3)) ((a 0) (b false) (c "")))
      (((tag 0) (total_words 3) (scannable_words 3)) ((a 1) (b true) (c hello)))
      |}]
  ;;

  let%expect_test "none scannable" =
    test
      (module struct
        type t =
          { mutable a : Float_u.t
          ; mutable b : Int64_u.t
          ; mutable c : Int32_u.t
          }
        [@@deriving equal, sexp_of]

        let scannable_words_if_non_native _ = 0

        let copy ~src:{ a; b; c } ~dst =
          dst.a <- a;
          dst.b <- b;
          dst.c <- c
        ;;

        let examples = [ { a = #0.; b = #1L; c = #2l }; { a = #3.; b = #4L; c = #5l } ]
      end);
    [%expect
      {|
      (((tag 0) (total_words 3) (scannable_words 0)) ((a 0) (b 1) (c 2)))
      (((tag 0) (total_words 3) (scannable_words 0)) ((a 3) (b 4) (c 5)))
      |}]
  ;;

  let%expect_test "some scannable" =
    test
      (module struct
        type t =
          { mutable a : Float_u.t
          ; mutable b : int64
          ; mutable c : Int32_u.t
          ; mutable d : string
          }
        [@@deriving equal, sexp_of]

        let scannable_words_if_non_native _ = 2

        let copy ~src:{ a; b; c; d } ~dst =
          dst.a <- a;
          dst.b <- b;
          dst.c <- c;
          dst.d <- d
        ;;

        let examples =
          [ { a = #0.; b = 1L; c = #2l; d = "hello" }
          ; { a = #3.; b = 4L; c = #5l; d = "world" }
          ]
        ;;
      end);
    [%expect
      {|
      (((tag 0) (total_words 4) (scannable_words 2)) ((a 0) (b 1) (c 2) (d hello)))
      (((tag 0) (total_words 4) (scannable_words 2)) ((a 3) (b 4) (c 5) (d world)))
      |}]
  ;;

  let%expect_test "varying tags" =
    test
      (module struct
        type t =
          | A of
              { mutable a : float
              ; mutable b : bool
              ; mutable c : string
              }
          | B of
              { mutable a : Float_u.t
              ; mutable b : Int64_u.t
              ; mutable c : Int32_u.t
              }
          | C of
              { mutable a : Float_u.t
              ; mutable b : int64
              ; mutable c : Int32_u.t
              ; mutable d : string
              }
        [@@deriving equal, sexp_of]

        let scannable_words_if_non_native = function
          | A _ -> 3
          | B _ -> 0
          | C _ -> 2
        ;;

        let copy ~src ~dst =
          match src, dst with
          | A { a; b; c }, A dst ->
            dst.a <- a;
            dst.b <- b;
            dst.c <- c
          | B { a; b; c }, B dst ->
            dst.a <- a;
            dst.b <- b;
            dst.c <- c
          | C { a; b; c; d }, C dst ->
            dst.a <- a;
            dst.b <- b;
            dst.c <- c;
            dst.d <- d
          | A _, (B _ | C _) | B _, (A _ | C _) | C _, (A _ | B _) -> assert false
        ;;

        let examples =
          [ A { a = 0.; b = false; c = "" }
          ; A { a = 1.; b = true; c = "hello" }
          ; B { a = #0.; b = #1L; c = #2l }
          ; B { a = #3.; b = #4L; c = #5l }
          ; C { a = #0.; b = 1L; c = #2l; d = "hello" }
          ; C { a = #3.; b = 4L; c = #5l; d = "world" }
          ]
        ;;
      end);
    [%expect
      {|
      (((tag 0) (total_words 3) (scannable_words 3)) (A (a 0) (b false) (c "")))
      (((tag 0) (total_words 3) (scannable_words 3)) (A (a 1) (b true) (c hello)))
      (((tag 1) (total_words 3) (scannable_words 0)) (B (a 0) (b 1) (c 2)))
      (((tag 1) (total_words 3) (scannable_words 0)) (B (a 3) (b 4) (c 5)))
      (((tag 2) (total_words 4) (scannable_words 2))
       (C (a 0) (b 1) (c 2) (d hello)))
      (((tag 2) (total_words 4) (scannable_words 2))
       (C (a 3) (b 4) (c 5) (d world)))
      |}]
  ;;
end
