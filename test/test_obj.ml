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

(* global *)
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

(* Nullable. *)

let%test_unit "tagged integers" =
  let x = This 42 in
  let r = Nullable.repr x in
  [%test_result: bool] (Nullable.is_null r) ~expect:false;
  [%test_result: bool] (Nullable.is_int r) ~expect:true;
  [%test_result: bool] (Nullable.is_immediate r) ~expect:true;
  [%test_result: bool] (Nullable.is_block r) ~expect:false;
  let r' = Sys.opaque_identity r in
  [%test_result: int Or_null.t] (Nullable.Expert.obj r') ~expect:x;
  [%test_result: stack_or_heap] (Nullable.stack_or_heap r') ~expect:Immediate;
  [%test_result: uniform_or_mixed]
    ([%globalize: uniform_or_mixed] (Nullable.uniform_or_mixed r'))
    ~expect:Immediate;
  [%test_result: int] (Nullable.tag r') ~expect:int_tag
;;

let%test_unit "blocks" =
  let s = This "hello" in
  let r = Nullable.repr s in
  [%test_result: bool] (Nullable.is_null r) ~expect:false;
  [%test_result: bool] (Nullable.is_int r) ~expect:false;
  [%test_result: bool] (Nullable.is_immediate r) ~expect:false;
  [%test_result: bool] (Nullable.is_block r) ~expect:true;
  let r' = Sys.opaque_identity r in
  [%test_result: string Or_null.t] (Nullable.Expert.obj r') ~expect:s;
  [%test_result: stack_or_heap] (Nullable.stack_or_heap r') ~expect:Heap;
  [%test_result: uniform_or_mixed]
    ([%globalize: uniform_or_mixed] (Nullable.uniform_or_mixed r'))
    ~expect:Uniform;
  [%test_result: int] (Nullable.tag r') ~expect:string_tag
;;

let%test_unit "nulls" =
  let r = Nullable.repr Null in
  [%test_result: bool] (Nullable.is_null r) ~expect:true;
  [%test_result: bool] (Nullable.is_int r) ~expect:false;
  [%test_result: bool] (Nullable.is_immediate r) ~expect:true;
  [%test_result: bool] (Nullable.is_block r) ~expect:false;
  let r' = Sys.opaque_identity r in
  [%test_result: int Or_null.t] (Nullable.Expert.obj r') ~expect:Null;
  [%test_result: stack_or_heap] (Nullable.stack_or_heap r') ~expect:Immediate;
  [%test_result: uniform_or_mixed]
    ([%globalize: uniform_or_mixed] (Nullable.uniform_or_mixed r'))
    ~expect:Immediate;
  [%test_result: int] (Nullable.tag r') ~expect:Nullable.null_tag
;;

let%test_unit "[magic]ing and [or_null]" =
  [%test_result: string] (magic (This "hello")) ~expect:"hello";
  [%test_result: string Or_null.t] (magic "world") ~expect:(This "world");
  [%test_result: int Or_null.t] (magic (Null : int or_null)) ~expect:Null
;;

type record =
  { mutable a : string Or_null.t
  ; mutable b : int Or_null.t
  }
[@@deriving sexp_of, compare, equal]

let%test_unit "[or_null] fields" =
  let x = Nullable.repr { a = Null; b = This 33 } in
  let f1 = Nullable.Expert.field x 0 in
  [%test_result: int Or_null.t] (Nullable.Expert.obj f1) ~expect:Null;
  let f2 = Nullable.Expert.field x 1 in
  [%test_result: int Or_null.t] (Nullable.Expert.obj f2) ~expect:(This 33);
  let y = { a = This "hello"; b = Null } in
  Nullable.Expert.set_field x 0 (Nullable.repr y.a);
  Nullable.Expert.set_field x 1 (Nullable.repr y.b);
  [%test_result: record] (Nullable.Expert.obj x) ~expect:y
;;

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
