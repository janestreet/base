open! Base
open Expect_test_helpers_base

type int_hashtbl = int Hashtbl.M(Int).t [@@deriving sexp]

let%test "Hashtbl.merge succeeds with first-class-module interface" =
  let t1 = Hashtbl.create (module Int) in
  let t2 = Hashtbl.create (module Int) in
  let result =
    Hashtbl.merge t1 t2 ~f:(fun ~key:_ -> function
      | `Left x -> x
      | `Right x -> x
      | `Both _ -> assert false)
    |> Hashtbl.to_alist
  in
  List.equal Poly.equal result []
;;

module%test _ = Hashtbl_tests.Make (struct
    include Hashtbl

    let create_poly ?size () = Poly.create ?size ()
    let of_alist_poly_exn l = Poly.of_alist_exn l
    let of_alist_poly_or_error l = Poly.of_alist_or_error l
  end)

let%expect_test "Hashtbl.find_exn" =
  let table = Hashtbl.of_alist_exn (module String) [ "one", 1; "two", 2; "three", 3 ] in
  let test_success key =
    require_does_not_raise (fun () -> print_s [%sexp (Hashtbl.find_exn table key : int)])
  in
  test_success "one";
  [%expect {| 1 |}];
  test_success "two";
  [%expect {| 2 |}];
  test_success "three";
  [%expect {| 3 |}];
  let test_failure key = require_does_raise (fun () -> Hashtbl.find_exn table key) in
  test_failure "zero";
  [%expect {| (Not_found_s ("Hashtbl.find_exn: not found" zero)) |}];
  test_failure "four";
  [%expect {| (Not_found_s ("Hashtbl.find_exn: not found" four)) |}]
;;

let%expect_test "[t_of_sexp] error on duplicate" =
  let sexp = Sexplib.Sexp.of_string "((0 a)(1 b)(2 c)(1 d))" in
  (match [%of_sexp: string Hashtbl.M(String).t] sexp with
   | t -> print_cr [%message "did not raise" (t : string Hashtbl.M(String).t)]
   | exception (Sexp.Of_sexp_error _ as exn) -> print_s (sexp_of_exn exn)
   | exception exn -> print_cr [%message "wrong kind of exception" (exn : exn)]);
  [%expect {| (Of_sexp_error "Hashtbl.t_of_sexp: duplicate key" (invalid_sexp 1)) |}]
;;

let%expect_test "[choose], [choose_exn], [choose_randomly], [choose_randomly_exn]" =
  let test ?size l =
    let t = l |> List.map ~f:(fun i -> i, i) |> Hashtbl.of_alist_exn ?size (module Int) in
    print_s
      [%message
        ""
          ~input:(t : int_hashtbl)
          ~choose:(Hashtbl.choose t : (_ * _) option)
          ~choose_exn:
            (Or_error.try_with (fun () -> Hashtbl.choose_exn t) : (_ * _) Or_error.t)
          ~choose_local:
            ([%globalize: (int Modes.Global.t * int Modes.Global.t) option]
               [%template (Hashtbl.choose [@mode local]) t]
             : (_ * _) option)
          ~choose_local_exn:
            (Or_error.try_with (fun () ->
               [%globalize: int Modes.Global.t * int Modes.Global.t]
                 [%template (Hashtbl.choose_exn [@mode local]) t] [@nontail])
             : (_ * _) Or_error.t)
          ~choose_randomly:(Hashtbl.choose_randomly t : (_ * _) option)
          ~choose_randomly_exn:
            (Or_error.try_with (fun () -> Hashtbl.choose_randomly_exn t)
             : (_ * _) Or_error.t)]
  in
  test [];
  [%expect
    {|
    ((input  ())
     (choose ())
     (choose_exn (Error ("[Hashtbl.choose_exn] of empty hashtbl")))
     (choose_local ())
     (choose_local_exn (Error ("[Hashtbl.choose_exn] of empty hashtbl")))
     (choose_randomly ())
     (choose_randomly_exn (
       Error ("[Hashtbl.choose_randomly_exn] of empty hashtbl"))))
    |}];
  test [] ~size:100;
  [%expect
    {|
    ((input  ())
     (choose ())
     (choose_exn (Error ("[Hashtbl.choose_exn] of empty hashtbl")))
     (choose_local ())
     (choose_local_exn (Error ("[Hashtbl.choose_exn] of empty hashtbl")))
     (choose_randomly ())
     (choose_randomly_exn (
       Error ("[Hashtbl.choose_randomly_exn] of empty hashtbl"))))
    |}];
  test [ 1 ];
  [%expect
    {|
    ((input  ((1 1)))
     (choose ((_ _)))
     (choose_exn (Ok (_ _)))
     (choose_local ((_ _)))
     (choose_local_exn (Ok (_ _)))
     (choose_randomly ((_ _)))
     (choose_randomly_exn (Ok (_ _))))
    |}];
  test [ 1 ] ~size:100;
  [%expect
    {|
    ((input  ((1 1)))
     (choose ((_ _)))
     (choose_exn (Ok (_ _)))
     (choose_local ((_ _)))
     (choose_local_exn (Ok (_ _)))
     (choose_randomly ((_ _)))
     (choose_randomly_exn (Ok (_ _))))
    |}];
  test [ 1; 2 ];
  [%expect
    {|
    ((input (
       (1 1)
       (2 2)))
     (choose ((_ _)))
     (choose_exn (Ok (_ _)))
     (choose_local ((_ _)))
     (choose_local_exn (Ok (_ _)))
     (choose_randomly ((_ _)))
     (choose_randomly_exn (Ok (_ _))))
    |}];
  test [ 1; 2 ] ~size:100;
  [%expect
    {|
    ((input (
       (1 1)
       (2 2)))
     (choose ((_ _)))
     (choose_exn (Ok (_ _)))
     (choose_local ((_ _)))
     (choose_local_exn (Ok (_ _)))
     (choose_randomly ((_ _)))
     (choose_randomly_exn (Ok (_ _))))
    |}]
;;

let%expect_test "update_and_return" =
  let t = Hashtbl.create (module String) in
  let update_and_return str ~f =
    let x = Hashtbl.update_and_return t str ~f in
    print_s [%message (t : (string, int) Hashtbl.t) (x : int)]
  in
  update_and_return "foo" ~f:(function
    | None -> 1
    | Some _ -> failwith "no");
  [%expect {| ((t ((foo 1))) (x 1)) |}];
  update_and_return "foo" ~f:(function
    | Some 1 -> 2
    | _ -> failwith "no");
  [%expect {| ((t ((foo 2))) (x 2)) |}]
;;

let%expect_test "smoke tests for templated versions" =
  let module%template StrO = struct
    type t = string option [@@deriving compare, equal, sexp]
  end
  in
  let t = (Hashtbl.create [@kind float64 value]) (module Float_u) in
  (Hashtbl.set [@kind float64 value]) t ~key:1.0 ~data:"foo";
  require ((Hashtbl.mem [@kind float64 value]) t 1.0);
  require (not @@ (Hashtbl.mem [@kind float64 value]) t 2.0);
  require_equal (module StrO) ((Hashtbl.find [@kind float64 value]) t 2.0) None;
  require_equal (module StrO) ((Hashtbl.find [@kind float64 value]) t 1.0) (Some "foo");
  (Hashtbl.add_exn [@kind float64 value]) t ~key:0.0 ~data:"zero";
  require_does_raise (fun () ->
    (Hashtbl.add_exn [@kind float64 value]) t ~key:0.0 ~data:"zero");
  [%expect {| ("Hashtbl.add_exn got key already present" 0) |}];
  print_s [%sexp (t : ((Float_u.t, string) Hashtbl.t[@kind float64 value]))];
  [%expect
    {|
    ((0 zero)
     (1 foo))
    |}]
;;
