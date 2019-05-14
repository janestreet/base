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

let%test_module _ =
  (module Hashtbl_tests.Make (struct
       include Hashtbl

       let create_poly ?size () = Poly.create ?size ()
       let of_alist_poly_exn l = Poly.of_alist_exn l
       let of_alist_poly_or_error l = Poly.of_alist_or_error l
     end))
;;

let%expect_test "Hashtbl.find_exn" =
  let table = Hashtbl.of_alist_exn (module String) [ "one", 1; "two", 2; "three", 3 ] in
  let test_success key =
    require_does_not_raise [%here] (fun () ->
      print_s [%sexp (Hashtbl.find_exn table key : int)])
  in
  test_success "one";
  [%expect {| 1 |}];
  test_success "two";
  [%expect {| 2 |}];
  test_success "three";
  [%expect {| 3 |}];
  let test_failure key =
    require_does_raise [%here] (fun () -> Hashtbl.find_exn table key)
  in
  test_failure "zero";
  [%expect {| (Not_found_s ("Hashtbl.find_exn: not found" zero)) |}];
  test_failure "four";
  [%expect {| (Not_found_s ("Hashtbl.find_exn: not found" four)) |}]
;;
