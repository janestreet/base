open! Import
open! Validate

let print t =
  List.iter (errors t) ~f:Caml.print_endline
;;

let%expect_test "Validate.all" =
  print
    (all [
       (fun _ -> fail "a");
       (fun _ -> pass);
       (fun _ -> fail "b");
       (fun _ -> pass);
       (fun _ -> fail "c");
     ]
       ());
  [%expect {|
    ("" a)
    ("" b)
    ("" c)
  |}]
;;

let%expect_test _ =
  print (first_failure pass (fail "foo"));
  [%expect {| ("" foo) |}]
;;

let%expect_test _ =
  print (first_failure (fail "foo") (fail "bar"));
  [%expect {| ("" foo) |}]
;;

let two_errors = of_list [fail "foo"; fail "bar"]

let%expect_test _ =
  print (first_failure two_errors (fail "snoo"));
  [%expect {|
    ("" foo)
    ("" bar)
  |}]
;;

let%expect_test _ =
  print (first_failure (fail "snoo") two_errors);
  [%expect {| ("" snoo) |}]
;;

let%expect_test _ =
  let v () =
    if true
    then
      failwith "This unit validation raises";
    Validate.pass
  in
  print (protect v ());
  [%expect {|
    (""
     ("Exception raised during validation"
      (Failure "This unit validation raises"))) |}]
;;

let%expect_test "try_with" =
  let v () =
    failwith "this function raises"
  in
  print (try_with v);
  [%expect {|
    ("" ("Exception raised during validation" (Failure "this function raises"))) |}]
;;
