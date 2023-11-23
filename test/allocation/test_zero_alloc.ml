let[@zero_alloc] [@inline never] foo x = Base.Printf.failwithf "%d" x ()
let[@zero_alloc] [@inline never] bar x y = Base.Printf.invalid_argf "%d" (x + y) ()

let%expect_test "foo" =
  let x = Sys.opaque_identity 5 in
  (try foo x with
   | Failure s ->
     print_string s;
     print_newline ());
  [%expect {| 5 |}]
;;

let%expect_test "bar" =
  let x = Sys.opaque_identity 5 in
  (try bar x x with
   | Invalid_argument s ->
     print_string s;
     print_newline ());
  [%expect {| 10 |}]
;;
