open! Base
open! Import

let%expect_test "bool_true" =
  printf "%b" (globalize_bool true);
  [%expect {| true |}]
;;

let%expect_test "bool_false" =
  printf "%b" (globalize_bool false);
  [%expect {| false |}]
;;

let%expect_test "char" =
  let local_ c = 'A' in
  let c' = globalize_char c in
  printf "%s" (Char.to_string c');
  [%expect {| A |}]
;;

let%expect_test "float" =
  let local_ f = 5.5 in
  let f' = globalize_float f in
  printf "%f" f';
  [%expect {| 5.500000 |}]
;;

let%expect_test "int" =
  printf "%d" (globalize_int 42);
  [%expect {| 42 |}]
;;

let%expect_test "int32" =
  let local_ i = 42l in
  let i' = globalize_int32 i in
  printf "%ld" i';
  [%expect {| 42 |}]
;;

let%expect_test "int64" =
  let local_ i = 42L in
  let i' = globalize_int64 i in
  printf "%Ld" i';
  [%expect {| 42 |}]
;;

let%expect_test "nativeint" =
  let local_ i = 42n in
  let i' = globalize_nativeint i in
  printf "%nd" i';
  [%expect {| 42 |}]
;;

let%expect_test "string" =
  let local_ s = "hello" in
  let s' = globalize_string s in
  printf "%s" s';
  [%expect {| hello |}]
;;

let%expect_test "array" =
  let local_ a = [| "one"; "two"; "three" |] in
  let a' = globalize_array globalize_string a in
  Array.iter ~f:print_endline a';
  [%expect
    {|
    one
    two
    three
    |}]
;;

let%expect_test "list" =
  let local_ l = [ "one"; "two"; "three" ] in
  let l' = globalize_list globalize_string l in
  List.iter ~f:print_endline l';
  [%expect
    {|
    one
    two
    three
    |}]
;;

let%expect_test "list does not stack overflow" =
  let l = List.init 5_000_000 ~f:(fun i -> i) in
  (* Test for timeout / stack overflow on a long list. *)
  (match globalize_list globalize_int l with
   | (_ : int list) -> ()
   | exception Stack_overflow -> print_cr [%message "stack overflow"]);
  [%expect {| |}]
;;

let%expect_test "option" =
  let local_ o = Some "hello" in
  let o' = globalize_option globalize_string o in
  Option.iter ~f:print_endline o';
  [%expect {| hello |}]
;;

let%expect_test "ref" =
  let local_ r = ref "hello" in
  let r' = globalize_ref globalize_string r in
  print_endline !r';
  [%expect {| hello |}]
;;

let%expect_test "no sharing between globalized refs" =
  let local_ r = ref "initial" in
  let r' = globalize_ref (fun _ -> assert false) r in
  print_endline !r;
  [%expect {| initial |}];
  print_endline !r';
  [%expect {| initial |}];
  r := "local";
  r' := "global";
  print_endline !r;
  [%expect {| local |}];
  print_endline !r';
  [%expect {| global |}]
;;

external get : local_ 'a array -> int -> 'a = "%array_safe_get"
external set : local_ 'a array -> int -> 'a -> unit = "%array_safe_set"

let%expect_test "no sharing between globalized arrays" =
  let local_ a = [| "initial" |] in
  let a' = globalize_array (fun _ -> assert false) a in
  print_endline (get a 0);
  [%expect {| initial |}];
  print_endline (get a' 0);
  [%expect {| initial |}];
  set a 0 "local";
  set a' 0 "global";
  print_endline (get a 0);
  [%expect {| local |}];
  print_endline (get a' 0);
  [%expect {| global |}]
;;
