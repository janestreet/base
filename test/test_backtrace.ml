open! Import
open! Backtrace

let%test_unit _ [@tags "no-js"] =
  let t = get () in
  assert (String.length (to_string t) > 0)
;;

let%expect_test _ =
  Stdio.Out_channel.(output_string stdout) (Exn.with_recording false ~f:Exn.most_recent);
  [%expect {|
    <backtrace elided in test> |}];
;;
