open! Async
open Expect_test_helpers_async

let%expect_test _ =
  (* Sadly, the test is sensitive to cross-library inlining, which we can only detect
     using the build info in version_util, which isn't available while compiling a test.
     So we delegate the whole test to this executable: *)
  let%bind () = run "bin/test_option_array_allocation.exe" [] in
  [%expect {| |}];
  return ()
;;
