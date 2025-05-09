open! Base
open Expect_test_helpers_base

(* Some examples of using the API for [Capsule] exposed in [Base]. *)

module%test [@name "[Capsule.Isolated]"] _ = struct
  module Some_library = struct
    let do_stuff (r : int ref) : string =
      r := 1;
      "hello"
    ;;
  end

  let%expect_test _ =
    let data = Capsule.Isolated.create (fun () -> ref 0) in
    let data, { aliased = do_stuff_result } =
      Capsule.Isolated.with_unique data ~f:(fun r -> Some_library.do_stuff r)
    in
    (* Even though [get] is [portable], it can still read the contents of [data] since it
     has [shared] access to it. *)
    let (get @ portable) () =
      Capsule.Isolated.with_shared data ~f:(fun r -> r.contents)
    in
    print_s [%message (get () : int)];
    print_s [%message (do_stuff_result : string)];
    [%expect
      {|
      ("get ()" 1)
      (do_stuff_result hello)
      |}]
  ;;
end
