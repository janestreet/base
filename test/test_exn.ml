open! Import
open! Exn

let%expect_test "[create_s]" =
  print_s [%sexp (create_s [%message "foo"] : t)];
  [%expect {|
    foo |}];
  print_s [%sexp (create_s [%message "foo" "bar"] : t)];
  [%expect {|
    (foo bar) |}];
  let sexp = [%message "foo"] in
  print_s [%sexp (phys_equal sexp (sexp_of_t (create_s sexp)) : bool)];
  [%expect {|
    true |}];
;;

let%test_module _ [@tags "no-js"] =
  (module struct
    exception Test_exception

    let with_backtraces_enabled f =
      let saved = Caml.Printexc.backtrace_status () in
      Caml.Printexc.record_backtrace true;
      protect ~f ~finally:(fun () -> Caml.Printexc.record_backtrace saved)
    ;;

    (* Disable inlining to force a backtrace to be generated *)
    let [@inline never] raise exn = raise exn

    let%test_unit "clear_backtrace" =
      with_backtraces_enabled (fun () ->
        begin try raise Test_exception with _ -> () end;
        assert (not (String.equal (backtrace ()) ""));
        Private.clear_backtrace ();
        assert (String.equal (backtrace ()) ""))
    ;;

    let check_if_empty_backtrace raise_f =
      with_backtraces_enabled (fun () ->
        Private.clear_backtrace ();
        (* The call to [raise] installs a new saved backtrace.  Then, the call to [raise_f],
           if it's [raise], should save a new, different backtrace, while if it's
           [raise_without_backtrace], should clear the backtrace and then not install a new
           one when raising. *)
        let old_backtrace = try raise   Not_found      with Not_found      -> backtrace () in
        assert (not (String.equal old_backtrace ""));
        let new_backtrace = try raise_f Test_exception with Test_exception -> backtrace () in
        assert (not (String.equal new_backtrace old_backtrace));
        String.equal new_backtrace "");
    ;;

    let%test _ = not (check_if_empty_backtrace raise)
    let%test _ = check_if_empty_backtrace raise_without_backtrace
  end)

let%test _ = not (does_raise Fn.ignore)
let%test _ = does_raise (fun () -> failwith "foo")
