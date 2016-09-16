(* belongs in Common, but moved here to avoid circular dependencies *)

open! Import

type 'a return = { return : 'b. 'a -> 'b }

let with_return (type a) f =
  let module M = struct
    (* Raised to indicate ~return was called.  Local so that the exception is tied to a
       particular call of [with_return]. *)
    exception Return of a
  end in
  let is_alive = ref true in
  let return a =
    if not !is_alive
    then failwith "use of [return] from a [with_return] that already returned";
    Exn.raise_without_backtrace (M.Return a);
  in
  try
    let a = f { return } in
    is_alive := false;
    a
  with exn ->
    is_alive := false;
    match exn with
    | M.Return a -> a
    | _ -> raise exn
;;

let with_return_option f =
  with_return (fun return ->
    f { return = fun a -> return.return (Some a) }; None)
;;

let prepend { return } ~f = { return = fun x -> return (f x) }

let%test_module "with_return" = (module struct
  let test_loop loop_limit jump_out =
    with_return (fun { return } ->
      for i = 0 to loop_limit do begin
        if i = jump_out then return (`Jumped_out i);
      end done;
      `Normal)
  ;;

  let%test _ = test_loop 5 10 = `Normal
  let%test _ = test_loop 10 5 = `Jumped_out 5
  let%test _ = test_loop 5 5  = `Jumped_out 5

  let test_nested outer inner =
    with_return (fun { return = return_outer } ->
      if outer = `Outer_jump then return_outer `Outer_jump;
      let inner_res =
        with_return (fun { return = return_inner } ->
          if inner = `Inner_jump_out_completely then return_outer `Inner_jump;
          if inner = `Inner_jump then return_inner `Inner_jump;
          `Inner_normal)
      in
      if outer = `Jump_with_inner then return_outer (`Outer_later_jump inner_res);
      `Outer_normal inner_res)
  ;;

  let%test _ = test_nested `Outer_jump `Inner_jump                = `Outer_jump
  let%test _ = test_nested `Outer_jump `Inner_jump_out_completely = `Outer_jump
  let%test _ = test_nested `Outer_jump `Foo                       = `Outer_jump

  let%test _ = test_nested `Jump_with_inner `Inner_jump_out_completely = `Inner_jump
  let%test _ = test_nested `Jump_with_inner `Inner_jump = `Outer_later_jump `Inner_jump
  let%test _ = test_nested `Jump_with_inner `Foo        = `Outer_later_jump `Inner_normal

  let%test _ = test_nested `Foo `Inner_jump_out_completely = `Inner_jump
  let%test _ = test_nested `Foo `Inner_jump = `Outer_normal `Inner_jump
  let%test _ = test_nested `Foo `Foo = `Outer_normal `Inner_normal
end)

let%test_module "with_return_option" = (module struct
  let test_loop loop_limit jump_out =
    with_return_option (fun { return } ->
      for i = 0 to loop_limit do begin
        if i = jump_out then return (`Jumped_out i);
      end done)
  ;;

  let%test _ = test_loop 5 10 = None
  let%test _ = test_loop 10 5 = Some (`Jumped_out 5)
  let%test _ = test_loop 5 5  = Some (`Jumped_out 5)
end)
