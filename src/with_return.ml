(* belongs in Common, but moved here to avoid circular dependencies *)

open! Import

type 'a return = { return : 'b. 'a -> 'b } [@@unboxed]

let with_return (type a) f =
  (* Raised to indicate ~return was called.  Local so that the exception is tied to a
     particular call of [with_return]. *)
  let exception Return of a in
  let is_alive = ref true in
  let return a =
    if not !is_alive
    then failwith "use of [return] from a [with_return] that already returned";
    Exn.raise_without_backtrace (Return a)
  in
  try
    let a = f { return } in
    is_alive := false;
    a
  with
  | exn ->
    is_alive := false;
    (match exn with
     | Return a -> a
     | _ -> raise exn)
;;

let with_return_option f =
  with_return (fun return ->
    f { return = (fun a -> return.return (Some a)) };
    None) [@nontail]
;;

let prepend { return } ~f = { return = (fun x -> return (f x)) }
