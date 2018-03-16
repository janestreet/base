open! Import

let invalid_argf = Printf.invalid_argf

let [@inline never] slow_check_pos_len_exn ~pos ~len ~length =
  if pos < 0
  then invalid_argf "Negative position: %d" pos ();
  if len < 0
  then invalid_argf "Negative length: %d" len ();
  if pos > length - len
  then invalid_argf "pos + len past end: %d + %d > %d" pos len length ()
;;

let check_pos_len_exn ~pos ~len ~length =
  (* This is better than [slow_check_pos_len_exn] for two reasons:

     - much less inlined code
     - only one conditional jump

     The reason it works is that checking [< 0] is testing the highest order bit, so
     [a < 0 || b < 0] is the same as [a lor b < 0].

     [pos + len] can overflow, so [pos > length - len] is not equivalent to [length - len
     - pos < 0], we need to test for [pos + len] overflow as well. *)
  let stop = pos + len in
  if pos lor len lor stop lor (length - stop) < 0 then
    slow_check_pos_len_exn ~pos ~len ~length
;;

let get_pos_len_exn ?(pos = 0) ?len ~length =
  let len = match len with Some i -> i | None -> length - pos in
  check_pos_len_exn ~pos ~len ~length;
  pos, len
;;

let get_pos_len ?pos ?len ~length =
  try Result.Ok (get_pos_len_exn ?pos ?len ~length)
  with Invalid_argument s -> Result.Error s
;;

module Private = struct
  let slow_check_pos_len_exn = slow_check_pos_len_exn
end
