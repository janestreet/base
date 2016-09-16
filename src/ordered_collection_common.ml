open! Import

module List = ListLabels

let invalid_argf = Base_printf.invalid_argf

let normalize ~length_fun t i =
  if i < 0
  then i + length_fun t
  else i

let slice ~length_fun ~sub_fun t start stop =
  let stop = if stop = 0 then length_fun t else stop in
  let pos = normalize ~length_fun t start in
  let len = (normalize ~length_fun t stop) - pos in
  sub_fun t ~pos ~len

let slow_check_pos_len_exn ~pos ~len ~length =
  let _f () = () in (* prevents inlining *)
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

let%test_unit "fast check_pos_len_exn is correct" =
  let n_vals =
    [ 0
    ; 1
    ; 2
    ; 10
    ; 100
    ; max_int / 2 - 2
    ; max_int / 2 - 1
    ; max_int / 2
    ; max_int - 2
    ; max_int - 1
    ; max_int
    ]
  in
  let z_vals =
    [ min_int
    ; min_int + 1
    ; min_int + 2
    ; min_int / 2
    ; min_int / 2 + 1
    ; min_int / 2 + 2
    ; -100
    ; -10
    ; -2
    ; -1
    ] @ n_vals
  in
  let does_raise f = try f (); false with _ -> true in
  List.iter z_vals ~f:(fun pos ->
    List.iter z_vals ~f:(fun len ->
      List.iter n_vals ~f:(fun length ->
        assert (does_raise (fun () -> slow_check_pos_len_exn ~pos ~len ~length) =
                does_raise (fun () ->      check_pos_len_exn ~pos ~len ~length)))))
;;

let%bench_module "check_pos_len_exn" =
  (module struct
    let fast_check_pos_len_exn ~pos ~len ~length =
      let _f () = () in (* prevents inlining, otherwise it'd compile to [()] *)
      check_pos_len_exn ~pos ~len ~length
    ;;

    let%bench "slow" = slow_check_pos_len_exn ~pos:42 ~len:42 ~length:100
    let%bench "fast" = fast_check_pos_len_exn ~pos:42 ~len:42 ~length:100
  end)
;;

let%test_unit _ =
  let vals = [ -1; 0; 1; 2; 3 ] in
  List.iter [ 0; 1; 2 ] ~f:(fun length ->
    List.iter vals ~f:(fun pos ->
      List.iter vals ~f:(fun len ->
        let result = Result.try_with (fun () -> check_pos_len_exn ~pos ~len ~length) in
        let valid = pos >= 0 && len >= 0 && len <= length - pos in
        assert (valid = Result.is_ok result))))
;;

let get_pos_len_exn ?(pos = 0) ?len ~length =
  let len = match len with Some i -> i | None -> length - pos in
  check_pos_len_exn ~pos ~len ~length;
  pos, len
;;

let%test_unit _ =
  let opts = [ None; Some (-1); Some 0; Some 1; Some 2 ] in
  List.iter [ 0; 1; 2 ] ~f:(fun length ->
    List.iter opts ~f:(fun pos ->
      List.iter opts ~f:(fun len ->
        let result = Result.try_with (fun () -> get_pos_len_exn ?pos ?len ~length) in
        let pos = match pos with Some x -> x | None -> 0 in
        let len = match len with Some x -> x | None -> length - pos in
        let valid = pos >= 0 && len >= 0 && len <= length - pos in
        match result with
        | Error _ -> assert (not valid);
        | Ok (pos', len') ->
          assert (pos' = pos);
          assert (len' = len);
          assert valid)))
;;

let get_pos_len ?pos ?len ~length =
  try Result.Ok (get_pos_len_exn ?pos ?len ~length)
  with Invalid_argument s -> Result.Error s
