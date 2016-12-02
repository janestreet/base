open! Import
open! Ordered_collection_common

let%test_unit "fast check_pos_len_exn is correct" =
  let n_vals =
    [ 0
    ; 1
    ; 2
    ; 10
    ; 100
    ; Int.max_value / 2 - 2
    ; Int.max_value / 2 - 1
    ; Int.max_value / 2
    ; Int.max_value - 2
    ; Int.max_value - 1
    ; Int.max_value
    ]
  in
  let z_vals =
    [ Int.min_value
    ; Int.min_value + 1
    ; Int.min_value + 2
    ; Int.min_value / 2
    ; Int.min_value / 2 + 1
    ; Int.min_value / 2 + 2
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
        assert
          (Bool.equal
             (does_raise (fun () -> Private.slow_check_pos_len_exn ~pos ~len ~length))
             (does_raise (fun () ->              check_pos_len_exn ~pos ~len ~length))))))
;;

let%test_unit _ =
  let vals = [ -1; 0; 1; 2; 3 ] in
  List.iter [ 0; 1; 2 ] ~f:(fun length ->
    List.iter vals ~f:(fun pos ->
      List.iter vals ~f:(fun len ->
        let result = Result.try_with (fun () -> check_pos_len_exn ~pos ~len ~length) in
        let valid = pos >= 0 && len >= 0 && len <= length - pos in
        assert (Bool.equal valid (Result.is_ok result)))))
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
