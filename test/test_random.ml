open! Import
open! Random

module State = struct
  include State

  let%test_unit "random int above 2^30" [@tags "64-bits-only"] =
    let state = make [| 1 ; 2 ; 3 ; 4 ; 5 |] in
    for _ = 1 to 100 do
      let bound = Int.shift_left 1 40 in
      let n = int state bound in
      if n < 0 || n >= bound then
        failwith (Printf.sprintf "random result %d out of bounds (0,%d)" n (bound-1))
    done
  ;;
end

external random_seed: unit -> Caml.Obj.t = "caml_sys_random_seed";;
let%test_unit _ =
  (* test that the return type of "caml_sys_random_seed" is what we expect *)
  let module Obj = Caml.Obj in
  let obj = random_seed () in
  assert (Obj.is_block obj);
  assert (Obj.tag obj = Obj.tag (Obj.repr [| 13 |]));
  for i = 0 to Obj.size obj - 1 do
    assert (Obj.is_int (Obj.field obj i));
  done
;;
