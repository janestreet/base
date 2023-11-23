let () =
  let ver, output =
    match Sys.argv with
    | [| _; "-ocaml-version"; v; "-o"; fn |] ->
      Scanf.sscanf v "%d.%d" (fun major minor -> major, minor), fn
    | _ -> failwith "bad command line arguments"
  in
  let oc = open_out output in
  if ver >= (5, 0)
  then
    Printf.fprintf
      oc
      {|
type t = Stdlib.Random.State.t Stdlib.Domain.DLS.key

let assign t state = Stdlib.Domain.DLS.set t (Stdlib.Random.State.copy state)

let make state =
  let split_from_parent v = Stdlib.Random.State.split v in
  let t = Stdlib.Domain.DLS.new_key ~split_from_parent (fun () -> state) in
  Stdlib.Domain.DLS.get t |> ignore;
  t
;;

let make_lazy ~f =
  let split_from_parent v = Stdlib.Random.State.split v in
  Stdlib.Domain.DLS.new_key ~split_from_parent f
;;

let[@inline always] get_state t = Stdlib.Domain.DLS.get t
|}
  else
    Printf.fprintf
      oc
      {|
module Array = Array0

type t = Stdlib.Random.State.t Lazy.t

module Repr = struct
  type t =
    { st : int array
    ; mutable idx : int
    }

  let of_state : Stdlib.Random.State.t -> t = Stdlib.Obj.magic
end

let assign t state =
  let t1 = Repr.of_state (Lazy.force t) in
  let t2 = Repr.of_state state in
  Array.blit ~src:t2.st ~src_pos:0 ~dst:t1.st ~dst_pos:0 ~len:(Array.length t1.st);
  t1.idx <- t2.idx

let make state = Lazy.from_val state

let make_lazy ~f = Lazy.from_fun f

let[@inline always] get_state t = Lazy.force t
|};
  close_out oc
;;
