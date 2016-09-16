(** An internal-only module factored out due to a circular dependency between core_array
    and core_list.  Contains code for permuting an array. *)

open! Import

module Random = Base_random

let swap t i j =
  let tmp = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- tmp

(** randomly permute an array. *)
let permute ?(random_state = Random.State.default) t =
  for i = Array.length t downto 2 do
    swap t (i - 1) (Random.State.int random_state i)
  done
