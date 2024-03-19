open! Import
open! Map
open! Int

module Lazy_apply = struct
  module T = struct
    type 'a t = { compute : unit -> 'a } [@@unboxed]

    let run t = t.compute ()
    let return x = { compute = (fun () -> x) }
    let map x ~f = { compute = (fun () -> f (run x)) }
    let both x y = { compute = (fun () -> run x, run y) }
    let map2 a b ~f = map (both a b) ~f:(fun (x, y) -> f x y)
    let map = `Custom map
  end

  include T
  include Applicative.Make_using_map2 (T)

  let of_thunk f = { compute = (fun () -> run (f ())) }
end

module Lazy_map = Map.Make_applicative_traversals (Lazy_apply)

let%expect_test "mapi correctness check" =
  let map = Map.of_alist_exn (module Int) (List.init 100 ~f:(fun x -> x, x)) in
  let f ~key:_ ~data = data + 1 in
  let test_output =
    Lazy_map.mapi map ~f:(fun ~key ~data -> Lazy_apply.return (f ~key ~data))
    |> Lazy_apply.run
  in
  let reference_output = Map.mapi map ~f in
  require [%here] (Map.equal Int.equal test_output reference_output);
  require [%here] (Map.invariants test_output);
  [%expect {| |}]
;;

let%expect_test "filter_mapi correctness check" =
  let map = Map.of_alist_exn (module Int) (List.init 1000 ~f:(fun x -> x, x)) in
  let f ~key:_ ~data = if data % 50 > 10 then None else Some data in
  let test_output =
    Lazy_map.filter_mapi map ~f:(fun ~key ~data -> Lazy_apply.return (f ~key ~data))
    |> Lazy_apply.run
  in
  let reference_output = Map.filter_mapi map ~f in
  require [%here] (Map.equal Int.equal test_output reference_output);
  require [%here] (Map.invariants test_output);
  [%expect {| |}]
;;

module Step_applicative = struct
  module M = struct
    type 'a t = { compute : steps:int -> ('a * int, 'a t) Either.t }

    let return x = { compute = (fun ~steps -> First (x, steps)) }

    let step x =
      let rec t =
        { compute = (fun ~steps -> if steps > 0 then First (x, steps - 1) else Second t) }
      in
      t
    ;;

    let internal_map x ~f =
      let rec fn t =
        { compute =
            (fun ~steps ->
              match t.compute ~steps with
              | First (x, steps) -> First (f x, steps)
              | Second t -> Second (fn t))
        }
      in
      fn x
    ;;

    let map2 a b ~f =
      let rec fn a =
        { compute =
            (fun ~steps ->
              match a.compute ~steps with
              | First (x, steps) -> (internal_map b ~f:(fun y -> f x y)).compute ~steps
              | Second t -> Second (fn t))
        }
      in
      fn a
    ;;

    let map = `Custom internal_map

    let of_thunk f =
      { compute =
          (fun ~steps ->
            let t = f () in
            t.compute ~steps)
      }
    ;;
  end

  include M
  include Applicative.Make_using_map2 (M)
end

module Step_map = Map.Make_applicative_traversals (Step_applicative)

let%expect_test "mapi lazy check" =
  let map = Map.of_alist_exn (module Int) (List.init 10 ~f:(fun x -> x, x)) in
  let f ~key:_ ~data = data * 2 in
  (* transform the map, expect no output yet *)
  let step_computation =
    Step_map.mapi map ~f:(fun ~key ~data ->
      Step_applicative.of_thunk (fun () ->
        print_s [%message (key : int) (data : int)];
        Step_applicative.step (f ~key ~data)))
  in
  [%expect {| |}];
  (* take a few steps, expect some but not all output *)
  let more_computation =
    match step_computation.compute ~steps:3 with
    | First _ -> assert false
    | Second c -> c
  in
  [%expect
    {|
    ((key  0)
     (data 0))
    ((key  1)
     (data 1))
    ((key  2)
     (data 2))
    ((key  3)
     (data 3))
    |}];
  (* take more than enough steps to finish, expect the rest of the output *)
  let test_output =
    match more_computation.compute ~steps:100 with
    | First (r, _) -> r
    | Second _ -> assert false
  in
  let reference_output = Map.mapi map ~f in
  require [%here] (Map.equal Int.equal test_output reference_output);
  [%expect
    {|
    ((key  4)
     (data 4))
    ((key  5)
     (data 5))
    ((key  6)
     (data 6))
    ((key  7)
     (data 7))
    ((key  8)
     (data 8))
    ((key  9)
     (data 9))
    |}]
;;
