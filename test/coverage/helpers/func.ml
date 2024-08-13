open! Base
open Base_quickcheck
open Overrides
include Func_intf.Definitions

(* Randomly generated functions with sexpable representations. Represented as an initial
   output value and a set of transitions to new outputs at intervals along the inputs. The
   transitions need not be sorted; the [apply] functions find the closest applicable
   input. *)
type ('input, 'output) t =
  { initial : 'output
  ; transitions : ('input * 'output) list
  }
[@@deriving equal, quickcheck, sexp_of]

let inputs t = List.map t.transitions ~f:fst
let outputs t = t.initial :: List.map t.transitions ~f:snd

let map { initial; transitions } ~i ~o =
  { initial = o initial
  ; transitions = List.map transitions ~f:(fun (input, output) -> i input, o output)
  }
;;

let apply (type a) t (module Input : With_compare with type t = a) x =
  List.fold
    ~init:(None, t.initial)
    t.transitions
    ~f:(fun (prev_input, prev_output) (input, output) ->
      if Input.compare x input < 0
      then prev_input, prev_output
      else (
        match prev_input with
        | None -> Some input, output
        | Some prev_input ->
          if Input.compare prev_input input < 0
          then Some input, output
          else Some prev_input, prev_output))
  |> snd
;;

let apply2 t m1 m2 x1 x2 =
  let t = apply t m1 x1 in
  apply t m2 x2
;;

let apply3 t m1 m2 m3 x1 x2 x3 =
  let t = apply t m1 x1 in
  let t = apply t m2 x2 in
  apply t m3 x3
;;

(* Associate each (input,output) pair in [transitions] with the subset of [inputs] that
   would be assigned that output. Also return unmatched values that would get the
   [initial] output. *)
let associate
  (type a)
  (module Input : With_compare with type t = a)
  ~sorted_transitions:transitions
  ~sorted_inputs:inputs
  =
  let rev_inputs, transitions =
    List.fold_right
      transitions
      ~init:(List.rev inputs, [])
      ~f:(fun (input, output) (inputs, transitions) ->
        let matching, non_matching =
          List.split_while inputs ~f:(fun i -> Input.compare i input >= 0)
        in
        non_matching, ((input, output), List.rev matching) :: transitions)
  in
  List.rev rev_inputs, transitions
;;

(* Assign every input a separate transition that will apply to only it. *)
let split_transitions_by_input transitions =
  List.concat_map transitions ~f:(fun ((maybe_input, output), inputs) ->
    match inputs with
    | [] | [ _ ] -> [ maybe_input, output ]
    | _ :: _ :: _ ->
      List.mapi inputs ~f:(fun i input ->
        if i = 0 then maybe_input, output else Some input, output))
;;

(* Produce a version of [t] where each of [inputs] is assigned a different output. *)
let injective
  (type a b)
  { initial; transitions }
  (module Input : With_compare with type t = a)
  (module Output : Adjustable.S with type t = b)
  inputs
  =
  let inputs = List.dedup_and_sort inputs ~compare:Input.compare in
  let transitions =
    List.stable_sort transitions ~compare:(Comparable.lift Input.compare ~f:fst)
  in
  let transitions =
    let inputs, transitions =
      associate (module Input) ~sorted_transitions:transitions ~sorted_inputs:inputs
    in
    ((None, initial), inputs)
    :: List.map transitions ~f:(fun ((input, output), inputs) ->
      (Some input, output), inputs)
  in
  let transitions = split_transitions_by_input transitions in
  let transitions =
    Adjustable.non_overlapping
      (module struct
        type t = Input.t option * Output.t

        let get (_, output) = Output.get output
        let set (input, output) n = input, Output.set output n
      end)
      transitions
  in
  let initials, transitions =
    List.partition_map transitions ~f:(function
      | None, output -> First output
      | Some input, output -> Second (input, output))
  in
  match initials with
  | [] | _ :: _ :: _ -> assert false
  | [ initial ] -> { initial; transitions }
;;
