open! Import
module Int = Int0
module Char = Char0

(* Unfortunately, because the standard library does not expose
   [Stdlib.Random.State.default], we have to construct our own.  We then build the
   [Stdlib.Random.int], [Stdlib.Random.bool] functions and friends using that default state in
   exactly the same way as the standard library. *)

(* Regression tests ought to be deterministic because that way anyone who breaks the test
   knows that it's their code that broke the test.  If tests are nondeterministic, a test
   failure may instead happen because the test runner got unlucky and uncovered an
   existing bug in the code supposedly being "protected" by the test in question. *)
let forbid_nondeterminism_in_tests ~allow_in_tests =
  if am_testing
  then (
    match allow_in_tests with
    | Some true -> ()
    | None | Some false ->
      failwith
        "initializing Random with a nondeterministic seed is forbidden in inline tests")
;;

external random_seed : unit -> int array = "caml_sys_random_seed"

let random_seed ?allow_in_tests () =
  forbid_nondeterminism_in_tests ~allow_in_tests;
  random_seed ()
;;

module Repr = Random_repr

module State = struct
  type t = Repr.t

  let bits t = Stdlib.Random.State.bits (Repr.get_state t)
  let bits64 t = Stdlib.Random.State.bits64 (Repr.get_state t)
  let bool t = Stdlib.Random.State.bool (Repr.get_state t)
  let int t x = Stdlib.Random.State.int (Repr.get_state t) x
  let int32 t x = Stdlib.Random.State.int32 (Repr.get_state t) x
  let int64 t x = Stdlib.Random.State.int64 (Repr.get_state t) x
  let nativeint t x = Stdlib.Random.State.nativeint (Repr.get_state t) x
  let make seed = Repr.make (Stdlib.Random.State.make seed)
  let copy t = Repr.make (Stdlib.Random.State.copy (Repr.get_state t))
  let char t = int t 256 |> Char.unsafe_of_int
  let ascii t = int t 128 |> Char.unsafe_of_int

  let make_self_init ?allow_in_tests () =
    forbid_nondeterminism_in_tests ~allow_in_tests;
    Repr.make_lazy ~f:Stdlib.Random.State.make_self_init
  ;;

  let assign = Repr.assign
  let full_init t seed = assign t (Stdlib.Random.State.make seed)

  let default =
    if am_testing
    then (
      (* We define Base's default random state as a copy of OCaml's default random state.
         This means that programs that use Base.Random will see the same sequence of
         random bits as if they had used Stdlib.Random. However, because [get_state] returns
         a copy, Base.Random and OCaml.Random are not using the same state. If a program
         used both, each of them would go through the same sequence of random bits. To
         avoid that, we reset OCaml's random state to a different seed, giving it a
         different sequence. *)
      let t = Stdlib.Random.get_state () in
      Stdlib.Random.init 137;
      Repr.make t)
    else
      (* Outside of tests, we initialize random state nondeterministically and lazily.
         We force the random initialization to be lazy so that we do not pay any cost
         for it in programs that do not use randomness. *)
      make_self_init ()
  ;;

  let int_on_64bits t bound =
    if bound <= 0x3FFFFFFF (* (1 lsl 30) - 1 *)
    then int t bound
    else Stdlib.Int64.to_int (int64 t (Stdlib.Int64.of_int bound))
  ;;

  let int_on_32bits t bound =
    (* Not always true with the JavaScript backend. *)
    if bound <= 0x3FFFFFFF (* (1 lsl 30) - 1 *)
    then int t bound
    else Stdlib.Int32.to_int (int32 t (Stdlib.Int32.of_int bound))
  ;;

  let int =
    match Word_size.word_size with
    | W64 -> int_on_64bits
    | W32 -> int_on_32bits
  ;;

  let full_range_int64 =
    let open Stdlib.Int64 in
    let bits state = of_int (bits state) in
    fun state ->
      logxor
        (bits state)
        (logxor (shift_left (bits state) 30) (shift_left (bits state) 60))
  ;;

  let full_range_int32 =
    let open Stdlib.Int32 in
    let bits state = of_int (bits state) in
    fun state -> logxor (bits state) (shift_left (bits state) 30)
  ;;

  let full_range_int_on_64bits state = Stdlib.Int64.to_int (full_range_int64 state)
  let full_range_int_on_32bits state = Stdlib.Int32.to_int (full_range_int32 state)

  let full_range_int =
    match Word_size.word_size with
    | W64 -> full_range_int_on_64bits
    | W32 -> full_range_int_on_32bits
  ;;

  let full_range_nativeint_on_64bits state =
    Stdlib.Int64.to_nativeint (full_range_int64 state)
  ;;

  let full_range_nativeint_on_32bits state =
    Stdlib.Nativeint.of_int32 (full_range_int32 state)
  ;;

  let full_range_nativeint =
    match Word_size.word_size with
    | W64 -> full_range_nativeint_on_64bits
    | W32 -> full_range_nativeint_on_32bits
  ;;

  let raise_crossed_bounds name lower_bound upper_bound string_of_bound =
    Printf.failwithf
      "Random.%s: crossed bounds [%s > %s]"
      name
      (string_of_bound lower_bound)
      (string_of_bound upper_bound)
      ()
    [@@cold] [@@inline never] [@@local never] [@@specialise never]
  ;;

  let int_incl =
    let rec in_range state lo hi =
      let int = full_range_int state in
      if int >= lo && int <= hi then int else in_range state lo hi
    in
    fun state lo hi ->
      if lo > hi then raise_crossed_bounds "int" lo hi Int.to_string;
      let diff = hi - lo in
      if diff = Int.max_value
      then lo + (full_range_int state land Int.max_value)
      else if diff >= 0
      then lo + int state (Int.succ diff)
      else in_range state lo hi
  ;;

  let int32_incl =
    let open Int32_replace_polymorphic_compare in
    let rec in_range state lo hi =
      let int = full_range_int32 state in
      if int >= lo && int <= hi then int else in_range state lo hi
    in
    let open Stdlib.Int32 in
    fun state lo hi ->
      if lo > hi then raise_crossed_bounds "int32" lo hi to_string;
      let diff = sub hi lo in
      if diff = max_int
      then add lo (logand (full_range_int32 state) max_int)
      else if diff >= 0l
      then add lo (int32 state (succ diff))
      else in_range state lo hi
  ;;

  let nativeint_incl =
    let open Nativeint_replace_polymorphic_compare in
    let rec in_range state lo hi =
      let int = full_range_nativeint state in
      if int >= lo && int <= hi then int else in_range state lo hi
    in
    let open Stdlib.Nativeint in
    fun state lo hi ->
      if lo > hi then raise_crossed_bounds "nativeint" lo hi to_string;
      let diff = sub hi lo in
      if diff = max_int
      then add lo (logand (full_range_nativeint state) max_int)
      else if diff >= 0n
      then add lo (nativeint state (succ diff))
      else in_range state lo hi
  ;;

  let int64_incl =
    let open Int64_replace_polymorphic_compare in
    let rec in_range state lo hi =
      let int = full_range_int64 state in
      if int >= lo && int <= hi then int else in_range state lo hi
    in
    let open Stdlib.Int64 in
    fun state lo hi ->
      if lo > hi then raise_crossed_bounds "int64" lo hi to_string;
      let diff = sub hi lo in
      if diff = max_int
      then add lo (logand (full_range_int64 state) max_int)
      else if diff >= 0L
      then add lo (int64 state (succ diff))
      else in_range state lo hi
  ;;

  (* Return a uniformly random float in [0, 1). *)
  let rec rawfloat state =
    let open Float_replace_polymorphic_compare in
    let scale = 0x1p-30 in
    (* 2^-30 *)
    let r1 = Stdlib.float_of_int (bits state) in
    let r2 = Stdlib.float_of_int (bits state) in
    let result = ((r1 *. scale) +. r2) *. scale in
    (* With very small probability, result can round up to 1.0, so in that case, we just
       try again. *)
    if result < 1.0 then result else rawfloat state
  ;;

  let float state hi = rawfloat state *. hi

  let float_range state lo hi =
    let open Float_replace_polymorphic_compare in
    if lo > hi then raise_crossed_bounds "float" lo hi Stdlib.string_of_float;
    lo +. float state (hi -. lo)
  ;;
end

let default = State.default
let bits () = State.bits default
let bits64 () = State.bits64 default
let int x = State.int default x
let int32 x = State.int32 default x
let nativeint x = State.nativeint default x
let int64 x = State.int64 default x
let float x = State.float default x
let int_incl x y = State.int_incl default x y
let int32_incl x y = State.int32_incl default x y
let nativeint_incl x y = State.nativeint_incl default x y
let int64_incl x y = State.int64_incl default x y
let float_range x y = State.float_range default x y
let bool () = State.bool default
let char () = State.char default
let ascii () = State.ascii default
let full_init seed = State.full_init default seed
let init seed = full_init [| seed |]
let self_init ?allow_in_tests () = full_init (random_seed ?allow_in_tests ())
let set_state s = State.assign default (Repr.get_state s)
