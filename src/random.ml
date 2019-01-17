open! Import
open  Caml.Random

module Array = Array0
module Int   = Int0

(* Unfortunately, because the standard library does not expose
   [Caml.Random.State.default], we have to construct our own.  We then build the
   [Caml.Random.int], [Caml.Random.bool] functions and friends using that default state in
   exactly the same way as the standard library.

   One other trickiness is that we need access to the unexposed [Caml.Random.State.assign]
   function, which accesses the unexposed state representation.  So, we copy the
   [State.repr] type definition and [assign] function to here from the standard library,
   and use [Obj.magic] to get access to the underlying implementation. *)

(* Regression tests ought to be deterministic because that way anyone who breaks the test
   knows that it's their code that broke the test.  If tests are nondeterministic, a test
   failure may instead happen because the test runner got unlucky and uncovered an
   existing bug in the code supposedly being "protected" by the test in question. *)
let forbid_nondeterminism_in_tests ~allow_in_tests =
  if am_testing then
    match allow_in_tests with
    | Some true -> ()
    | None | Some false ->
      failwith "\
initializing Random with a nondeterministic seed is forbidden in inline tests"
;;

external random_seed: unit -> int array = "caml_sys_random_seed";;
let random_seed ?allow_in_tests () =
  forbid_nondeterminism_in_tests ~allow_in_tests;
  random_seed ()
;;

module State = struct
  include State

  let make_self_init ?allow_in_tests () =
    forbid_nondeterminism_in_tests ~allow_in_tests;
    make_self_init ()
  ;;

  type repr = { st : int array; mutable idx : int }

  let assign t1 t2 =
    let t1 = (Caml.Obj.magic t1 : repr) in
    let t2 = (Caml.Obj.magic t2 : repr) in
    Array.blit ~src:t2.st ~src_pos:0 ~dst:t1.st ~dst_pos:0
      ~len:(Array.length t1.st);
    t1.idx <- t2.idx;
  ;;

  let full_init t seed = assign t (make seed)

  let default =
    (* We define Core's default random state as a copy of OCaml's default random state.
       This means that programs that use Core.Random will see the same sequence of random
       bits as if they had used Caml.Random.  However, because [get_state] returns a
       copy, Core.Random and OCaml.Random are not using the same state.  If a program used
       both, each of them would go through the same sequence of random bits.  To avoid
       that, we reset OCaml's random state to a different seed, giving it a different
       sequence. *)
    let t = Caml.Random.get_state () in
    Caml.Random.init 137;
    t
  ;;

  let int_on_64bits t bound =
    if bound <= 0x3FFFFFFF (* (1 lsl 30) - 1 *)
    then int t bound
    else Caml.Int64.to_int (int64 t (Caml.Int64.of_int bound))
  ;;

  let int_on_32bits t bound =
    (* Not always true with the JavaScript backend. *)
    if bound <= 0x3FFFFFFF (* (1 lsl 30) - 1 *)
    then int t bound
    else Caml.Int32.to_int (int32 t (Caml.Int32.of_int bound))
  ;;

  let int =
    match Word_size.word_size with
    | W64 -> int_on_64bits
    | W32 -> int_on_32bits
  ;;

  let full_range_int64 =
    let open Caml.Int64 in
    let bits state = of_int (bits state) in
    fun state ->
      logxor (bits state)
        (logxor
           (shift_left (bits state) 30)
           (shift_left (bits state) 60))
  ;;

  let full_range_int32 =
    let open Caml.Int32 in
    let bits state = of_int (bits state) in
    fun state ->
      logxor (bits state)
        (shift_left (bits state) 30)
  ;;

  let full_range_int_on_64bits state =
    Caml.Int64.to_int (full_range_int64 state)
  ;;

  let full_range_int_on_32bits state =
    Caml.Int32.to_int (full_range_int32 state)
  ;;

  let full_range_int =
    match Word_size.word_size with
    | W64 -> full_range_int_on_64bits
    | W32 -> full_range_int_on_32bits
  ;;

  let full_range_nativeint_on_64bits state =
    Caml.Int64.to_nativeint (full_range_int64 state)
  ;;

  let full_range_nativeint_on_32bits state =
    Caml.Nativeint.of_int32 (full_range_int32 state)
  ;;

  let full_range_nativeint =
    match Word_size.word_size with
    | W64 -> full_range_nativeint_on_64bits
    | W32 -> full_range_nativeint_on_32bits
  ;;

  let [@inline never] raise_crossed_bounds name lower_bound upper_bound string_of_bound =
    Printf.failwithf "Random.%s: crossed bounds [%s > %s]"
      name (string_of_bound lower_bound) (string_of_bound upper_bound) ()
  ;;

  let int_incl =
    let rec in_range state lo hi =
      let int = full_range_int state in
      if int >= lo && int <= hi
      then int
      else in_range state lo hi
    in
    fun state lo hi ->
      if lo > hi then raise_crossed_bounds "int" lo hi Int.to_string;
      let diff = hi - lo in
      if diff = Int.max_value
      then lo + ((full_range_int state) land Int.max_value)
      else if diff >= 0
      then lo + int state (Int.succ diff)
      else in_range state lo hi
  ;;

  let int32_incl =
    let open Int32_replace_polymorphic_compare in
    let rec in_range state lo hi =
      let int = full_range_int32 state in
      if int >= lo && int <= hi
      then int
      else in_range state lo hi
    in
    let open Caml.Int32 in
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
      if int >= lo && int <= hi
      then int
      else in_range state lo hi
    in
    let open Caml.Nativeint in
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
      if int >= lo && int <= hi
      then int
      else in_range state lo hi
    in
    let open Caml.Int64 in
    fun state lo hi ->
      if lo > hi then raise_crossed_bounds "int64" lo hi to_string;
      let diff = sub hi lo in
      if diff = max_int
      then add lo (logand (full_range_int64 state) max_int)
      else if diff >= 0L
      then add lo (int64 state (succ diff))
      else in_range state lo hi
  ;;

  let float_range state lo hi =
    let open Float_replace_polymorphic_compare in
    if lo > hi then raise_crossed_bounds "float" lo hi Caml.string_of_float;
    lo +. float state (hi -. lo)
  ;;
end

let default = State.default

let bits () = State.bits default

let int       x = State.int       default x
let int32     x = State.int32     default x
let nativeint x = State.nativeint default x
let int64     x = State.int64     default x
let float     x = State.float     default x

let int_incl       x y = State.int_incl       default x y
let int32_incl     x y = State.int32_incl     default x y
let nativeint_incl x y = State.nativeint_incl default x y
let int64_incl     x y = State.int64_incl     default x y
let float_range    x y = State.float_range    default x y

let bool () = State.bool default

let full_init seed = State.full_init default seed
let init seed = full_init [| seed |]
let self_init ?allow_in_tests () = full_init (random_seed ?allow_in_tests ())

let set_state s = State.assign default s
