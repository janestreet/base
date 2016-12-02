open! Import
open  Caml.Random

module Array = Array0

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
       bits as if they had used OCaml.Caml.Random.  However, because [get_state] returns a
       copy, Core.Random and OCaml.Random are not using the same state.  If a program used
       both, each of them would go through the same sequence of random bits.  To avoid
       that, we reset OCaml's random state to a different seed, giving it a different
       sequence. *)
    let t = Caml.Random.get_state () in
    Caml.Random.init 137;
    t
  ;;

  let int_on_64bits t bound =
    if bound < (1 lsl 30)
    then int t bound
    else Caml.Int64.to_int (int64 t (Caml.Int64.of_int bound))
  ;;

  let int =
    match Word_size.word_size with
    | W64 -> int_on_64bits
    | W32 -> State.int
  ;;
end

let default = State.default

let bits () = State.bits default
let int bound = State.int default bound
let int32 bound = State.int32 default bound
let nativeint bound = State.nativeint default bound
let int64 bound = State.int64 default bound
let float scale = State.float default scale
let bool () = State.bool default

let full_init seed = State.full_init default seed
let init seed = full_init [| seed |]
let self_init ?allow_in_tests () = full_init (random_seed ?allow_in_tests ())

let set_state s = State.assign default s
