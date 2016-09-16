open! Import

(* Unfortunately, because the standard library does not expose [Random.State.default],
   we have to construct our own.  We then build the [Random.int], [Random.bool] functions
   and friends using that default state in exactly the same way as the standard library.

   One other trickiness is that we need access to the unexposed [Random.State.assign]
   function, which accesses the unexposed state representation.  So, we copy the
   [State.repr] type definition and [assign] function to here from the standard library,
   and use [Obj.magic] to get access to the underlying implementation. *)
open Random

(* Imported from Ppx_inline_test_lib.Runtime so that the bootstrapped Base doesn't depend
   on ppx_inline_test. *)
let testing = Array.length Sys.argv >= 2 && Sys.argv.(1) = "inline-test-runner"

(* Regression tests ought to be deterministic because that way anyone who breaks the test
   knows that it's their code that broke the test.  If tests are nondeterministic, a test
   failure may instead happen because the test runner got unlucky and uncovered an
   existing bug in the code supposedly being "protected" by the test in question. *)
let forbid_nondeterminism_in_tests ~allow_in_tests =
  if testing then
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

let%test_unit _ =
  (* test that the return type of "caml_sys_random_seed" is what we expect *)
  let obj = Obj.repr (random_seed ~allow_in_tests:true ()) in
  assert (Obj.is_block obj);
  assert (Obj.tag obj = Obj.tag (Obj.repr [| 13 |]));
  for i = 0 to Obj.size obj - 1 do
    assert (Obj.is_int (Obj.field obj i));
  done
;;

module State = struct
  include State

  let make_self_init ?allow_in_tests () =
    forbid_nondeterminism_in_tests ~allow_in_tests;
    make_self_init ()
  ;;

  type repr = { st : int array; mutable idx : int }

  let assign t1 t2 =
    let t1 = (Obj.magic t1 : repr) in
    let t2 = (Obj.magic t2 : repr) in
    Array.blit t2.st 0 t1.st 0 (Array.length t1.st);
    t1.idx <- t2.idx;
  ;;

  let full_init t seed = assign t (make seed)

  let default =
    (* We define Core's default random state as a copy of OCaml's default random state.
       This means that programs that use Core.Random will see the same sequence of random
       bits as if they had used OCaml.Random.  However, because [get_state] returns a
       copy, Core.Random and OCaml.Random are not using the same state.  If a program used
       both, each of them would go through the same sequence of random bits.  To avoid
       that, we reset OCaml's random state to a different seed, giving it a different
       sequence. *)
    let t = Random.get_state () in
    Random.init 137;
    t
  ;;

  let int_on_64bits t bound =
    if bound < (1 lsl 30)
    then int t bound
    else Int64.to_int (int64 t (Int64.of_int bound))

  let int = if Sys.word_size = 64 then int_on_64bits else State.int

  let%test_unit "random int above 2^30" [@tags "no-js"]=
    let state = make [| 1 ; 2 ; 3 ; 4 ; 5 |] in
    for _ = 1 to 100 do
      let bound = 1 lsl 40 in
      let n = int state bound in
      if n < 0 || n >= bound then
        failwith (Printf.sprintf "random result %d out of bounds (0,%d)" n (bound-1))
    done
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

let get_state () = `Consider_using_Random_State_default
let set_state s = State.assign default s
