open! Import
module Int = Int0
module Char = Char0
module Capsule = Capsule_expert

(* Unfortunately, because the standard library does not expose
   [Stdlib.Random.State.default], we have to construct our own. We then build the
   [Stdlib.Random.int], [Stdlib.Random.bool] functions and friends using that default
   state in exactly the same way as the standard library. *)
module Random_repr : sig
  val assign
    :  src:Stdlib.Random.State.t @ local
    -> dst:Stdlib.Random.State.t @ local
    -> unit
    @@ portable
end = struct
  module Repr = struct
    open Stdlib.Bigarray

    type t = (int64, int64_elt, c_layout) Array1.t

    external of_state
      :  (Stdlib.Random.State.t[@local_opt])
      -> (t[@local_opt])
      @@ portable
      = "%identity"
  end

  module Array1 = struct
    external blit
      :  (('a, 'b, 'c) Stdlib.Bigarray.Array1.t[@local_opt])
      -> (('a, 'b, 'c) Stdlib.Bigarray.Array1.t[@local_opt])
      -> unit
      @@ portable
      = "caml_ba_blit"
  end

  let assign ~src ~dst =
    let dst = Repr.of_state dst in
    let src = Repr.of_state src in
    Array1.blit src dst
  ;;
end

(* Regression tests ought to be deterministic because that way anyone who breaks the test
   knows that it's their code that broke the test. If tests are nondeterministic, a test
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

external random_seed : unit -> int array @@ portable = "caml_sys_random_seed"

let random_seed ?allow_in_tests () =
  forbid_nondeterminism_in_tests ~allow_in_tests;
  random_seed ()
;;

(* Define functions for safely letting [Stdlib.Random.State.t] interact with capsules. *)
open (
struct
  external magic_unwrap_capsule
    :  ('a, 'k) Capsule.Data.t @ local
    -> 'a @ local
    @@ portable
    = "%identity"

  let assign_capsule ~password:_ ~src ~dst =
    Random_repr.assign ~src:(magic_unwrap_capsule src) ~dst [@nontail]
  ;;

  external magic_wrap_capsule : 'a -> ('a, 'k) Capsule.Data.t @@ portable = "%identity"

  let copy_into_capsule src = magic_wrap_capsule (Stdlib.Random.State.copy src)
  let split_into_capsule src = magic_wrap_capsule (Stdlib.Random.State.split src)
end :
sig
@@ portable
  (* This is safe because we have shared access to the capsule ['k], and only copy
     immutable data from [src] into [dst] (the integer values of the underlying state
     array). *)
  val assign_capsule
    :  password:'k Capsule.Password.Shared.t @ local
    -> src:(Stdlib.Random.State.t, 'k) Capsule.Data.t @ local
    -> dst:Stdlib.Random.State.t @ local
    -> unit

  (* This is safe because the resulting state is unique. *)
  val copy_into_capsule
    :  Stdlib.Random.State.t
    -> (Stdlib.Random.State.t, 'k) Capsule.Data.t

  (* This is safe because the resulting state is unique. *)
  val split_into_capsule
    :  Stdlib.Random.State.t
    -> (Stdlib.Random.State.t, 'k) Capsule.Data.t
end)

module State = struct
  (* Make [t] abstract for the implementation of the functions below. *)
  module T : sig @@ portable
    type t : value mod portable

    val default : t
    val get_default : unit -> t
    val of_stdlib : Stdlib.Random.State.t -> t
    val of_stdlib_lazy : f:(unit -> Stdlib.Random.State.t) -> t

    val with_stdlib
      : ('a : value mod contended portable).
      f:(Stdlib.Random.State.t -> 'a) @ local portable unyielding -> t -> 'a
  end = struct
    module DLS = Basement.Stdlib_shim.Domain.Safe.DLS

    let default_state =
      let%template state_initializer =
        if am_testing
        then (
          (* We define Base's default random state as a copy of OCaml's default random
             state. This means that programs that use Base.Random will see the same
             sequence of random bits as if they had used Stdlib.Random. However, because
             [get_state] returns a copy, Base.Random and OCaml.Random are not using the
             same state. If a program used both, each of them would go through the same
             sequence of random bits. To avoid that, we reset OCaml's random state to a
             different seed, giving it a different sequence. *)
          let (P (type k) (key : k Capsule.Key.t)) = Capsule.create () in
          let initial_state = copy_into_capsule (Stdlib.Random.get_state ()) in
          Stdlib.Random.init 137;
          (* This function is run at most once per domain, and we only destroy the key on
             the initial domain. *)
          (Obj.magic_many [@mode portable]) (fun () ->
            if Stdlib.Domain.is_main_domain ()
            then (
              let access = Capsule.Key.destroy key in
              Capsule.Data.unwrap ~access initial_state)
            else Stdlib.Random.State.make_self_init ()))
        else
          (* Outside of tests, we initialize random state nondeterministically and lazily.
             We force the random initialization to be lazy so that we do not pay any cost
             for it in programs that do not use randomness. *)
          Stdlib.Random.State.make_self_init
      in
      let default_state_key =
        DLS.new_key
          ~split_from_parent:(fun state ->
            let (P (type k) (key : k Capsule.Key.t)) = Capsule.create () in
            let state = split_into_capsule state in
            fun () ->
              let access = Capsule.Key.destroy key in
              Capsule.Data.unwrap ~access state)
          state_initializer
      in
      default_state_key
    ;;

    type inner =
      | Default
      | Custom of Stdlib.Random.State.t Lazy.t

    type t = inner Modes.Portable_via_contended.t

    let wrap = Modes.Portable_via_contended.wrap
    let unwrap = Modes.Portable_via_contended.unwrap
    let default = wrap Default
    let[@inline] get_default () = wrap Default
    let of_stdlib st = wrap (Custom (Lazy.from_val st))
    let of_stdlib_lazy ~f = wrap (Custom (Lazy.from_fun f))

    let[@inline] with_stdlib ~f t =
      match unwrap t with
      | Default ->
        (* This is thread safe because the state is only updated via a non-preemptable C
           call, [f] does not yield, and [f] does not borrow the state. *)
        let default_state = Obj.magic_uncontended (DLS.get default_state) in
        f default_state [@nontail]
      | Custom st -> f (Lazy.force st)
    ;;
  end

  include T

  let make_self_init ?allow_in_tests () =
    forbid_nondeterminism_in_tests ~allow_in_tests;
    of_stdlib_lazy ~f:Stdlib.Random.State.make_self_init
  ;;

  let bits t = with_stdlib ~f:Stdlib.Random.State.bits t
  let[@inline] bits64 t = with_stdlib ~f:Stdlib.Random.State.bits64 t
  let bool t = with_stdlib ~f:Stdlib.Random.State.bool t
  let int t x = with_stdlib ~f:(fun state -> Stdlib.Random.State.int state x) t

  let[@inline] int32 t x =
    with_stdlib ~f:(fun state -> Stdlib.Random.State.int32 state x) t
  ;;

  let[@inline] int64 t x =
    with_stdlib ~f:(fun state -> Stdlib.Random.State.int64 state x) t
  ;;

  let[@inline] nativeint t x =
    with_stdlib ~f:(fun state -> Stdlib.Random.State.nativeint state x) t
  ;;

  let make seed = of_stdlib (Stdlib.Random.State.make seed)

  let copy t =
    let (P access) = Capsule.current () in
    with_stdlib ~f:(fun state -> copy_into_capsule state) t
    |> Capsule.Data.unwrap ~access
    |> of_stdlib
  ;;

  let char t = int t 256 |> Char.unsafe_of_int
  let ascii t = int t 128 |> Char.unsafe_of_int

  let full_init t seed =
    let seed = Stdlib.Random.State.make seed in
    let (P access) = Capsule.current () in
    let seed = Capsule.Data.wrap ~access seed in
    Capsule.Password.with_current access (stack_ fun password ->
      let password = Capsule.Password.shared password in
      with_stdlib t ~f:(stack_ fun state -> assign_capsule ~password ~src:seed ~dst:state)
      [@nontail])
    [@nontail]
  ;;

  let[@inline] int_on_64bits t bound =
    if bound <= 0x3FFFFFFF (* (1 lsl 30) - 1 *)
    then int t bound
    else Stdlib.Int64.to_int (int64 t (Stdlib.Int64.of_int bound))
  ;;

  let[@inline] int_on_32bits t bound =
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
    fun [@inline] state ->
      logxor
        (bits state)
        (logxor (shift_left (bits state) 30) (shift_left (bits state) 60))
  ;;

  let full_range_int32 =
    let open Stdlib.Int32 in
    let bits state = of_int (bits state) in
    fun [@inline] state -> logxor (bits state) (shift_left (bits state) 30)
  ;;

  let[@inline] full_range_int_on_64bits state =
    Stdlib.Int64.to_int (full_range_int64 state)
  ;;

  let[@inline] full_range_int_on_32bits state =
    Stdlib.Int32.to_int (full_range_int32 state)
  ;;

  let full_range_int =
    match Word_size.word_size with
    | W64 -> full_range_int_on_64bits
    | W32 -> full_range_int_on_32bits
  ;;

  let[@inline] full_range_nativeint_on_64bits state =
    Stdlib.Int64.to_nativeint (full_range_int64 state)
  ;;

  let[@inline] full_range_nativeint_on_32bits state =
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
  [@@cold]
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

  (*=Return a uniformly random float in [0, 1). *)
  let[@inline] rawfloat state =
    let result = ref 0.0 in
    while
      let open Float_replace_polymorphic_compare in
      let scale = 0x1p-30 in
      (* 2^-30 *)
      let r1 = Stdlib.float_of_int (bits state) in
      let r2 = Stdlib.float_of_int (bits state) in
      result := ((r1 *. scale) +. r2) *. scale;
      (* With very small probability, result can round up to 1.0, so in that case, we just
         try again. *)
      !result >= 1.0
    do
      ()
    done;
    !result
  ;;

  let[@inline] float state hi = rawfloat state *. hi

  let[@inline] float_range state lo hi =
    let open Float_replace_polymorphic_compare in
    if lo > hi then raise_crossed_bounds "float" lo hi Stdlib.string_of_float;
    lo +. float state (hi -. lo)
  ;;
end

let bits () = State.bits (State.get_default ())
let[@inline] bits64 () = State.bits64 (State.get_default ())
let int x = State.int (State.get_default ()) x
let[@inline] int32 x = State.int32 (State.get_default ()) x
let[@inline] nativeint x = State.nativeint (State.get_default ()) x
let[@inline] int64 x = State.int64 (State.get_default ()) x
let[@inline] float x = State.float (State.get_default ()) x
let int_incl x y = State.int_incl (State.get_default ()) x y
let[@inline] int32_incl x y = State.int32_incl (State.get_default ()) x y
let[@inline] nativeint_incl x y = State.nativeint_incl (State.get_default ()) x y
let[@inline] int64_incl x y = State.int64_incl (State.get_default ()) x y
let[@inline] float_range x y = State.float_range (State.get_default ()) x y
let bool () = State.bool (State.get_default ())
let char () = State.char (State.get_default ())
let ascii () = State.ascii (State.get_default ())
let full_init seed = State.full_init (State.get_default ()) seed
let init seed = full_init [| seed |]
let self_init ?allow_in_tests () = full_init (random_seed ?allow_in_tests ())

let set_state s =
  State.with_stdlib s ~f:(stack_ fun state ->
    let (P access) = Capsule.current () in
    let capsule = Capsule.Data.wrap ~access state in
    Capsule.Password.with_current access (stack_ fun password ->
      let password = Capsule.Password.shared password in
      State.with_stdlib (State.get_default ()) ~f:(stack_ fun default ->
        assign_capsule ~password ~src:capsule ~dst:default)
      [@nontail])
    [@nontail])
  [@nontail]
;;
