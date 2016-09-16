(** This is a slightly modified version of the OCaml standard library's random.mli.  We
    want Core's [Random] module to be different from OCaml's standard one:

    - We expose [Random.State.default], so that user code can easily share the default
      random state if it wants.

    - We disallow [Random.get_state], because it misleadingly makes a copy of random
      state.  And it is what people naturally, albeit incorrectly, grab for when they want
      to use shared random state.

    The fact that we construct our own default random state means that code using
    Core.Std.Random and code using OCaml's Random will not share the default state. *)

(*_
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*              Damien Doligez, projet Para, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Apache 2.0 license. See ../THIRD-PARTY.txt  *)
(*  for details.                                                       *)
(*                                                                     *)
(***********************************************************************)
*)

open! Import

(** Pseudo-random number generators (PRNG). *)

(** {6 Basic functions} *)

(** Note that all of these "basic" functions mutate a global random state. *)

(** Initialize the generator, using the argument as a seed.  The same seed will always
    yield the same sequence of numbers. *)
val init : int -> unit

(** Same as {!Random.init} but takes more data as seed. *)
val full_init : int array -> unit

(** Initialize the generator with a more-or-less random seed chosen in a system-dependent
    way.  By default, [self_init] is disallowed in inline tests, as it's often used for no
    good reason and it just creates non deterministic failures for everyone.  Passing
    [~allow_in_tests:true] removes this restriction in case you legitimately want
    non-deterministic values, like in [Filename.temp_dir]. *)
val self_init : ?allow_in_tests:bool -> unit -> unit

(** Return 30 random bits in a nonnegative integer.  @before 3.12.0 used a different
    algorithm (affects all the following functions)
*)
val bits : unit -> int

(** [Random.int bound] returns a random integer between 0 (inclusive) and [bound]
    (exclusive).  [bound] must be greater than 0. *)
val int : int -> int

(** [Random.int32 bound] returns a random integer between 0 (inclusive) and [bound]
    (exclusive).  [bound] must be greater than 0. *)
val int32 : Int32.t -> Int32.t

(** [Random.nativeint bound] returns a random integer between 0 (inclusive) and [bound]
    (exclusive).  [bound] must be greater than 0. *)
val nativeint : Nativeint.t -> Nativeint.t

(** [Random.int64 bound] returns a random integer between 0 (inclusive) and [bound]
    (exclusive).  [bound] must be greater than 0. *)
val int64 : Int64.t -> Int64.t

(** [Random.float bound] returns a random floating-point number between 0 (inclusive) and
    [bound] (exclusive).  If [bound] is negative, the result is negative or zero.  If
    [bound] is 0, the result is 0. *)
val float : float -> float

(** [Random.bool ()] returns [true] or [false] with probability 0.5 each. *)
val bool : unit -> bool

(** {6 Advanced functions} *)

(** The functions from module [State] manipulate the current state of the random generator
    explicitly.  This allows using one or several deterministic PRNGs, even in a
    multi-threaded program, without interference from other parts of the program.

    Obtaining multiple generators with good independence properties is nontrivial; see
    [Splittable_random]. *)
module State : sig
  type t

  val default : t

  (** Create a new state and initialize it with the given seed. *)
  val make : int array -> t

  (** Create a new state and initialize it with a system-dependent low-entropy seed. *)
  val make_self_init : ?allow_in_tests:bool -> unit -> t

  val copy : t -> t

  (** These functions are the same as the basic functions, except that they use (and
      update) the given PRNG state instead of the default one.  *)
  val bits : t -> int
  val int : t -> int -> int
  val int32 : t -> Int32.t -> Int32.t
  val nativeint : t -> Nativeint.t -> Nativeint.t
  val int64 : t -> Int64.t -> Int64.t
  val float : t -> float -> float
  val bool : t -> bool
end

(** OCaml's [Random.get_state] makes a copy of the default state, which is almost
    certainly not what you want.  [State.default], which is the actual default state, is
    probably what you want. *)
val get_state : unit -> [ `Consider_using_Random_State_default ]

(** Set the state of the generator used by the basic functions. *)
val set_state : State.t -> unit
