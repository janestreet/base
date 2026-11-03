@@ portable

(** This module implements derived integer operations (e.g., modulo, rounding to
    multiples) based on other basic operations. *)

open! Import0
open Int_intf.Definitions

module type Make_arg = sig @@ portable
  type t : value mod contended portable

  val globalize : t @ local -> t

  include Floatable.S_local_input with type t := t
  include Stringable.S with type t := t

  val ( + ) : t @ local -> t @ local -> t
  val ( - ) : t @ local -> t @ local -> t
  val ( * ) : t @ local -> t @ local -> t
  val ( / ) : t @ local -> t @ local -> t
  val ( ~- ) : t @ local -> t
  val ( <> ) : t @ local -> t @ local -> bool
  val ( <= ) : t @ local -> t @ local -> bool
  val ( >= ) : t @ local -> t @ local -> bool
  val ( = ) : t @ local -> t @ local -> bool
  val ( < ) : t @ local -> t @ local -> bool
  val ( > ) : t @ local -> t @ local -> bool
  val abs : t -> t
  val neg : t -> t
  val zero : t
  val of_int_exn : int -> t
  val rem : t @ local -> t @ local -> t
end

(** Derived operations common to various integer modules.

    See {{!Base.Int.S_common} [Int.S_common]} for a description of the operations derived
    by this module. *)
module Make (X : Make_arg) : sig @@ portable
  val ( % ) : X.t @ local -> X.t @ local -> X.t
  val ( /% ) : X.t @ local -> X.t @ local -> X.t
  val ( // ) : X.t @ local -> X.t @ local -> float

  include Round with type t := X.t
end

(*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

    https://opensource.janestreet.com/standards/#private-submodules *)
module Private : sig
  val int_pow : int @ local -> int @ local -> int
  val int64_pow : int64 @ local -> int64 @ local -> int64
  val int63_pow_on_int64 : int64 @ local -> int64 @ local -> int64

  module Pow_overflow_bounds = Pow_overflow_bounds
end
