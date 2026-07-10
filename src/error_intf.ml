open! Import
module Sexp = Sexp0
module Info = Info0

module type Error = sig @@ portable
  (** A lazy string, implemented with [Info], but intended specifically for error
      messages. *)

  type t = private Info.t

  include Info.S0 with type t := t (** @open *)

  [%%template:
  [@@@kind.default k = (base_or_null, bits32 & bits32)]

  (** Note that the exception raised by this function maintains a reference to the [t]
      passed in. *)
  val raise : ('a : k). t -> 'a @ portable

  val raise_s : ('a : k). Sexp.t -> 'a @ portable]

  (** Re-raise exceptions raised from [f] with this [t] as context. If this function
      raises, the exception maintains a reference to the [t] passed in. *)
  val reraise_uncaught : t -> f:(unit -> 'a) @ local once -> 'a

  [%%template:
  [@@@mode.default p = (portable, nonportable)]

  val to_info : t @ p -> Info.t @ p
  val of_info : Info.t @ p -> t @ p]
end

module type Error_with_extras = sig @@ portable
  include Error (** @inline *)

  include Info.S with type t := t (** @inline *)
end
