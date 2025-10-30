open! Import
module Sexp = Sexp0
module Info = Info0

module type Error = sig
  (** A lazy string, implemented with [Info], but intended specifically for error
      messages. *)

  type t = private Info.t

  include Info.S0 with type t := t (** @open *)

  [%%template:
  [@@@kind.default k = (base_or_null_with_imm, bits32 & bits32)]

  (** Note that the exception raised by this function maintains a reference to the [t]
      passed in. *)
  val raise : 'a. t -> 'a

  val raise_s : 'a. Sexp.t -> 'a]

  (** Re-raise exceptions raised from [f] with this [t] as context. If this function
      raises, the exception maintains a reference to the [t] passed in. *)
  val reraise_uncaught : t -> f:(unit -> 'a) -> 'a

  val to_info : t -> Info.t
  val of_info : Info.t -> t
end

module type Error_with_extras = sig
  include Error (** @inline *)

  include Info.S with type t := t (** @inline *)
end
