@@ portable

open! Import
open Int_intf.Definitions
module Sexp := Sexp0

(** human-friendly string (and possibly sexp) conversions *)
module Make (I : sig
  @@ portable
    type t

    val to_string : local_ t -> string
  end) : sig
  @@ portable
  val to_string_hum : ?delimiter:char (** defaults to ['_'] *) -> local_ I.t -> string
  val sexp_of_t : I.t -> Sexp.t
end

(** in the output, [to_string], [of_string], [sexp_of_t], and [t_of_sexp] convert between
    [t] and signed hexadecimal with an optional "0x" or "0X" prefix. *)
module Make_hex (I : sig
  @@ portable
    type t : value mod contended portable [@@deriving compare ~localize, hash]

    (** [to_string] and [of_string] convert between [t] and unsigned, unprefixed
        hexadecimal. They must be able to handle all non-negative values and also
        [min_value]. [to_string min_value] must write a positive hex representation. *)
    val to_string : local_ t -> string

    val of_string : string -> t
    val zero : t
    val ( < ) : local_ t -> local_ t -> bool
    val neg : local_ t -> t
    val module_name : string
  end) : sig
  @@ portable
  include Hexable with type t := I.t
end

(** in the output, [to_string], [to_string_hum], and [sexp_of_t] convert [t] to an
    unsigned binary representation with an "0b" prefix. *)
module Make_binary (I : sig
  @@ portable
    type t : value mod contended portable
    [@@deriving compare ~localize, equal ~localize, hash]

    val clz : local_ t -> t
    val ( lsr ) : local_ t -> int -> t
    val ( land ) : local_ t -> local_ t -> t
    val to_int_trunc : local_ t -> int
    val num_bits : t
    val one : t
    val zero : t
    val ( - ) : local_ t -> local_ t -> t
  end) : sig
  @@ portable
  include Binaryable with type t := I.t
end

(** global ref affecting whether the [sexp_of_t] returned by [Make] is consistent with the
    [to_string] input or the [to_string_hum] output *)
val sexp_of_int_style : [ `No_underscores | `Underscores ] Dynamic.t

(** {v
 utility for defining to_string_hum on numeric types -- takes a string matching
    (-|+)?[0-9a-fA-F]+ and puts [delimiter] every [chars_per_delimiter] characters
    starting from the right.
    v} *)
val insert_delimiter_every : string -> delimiter:char -> chars_per_delimiter:int -> string

(** [insert_delimiter_every ~chars_per_delimiter:3] *)
val insert_delimiter : string -> delimiter:char -> string

(** [insert_delimiter ~delimiter:'_'] *)
val insert_underscores : string -> string
