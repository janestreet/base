@@ portable

open! Import0
open Int_intf.Definitions
module Sexp := Sexp0

(** human-friendly string (and possibly sexp) conversions *)
module Make (I : sig
  @@ portable
    type t

    val%template to_string : t @ local -> string @ m
    [@@alloc a @ m = (heap_global, stack_local)]
  end) : sig
  @@ portable
  [%%template:
  [@@@alloc.default a @ m = (heap_global, stack_local)]

  val to_string_hum
    :  ?delimiter:char (** defaults to ['_'] *)
    -> I.t @ local
    -> string @ m

  val sexp_of_t : I.t @ local -> Sexp.t @ m]

  (** Provides conversions for the unboxed version of the type. Includes [value] for OCaml
      compatibility after template erasure. *)
  module%template
    [@kind k = (bits32, bits64, word, value)]
    Provide_unboxed (U : sig
    @@ portable
      type t : k

      val box : t -> I.t @ local
    end) : sig
    @@ portable
    val sexp_of_t_u : U.t -> Sexp.t @ m [@@alloc __ @ m = (heap_global, stack_local)]
  end
end

(** in the output, [to_string], [of_string], [sexp_of_t], and [t_of_sexp] convert between
    [t] and signed hexadecimal with an optional "0x" or "0X" prefix. *)
module Make_hex (I : sig
  @@ portable
    type t : value mod contended portable [@@deriving compare ~localize, hash]

    (** [to_string] and [of_string] convert between [t] and unsigned, unprefixed
        hexadecimal. They must be able to handle all non-negative values and also
        [min_value]. [to_string min_value] must write a positive hex representation. *)
    val to_string : t @ local -> string

    val of_string : string -> t
    val zero : t
    val ( < ) : t @ local -> t @ local -> bool
    val neg : t @ local -> t
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

    val clz : t @ local -> t
    val ( lsr ) : t @ local -> int -> t
    val ( land ) : t @ local -> t @ local -> t
    val to_int_trunc : t @ local -> int
    val num_bits : t
    val one : t
    val zero : t
    val ( - ) : t @ local -> t @ local -> t
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

[%%template:
[@@@alloc.default a @ m = (heap_global, stack_local)]

val insert_delimiter_every
  :  string @ m
  -> delimiter:char
  -> chars_per_delimiter:int
  -> string @ m

(** [insert_delimiter_every ~chars_per_delimiter:3] *)
val insert_delimiter : string @ m -> delimiter:char -> string @ m

(** [insert_delimiter ~delimiter:'_'] *)
val insert_underscores : string @ m -> string @ m]
