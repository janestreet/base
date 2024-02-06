open! Import

(** Interface for encoding and decoding individual Unicode scalar values. See [String.Utf]
    for working with Unicode strings. *)
module type Utf = sig
  (** [to_string] encodes a Unicode scalar value in this encoding.

      [of_string] interprets a string as one Unicode scalar value in this encoding, and
      raises if the string cannot be interpreted as such. *)
  include Stringable.S with type t := Uchar0.t

  (** Returns the number of bytes used for a given scalar value in this encoding. *)
  val byte_length : Uchar0.t -> int

  (** The name of this encoding scheme; e.g., "UTF-8". *)
  val codec_name : string
end

module type Uchar = sig
  (** Unicode operations.

      A [Uchar.t] represents a Unicode scalar value, which is the basic unit of Unicode.

      See also [String.Utf*] submodules for Unicode support with multiple [Uchar.t] values
      encoded in a string. *)

  open! Import

  type t = Uchar0.t [@@deriving_inline hash, sexp, sexp_grammar]

  include Ppx_hash_lib.Hashable.S with type t := t
  include Sexplib0.Sexpable.S with type t := t

  val t_sexp_grammar : t Sexplib0.Sexp_grammar.t

  [@@@end]

  type uchar := t

  include Comparable.S with type t := t
  include Ppx_compare_lib.Comparable.S_local with type t := t
  include Ppx_compare_lib.Equal.S_local with type t := t
  include Pretty_printer.S with type t := t
  include Invariant.S with type t := t

  (** [succ_exn t] is the scalar value after [t] in the set of Unicode scalar values, and
      raises if [t = max_value]. *)
  val succ : t -> t option

  val succ_exn : t -> t

  (** [pred_exn t] is the scalar value before [t] in the set of Unicode scalar values, and
      raises if [t = min_value]. *)
  val pred : t -> t option

  val pred_exn : t -> t

  (** [is_char t] is [true] iff [n] is in the latin-1 character set. *)
  val is_char : t -> bool

  (** [to_char_exn t] is [t] as a [char] if it is in the latin-1 character set, and raises
      otherwise. *)
  val to_char : t -> char option

  val to_char_exn : t -> char

  (** [of_char c] is [c] as a Unicode scalar value. *)
  val of_char : char -> t

  (** [int_is_scalar n] is [true] iff [n] is an Unicode scalar value (i.e., in the ranges
      [0x0000]...[0xD7FF] or [0xE000]...[0x10FFFF]). *)
  val int_is_scalar : int -> bool

  (** [of_scalar_exn n] is [n] as a Unicode scalar value.  Raises if [not (int_is_scalar
      i)]. *)
  val of_scalar : int -> t option

  val of_scalar_exn : int -> t

  (** [to_scalar t] is [t] as an integer scalar value. *)
  val to_scalar : t -> int

  (** Number of bytes needed to represent [t] in UTF-8. *)
  val utf_8_byte_length : t -> int
    [@@deprecated "[since 2023-11] use [Utf8.byte_length]"]

  (** Number of bytes needed to represent [t] in UTF-16. *)
  val utf_16_byte_length : t -> int
    [@@deprecated "[since 2023-11] use [Utf16le.byte_length] or [Utf16be.byte_length]"]

  val min_value : t
  val max_value : t

  (** U+FEFF, the byte order mark. https://en.wikipedia.org/wiki/Byte_order_mark *)
  val byte_order_mark : t

  (** U+FFFD, the Unicode replacement character.
      https://en.wikipedia.org/wiki/Specials_(Unicode_block)#Replacement_character *)
  val replacement_char : t

  (** Result of decoding a UTF codec that may contain invalid encodings. *)
  module Decode_result : sig
    type t = Uchar0.utf_decode
    [@@immediate] [@@deriving_inline compare, equal, hash, sexp_of]

    include Ppx_compare_lib.Comparable.S with type t := t
    include Ppx_compare_lib.Equal.S with type t := t
    include Ppx_hash_lib.Hashable.S with type t := t

    val sexp_of_t : t -> Sexplib0.Sexp.t

    [@@@end]

    (** [true] iff [t] represents a Unicode scalar value. *)
    val is_valid : t -> bool

    (** Number of bytes consumed to decode [t]. *)
    val bytes_consumed : t -> int

    (** Returns the corresponding [uchar] if [is_valid t]. *)
    val uchar : t -> uchar option

    (** Like [uchar]. Raises if [not (is_valid t)]. *)
    val uchar_exn : t -> uchar

    (** Like [uchar]. Returns [replacement_char] if [not (is_valid t)]. *)
    val uchar_or_replacement_char : t -> uchar
  end

  (** UTF-8 encoding. See [Utf] interface. *)
  module Utf8 : Utf

  (** UTF-16 little-endian encoding. See [Utf] interface. *)
  module Utf16le : Utf

  (** UTF-16 big-endian encoding. See [Utf] interface. *)
  module Utf16be : Utf

  (** UTF-32 little-endian encoding. See [Utf] interface. *)
  module Utf32le : Utf

  (** UTF-32 big-endian encoding. See [Utf] interface. *)
  module Utf32be : Utf

  module type Utf = Utf
end
