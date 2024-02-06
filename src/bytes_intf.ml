open! Import

(** Interface for Unicode encodings, such as UTF-8. *)
module type Utf = sig
  type t := bytes

  (** Writes a Unicode character to a given position using this encoding. *)
  val set : t -> int -> Uchar0.t -> int
end

module type Bytes = sig
  (** OCaml's byte sequence type, semantically similar to a [char array], but
      taking less space in memory.

      A byte sequence is a mutable data structure that contains a fixed-length
      sequence of bytes (of type [char]). Each byte can be indexed in constant
      time for reading or writing. *)

  open! Import

  type t = bytes [@@deriving_inline globalize, sexp, sexp_grammar]

  val globalize : t -> t

  include Sexplib0.Sexpable.S with type t := t

  val t_sexp_grammar : t Sexplib0.Sexp_grammar.t

  [@@@end]

  (** {1 Common Interfaces} *)

  include Blit.S with type t := t
  include Comparable.S with type t := t
  include Ppx_compare_lib.Comparable.S_local with type t := t
  include Ppx_compare_lib.Equal.S_local with type t := t
  include Stringable.S with type t := t

  (** Note that [pp] allocates in order to preserve the state of the byte
      sequence it was initially called with. *)
  include Pretty_printer.S with type t := t

  include Invariant.S with type t := t

  module To_string : sig
    val sub : (t, string) Blit.sub
    val subo : (t, string) Blit.subo
  end

  module From_string : Blit.S_distinct with type src := string and type dst := t

  (** [create len] returns a newly-allocated and uninitialized byte sequence of
      length [len].  No guarantees are made about the contents of the return
      value. *)
  val create : int -> t

  (** [create_local] is like [create], but returns a stack-allocated [Bytes.t]. *)
  val create_local : int -> t

  (** [make len c] returns a newly-allocated byte sequence of length [len] filled
      with the byte [c]. *)
  val make : int -> char -> t

  (** [map f t] applies function [f] to every byte, in order, and builds the byte
      sequence with the results returned by [f]. *)
  val map : t -> f:(char -> char) -> t

  (** Like [map], but passes each character's index to [f] along with the char. *)
  val mapi : t -> f:(int -> char -> char) -> t

  (** [copy t] returns a newly-allocated byte sequence that contains the same
      bytes as [t]. *)
  val copy : t -> t

  (** [init len ~f] returns a newly-allocated byte sequence of length [len] with
      index [i] in the sequence being initialized with the result of [f i]. *)
  val init : int -> f:(int -> char) -> t

  (** [of_char_list l] returns a newly-allocated byte sequence where each byte in
      the sequence corresponds to the byte in [l] at the same index. *)
  val of_char_list : char list -> t

  (** [length t] returns the number of bytes in [t]. *)
  external length : (t[@local_opt]) -> int = "%bytes_length"

  (** [get t i] returns the [i]th byte of [t]. *)
  val get : t -> int -> char

  external unsafe_get : (t[@local_opt]) -> (int[@local_opt]) -> char = "%bytes_unsafe_get"

  (** [set t i c] sets the [i]th byte of [t] to [c]. *)
  external set
    :  (t[@local_opt])
    -> (int[@local_opt])
    -> (char[@local_opt])
    -> unit
    = "%bytes_safe_set"

  external unsafe_set
    :  (t[@local_opt])
    -> (int[@local_opt])
    -> (char[@local_opt])
    -> unit
    = "%bytes_unsafe_set"

  external unsafe_get_int64
    :  (t[@local_opt])
    -> (int[@local_opt])
    -> int64
    = "%caml_bytes_get64u"

  external unsafe_set_int64
    :  (t[@local_opt])
    -> (int[@local_opt])
    -> (int64[@local_opt])
    -> unit
    = "%caml_bytes_set64u"

  external unsafe_get_int32
    :  (t[@local_opt])
    -> (int[@local_opt])
    -> int32
    = "%caml_bytes_get32u"

  external unsafe_set_int32
    :  (t[@local_opt])
    -> (int[@local_opt])
    -> (int32[@local_opt])
    -> unit
    = "%caml_bytes_set32u"

  external unsafe_get_int16
    :  (t[@local_opt])
    -> (int[@local_opt])
    -> int
    = "%caml_bytes_get16u"

  external unsafe_set_int16
    :  (t[@local_opt])
    -> (int[@local_opt])
    -> (int[@local_opt])
    -> unit
    = "%caml_bytes_set16u"

  (** [fill t ~pos ~len c] modifies [t] in place, replacing all the bytes from
      [pos] to [pos + len] with [c]. *)
  val fill : t -> pos:int -> len:int -> char -> unit

  (** [tr ~target ~replacement t] modifies [t] in place, replacing every instance
      of [target] in [s] with [replacement]. *)
  val tr : target:char -> replacement:char -> t -> unit

  (** [tr_multi ~target ~replacement] returns an in-place function that replaces
      every instance of a character in [target] with the corresponding character
      in [replacement].

      If [replacement] is shorter than [target], it is lengthened by repeating
      its last character. Empty [replacement] is illegal unless [target] also is.

      If [target] contains multiple copies of the same character, the last
      corresponding [replacement] character is used. Note that character ranges
      are {b not} supported, so [~target:"a-z"] means the literal characters ['a'],
      ['-'], and ['z']. *)
  val tr_multi : target:string -> replacement:string -> (t -> unit) Staged.t

  (** [to_list t] returns the bytes in [t] as a list of chars. *)
  val to_list : t -> char list

  (** [to_array t] returns the bytes in [t] as an array of chars. *)
  val to_array : t -> char array

  (** [fold a ~f ~init:b] is [f a1 (f a2 (...))] *)
  val fold : t -> init:'acc -> f:('acc -> char -> 'acc) -> 'acc

  (** [foldi] works similarly to [fold], but also passes the index of each character to
      [f]. *)
  val foldi : t -> init:'acc -> f:(int -> 'acc -> char -> 'acc) -> 'acc

  (** [contains ?pos ?len t c] returns [true] iff [c] appears in [t] between [pos]
      and [pos + len]. *)
  val contains : ?pos:int -> ?len:int -> t -> char -> bool

  (** Maximum length of a byte sequence, which is architecture-dependent.  Attempting to
      create a [Bytes] larger than this will raise an exception. *)
  val max_length : int

  (** {2:unsafe Unsafe conversions (for advanced users)}

      This section describes unsafe, low-level conversion functions between
      [bytes] and [string]. They might not copy the internal data; used
      improperly, they can break the immutability invariant on strings provided
      by the [-safe-string] option. They are available for expert library
      authors, but for most purposes you should use the always-correct
      {!Bytes.to_string} and {!Bytes.of_string} instead.
  *)

  (** Unsafely convert a byte sequence into a string.

      To reason about the use of [unsafe_to_string], it is convenient to
      consider an "ownership" discipline. A piece of code that
      manipulates some data "owns" it; there are several disjoint ownership
      modes, including:
      {ul
      {- Unique ownership: the data may be accessed and mutated}
      {- Shared ownership: the data has several owners, that may only
      access it, not mutate it.}}
      Unique ownership is linear: passing the data to another piece of
      code means giving up ownership (we cannot access the
      data again). A unique owner may decide to make the data shared
      (giving up mutation rights on it), but shared data may not become
      uniquely-owned again.
      [unsafe_to_string s] can only be used when the caller owns the byte
      sequence [s] -- either uniquely or as shared immutable data. The
      caller gives up ownership of [s], and gains (the same mode of) ownership
      of the returned string.
      There are two valid use-cases that respect this ownership
      discipline:
      {ol
      {- The first is creating a string by initializing and mutating a byte
      sequence that is never changed after initialization is performed.
      {[
        let string_init len f : string =
          let s = Bytes.create len in
          for i = 0 to len - 1 do Bytes.set s i (f i) done;
          Bytes.unsafe_to_string ~no_mutation_while_string_reachable:s
      ]}
      This function is safe because the byte sequence [s] will never be
      accessed or mutated after [unsafe_to_string] is called. The
      [string_init] code gives up ownership of [s], and returns the
      ownership of the resulting string to its caller.

      Note that it would be unsafe if [s] was passed as an additional
      parameter to the function [f] as it could escape this way and be
      mutated in the future -- [string_init] would give up ownership of
      [s] to pass it to [f], and could not call [unsafe_to_string]
      safely.

      We have provided the {!String.init}, {!String.map} and
      {!String.mapi} functions to cover most cases of building
      new strings. You should prefer those over [to_string] or
      [unsafe_to_string] whenever applicable.}
      {- The second is temporarily giving ownership of a byte sequence to
      a function that expects a uniquely owned string and returns ownership
      back, so that we can mutate the sequence again after the call ended.
      {[
        let bytes_length (s : bytes) =
          String.length
            (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:s)
      ]}
      In this use-case, we do not promise that [s] will never be mutated
      after the call to [bytes_length s]. The {!String.length} function
      temporarily borrows unique ownership of the byte sequence
      (and sees it as a [string]), but returns this ownership back to
      the caller, which may assume that [s] is still a valid byte
      sequence after the call. Note that this is only correct because we
      know that {!String.length} does not capture its argument -- it could
      escape by a side-channel such as a memoization combinator.
      The caller may not mutate [s] while the string is borrowed (it has
      temporarily given up ownership). This affects concurrent programs,
      but also higher-order functions: if {!String.length} returned
      a closure to be called later, [s] should not be mutated until this
      closure is fully applied and returns ownership.}}
  *)
  external unsafe_to_string
    :  no_mutation_while_string_reachable:(t[@local_opt])
    -> (string[@local_opt])
    = "%bytes_to_string"

  (** Unsafely convert a shared string to a byte sequence that should
      not be mutated.

      The same ownership discipline that makes [unsafe_to_string]
      correct applies to [unsafe_of_string_promise_no_mutation],
      however unique ownership of string values is extremely difficult
      to reason about correctly in practice. As such, one should always
      assume strings are shared, never uniquely owned (For example,
      string literals are implicitly shared by the compiler, so you
      never uniquely own them)

      The only case we have reasonable confidence is safe is if the
      produced [bytes] is shared -- used as an immutable byte
      sequence. This is possibly useful for incremental migration of
      low-level programs that manipulate immutable sequences of bytes
      (for example {!Marshal.from_bytes}) and previously used the
      [string] type for this purpose.
  *)
  external unsafe_of_string_promise_no_mutation
    :  (string[@local_opt])
    -> (t[@local_opt])
    = "%bytes_of_string"

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
