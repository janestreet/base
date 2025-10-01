open! Import

module Definitions = struct
  module type S = sig
    (** The abstract type of buffers. *)
    type t [@@deriving sexp_of]

    (** [create n] returns a fresh buffer, initially empty. The [n] parameter is the
        initial size of the internal storage medium that holds the buffer contents. That
        storage is automatically reallocated when more than [n] characters are stored in
        the buffer, but shrinks back to [n] characters when [reset] is called.

        For best performance, [n] should be of the same order of magnitude as the number
        of characters that are expected to be stored in the buffer (for instance, 80 for a
        buffer that holds one output line). Nothing bad will happen if the buffer grows
        beyond that limit, however. In doubt, take [n = 16] for instance. *)
    val create : int -> t

    (** Return a copy of the current contents of the buffer. The buffer itself is
        unchanged. *)
    val contents : t -> string

    val contents_bytes : t -> bytes

    (** [blit ~src ~src_pos ~dst ~dst_pos ~len] copies [len] characters from the current
        contents of the buffer [src], starting at offset [src_pos] to bytes [dst],
        starting at character [dst_pos].

        Raises [Invalid_argument] if [src_pos] and [len] do not designate a valid
        substring of [src], or if [dst_pos] and [len] do not designate a valid substring
        of [dst]. *)

    include Blit.S_distinct with type src := t with type dst := bytes
    module To_string : Blit.S_to_string with type t := t

    (** Gets the (zero-based) n-th character of the buffer. Raises [Invalid_argument] if
        index out of bounds. *)
    val nth : t -> int -> char

    (** Returns the number of characters currently contained in the buffer. *)
    val length : t -> int

    (** Empties the buffer. *)
    val clear : t -> unit

    (** Empties the buffer and deallocates the internal storage holding the buffer
        contents, replacing it with the initial internal storage of length [n] that was
        allocated by [create n]. For long-lived buffers that may have grown a lot, [reset]
        allows faster reclamation of the space used by the buffer. *)
    val reset : t -> unit

    (** [truncate b len] truncates the length of [b] to [len].

        Note: the internal byte sequence is not shortened. Raises [Invalid_argument] if
        [len < 0] or [len > length b]. *)
    val truncate : t -> int -> unit

    (** [add_char b c] appends the character [c] at the end of the buffer [b]. *)
    val add_char : t -> char -> unit

    (** [add_string b s] appends the string [s] at the end of the buffer [b]. *)
    val add_string : t -> string -> unit

    (** [add_substring b s pos len] takes [len] characters from offset [pos] in string [s]
        and appends them at the end of the buffer [b]. *)
    val add_substring : t -> string -> pos:int -> len:int -> unit

    (** [add_bytes b s] appends the bytes [s] at the end of the buffer [b]. *)
    val add_bytes : t -> bytes -> unit

    (** [add_subbytes b s pos len] takes [len] characters from offset [pos] in bytes [s]
        and appends them at the end of the buffer [b]. *)
    val add_subbytes : t -> bytes -> pos:int -> len:int -> unit

    (** [add_buffer b1 b2] appends the current contents of buffer [b2] at the end of
        buffer [b1]. [b2] is not modified. *)
    val add_buffer : t -> t -> unit
  end
end

module type Buffer = sig @@ portable
  include module type of struct
    include Definitions
  end

  (** Buffers using strings as underlying storage medium: *)

  include S with type t = Stdlib.Buffer.t (** @open *)

  (** [Buffer.t] does not support [local_], even on compilers supporting modes. *)

  (** @open *)
  include Blit.S_distinct_global with type src := t and type dst := bytes

  (** For compilers supporting modes: [Buffer] does not support [To_string.sub*] for local
      values. Other implementations of [S], such as [Core.Bigbuffer], do. *)
  module To_string : Blit.S_to_string_global with type t := t
end
