(** OCaml's byte sequence type, semantically similar to a [char array], but
    taking less space in memory.

    A byte sequence is a mutable data structure that contains a fixed-length
    sequence of bytes (of type [char]). Each byte can be indexed in constant
    time for reading or writing. *)

open! Import

type t = bytes [@@deriving_inline sexp]
include
sig
  [@@@ocaml.warning "-32"]
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
end
[@@@end]


(** {1 Common Interfaces} *)

include Blit          .S with type t := t
include Comparable    .S with type t := t
include Stringable    .S with type t := t

(** Note that [pp] allocates in order to preserve the state of the byte
    sequence it was initially called. *)
include Pretty_printer.S with type t := t

module To_string : sig
  val sub : (t, string) Blit.sub
  val subo : (t, string) Blit.subo
end

module From_string : sig
  val blit : (string, t) Blit.blit
  val blito : (string, t) Blit.blito
end

(** {1 Constructors} *)

(** [create len] returns a newly-allocated and uninitialized byte sequence of
    length [len].  No guarantees are made about the contents of the return
    value. *)
val create : int -> t

(** [make len c] returns a newly-allocated byte sequence of length [len] filled
    with the byte [c]. *)
val make : int -> char -> t

(** [copy t] returns a newly-allocated byte sequence that contains the same
    bytes as [t] *)
val copy : t -> t

(** [init len ~f] returns a newly-allocated byte sequence of length [len] with
    index [i] in the sequence being initialized with the result of [f i] *)
val init : int -> f:(int -> char) -> t

(** [of_char_list l] returns a newly-alloated byte sequence where each byte in
    the sequence corresponds to the byte in [l] at the same index. *)
val of_char_list : char list -> t


(** {1 Primitives} *)

(** [length t] returns the number of bytes in [t]. *)
val length : t -> int

(** [get t i] returns the [i]th byte of [t]. *)
val             get : t -> int -> char
external unsafe_get : t -> int -> char         = "%bytes_unsafe_get"

(** [set t i c] sets the [i]th byte of [t] to [c]. *)
val             set : t -> int -> char -> unit
external unsafe_set : t -> int -> char -> unit = "%bytes_unsafe_set"


(** {1 Iteration} *)

(** [fill t ~pos ~len c] modifies [t] in-place, replacing all the bytes from
    [pos] to [pos + len] with [c]. *)
val fill : t -> pos:int -> len:int -> char -> unit

(** [tr ~target ~replacement t] modifies [t] in-place, replacing every instance
    of [target] in [s] with [replacement]. *)

val tr : target:char -> replacement:char -> t -> unit

(** [to_list t] returns the bytes in [t] as a list of chars. *)
val to_list : t -> char list

(** Maximum length of a byte sequence, which is architecture-dependent.
    Attempting to create a [Bytes] larger than this will raise an exception. *)
val max_length : int


(**/**)

(*_ The values and types below are intentionally undocumented. Do not use any
  of them without reading and understanding their implementation. Some are
  compiler intrinsics, so make sure you go the distance. *)

val unsafe_to_string : t -> string
val unsafe_of_string : string -> t
