(** Type of S-expressions *)

open! Import0

type t = Atom of string | List of t list [@@deriving_inline compare, hash]
include
sig
  [@@@ocaml.warning "-32"]
  val hash_fold_t :
    Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state
  val hash : t -> Ppx_hash_lib.Std.Hash.hash_value
  val compare : t -> t -> int
end
[@@@end]

(*_ We don't use [@@deriving_inline sexp][@@@end] as this would generated references to [Sexplib],
  creating a circular dependency *)
val t_of_sexp : t -> t
val sexp_of_t : t -> t

val equal : t -> t -> bool

exception Of_sexp_error of exn * t
(** [Of_sexp_error (exn, sexp)] the exception raised when an S-expression could not be
    successfully converted to an OCaml-value. *)

(** {1 Helpers} *)

val message : string -> (string * t) list -> t
(** Helper to build nice s-expressions for error messages.  It immitates the behavior of
    [[%message ...]] from the ppx_sexp_message rewriter.

    [message name key_values] produces a s-expression list starting with atom [name] and
    followed by list of size 2 of the form [(key value)].  When the key is the empty
    string, [value] is used directly instead as for [[%message]].

    For instance the following code:

    {[
      Sexp.message "error"
        [ "x", sexp_of_int 42
        ; "" , sexp_of_exn Exit
        ]
    ]}

    produces the s-expression:

    {[
      (error (x 42) Exit)
    ]} *)

(** {1 Defaults} *)

val default_indent : int ref
(** [default_indent] reference to default indentation level for human-readable
    conversions.

    Initialisation value: 2. *)

(** {1 Pretty printing of S-expressions} *)

val pp_hum : Caml.Format.formatter -> t -> unit
(** [pp_hum ppf sexp] outputs S-expression [sexp] to formatter [ppf] in human readable
    form. *)

val pp_hum_indent : int -> Caml.Format.formatter -> t -> unit
(** [pp_hum_indent n ppf sexp] outputs S-expression [sexp] to formatter [ppf] in human
    readable form and indentation level [n]. *)

val pp_mach : Caml.Format.formatter -> t -> unit
(** [pp_mach ppf sexp] outputs S-expression [sexp] to formatter [ppf] in machine readable
    (i.e. most compact) form. *)

val pp : Caml.Format.formatter -> t -> unit
(** Same as [pp_mach]. *)

(** {1 Conversion to strings} *)

val to_string_hum : ?indent : int -> t -> string
(** [to_string_hum ?indent sexp] converts S-expression [sexp] to a
    string in human readable form with indentation level [indent].

    @param indent default = [!default_indent] *)

val to_string_mach : t -> string
(** [to_string_mach sexp] converts S-expression [sexp] to a string in
    machine readable (i.e. most compact) form. *)

val to_string : t -> string
(** Same as [to_string_mach]. *)

(** {1 Styles} *)

val of_float_style : [ `Underscores | `No_underscores ] ref
val of_int_style   : [ `Underscores | `No_underscores ] ref


module Private : sig
  (*_ Exported for sexplib *)

  val size : t -> int * int

  val buffer : unit -> Caml.Buffer.t

  val to_buffer      : buf:Caml.Buffer.t ->                t -> unit
  val to_buffer_hum  : buf:Caml.Buffer.t -> ?indent:int -> t -> unit
  val to_buffer_mach : buf:Caml.Buffer.t ->                t -> unit
  val to_buffer_gen
    :  buf : 'buffer
    -> add_char : ('buffer -> char -> unit)
    -> add_string : ('buffer -> string -> unit)
    -> t
    -> unit

  val mach_maybe_esc_str : string -> string
  val must_escape : string -> bool
  val esc_str : string -> string
end
