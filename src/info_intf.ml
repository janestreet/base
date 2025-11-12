(** [Info] is a library for lazily constructing human-readable information as a string or
    sexp, with a primary use being error messages.

    Using [Info] is often preferable to [sprintf] or manually constructing strings because
    you don't have to eagerly construct the string -- you only need to pay when you
    actually want to display the info, which for many applications is rare. Using [Info]
    is also better than creating custom exceptions because you have more control over the
    format.

    Info is intended to be constructed in the following style; for simple info, you write:

    {[
      Info.of_string "Unable to find file"
    ]}

    Or for a more descriptive [Info] without attaching any content (but evaluating the
    result eagerly):

    {[
      Info.createf "Process %s exited with code %d" process exit_code
    ]}

    For info where you want to attach some content, you would write:

    {[
      Info.create "Unable to find file" filename [%sexp_of: string]
    ]}

    Or even,

    {[
      Info.create
        "price too big"
        (price, [ `Max max_price ])
        [%sexp_of: float * [ `Max of float ]]
    ]}

    Note that an [Info.t] can be created from any arbitrary sexp with [Info.t_of_sexp]. *)

open! Import
module Invariant = Invariant_intf.Definitions
module Sexp = Sexp0

module Definitions = struct
  module type Internal_repr = sig
    type info

    (** The internal representation. It is exposed so that we can write efficient
        serializers outside of this module. *)
    type t =
      | Could_not_construct of Sexp.t
      | String of string
      | Exn of exn Modes.Global.t
      | Sexp of Sexp.t
      | Tag_sexp of string * Sexp.t * Source_code_position0.t option
      | Tag_t of string * t
      | Tag_arg of string * Sexp.t * t
      | Of_list of int option * t list
      | With_backtrace of t * string (** The second argument is the backtrace *)
    [@@deriving sexp_of]

    val of_info : local_ info -> t

    val%template to_info : t @ p -> info @ p [@@mode p = (portable, nonportable)]
  end

  (** [Info.t Modes.Portable.t] with some common ppxes derived on it. It also re-exports
      some functions of [Info] so users don't need to frequently wrap and unwrap
      [Modes.Portable.t] just to manipulate the underlying info. *)
  module type Portable = sig
    type info

    type t = info Modes.Portable.t
    [@@deriving compare ~localize, equal ~localize, globalize, hash, sexp, sexp_grammar]

    val create_s : Sexp.t -> t
    val of_string : string -> t
    val of_list : t list -> t
    val of_portable_lazy : string Portable_lazy.t -> t
    val of_portable_lazy_sexp : Sexp.t Portable_lazy.t -> t
    val of_portable_lazy_t : t Portable_lazy.t -> t

    val%template create
      : ('a : k mod contended).
      ?here:Source_code_position0.t
      -> ?strict:unit
      -> string
      -> 'a @ portable
      -> ('a -> Sexp.t) @ portable
      -> t
    [@@kind k = (bits64, float64, value_or_null)]

    val tag_s : t -> tag:Sexp.t -> t
    val tag : t -> tag:string -> t

    val tag_arg
      : ('a : value_or_null mod contended).
      t -> string -> 'a @ portable -> ('a -> Sexp.t) @ portable -> t

    val of_thunk : (unit -> string) @ portable -> t
    val createf : ('a, unit, string, t) format4 -> 'a
  end

  module type S0 = sig @@ portable
    (** Serialization and comparison force the lazy message. *)
    type t : value mod contended global
    [@@deriving compare ~localize, equal ~localize, globalize, hash, sexp, sexp_grammar]

    type info := t

    (** Explicitly indicate that [t_of_sexp] produces a portable [t]. This is nicer for
        the user: you can do more things with a portable [t], e.g. move it between
        domains. *)
    val t_of_sexp : Sexp.t -> t @ portable

    include Invariant.S with type t := t

    (** [to_string_hum] forces the lazy message, which might be an expensive operation.

        [to_string_hum] usually produces a sexp; however, it is guaranteed that
        [to_string_hum (of_string s) = s].

        If this string is going to go into a log file, you may find it useful to ensure
        that the string is only one line long. To do this, use [to_string_mach t]. *)
    val to_string_hum : t -> string

    (** [to_string_mach t] outputs [t] as a sexp on a single line. *)
    val to_string_mach : t -> string

    val of_string : string -> t @ portable

    (** Be careful that the body of the lazy or thunk does not access mutable data, since
        it will only be called at an undetermined later point. *)

    val of_lazy : string Lazy.t -> t
    val of_lazy_sexp : Sexp.t Lazy.t -> t
    val of_lazy_t : t Lazy.t -> t

    val%template of_thunk : (unit -> string) @ p -> t @ p
    [@@mode p = (portable, nonportable)]

    val of_portable_lazy : string Portable_lazy.t -> t @ portable
    val of_portable_lazy_sexp : Sexp.t Portable_lazy.t -> t @ portable
    val of_portable_lazy_t : t Portable_lazy.t -> t @ portable

    (** For [create message a sexp_of_a], [sexp_of_a a] is lazily computed, when the info
        is converted to a sexp. So if [a] is mutated in the time between the call to
        [create] and the sexp conversion, those mutations will be reflected in the sexp.
        Use [~strict:()] to force [sexp_of_a a] to be computed immediately. *)
    val%template create
      : ('a : k).
      ?here:Source_code_position0.t
      -> ?strict:unit
      -> string
      -> 'a @ p
      -> ('a @ c -> Sexp.t) @ p
      -> t @ p
    [@@kind k = (bits64, float64, value_or_null)]
    [@@mode (p, c) = ((nonportable, uncontended), (portable, contended))]

    val create_s : Sexp.t -> t @ portable

    (** Constructs a [t] containing only a string from a format. This eagerly constructs
        the string. *)
    val createf : ('a, unit, string, t) format4 -> 'a

    (** [createf_portable format arg1 arg2 ... ()] is like [createf format arg1 arg2 ...],
        except creating a portable error. *)
    val createf_portable : ('a, unit, string, unit -> t @ portable) format4 -> 'a

    (** Adds a string to the front. *)
    val%template tag : t @ p -> tag:string -> t @ p
    [@@mode p = (portable, nonportable)]

    (** Adds a sexp to the front. *)
    val%template tag_s : t @ p -> tag:Sexp.t -> t @ p
    [@@mode p = (portable, nonportable)]

    (** Adds a lazy sexp to the front. *)
    val tag_s_lazy : t -> tag:Sexp.t Lazy.t -> t

    (** Adds a portable lazy sexp to the front. *)
    val%template tag_s_portable_lazy : t @ p -> tag:Sexp.t Portable_lazy.t -> t @ p
    [@@mode p = (portable, nonportable)]

    (** Adds a string and some other data in the form of an s-expression at the front. *)
    val%template tag_arg
      : ('a : value_or_null).
      t @ p -> string -> 'a @ p -> ('a @ c -> Sexp.t) @ p -> t @ p
    [@@mode (p, c) = ((nonportable, uncontended), (portable, contended))]

    (** Combines multiple infos into one. *)
    val%template of_list : t list @ p -> t @ p
    [@@mode p = (portable, nonportable)]

    (** [of_exn] and [to_exn] are primarily used with [Error], but their definitions have
        to be here because they refer to the underlying representation.

        [~backtrace:`Get] attaches the backtrace for the most recent exception. The same
        caveats as for [Printexc.print_backtrace] apply. [~backtrace:(`This s)] attaches
        the backtrace [s]. The default is no backtrace. *)
    val of_exn : ?backtrace:[ `Get | `This of string ] -> exn -> t @ portable

    val to_exn : t -> exn
    val pp : Formatter.t -> t -> unit

    module Internal_repr : Internal_repr with type info := t
    module Portable : Portable with type info := info

    val of_portable : Portable.t -> t @ portable
    val to_portable : t @ portable -> Portable.t

    (** Constructs a portable info out of a possibly non-portable info. This operation is
        less expensive than [err |> sexp_of_t |> create_s], but it's not a no-op.

        It's not a no-op because it needs to force any non-portable computation of the
        input info. *)
    val portabilize : t -> t @ portable

    (** Convert to [Portable.t] via [portabilize]. *)
    val to_portable_portabilize : t -> Portable.t
  end

  module type S = sig @@ portable
    include S0

    (** Unlike the standard [to_string_*] functions under [S], these encode any [t] that's
        represented by a sexp [1] using [Sexp.Utf8.to_string_*], ensuring that valid UTF-8
        sexp atoms get printed as-is. Standard [to_string_*] functions, by contrast,
        encode sexps using [Sexp.to_string_*], encoding UTF-8 / other non-ASCII characters
        using escape sequences.

        [1] this will be true in most cases except when created out of [of_string], which
        simply gets stored as a string, not a sexp. *)
    module Utf8 : sig
      (** [to_string_hum] forces the lazy message, which might be an expensive operation.

          [to_string_hum] usually produces a sexp; however, it is guaranteed that
          [to_string_hum (of_string s) = s].

          If this string is going to go into a log file, you may find it useful to ensure
          that the string is only one line long. To do this, use [to_string_mach t]. *)
      val to_string_hum : t -> string

      (** [to_string_mach t] outputs [t] as a sexp on a single line. *)
      val to_string_mach : t -> string
    end
  end
end

module type Info0 = sig @@ portable
  include module type of struct
    include Definitions
  end

  include S0 (** @inline *)
end

module type Info = sig @@ portable
  include Info0 (** @inline *)

  include S with type t := t (** @inline *)
end
