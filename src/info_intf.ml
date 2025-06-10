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
      | Exn of exn Modes.Contended_via_portable.t Modes.Global.t
      | Sexp of Sexp.t
      | Tag_sexp of string * Sexp.t * Source_code_position0.t option
      | Tag_t of string * t
      | Tag_arg of string * Sexp.t * t
      | Of_list of int option * t list
      | With_backtrace of t * string (** The second argument is the backtrace *)
    [@@deriving sexp_of]

    val of_info : local_ info -> t
    val to_info : t -> info
  end

  module type S = sig @@ portable
    (** Serialization and comparison force the lazy message. *)
    type t : value mod contended global
    [@@deriving compare ~localize, equal ~localize, globalize, hash, sexp, sexp_grammar]

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
    val of_thunk : (unit -> string) -> t
    val of_lazy_t : t Lazy.t -> t

    (** For [create message a sexp_of_a], [sexp_of_a a] is lazily computed, when the info
        is converted to a sexp. So if [a] is mutated in the time between the call to
        [create] and the sexp conversion, those mutations will be reflected in the sexp.
        Use [~strict:()] to force [sexp_of_a a] to be computed immediately. *)
    val%template create
      :  ?here:Source_code_position0.t
      -> ?strict:unit
      -> string
      -> ('a : k)
      -> ('a -> Sexp.t)
      -> t
    [@@kind k = (bits64, float64, value)]

    val create_s : Sexp.t -> t @ portable

    (** Constructs a [t] containing only a string from a format. This eagerly constructs
        the string. *)
    val createf : ('a, unit, string, t) format4 -> 'a

    (** Adds a string to the front. *)
    val tag : t -> tag:string -> t

    (** Adds a sexp to the front. *)
    val tag_s : t -> tag:Sexp.t -> t

    (** Adds a lazy sexp to the front. *)
    val tag_s_lazy : t -> tag:Sexp.t Lazy.t -> t

    (** Adds a string and some other data in the form of an s-expression at the front. *)
    val tag_arg : t -> string -> 'a -> ('a -> Sexp.t) -> t

    (** Combines multiple infos into one. *)
    val of_list : t list -> t

    (** [of_exn] and [to_exn] are primarily used with [Error], but their definitions have
        to be here because they refer to the underlying representation.

        [~backtrace:`Get] attaches the backtrace for the most recent exception. The same
        caveats as for [Printexc.print_backtrace] apply. [~backtrace:(`This s)] attaches
        the backtrace [s]. The default is no backtrace. *)
    val of_exn : ?backtrace:[ `Get | `This of string ] -> exn -> t

    val to_exn : t -> exn
    val pp : Formatter.t -> t -> unit

    module Internal_repr : Internal_repr with type info := t
  end
end

module type Info = sig @@ portable
  include module type of struct
    include Definitions
  end

  include S (** @inline *)
end
