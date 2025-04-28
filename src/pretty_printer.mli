(** A list of pretty printers for various types, for use in toplevels.

    [Pretty_printer] has a [string list ref] with the names of [pp] functions matching the
    interface:

    {[
      val pp : Format.formatter -> t -> unit
    ]}

    The names are actually OCaml identifier names, e.g., "Base.Int.pp". Code for building
    toplevels evaluates the strings to yield the pretty printers and register them with
    the OCaml runtime.

    This module is only responsible for collecting the pretty-printers. Another mechanism
    is needed to register this collection with the "toploop" library for pretty-printing
    to actually happen. How to do that depends on how you build and deploy the OCaml
    toplevel. One common way to do it in vanilla toplevel is to call
    [#require "core.top"]. *)

open! Import

(** [all ()] returns all pretty printers that have been [register]ed. *)
val all : unit -> string list

(** Modules that provide a pretty printer will match [S]. *)
module type S = sig
  type t

  val pp : Formatter.t -> t -> unit
end

(** [Register] builds a [pp] function from a [to_string] function, and adds the
    [module_name ^ ".pp"] to the list of pretty printers. The idea is to statically
    guarantee that one has the desired [pp] function at the same point where the [name] is
    added.

    [module_name ^ ".pp"] must be a valid OCaml identifier. It is recommended to not have
    any "."s in [module_name]. For example, if [module_name] is "A.B" and "A.B" is not a
    valid identifier because "A" is a valid library that doesn't expose a module "B", then
    "A.B.pp" will not be a valid identifier. *)
module%template.portable Register (M : sig
    type t

    val module_name : string
    val to_string : t -> string
  end) : S with type t := M.t

(** [Register_pp] is like [Register], but allows a custom [pp] function rather than using
    [to_string]. *)
module%template.portable Register_pp (M : sig
    include S

    val module_name : string
  end) : S with type t := M.t

(** [register name] adds [name] to the list of pretty printers. Use the [Register] functor
    if possible. *)
val register : string -> unit
