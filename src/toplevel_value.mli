(** This module provides a wrapper that can be used to simulate having toplevel items of
    non-value layout in modules. For example,
    {[
      module type%template [@kind k = (value, bits64)] S = sig
        type t

        val zero : (t Toplevel_value.t[@kind k])
      end
    ]}

    Here, [zero] is really a [t] in [S], but it's a [unit -> t] in [S [@kind bits64]]
    (since toplevel [bits64]s are currently prohibited). Both can be accessed via
    [Toplevel_value.get]:

    {[
      let%template zero (type t) (module M : S with type t = t[@kind k]) =
        (Toplevel_value.get [@kind k]) M.zero
      [@@kind k = (value, bits64)]
      ;;
    ]} *)

[%%template:
[@@@kind_set.define
  supported_non_values
  = (base_non_value, value & value, value & value & value, value & value & value & value)]

[@@@kind_set.define
  supported_values
  = (value_or_null_with_imm, value_or_null mod external_, value_or_null mod external64)]

type 'a t = 'a [@@kind k = supported_values]
type 'a t = unit -> 'a [@@kind k = supported_non_values]

external get : 'a. ('a t[@kind k]) -> 'a = "%identity" [@@kind k = supported_values]
val get : 'a. ('a t[@kind k]) -> 'a [@@kind k = supported_non_values]]
