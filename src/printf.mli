(** Functions for formatted output. *)

open! Import0


val ifprintf : 'a ->            ('r, 'a, 'c, unit)        format4 -> 'r
val sprintf  :                  ('r, unit, string)        format  -> 'r
val bprintf  : Caml.Buffer.t -> ('r, Caml.Buffer.t, unit) format  -> 'r

val ksprintf : (string -> 'a) -> ('r, unit, string, 'a) format4 -> 'r
val kbprintf : (Caml.Buffer.t -> 'a) -> Caml.Buffer.t -> ('r, Caml.Buffer.t, unit, 'a) format4 -> 'r

(** {6 Formatting error and exit functions}

    These functions have a polymorphic return type, since they do not return.  Naively,
    this doesn't mix well with variadic functions: if you define, say,

    {[
      let f fmt = ksprintf (fun s -> failwith s) fmt
    ]}

    then you find that [f "%d" : int -> 'a], as you'd expect, and [f "%d" 7 : 'a]. The
    problem with this is that ['a] unifies with (say) [int -> 'b], so [f "%d" 7 4] is not
    a type error -- the [4] is simply ignored.

    To mitigate this problem, these functions all take a final unit parameter. These
    rarely arise as formatting positional parameters (they can do with e.g. "%a", but not
    in a useful way) so they serve as an effective signpost for
    "end of formatting arguments". *)

(** Raises [Failure]. *)
val failwithf : ('r, unit, string, unit -> _) format4 -> 'r

(** Raises [Invalid_arg]. *)
val invalid_argf : ('r, unit, string, unit -> _) format4 -> 'r

