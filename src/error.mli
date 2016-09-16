open! Import

include Info_intf.S

(** Note that the exception raised by this function maintains a reference to the [t]
    passed in. *)
val raise : t -> _

val raise_s : Sexp.t -> _

val to_info : t -> Info.t
val of_info : Info.t -> t
