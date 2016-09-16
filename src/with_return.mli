(** This is [include]'d and documented in {! module: Common}.  It is defined here to avoid
    circular dependencies. *)

open! Import

type -'a return = private { return : 'b. 'a -> 'b }

val with_return        : ('a return -> 'a  ) -> 'a

(** Note that [with_return_option] allocates ~5 words more than equivalent [with_return]
    call *)
val with_return_option : ('a return -> unit) -> 'a option

(** [prepend a ~f] returns a value [x] such that each call to [x.return] first applies [f]
    before applying [a.return].  The call to [f] is "prepended" to the call to the
    original [a.return].  A possible use case is to hand [x] over to an other function
    which returns ['b] a subtype of ['a], or to capture a common transformation [f]
    applied to returned values at several call sites. *)
val prepend : 'a return -> f:('b -> 'a) -> 'b return
