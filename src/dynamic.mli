@@ portable

open! Import

(** A value of type ['a Dynamic.t] is a dynamically scoped variable of type ['a].

    [Dynamic] works like [Ref], except that changes to its value are only visible to the
    current fiber and its children.

    Every dynamic variable has a single "root" value, which is visible by default to all
    running fibers. A fiber can temporarily change the locally visible value of a dynamic
    variable within the scope of a function by calling [with_temporarily]. During the
    execution of [with_temporarily], any changes to the root value of the dynamic variable
    (eg via calls to [set_root] by the current or other fibers) are unobservable until
    after the outermost call to [with_temporarily] returns. *)

type 'a t : value mod contended portable = 'a Basement.Dynamic.t

(** [make v] creates a new dynamic variable with initial root value [v].

    Since any domain can access the [Dynamic.t], the value must be portable. Since any
    domain can access and modify the [Dynamic.t] without synchronization, the type of
    values in the [Dynamic.t] must cross the contention axis. *)
val make : ('a : value mod contended). 'a @ portable -> 'a t

(** [get dynamic] retrieves the current value of the dynamic variable [dynamic] in the
    current fiber.

    Within a call to [with_temporarily], this returns the {i local} value of the dynamic
    variable (the value that was passed to [with_temporarily]. Outside of a call to
    [with_temporarily], this returns the {i root} value *)
val get : ('a : value mod contended). 'a t -> 'a @ portable

(** [set_root dynamic v] sets the {i root} value of [dynamic] to [v].

    Any fibers which have called [with_temporarily] will not see the new value until after
    [with_temporarily] exits, but any new fibers and any fibers which have not called
    [with_temporarily] will see the new value immediately after [set_root] returns.

    Note that it is almost always the wrong choice to call this function anywhere other
    than the top-level. *)
val set_root : ('a : value mod contended). 'a t -> 'a @ portable -> unit

(** [with_temporarily dynamic v ~f] invokes [f] in a context where [dynamic] is set to
    [v], then restores [dynamic] to the parent value (either the root value, or the value
    set by a surrounding call to [with_temporarily]). *)
val with_temporarily
  : ('a : value mod contended) 'b.
  'a t -> 'a @ portable -> f:(unit -> 'b) @ once -> 'b
