open! Import

(** A value of type ['a Dynamic.t] is a dynamically scoped variable of type ['a].

    [Dynamic] works like [Ref], except that changes to its value are only visible to the
    current fiber and its children.

    (A note on fibers: currently, common libraries for parallel programming in OxCaml are
    nascent, and a work-in-progress. If you're not using parallelism, you can safely
    assume that you can ignore any references in documentation to "fibers".)

    Every dynamic variable has a single "root" value, which is visible by default to all
    running fibers. A fiber can temporarily change the locally visible value of a dynamic
    variable within the scope of a function by calling [with_temporarily]. During the
    execution of [with_temporarily], any changes to the root value of the dynamic variable
    (eg via calls to [set_root] by the current or other fibers) are unobservable until
    after the outermost call to [with_temporarily] returns.

    NOTE: This module currently contains a "stub" implementation of dynamically scoped
    variables, intended as a safe stop-gap until we add support for native dynamic scoping
    in the runtime. It is currently unsafe to use this module in the presence of fibers.
    The intention is that the interface exposed by this module will not change when it is
    replaced with a fiber-safe implementation. *)

type 'a t = 'a Basement.Dynamic.t

(** [make v] creates a new dynamic variable with initial root value [v].

    Since any domain can access the [Dynamic.t], the value must be portable. Since any
    domain can access and modify the [Dynamic.t] without synchronization, the type of
    values in the [Dynamic.t] must cross the contention axis. *)
val make : 'a. 'a -> 'a t

(** [get dynamic] retrieves the current value of the dynamic variable [dynamic] in the
    current fiber.

    Within a call to [with_temporarily], this returns the {i local} value of the dynamic
    variable (the value that was passed to [with_temporarily]. Outside of a call to
    [with_temporarily], this returns the {i root} value *)
val get : 'a. 'a t -> 'a

(** [set_root dynamic v] sets the {i root} value of [dynamic] to [v].

    Any fibers which have called [with_temporarily] will not see the new value until after
    [with_temporarily] exits, but any new fibers and any fibers which have not called
    [with_temporarily] will see the new value immediately after [set_root] returns.

    Note that it is almost always the wrong choice to call this function anywhere other
    than the top-level. *)
val set_root : 'a. 'a t -> 'a -> unit

(** [with_temporarily dynamic v ~f] invokes [f] in a context where [dynamic] is set to
    [v], then restores [dynamic] to the parent value (either the root value, or the value
    set by a surrounding call to [with_temporarily]). *)
val with_temporarily : 'a 'b. 'a t -> 'a -> f:(unit -> 'b) -> 'b
