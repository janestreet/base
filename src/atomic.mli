(** An atomic (mutable) reference to a value of type ['a].

    Atomic references mode cross both contention and portability, meaning they are always
    uncontended and always portable, regardless of the kind of the ['a] type parameter, or
    the mode of the atomic reference itself.

    - They are always uncontended because mutating an atomic reference happens
      {i atomically} - multiple fibers mutating the same atomic reference simultaneously
      do not cause a data race.

    - They are always portable because all the functions for creating or mutating an
      atomic reference require the ['a] argument to be provided at the portable mode

    For atomic integers, always mutate their value using one of the intrinsic operations
    ([fetch_and_add], [add], [sub], [logand], [logor], [logxor], [incr], or [decr]). For
    atomic references to complex structures, use [update] to atomically update the atomic.
    For example, to atomically add a value to a [Set]:

    {[
      open! Base

      let atomically_add_to_set (set_atomic : (_, _) Set.t Atomic.t) value =
        Atomic.update set_atomic ~pure_f:(fun set -> Set.add set value)
      ;;
    ]} *)

open! Import

module Compare_failed_or_set_here : sig
  (** The result of a call to [compare_and_set]. See the documentation of that function
      for more information. *)
  type t =
    | Compare_failed
    | Set_here
  [@@deriving sexp_of ~localize]
end

type !'a t = 'a Basement.Portable_atomic.t

[%%rederive: type nonrec !'a t = 'a t [@@deriving sexp_of]]
[%%rederive: type nonrec !'a t = 'a t [@@deriving of_sexp]]

(** [make v] creates an atomic reference with initial value [v] *)
val make : 'a -> 'a t

(** [make_alone v] creates an atomic reference with initial value [v] which is alone on a
    cache line. It occupies 4-16x the memory of one allocated with [make v].

    The primary purpose of [make_alone] is to prevent performance degradation caused by
    false sharing. When a CPU performs an atomic operation, it temporarily takes ownership
    of the entire cache line containing the atomic reference. If multiple atomic
    references share the same cache line, modifying these disjoint memory regions
    simultaneously becomes impossible, which can create a bottleneck. Hence, as a general
    guideline, if an atomic reference is experiencing contention, assigning it its own
    cache line may improve performance. *)
val make_alone : 'a -> 'a t

(** [get r] gets the the current value of [r]. *)
external get : ('a t[@local_opt]) -> 'a = "%atomic_load"

(** [set r v] sets the value of [r] to [v] *)
external set : ('a t[@local_opt]) -> 'a -> unit = "caml_atomic_set_stub"

(** [exchange r v] sets the value of [r] to [v], and returns the previous value *)
external exchange : ('a t[@local_opt]) -> 'a -> 'a = "%atomic_exchange"

(** [compare_and_set r ~if_phys_equal_to ~replace_with] sets the new value of [r] to
    [replace_with] {i only} if its current value is physically equal to [if_phys_equal_to]
    -- the comparison and the set occur atomically. Returns [Set_here] if the value was
    set to [replace_with] by this call to [compare_and_set], or [Compare_failed] if the
    current value was not physically equal to [if_phys_equal_to] and hence the atomic
    reference was left unchanged. *)
external compare_and_set
  :  ('a t[@local_opt])
  -> if_phys_equal_to:'a
  -> replace_with:'a
  -> Compare_failed_or_set_here.t
  = "%atomic_cas"

(** [compare_exchange r ~if_phys_equal_to ~replace_with] sets the new value of [r] to
    [replace_with] only if its current value is physically equal to [if_phys_equal_to] --
    the comparison and the set occur atomically. Returns the previous value of [r], or the
    current (unchanged) value if the comparison failed. *)
external compare_exchange
  :  ('a t[@local_opt])
  -> if_phys_equal_to:'a
  -> replace_with:'a
  -> 'a
  = "caml_atomic_compare_exchange_stub"

(** [update t ~pure_f] atomically updates [t] to be the result of [pure_f (get t)].
    [pure_f] may be called multiple times, so should be free of side effects. *)
val update : 'a t -> pure_f:('a -> 'a) -> unit

(** [update_and_return t ~pure_f] atomically updates [t] to be the result of
    [pure_f (get t)]. [pure_f] may be called multiple times, so should be free of side
    effects. Returns the old value. *)
val update_and_return : 'a t -> pure_f:('a -> 'a) -> 'a

(** [fetch_and_add r n] atomically increments the value of [r] by [n], and returns the
    previous value (before the increment). *)
external fetch_and_add : (int t[@local_opt]) -> int -> int = "%atomic_fetch_add"

(** [add r i] atomically adds [i] to the value of [r]. *)
external add : (int t[@local_opt]) -> int -> unit = "caml_atomic_add_stub"

(** [sub r i] atomically subtracts [i] from the value of [r]. *)
external sub : (int t[@local_opt]) -> int -> unit = "caml_atomic_sub_stub"

(** [logand r i] atomically bitwise-ands [i] onto [r]. *)
external logand : (int t[@local_opt]) -> int -> unit = "caml_atomic_land_stub"

(** [logor r i] atomically bitwise-ands [i] onto [r]. *)
external logor : (int t[@local_opt]) -> int -> unit = "caml_atomic_lor_stub"

(** [logxor r i] atomically bitwise-xors [i] onto [r]. *)
external logxor : (int t[@local_opt]) -> int -> unit = "caml_atomic_lxor_stub"

(** [incr r] atomically increments the value of [r] by [1]. *)
val incr : int t -> unit

(** [decr r] atomically decrements the value of [r] by [1]. *)
val decr : int t -> unit

module Expert : sig
  (** Load the value referenced by the given atomic, without using any compiler or
      hardware fences.

      This is dubiously safe, and has no explicit semantics within the OCaml memory
      model - and may do the wrong thing entirely on backends with weak memory models such
      as ARM. Use with caution! *)
  val fenceless_get : 'a t -> 'a
end
