(** [Hash_intf.S] is the interface which a hash-function must support

    The functions of [Hash_intf.S] are only allowed to be used in specific sequence:

    alloc, reset ?seed, fold_..*, get_hash_value,
    reset ?seed, fold_..*, get_hash_value, ...

    (The optional [seed]s passed to each reset may differ.)

    The chain of applications from [reset] to [get_hash_value] must be done in a
    single-threaded manner (you can't use fold_* on a state that's been used before)

    More precisely, [alloc ()] creates a new family of states. All functions that take [t]
    and produce [t] return a new state from the same family.

    At any point in time, at most one state in the family is "valid". The other states
    are "invalid".

    The state returned by [alloc] is invalid.
    The state returned by [reset] is valid (all of the other states become invalid).
    The [fold_*] family of functions requres a valid state and produces a valid state
    (thereby making the input state invalid).
    [get_hash_value] requires a valid state and makes it invalid.

    These requirements are currently formally encoded in [Check_initialized_correctly]
    module in bench/bench.ml. *)

open! Import0

module type S = sig

  (** Name of the hash-function, e.g. "internalhash", "siphash" *)
  val description : string

  (** [state] is the internal hash-state used by the hash function. *)
  type state

  (** [fold_<T> state v] incorporates a value [v] of type <T> into the hash-state,
      returning a modified hash-state.  Implementations of the [fold_<T>] functions may
      mutate the [state] argument in place, and return a reference to it.  Implementations
      of the fold_<T> functions should not allocate. *)
  val fold_int    : state -> int    -> state
  val fold_int64  : state -> int64  -> state
  val fold_float  : state -> float  -> state
  val fold_string : state -> string -> state

  (** [seed] is the type used to seed the initial hash-state. *)
  type seed

  (** [alloc ()] returns a fresh uninitialized hash-state.  May allocate. *)
  val alloc : unit -> state

  (** [reset ?seed state] initializes/resets a hash-state with the given [seed], or else a
      default-seed. Argument [state] may be mutated. Should not allocate. *)
  val reset : ?seed:seed -> state -> state

  (** [hash_value] The type of hash values, returned by [get_hash_value] *)
  type hash_value

  (** [get_hash_value] extracts a hash-value from the hash-state. *)
  val get_hash_value : state -> hash_value

  module For_tests : sig
    val compare_state : state -> state -> int
    val state_to_string : state -> string
  end
end

module type Builtin_hash_fold_intf = sig
  type state
  type 'a folder = state -> 'a -> state

  val hash_fold_nativeint : nativeint folder
  val hash_fold_int64     : int64     folder
  val hash_fold_int32     : int32     folder
  val hash_fold_char      : char      folder
  val hash_fold_int       : int       folder
  val hash_fold_bool      : bool      folder
  val hash_fold_string    : string    folder
  val hash_fold_float     : float     folder
  val hash_fold_unit      : unit      folder

  val hash_fold_option : 'a folder -> 'a option folder
  val hash_fold_list   : 'a folder -> 'a list   folder
  val hash_fold_lazy_t : 'a folder -> 'a lazy_t folder

  (** Hash support for [array] and [ref] is provided, but is potentially DANGEROUS, since
      it incorporates the current contents of the array/ref into the hash value.  Because
      of this we add a [_frozen] suffix to the function name.

      Hash support for [string] is also potentially DANGEROUS, but strings are mutated
      less often, so we don't append [_frozen] to it.

      Also note that we don't support [bytes]. *)
  val hash_fold_ref_frozen   : 'a folder -> 'a ref   folder
  val hash_fold_array_frozen : 'a folder -> 'a array folder

end

module type Builtin_hash_intf = sig
  type hash_value

  val hash_nativeint : nativeint -> hash_value
  val hash_int64     : int64     -> hash_value
  val hash_int32     : int32     -> hash_value
  val hash_char      : char      -> hash_value
  val hash_int       : int       -> hash_value
  val hash_bool      : bool      -> hash_value
  val hash_string    : string    -> hash_value
  val hash_float     : float     -> hash_value
  val hash_unit      : unit      -> hash_value

end

module type Builtin_intf = sig
  include Builtin_hash_fold_intf
  include Builtin_hash_intf
end

module type Full = sig

  include S (** @inline *)

  type 'a folder = state -> 'a -> state

  (** [create ?seed ()] is a convenience.  Equivalent to [reset ?seed (alloc ())] *)
  val create : ?seed:seed -> unit -> state

  (** [of_fold fold] constructs a standard hash function from an existing fold function *)
  val of_fold : (state -> 'a -> state) -> ('a -> hash_value)

  module Builtin : Builtin_intf
    with type state := state
     and type 'a folder := 'a folder
     and type hash_value := hash_value

  (** [run ?seed folder x] runs [folder] on [x] in a newly allocated hash-state,
      initialized using optional [seed] or a default-seed.

      The following identity exists: [run [%hash_fold: T]] == [[%hash: T]]

      [run] can be used if we wish to run a hash-folder with a non-default seed. *)
  val run : ?seed:seed -> 'a folder -> 'a -> hash_value

end
