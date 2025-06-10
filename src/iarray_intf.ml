(** Immutable arrays.

    Immutable arrays have fixed contents and length, like a list. They have a flat, linear
    memory representation and constant-time random access, like an array. *)

open! Import

module Definitions = struct
  module type Operators = sig
    type 'a t := 'a iarray

    (** An alias for [get]. *)
    external ( .:() ) : ('a t[@local_opt]) -> int -> ('a[@local_opt]) = "%array_safe_get"
  end

  module type Public = sig
    type +'a t
    [@@deriving
      compare ~localize, equal ~localize, globalize, hash, sexp ~localize, sexp_grammar]

    (** Standard interfaces *)

    include%template Binary_searchable.S1 [@mode local] with type 'a t := 'a t

    include Indexed_container.S1_with_creators with type 'a t := 'a t
    include Invariant.S1 with type 'a t := 'a t

    (** Operators *)

    module O : Operators
    include Operators

    (** Indexing *)

    external length : ('a t[@local_opt]) @ contended -> int = "%array_length"
    external get : ('a t[@local_opt]) -> int -> ('a[@local_opt]) = "%array_safe_get"

    external unsafe_get
      :  ('a t[@local_opt])
      -> int
      -> ('a[@local_opt])
      = "%array_unsafe_get"

    val last_exn : 'a t -> 'a

    (** Functional update *)

    val set : 'a t -> int -> 'a -> 'a t
    val update : 'a t -> int -> f:('a -> 'a) -> 'a t

    (** Constructors *)

    val empty : _ t
    val singleton : 'a -> 'a t
    val create : len:int -> 'a -> mutate:local_ (local_ 'a array -> unit) -> 'a iarray

    val%template init : int -> f:(int -> 'a @ m) @ local -> 'a t @ m
    [@@alloc __ @ m = (heap_global, stack_local)]

    (** Conversions *)

    val of_sequence : 'a Sequence.t -> 'a t
    val to_sequence : 'a t -> 'a Sequence.t
    val of_list_rev : 'a list -> 'a t
    val of_list_map : 'a list -> f:local_ ('a -> 'b) -> 'b t
    val of_list_mapi : 'a list -> f:local_ (int -> 'a -> 'b) -> 'b t
    val of_list_rev_map : 'a list -> f:local_ ('a -> 'b) -> 'b t

    (** Subsequences *)

    val prefix : 'a t -> len:int -> 'a t
    val suffix : 'a t -> len:int -> 'a t
    val drop_prefix : 'a t -> len:int -> 'a t
    val drop_suffix : 'a t -> len:int -> 'a t
    val group : 'a t -> break:local_ ('a -> 'a -> bool) -> 'a t t

    (** Reordering *)

    val rev : 'a t -> 'a t
    val sort : 'a t -> compare:local_ ('a -> 'a -> int) -> 'a t
    val stable_sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
    val dedup_and_sort : 'a t -> compare:local_ ('a -> 'a -> int) -> 'a t
    val sort_and_group : 'a t -> compare:local_ ('a -> 'a -> int) -> 'a t t
    val is_sorted : 'a t -> compare:local_ ('a -> 'a -> int) -> bool
    val is_sorted_strictly : 'a t -> compare:local_ ('a -> 'a -> int) -> bool

    (** Combining elements *)

    val reduce : 'a t -> f:local_ ('a -> 'a -> 'a) -> 'a option
    val reduce_exn : 'a t -> f:local_ ('a -> 'a -> 'a) -> 'a
    val combine_errors : 'a Or_error.t t -> 'a t Or_error.t
    val combine_errors_unit : unit Or_error.t t -> unit Or_error.t

    [%%template:
    [@@@kind.default ka = value, kacc = (value, bits64, bits32, word, float64)]

    val fold
      : ('a : ka) ('acc : kacc).
      'a t -> init:'acc -> f:local_ ('acc -> 'a -> 'acc) -> 'acc

    val foldi
      : ('a : ka) ('acc : kacc).
      'a t -> init:'acc -> f:local_ (int -> 'acc -> 'a -> 'acc) -> 'acc

    val fold_right
      : ('a : ka) ('acc : kacc).
      'a t -> init:'acc -> f:local_ ('a -> 'acc -> 'acc) -> 'acc]

    val fold_map : 'a t -> init:'acc -> f:local_ ('acc -> 'a -> 'acc * 'b) -> 'acc * 'b t

    val fold_mapi
      :  'a t
      -> init:'acc
      -> f:local_ (int -> 'acc -> 'a -> 'acc * 'b)
      -> 'acc * 'b t

    (** Multiple arrays *)

    val zip : 'a t -> 'b t -> ('a * 'b) t option
    val zip_exn : 'a t -> 'b t -> ('a * 'b) t
    val unzip : ('a * 'b) t -> 'a t * 'b t
    val map2_exn : 'a t -> 'b t -> f:local_ ('a -> 'b -> 'c) -> 'c t
    val iter2_exn : 'a t -> 'b t -> f:local_ ('a -> 'b -> unit) -> unit
    val cartesian_product : 'a t -> 'b t -> ('a * 'b) t

    (** Random elements *)

    val random_element : ?random_state:Random.State.t -> 'a t -> 'a option
    val random_element_exn : ?random_state:Random.State.t -> 'a t -> 'a

    (** Blit operations *)

    val sub : 'a t -> pos:int -> len:int -> 'a t
    val subo : ?pos:int -> ?len:int -> 'a t -> 'a t

    module Of_array : sig
      val sub : local_ 'a array -> pos:int -> len:int -> 'a t
      val subo : ?pos:int -> ?len:int -> local_ 'a array -> 'a t
    end

    module To_array : sig
      val sub : 'a t -> pos:int -> len:int -> 'a array
      val subo : ?pos:int -> ?len:int -> 'a t -> 'a array

      val blito
        :  src:'a t
        -> ?src_pos:int
        -> ?src_len:int
        -> dst:local_ 'a array
        -> ?dst_pos:int
        -> unit
        -> unit

      val blit
        :  src:'a t
        -> src_pos:int
        -> dst:local_ 'a array
        -> dst_pos:int
        -> len:int
        -> unit

      val unsafe_blit
        :  src:'a t
        -> src_pos:int
        -> dst:local_ 'a array
        -> dst_pos:int
        -> len:int
        -> unit
    end

    (** Operations for local iarrays. *)
    module Local : sig
      include Container_with_local.S1_indexed_with_creators with type 'a t := 'a t

      val last_exn : local_ 'a t -> local_ 'a

      module Let_syntax : sig
        val return : local_ 'a -> local_ 'a t

        module Let_syntax : sig
          val return : local_ 'a -> local_ 'a t
          val bind : local_ 'a t -> f:local_ (local_ 'a -> local_ 'b t) -> local_ 'b t
          val map : local_ 'a t -> f:local_ (local_ 'a -> local_ 'b) -> local_ 'b t
          val both : local_ 'a t -> local_ 'b t -> local_ ('a * 'b) t

          module Open_on_rhs : sig end
        end
      end

      (** [init] has an unusual implementation: it temporarily uses O(n) space on the
          function-call stack ({i not} the locals stack). This is necessary because the
          contents of the array must be allocated {i before} the array itself, and so we
          use n pointers on the function-call stack to do so. Accordingly, allocating a
          large array with [init] might cause a stack overflow.

          An alternative implementation would use the locals stack to store the pointers,
          but this allocation would last as long as the array itself.

          Note that this function never itself allocates on the heap, only on the stack. *)
      val init : int -> f:local_ (int -> local_ 'a) -> local_ 'a t

      (** [init_with_globals] avoids the extra O(n) function-call stack space of [init],
          because the array contents are guaranteed not to be stored in the local data
          stack. Therefore, they do not need to be allocated before the array. *)
      val init_with_globals : int -> f:local_ (int -> 'a) -> local_ 'a t

      val singleton : local_ 'a -> local_ 'a t

      val create
        :  len:int
        -> 'a
        -> mutate:local_ (local_ 'a array -> unit)
        -> local_ 'a iarray

      val prefix : local_ 'a t -> len:int -> local_ 'a t
      val suffix : local_ 'a t -> len:int -> local_ 'a t
      val drop_prefix : local_ 'a t -> len:int -> local_ 'a t
      val drop_suffix : local_ 'a t -> len:int -> local_ 'a t
      val rev : local_ 'a t -> local_ 'a t
      val sub : local_ 'a t -> pos:int -> len:int -> local_ 'a t
      val subo : ?pos:local_ int -> ?len:local_ int -> local_ 'a t -> local_ 'a t

      (** [of_list_*] functions require temporary call-stack space proportional to the
          length of the input list, like [init]. *)

      val of_list_rev : local_ 'a list -> local_ 'a t
      val of_list_map : local_ 'a list -> f:local_ (local_ 'a -> local_ 'b) -> local_ 'b t

      val of_list_mapi
        :  local_ 'a list
        -> f:local_ (local_ int -> local_ 'a -> local_ 'b)
        -> local_ 'b t

      val of_list_rev_map
        :  local_ 'a list
        -> f:local_ (local_ 'a -> local_ 'b)
        -> local_ 'b t

      val iter2_exn
        :  local_ 'a t
        -> local_ 'b t
        -> f:local_ (local_ 'a -> local_ 'b -> unit)
        -> unit

      val map2_exn
        :  local_ 'a t
        -> local_ 'b t
        -> f:local_ (local_ 'a -> local_ 'b -> local_ 'c)
        -> local_ 'c t

      val cartesian_product : local_ 'a t -> local_ 'b t -> local_ ('a * 'b) t

      [%%template:
      [@@@kind.default ka = value, kacc = (value, bits64, bits32, word, float64)]

      val fold
        : ('a : ka) ('acc : kacc).
        local_ 'a t
        -> init:local_ 'acc
        -> f:local_ (local_ 'acc -> local_ 'a -> local_ 'acc)
        -> local_ 'acc

      val foldi
        : ('a : ka) ('acc : kacc).
        local_ 'a t
        -> init:local_ 'acc
        -> f:local_ (int -> local_ 'acc -> local_ 'a -> local_ 'acc)
        -> local_ 'acc

      val fold_right
        : ('a : ka) ('acc : kacc).
        local_ 'a t
        -> init:local_ 'acc
        -> f:local_ (local_ 'a -> local_ 'acc -> local_ 'acc)
        -> local_ 'acc]

      val fold_map
        :  local_ 'a t
        -> init:local_ 'acc
        -> f:local_ (local_ 'acc -> local_ 'a -> local_ 'acc * 'b)
        -> local_ 'acc * 'b t

      val fold_mapi
        :  local_ 'a t
        -> init:local_ 'acc
        -> f:local_ (int -> local_ 'acc -> local_ 'a -> local_ 'acc * 'b)
        -> local_ 'acc * 'b t
    end

    (** Unsafe conversions

        Immutable arrays can be converted to and from mutable arrays, as they can have the
        same representation. One must be careful not to modify the mutable array sharing a
        representation with an immutable array at any time when both are reachable. Doing
        so violates the invariants of the immutable array. The OCaml compiler might rely
        on these invariants when compiling or optimizing code that uses immutable arrays. *)

    val unsafe_to_array__promise_no_mutation : 'a t -> 'a array

    external unsafe_of_array__promise_no_mutation
      :  ('a array[@local_opt])
      -> ('a t[@local_opt])
      = "%array_to_iarray"
  end
end

module type Iarray = sig @@ portable
  include module type of struct
    include Definitions
  end

  include Public with type 'a t = 'a iarray (** @inline *)

  (**/**)

  module Private : sig
    module Test_unsafe_local_implementations : module type of struct
      let concat = Local.concat
      let concat_map = Local.concat_map
      let concat_mapi = Local.concat_mapi
      let filter = Local.filter
      let filteri = Local.filteri
      let filter_map = Local.filter_map
      let filter_mapi = Local.filter_mapi
      let partition_tf = Local.partition_tf
      let partition_map = Local.partition_map
      let fold_map = Local.fold_map
      let fold_mapi = Local.fold_mapi
    end
  end
  [@@alert
    private_iarray "These bindings are intended only for use in tests of Iarray itself."]
end
