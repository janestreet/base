(** Immutable arrays.

    Immutable arrays have fixed contents and length, like a list. They have a flat, linear
    memory representation and constant-time random access, like an array. *)

open! Import0
module Either = Either0
module List = List0
module Result = Result0
open Modes.Export

module Definitions = struct
  module type Operators = sig
    type 'a t := 'a iarray

    (** An alias for [get]. *)
    external ( .:() )
      : 'a.
      ('a t[@local_opt]) -> int -> ('a[@local_opt])
      = "%array_safe_get"
    [@@layout_poly]
  end

  module type Public = sig
    type +'a t

    [%%rederive:
      (** Default derivers use [value_or_null] rather than [any]. *)
      type nonrec 'a t = 'a t [@@deriving compare, equal, sexp, globalize, sexp_grammar]]

    (** We add type aliases for derivers that use other kinds. *)

    [%%rederive: type nonrec 'a t = 'a t [@@deriving hash]]

    (** Standard interfaces *)

    val init : 'a. int -> f:(int -> 'a) -> 'a t

    include Binary_searchable.S1 with type 'a t := 'a t
    include Indexed_container.S1_with_creators with type 'a t := 'a t

    val init : 'a. int -> f:(int -> 'a) -> 'a t
    val map : 'a 'b. 'a t -> f:('a -> 'b) -> 'b t
    val mapi : 'a 'b. 'a t -> f:(int -> 'a -> 'b) -> 'b t
    val iteri : 'a. 'a t -> f:(int -> 'a -> unit) -> unit
    val iter : 'a. 'a t -> f:('a -> unit) -> unit
    val for_all : 'a. 'a t -> f:('a -> bool) -> bool

    (** Like [find] but returning [or_null] instead of [option]. *)
    val find_or_null : 'a. 'a t -> f:('a -> bool) -> 'a or_null

    (** Like [findi] but returning [or_null] instead of [option]. *)
    val findi_or_null : 'a. 'a t -> f:(int -> 'a -> bool) -> (int * 'a) or_null

    (** Like [find_map] but with a function returning [or_null] instead of [option]. *)
    val find_map_or_null : 'a 'b. 'a t -> f:('a -> 'b or_null) -> 'b or_null

    (** Like [find_mapi] but with a function returning [or_null] instead of [option]. *)
    val find_mapi_or_null : 'a 'b. 'a t -> f:(int -> 'a -> 'b or_null) -> 'b or_null

    val of_array : 'a. 'a array -> 'a t
    val to_array : 'a. 'a t -> 'a array

    include Invariant.S1 with type 'a t := 'a t

    (** Operators *)

    module O : Operators
    include Operators

    (** Indexing *)

    external length : 'a. ('a t[@local_opt]) -> int = "%array_length" [@@layout_poly]

    external get : 'a. ('a t[@local_opt]) -> int -> ('a[@local_opt]) = "%array_safe_get"
    [@@layout_poly]

    val get_opt : 'a t -> int -> 'a option

    external unsafe_get
      : 'a.
      ('a t[@local_opt]) -> int -> ('a[@local_opt])
      = "%array_unsafe_get"
    [@@layout_poly]

    include sig
      [@@@implicit_kind: 'a]

      val last_exn : 'a t -> 'a

      (** Functional update *)

      val set : 'a t -> int -> 'a -> 'a t
      val update : 'a t -> int -> f:('a -> 'a) -> 'a t

      (** Constructors *)

      val empty : _ t
      val singleton : 'a -> 'a t
      val create : len:int -> 'a -> mutate:('a array -> unit) -> 'a iarray

      (** Conversions *)

      val of_sequence : 'a Sequence.t -> 'a t
      val to_sequence : 'a t -> 'a Sequence.t
      val of_list_rev : 'a. 'a list -> 'a t
      val of_list_map : 'a list -> f:('a -> 'b) -> 'b t
      val of_list_mapi : 'a list -> f:(int -> 'a -> 'b) -> 'b t
      val of_list_rev_map : 'a list -> f:('a -> 'b) -> 'b t

      (** Subsequences *)

      val prefix : 'a t -> len:int -> 'a t
      val suffix : 'a t -> len:int -> 'a t
      val drop_prefix : 'a t -> len:int -> 'a t
      val drop_suffix : 'a t -> len:int -> 'a t
      val group : 'a t -> break:('a -> 'a -> bool) -> 'a t t

      (** [split_n t n] returns a pair of iarrays [(first, second)] where [first] contains
          the first [n] elements of [t] and [second] contains the remaining elements.

          - If [n >= length t], returns [(t, empty)].
          - If [n <= 0], returns [(empty, t)]. *)
      val split_n : 'a t -> int -> 'a t * 'a t

      (** [chunks_of t ~length] returns an iarray of iarrays whose concatenation is equal
          to the original iarray. Every iarray has [length] elements, except for possibly
          the last iarray, which may have fewer. [chunks_of] raises if [length <= 0]. *)
      val chunks_of : 'a t -> length:int -> 'a t t

      (** Reordering *)

      val rev : 'a t -> 'a t
      val sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
      val stable_sort : 'b t -> compare:('b -> 'b -> int) -> 'b t
      val dedup_and_sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
      val sort_and_group : 'a t -> compare:('a -> 'a -> int) -> 'a t t
      val is_sorted : 'a t -> compare:('a -> 'a -> int) -> bool
      val is_sorted_strictly : 'a t -> compare:('a -> 'a -> int) -> bool
      val find_a_dup : 'a t -> compare:('a -> 'a -> int) -> 'a option

      (** Combining elements *)

      val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a option
      val reduce_exn : 'a t -> f:('a -> 'a -> 'a) -> 'a
      val combine_errors : 'a Or_error.t t -> 'a t Or_error.t
      val combine_errors_unit : unit Or_error.t t -> unit Or_error.t
      val fold : 'a 'acc. 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
      val foldi : 'a 'acc. 'a t -> init:'acc -> f:(int -> 'acc -> 'a -> 'acc) -> 'acc
      val fold_right : 'a 'acc. 'a t -> init:'acc -> f:('a -> 'acc -> 'acc) -> 'acc
      val fold_map : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) -> 'acc * 'b t

      val fold_mapi
        :  'a t
        -> init:'acc
        -> f:(int -> 'acc -> 'a -> 'acc * 'b)
        -> 'acc * 'b t

      (** Multiple arrays *)

      val zip : 'a t -> 'b t -> ('a * 'b) t option
      val zip_exn : 'a t -> 'b t -> ('a * 'b) t
      val unzip : ('a * 'b) t -> 'a t * 'b t
      val map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
      val iter2_exn : 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit
      val cartesian_product : 'a t -> 'b t -> ('a * 'b) t

      (** Random elements *)

      val random_element : ?random_state:Random.State.t -> 'a t -> 'a option
      val random_element_exn : ?random_state:Random.State.t -> 'a t -> 'a

      (** Blit operations *)

      val sub : 'a t -> pos:int -> len:int -> 'a t
      val subo : ?pos:int -> ?len:int -> 'a t -> 'a t

      module Of_array : sig
        val sub : 'a array -> pos:int -> len:int -> 'a t
        val subo : ?pos:int -> ?len:int -> 'a array -> 'a t
      end

      module To_array : sig
        val sub : 'a t -> pos:int -> len:int -> 'a array
        val subo : ?pos:int -> ?len:int -> 'a t -> 'a array

        val blito
          :  src:'a t
          -> ?src_pos:int
          -> ?src_len:int
          -> dst:'a array
          -> ?dst_pos:int
          -> unit
          -> unit

        val blit
          :  src:'a t
          -> src_pos:int
          -> dst:'a array
          -> dst_pos:int
          -> len:int
          -> unit

        val unsafe_blit
          :  src:'a t
          -> src_pos:int
          -> dst:'a array
          -> dst_pos:int
          -> len:int
          -> unit
      end
    end

    (** Operations for local iarrays. *)
    module Local : sig
      (*_ Lifted from [Container_with_local]. All instantiate [Container] functions *)

      [@@@implicit_kind: 'a]
      [@@@implicit_kind: 'b]
      [@@@implicit_kind: 'c]

      val length : 'a t -> int
      val is_empty : 'a t -> bool
      val mem : 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool
      val iter : 'a t -> f:('a -> unit) -> unit

      val fold_result
        :  'a t
        -> init:'acc
        -> f:('acc -> 'a -> ('acc, 'e) Result.t)
        -> ('acc, 'e) Result.t

      val fold_until
        :  'a t
        -> init:'acc
        -> f:('acc -> 'a -> ('acc, 'final) Container.Continue_or_stop.t)
        -> finish:('acc -> 'final)
        -> 'final

      val exists : 'a t -> f:('a -> bool) -> bool
      val for_all : 'a t -> f:('a -> bool) -> bool
      val count : 'a t -> f:('a -> bool) -> int

      val sum
        :  (module Container.Summable with type t = 'sum)
        -> 'a t
        -> f:('a -> 'sum)
        -> 'sum

      val find : 'a t -> f:('a -> bool) -> 'a option
      val find_map : 'a t -> f:('a -> 'b option) -> 'b option
      val to_list : 'a t -> 'a list
      val min_elt : 'a t -> compare:('a -> 'a -> int) -> 'a option
      val max_elt : 'a t -> compare:('a -> 'a -> int) -> 'a option
      val of_list : 'a list -> 'a t
      val append : 'a t -> 'a t -> 'a t
      val concat : 'a t t -> 'a t
      val map : 'a t -> f:('a -> 'b) -> 'b t
      val map_to_global : 'a t -> f:('a -> 'b) -> 'b t
      val map_of_global : 'a t -> f:('a -> 'b) -> 'b t
      val filter : 'a t -> f:('a -> bool) -> 'a t
      val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
      val concat_map : 'a t -> f:('a -> 'b t) -> 'b t
      val partition_tf : 'a t -> f:('a -> bool) -> 'a t * 'a t
      val partition_map : 'a t -> f:('a -> ('b, 'c) Either.t) -> 'b t * 'c t
      val iteri : 'a t -> f:(int -> 'a -> unit) -> unit
      val existsi : 'a t -> f:(int -> 'a -> bool) -> bool
      val for_alli : 'a t -> f:(int -> 'a -> bool) -> bool
      val counti : 'a t -> f:(int -> 'a -> bool) -> int
      val findi : 'a t -> f:(int -> 'a -> bool) -> (int * 'a) option
      val find_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b option
      val partitioni_tf : 'a t -> f:(int -> 'a -> bool) -> 'a t * 'a t
      val partition_mapi : 'a t -> f:(int -> 'a -> ('b, 'c) Either.t) -> 'b t * 'c t
      val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t
      val mapi_to_global : 'a t -> f:(int -> 'a -> 'b) -> 'b t
      val mapi_of_global : 'a t -> f:(int -> 'a -> 'b) -> 'b t
      val filteri : 'a t -> f:(int -> 'a -> bool) -> 'a t
      val filter_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b t
      val concat_mapi : 'a t -> f:(int -> 'a -> 'b t) -> 'b t

      (*_ [Iarray]-specific *)

      val last_exn : 'a t -> 'a
      val to_array_of_immediates : 'a t -> 'a array
      val sort_immediates : 'a t -> compare:('a -> 'a -> int) -> 'a t

      module Let_syntax : sig
        val return : 'a -> 'a t

        module Let_syntax : sig
          val return : 'a -> 'a t
          val bind : 'a t -> f:('a -> 'b t) -> 'b t
          val map : 'a t -> f:('a -> 'b) -> 'b t
          val both : 'a t -> 'b t -> ('a * 'b) t

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
      val init : int -> f:(int -> 'a) -> 'a t

      (** [init_with_globals] avoids the extra O(n) function-call stack space of [init],
          because the array contents are guaranteed not to be stored in the local data
          stack. Therefore, they do not need to be allocated before the array. *)
      val init_with_globals : int -> f:(int -> 'a) -> 'a t

      val singleton : 'a -> 'a t
      val create : len:int -> 'a -> mutate:('a array -> unit) -> 'a iarray
      val prefix : 'a t -> len:int -> 'a t
      val suffix : 'a t -> len:int -> 'a t
      val drop_prefix : 'a t -> len:int -> 'a t
      val drop_suffix : 'a t -> len:int -> 'a t
      val rev : 'a t -> 'a t
      val sub : 'a t -> pos:int -> len:int -> 'a t
      val subo : ?pos:int -> ?len:int -> 'a t -> 'a t

      (** [of_list_*] functions require temporary call-stack space proportional to the
          length of the input list, like [init]. *)

      val of_list_rev : 'a list -> 'a t
      val of_list_map : 'a list -> f:('a -> 'b) -> 'b t
      val of_list_mapi : 'a list -> f:(int -> 'a -> 'b) -> 'b t
      val of_list_rev_map : 'a list -> f:('a -> 'b) -> 'b t
      val iter2_exn : 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit
      val map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
      val cartesian_product : 'a t -> 'b t -> ('a * 'b) t
      val fold : 'a 'acc. 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
      val foldi : 'a 'acc. 'a t -> init:'acc -> f:(int -> 'acc -> 'a -> 'acc) -> 'acc
      val fold_right : 'a 'acc. 'a t -> init:'acc -> f:('a -> 'acc -> 'acc) -> 'acc
      val fold_map : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) -> 'acc * 'b t

      val fold_mapi
        :  'a t
        -> init:'acc
        -> f:(int -> 'acc -> 'a -> 'acc * 'b)
        -> 'acc * 'b t
    end

    (** Operations for unique iarrays *)
    module Unique : sig
      val init : int -> f:(int -> 'a) -> 'a t
      val map : 'a t -> f:('a -> 'b) -> 'b t
      val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t
      val iter : 'a t -> f:('a -> unit) -> unit
      val iteri : 'a t -> f:(int -> 'a -> unit) -> unit
      val unzip : ('a * 'b) t -> 'a t * 'b t
      val zip_exn : 'a t -> 'b t -> ('a * 'b) t

      (** Convert a unique array into a unique iarray without copying. This is safe
          because uniqueness of the input guarantees no other reference exists that could
          mutate the underlying array after the conversion.

          Elements are wrapped in [global] because the elements of the original array are
          conceptually mutable fields, which means they have an implicit [global aliased]
          modality on them, and [global] implies [aliased]. *)
      external of_array
        :  ('a array[@local_opt])
        -> ('a global t[@local_opt])
        = "%identity"
    end

    (** Unsafe conversions

        Immutable arrays can be converted to and from mutable arrays, as they can have the
        same representation. One must be careful not to modify the mutable array sharing a
        representation with an immutable array at any time when both are reachable. Doing
        so violates the invariants of the immutable array. The OCaml compiler might rely
        on these invariants when compiling or optimizing code that uses immutable arrays. *)

    val unsafe_to_array__promise_no_mutation : 'a. 'a t -> 'a array

    external unsafe_of_array__promise_no_mutation
      : 'a.
      ('a array[@local_opt]) -> ('a t[@local_opt])
      = "%identity"
  end
end

module type Iarray = sig
  include module type of struct
    include Definitions
  end

  include Public with type 'a t = 'a iarray (** @inline *)

  (**/**)

  module Private : sig
    module Test_unsafe_local_implementations : module type of struct
      let concat = concat
      let concat_map = concat_map
      let concat_mapi = concat_mapi
      let filter = filter
      let filteri = filteri
      let filter_map = filter_map
      let filter_mapi = filter_mapi
      let partition_tf = partition_tf
      let partition_map = partition_map
      let fold_map = Local.fold_map
      let fold_mapi = Local.fold_mapi
    end
  end
  [@@alert
    private_iarray "These bindings are intended only for use in tests of Iarray itself."]
end
