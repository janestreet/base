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
    type ('a : any mod separable) t := 'a iarray

    (** An alias for [get]. *)
    external ( .:() )
      : ('a : any mod separable).
      ('a t[@local_opt]) -> int -> ('a[@local_opt])
      = "%array_safe_get"
    [@@layout_poly]
  end

  module type Public = sig
    type (+'a : any mod separable) t

    [%%rederive:
      (** Default derivers use [value_or_null] rather than [any]. *)
      type nonrec ('a : value_or_null mod separable) t = 'a t
      [@@deriving
        compare ~localize, equal ~localize, sexp ~stackify, globalize, sexp_grammar]]

    (** We add type aliases for derivers that use other kinds. *)
    type%template nonrec ('a : k mod separable) t = 'a t
    [@@kind k = (base_non_value, value_or_null mod external64)]
    [@@deriving compare ~localize, equal ~localize, sexp ~stackify, globalize]

    [%%rederive: type nonrec 'a t = 'a t [@@deriving hash]]

    (** Standard interfaces *)

    val%template init
      : ('a : k mod separable).
      int -> f:(int -> 'a @ m) @ local -> 'a t @ m
    [@@kind k = (value_or_null, float64)] [@@alloc __ @ m = (heap_global, stack_local)]

    include%template Binary_searchable.S1 [@mode local] with type 'a t := 'a t

    include%template
      Indexed_container.S1_with_creators
      [@kind_set.explicit value_or_null] [@alloc stack]
      with type ('a : any) t := 'a t

    val%template init
      : ('a : k mod separable).
      int -> f:(int -> 'a @ l) @ local -> 'a t @ l
    [@@kind k = (base_or_null, value_or_null mod external64)]
    [@@alloc a @ l = (heap_global, stack_local)]

    val%template map
      : ('a : ki mod separable) ('b : ko mod separable).
      'a t @ mi -> f:('a @ mi -> 'b @ mo) @ local -> 'b t @ mo
    [@@kind ki = (value_or_null, float64), ko = (value_or_null, float64)]
    [@@mode mi = (global, local)]
    [@@alloc a @ mo = (heap_global, stack_local)]

    val%template mapi
      : ('a : ki mod separable) ('b : ko mod separable).
      'a t @ mi -> f:(int -> 'a @ mi -> 'b @ mo) @ local -> 'b t @ mo
    [@@kind ki = (value_or_null, float64), ko = (value_or_null, float64)]
    [@@mode mi = (global, local)]
    [@@alloc a @ mo = (heap_global, stack_local)]

    val%template iteri
      : ('a : ki mod separable).
      'a t @ mi -> f:(int -> 'a @ mi -> unit) @ local -> unit
    [@@kind ki = value_or_null] [@@mode mi = (global, local)]

    val%template iter
      : ('a : ki mod separable).
      'a t @ mi -> f:('a @ mi -> unit) @ local -> unit
    [@@kind ki = value_or_null] [@@mode mi = (global, local)]

    val%template for_all
      : ('a : ki mod separable).
      'a t @ mi -> f:('a @ mi -> bool) @ local -> bool
    [@@kind ki = value_or_null] [@@mode mi = (global, local)]

    (** Like [find] but returning [or_null] instead of [option]. *)
    val%template find_or_null
      : ('a : value).
      'a t @ m -> f:('a @ m -> bool) @ local -> 'a or_null @ m
    [@@mode m = (global, local)]

    (** Like [findi] but returning [or_null] instead of [option]. *)
    val%template findi_or_null
      : ('a : value).
      'a t @ m -> f:(int -> 'a @ m -> bool) @ local -> (int * 'a) or_null @ m
    [@@mode m = (global, local)]

    [%%template:
    [@@@mode.default
      li = (global, local), lo = (global, local), u = (unique, aliased), o = (many, once)]

    (** Like [find_map] but with a function returning [or_null] instead of [option]. *)
    val find_map_or_null
      : ('a : value) ('b : value).
      'a t @ li -> f:('a @ li -> 'b or_null @ lo o u) @ local -> 'b or_null @ lo o u

    (** Like [find_mapi] but with a function returning [or_null] instead of [option]. *)
    val find_mapi_or_null
      : ('a : value) ('b : value).
      'a t @ li
      -> f:(int -> 'a @ li -> 'b or_null @ lo o u) @ local
      -> 'b or_null @ lo o u]

    val%template of_array : ('a : k mod separable). 'a array -> 'a t
    [@@kind k = (base_or_null, value_or_null mod external64)]

    val%template to_array : ('a : k mod separable). 'a t -> 'a array
    [@@kind k = (base_or_null, value_or_null mod external64)]

    val%template to_list
      : ('a : k).
      'a t @ m -> ('a List.Constructors.t[@kind k or value_or_null]) @ m
    [@@kind k = base_non_value] [@@alloc __ @ m = (heap_global, stack_local)]

    include Invariant.S1 with type 'a t := 'a t

    (** Operators *)

    module O : Operators
    include Operators

    (** Indexing *)

    external length
      : ('a : any mod separable).
      ('a t[@local_opt]) @ immutable -> int
      = "%array_length"
    [@@layout_poly]

    [%%template:
    [@@@mode.default c = (uncontended, shared, contended), p = (portable, nonportable)]

    external get
      : ('a : any mod separable).
      ('a t[@local_opt]) @ c p -> int -> ('a[@local_opt]) @ c p
      = "%array_safe_get"
    [@@layout_poly]

    val%template get_opt : 'a t @ c m p -> int -> 'a option @ c m p
    [@@mode c] [@@alloc __ @ m = (heap_global, stack_local)]

    external unsafe_get
      : ('a : any mod separable).
      ('a t[@local_opt]) @ c p -> int -> ('a[@local_opt]) @ c p
      = "%array_unsafe_get"
    [@@layout_poly]]

    include sig
      [@@@implicit_kind: ('a : value_or_null mod separable)]

      val last_exn : 'a t -> 'a

      (** Functional update *)

      val set : 'a t -> int -> 'a -> 'a t
      val update : 'a t -> int -> f:('a -> 'a) -> 'a t

      (** Constructors *)

      val empty : _ t
      val singleton : 'a -> 'a t
      val create : len:int -> 'a -> mutate:('a array @ local -> unit) @ local -> 'a iarray

      (** Conversions *)

      val of_sequence : 'a Sequence.t -> 'a t
      val to_sequence : 'a t -> 'a Sequence.t
      val of_list_rev : ('a : value_or_null mod separable). 'a list -> 'a t
      val of_list_map : 'a list -> f:('a -> 'b) @ local -> 'b t
      val of_list_mapi : 'a list -> f:(int -> 'a -> 'b) @ local -> 'b t
      val of_list_rev_map : 'a list -> f:('a -> 'b) @ local -> 'b t

      (** Subsequences *)

      val prefix : 'a t -> len:int -> 'a t
      val suffix : 'a t -> len:int -> 'a t
      val drop_prefix : 'a t -> len:int -> 'a t
      val drop_suffix : 'a t -> len:int -> 'a t
      val group : 'a t -> break:('a -> 'a -> bool) @ local -> 'a t t

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
      val sort : 'a t -> compare:('a -> 'a -> int) @ local -> 'a t
      val stable_sort : 'b t -> compare:('b -> 'b -> int) -> 'b t
      val dedup_and_sort : 'a t -> compare:('a -> 'a -> int) @ local -> 'a t
      val sort_and_group : 'a t -> compare:('a -> 'a -> int) @ local -> 'a t t
      val is_sorted : 'a t -> compare:('a -> 'a -> int) @ local -> bool
      val is_sorted_strictly : 'a t -> compare:('a -> 'a -> int) @ local -> bool
      val find_a_dup : 'a t -> compare:('a -> 'a -> int) @ local -> 'a option

      (** Combining elements *)

      val reduce : 'a t -> f:('a -> 'a -> 'a) @ local -> 'a option
      val reduce_exn : 'a t -> f:('a -> 'a -> 'a) @ local -> 'a
      val combine_errors : 'a Or_error.t t -> 'a t Or_error.t
      val combine_errors_unit : unit Or_error.t t -> unit Or_error.t

      [%%template:
      [@@@kind.default ka = value_or_null, kacc = base_or_null]
      [@@@kind ka' = ka mod separable]
      [@@@mode.default li = (global, local), lo = (global, local)]

      val fold
        : ('a : ka') ('acc : kacc).
        'a t @ li
        -> init:'acc @ lo
        -> f:('acc @ lo -> 'a @ li -> 'acc @ lo) @ local
        -> 'acc @ lo

      val foldi
        : ('a : ka') ('acc : kacc).
        'a t @ li
        -> init:'acc @ lo
        -> f:(int -> 'acc @ lo -> 'a @ li -> 'acc @ lo) @ local
        -> 'acc @ lo

      val fold_right
        : ('a : ka') ('acc : kacc).
        'a t @ li
        -> init:'acc @ lo
        -> f:('a @ li -> 'acc @ lo -> 'acc @ lo) @ local
        -> 'acc @ lo]

      val fold_map
        :  'a t
        -> init:'acc
        -> f:('acc -> 'a -> 'acc * 'b) @ local
        -> 'acc * 'b t

      val fold_mapi
        :  'a t
        -> init:'acc
        -> f:(int -> 'acc -> 'a -> 'acc * 'b) @ local
        -> 'acc * 'b t

      (** Multiple arrays *)

      [%%template:
      [@@@alloc.default a @ l = (heap_global, stack_local)]

      val zip : 'a t @ l -> 'b t @ l -> ('a * 'b) t option @ l [@@zero_alloc_if_stack a]
      val zip_exn : 'a t @ l -> 'b t @ l -> ('a * 'b) t @ l [@@zero_alloc_if_stack a]
      val unzip : ('a * 'b) t @ l -> 'a t * 'b t @ l [@@zero_alloc_if_stack a]]

      val map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) @ local -> 'c t
      val iter2_exn : 'a t -> 'b t -> f:('a -> 'b -> unit) @ local -> unit
      val cartesian_product : 'a t -> 'b t -> ('a * 'b) t

      (** Random elements *)

      val random_element : ?random_state:Random.State.t @ local -> 'a t -> 'a option
      val random_element_exn : ?random_state:Random.State.t @ local -> 'a t -> 'a

      (** Blit operations *)

      val sub : 'a t -> pos:int -> len:int -> 'a t
      val subo : ?pos:int -> ?len:int -> 'a t -> 'a t

      module Of_array : sig
        val sub : 'a array @ local -> pos:int -> len:int -> 'a t
        val subo : ?pos:int -> ?len:int -> 'a array @ local -> 'a t
      end

      module To_array : sig
        val sub : 'a t -> pos:int -> len:int -> 'a array
        val subo : ?pos:int -> ?len:int -> 'a t -> 'a array

        val blito
          :  src:'a t
          -> ?src_pos:int
          -> ?src_len:int
          -> dst:'a array @ local
          -> ?dst_pos:int
          -> unit
          -> unit

        val blit
          :  src:'a t
          -> src_pos:int
          -> dst:'a array @ local
          -> dst_pos:int
          -> len:int
          -> unit

        val unsafe_blit
          :  src:'a t
          -> src_pos:int
          -> dst:'a array @ local
          -> dst_pos:int
          -> len:int
          -> unit
      end
    end

    (** Operations for local iarrays. *)
    module Local : sig
      (*_ Lifted from [Container_with_local]. All instantiate [Container] functions *)

      [@@@implicit_kind: ('a : value_or_null mod separable)]
      [@@@implicit_kind: ('b : value_or_null mod separable)]
      [@@@implicit_kind: ('c : value_or_null mod separable)]

      val length : 'a t @ local -> int
      val is_empty : 'a t @ local -> bool

      val mem
        :  'a t @ local
        -> 'a @ local
        -> equal:('a @ local -> 'a @ local -> bool) @ local
        -> bool

      val iter : 'a t @ local -> f:('a @ local -> unit) @ local -> unit

      val fold_result
        :  'a t @ local
        -> init:'acc @ local
        -> f:('acc @ local -> 'a @ local -> ('acc, 'e) Result.t @ local) @ local
        -> ('acc, 'e) Result.t @ local

      val fold_until
        :  'a t @ local
        -> init:'acc @ local
        -> f:
             ('acc @ local
              -> 'a @ local
              -> ('acc, 'final) Container.Continue_or_stop.t @ local)
           @ local
        -> finish:('acc @ local -> 'final @ local) @ local
        -> 'final @ local

      val exists : 'a t @ local -> f:('a @ local -> bool) @ local -> bool
      val for_all : 'a t @ local -> f:('a @ local -> bool) @ local -> bool
      val count : 'a t @ local -> f:('a @ local -> bool) @ local -> int

      val%template sum
        :  ((module Container.Summable with type t = 'sum)[@mode local])
        -> 'a t @ local
        -> f:('a @ local -> 'sum @ local) @ local
        -> 'sum @ local

      val find : 'a t @ local -> f:('a @ local -> bool) @ local -> 'a option @ local

      val find_map
        :  'a t @ local
        -> f:('a @ local -> 'b option @ local) @ local
        -> 'b option @ local

      val to_list : 'a t @ local -> 'a list @ local

      val min_elt
        :  'a t @ local
        -> compare:('a @ local -> 'a @ local -> int) @ local
        -> 'a option @ local

      val max_elt
        :  'a t @ local
        -> compare:('a @ local -> 'a @ local -> int) @ local
        -> 'a option @ local

      val of_list : 'a list @ local -> 'a t @ local
      val append : 'a t @ local -> 'a t @ local -> 'a t @ local
      val concat : 'a t t @ local -> 'a t @ local
      val map : 'a t @ local -> f:('a @ local -> 'b @ local) @ local -> 'b t @ local
      val map_to_global : 'a t @ local -> f:('a @ local -> 'b) @ local -> 'b t
      val map_of_global : 'a t -> f:('a -> 'b @ local) @ local -> 'b t @ local
      val filter : 'a t @ local -> f:('a @ local -> bool) @ local -> 'a t @ local

      val filter_map
        :  'a t @ local
        -> f:('a @ local -> 'b option @ local) @ local
        -> 'b t @ local

      val concat_map
        :  'a t @ local
        -> f:('a @ local -> 'b t @ local) @ local
        -> 'b t @ local

      val partition_tf
        :  'a t @ local
        -> f:('a @ local -> bool) @ local
        -> 'a t * 'a t @ local

      val partition_map
        :  'a t @ local
        -> f:('a @ local -> ('b, 'c) Either.t @ local) @ local
        -> 'b t * 'c t @ local

      val iteri : 'a t @ local -> f:(int -> 'a @ local -> unit) @ local -> unit
      val existsi : 'a t @ local -> f:(int -> 'a @ local -> bool) @ local -> bool
      val for_alli : 'a t @ local -> f:(int -> 'a @ local -> bool) @ local -> bool
      val counti : 'a t @ local -> f:(int -> 'a @ local -> bool) @ local -> int

      val findi
        :  'a t @ local
        -> f:(int -> 'a @ local -> bool) @ local
        -> (int * 'a) option @ local

      val find_mapi
        :  'a t @ local
        -> f:(int -> 'a @ local -> 'b option @ local) @ local
        -> 'b option @ local

      val partitioni_tf
        :  'a t @ local
        -> f:(int -> 'a @ local -> bool) @ local
        -> 'a t * 'a t @ local

      val partition_mapi
        :  'a t @ local
        -> f:(int -> 'a @ local -> ('b, 'c) Either.t @ local) @ local
        -> 'b t * 'c t @ local

      val mapi
        :  'a t @ local
        -> f:(int -> 'a @ local -> 'b @ local) @ local
        -> 'b t @ local

      val mapi_to_global : 'a t @ local -> f:(int -> 'a @ local -> 'b) @ local -> 'b t
      val mapi_of_global : 'a t -> f:(int -> 'a -> 'b @ local) @ local -> 'b t @ local
      val filteri : 'a t @ local -> f:(int -> 'a @ local -> bool) @ local -> 'a t @ local

      val filter_mapi
        :  'a t @ local
        -> f:(int -> 'a @ local -> 'b option @ local) @ local
        -> 'b t @ local

      val concat_mapi
        :  'a t @ local
        -> f:(int -> 'a @ local -> 'b t @ local) @ local
        -> 'b t @ local

      (*_ [Iarray]-specific *)

      val last_exn : 'a t @ local -> 'a @ local

      val to_array_of_immediates
        : ('i : immediate64_or_null).
        'i t @ local -> 'i array @ local
      [@@zero_alloc]

      val sort_immediates
        : ('i : immediate64_or_null).
        'i t @ local -> compare:('i -> 'i -> int) @ local -> 'i t @ local

      module Let_syntax : sig
        val return : 'a @ local -> 'a t @ local

        module Let_syntax : sig
          val return : 'a @ local -> 'a t @ local

          val bind
            :  'a t @ local
            -> f:('a @ local -> 'b t @ local) @ local
            -> 'b t @ local

          val map : 'a t @ local -> f:('a @ local -> 'b @ local) @ local -> 'b t @ local
          val both : 'a t @ local -> 'b t @ local -> ('a * 'b) t @ local

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
      val init : int -> f:(int -> 'a @ local) @ local -> 'a t @ local

      (** [init_with_globals] avoids the extra O(n) function-call stack space of [init],
          because the array contents are guaranteed not to be stored in the local data
          stack. Therefore, they do not need to be allocated before the array. *)
      val init_with_globals : int -> f:(int -> 'a) @ local -> 'a t @ local

      val singleton : 'a @ local -> 'a t @ local

      val create
        :  len:int
        -> 'a
        -> mutate:('a array @ local -> unit) @ local
        -> 'a iarray @ local

      val prefix : 'a t @ local -> len:int -> 'a t @ local
      val suffix : 'a t @ local -> len:int -> 'a t @ local
      val drop_prefix : 'a t @ local -> len:int -> 'a t @ local
      val drop_suffix : 'a t @ local -> len:int -> 'a t @ local
      val rev : 'a t @ local -> 'a t @ local
      val sub : 'a t @ local -> pos:int -> len:int -> 'a t @ local
      val subo : ?pos:int @ local -> ?len:int @ local -> 'a t @ local -> 'a t @ local

      (** [of_list_*] functions require temporary call-stack space proportional to the
          length of the input list, like [init]. *)

      val of_list_rev : 'a list @ local -> 'a t @ local

      val of_list_map
        :  'a list @ local
        -> f:('a @ local -> 'b @ local) @ local
        -> 'b t @ local

      val of_list_mapi
        :  'a list @ local
        -> f:(int @ local -> 'a @ local -> 'b @ local) @ local
        -> 'b t @ local

      val of_list_rev_map
        :  'a list @ local
        -> f:('a @ local -> 'b @ local) @ local
        -> 'b t @ local

      val iter2_exn
        :  'a t @ local
        -> 'b t @ local
        -> f:('a @ local -> 'b @ local -> unit) @ local
        -> unit

      val map2_exn
        :  'a t @ local
        -> 'b t @ local
        -> f:('a @ local -> 'b @ local -> 'c @ local) @ local
        -> 'c t @ local

      val cartesian_product : 'a t @ local -> 'b t @ local -> ('a * 'b) t @ local

      [%%template:
      [@@@kind.default ka = value_or_null, kacc = base_or_null]
      [@@@kind ka' = ka mod separable]

      val fold
        : ('a : ka') ('acc : kacc).
        'a t @ local
        -> init:'acc @ local
        -> f:('acc @ local -> 'a @ local -> 'acc @ local) @ local
        -> 'acc @ local

      val foldi
        : ('a : ka') ('acc : kacc).
        'a t @ local
        -> init:'acc @ local
        -> f:(int -> 'acc @ local -> 'a @ local -> 'acc @ local) @ local
        -> 'acc @ local

      val fold_right
        : ('a : ka') ('acc : kacc).
        'a t @ local
        -> init:'acc @ local
        -> f:('a @ local -> 'acc @ local -> 'acc @ local) @ local
        -> 'acc @ local]

      val fold_map
        :  'a t @ local
        -> init:'acc @ local
        -> f:('acc @ local -> 'a @ local -> 'acc * 'b @ local) @ local
        -> 'acc * 'b t @ local

      val fold_mapi
        :  'a t @ local
        -> init:'acc @ local
        -> f:(int -> 'acc @ local -> 'a @ local -> 'acc * 'b @ local) @ local
        -> 'acc * 'b t @ local
    end

    (** Operations for unique iarrays *)
    module Unique : sig
      val init : int -> f:(int -> 'a @ unique) -> 'a t @ unique
      val map : 'a t @ unique -> f:('a @ unique -> 'b @ unique) @ local -> 'b t @ unique

      val mapi
        :  'a t @ unique
        -> f:(int -> 'a @ unique -> 'b @ unique) @ local
        -> 'b t @ unique

      val iter : 'a t @ unique -> f:('a @ unique -> unit) @ local -> unit
      val iteri : 'a t @ unique -> f:(int -> 'a @ unique -> unit) @ local -> unit
      val unzip : ('a * 'b) t @ unique -> 'a t * 'b t @ unique
      val zip_exn : 'a t @ unique -> 'b t @ unique -> ('a * 'b) t @ unique

      (** Convert a unique array into a unique iarray without copying. This is safe
          because uniqueness of the input guarantees no other reference exists that could
          mutate the underlying array after the conversion.

          Elements are wrapped in [global] because the elements of the original array are
          conceptually mutable fields, which means they have an implicit [global aliased]
          modality on them, and [global] implies [aliased]. *)
      external of_array
        :  ('a array[@local_opt]) @ unique
        -> ('a global t[@local_opt]) @ unique
        = "%array_to_iarray"
    end

    (** Unsafe conversions

        Immutable arrays can be converted to and from mutable arrays, as they can have the
        same representation. One must be careful not to modify the mutable array sharing a
        representation with an immutable array at any time when both are reachable. Doing
        so violates the invariants of the immutable array. The OCaml compiler might rely
        on these invariants when compiling or optimizing code that uses immutable arrays. *)

    [%%template:
    [@@@mode.default c = (uncontended, shared)]

    val unsafe_to_array__promise_no_mutation
      : ('a : any mod separable).
      'a t @ c -> 'a array @ c

    external unsafe_of_array__promise_no_mutation
      : ('a : any mod separable).
      ('a array[@local_opt]) @ c -> ('a t[@local_opt]) @ c
      = "%array_to_iarray"]
  end
end

module type Iarray = sig @@ portable
  include module type of struct
    include Definitions
  end

  include Public with type ('a : any mod separable) t = 'a iarray (** @inline *)

  (**/**)

  module Private : sig
    module%template Test_unsafe_local_implementations : module type of struct
      let concat = (concat [@alloc stack])
      let concat_map = (concat_map [@mode local] [@alloc stack])
      let concat_mapi = (concat_mapi [@mode local] [@alloc stack])
      let filter = (filter [@alloc stack])
      let filteri = (filteri [@alloc stack])
      let filter_map = (filter_map [@mode local] [@alloc stack])
      let filter_mapi = (filter_mapi [@mode local] [@alloc stack])
      let partition_tf = (partition_tf [@alloc stack])
      let partition_map = (partition_map [@mode local] [@alloc stack])
      let fold_map = Local.fold_map
      let fold_mapi = Local.fold_mapi
    end
  end
  [@@alert
    private_iarray "These bindings are intended only for use in tests of Iarray itself."]
end
