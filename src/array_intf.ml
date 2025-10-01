(** Fixed-length, mutable vector of elements with O(1) [get] and [set] operations. *)

open! Import
module Option = Option0
module List = List0.Constructors

module Definitions = struct
  module type Public = sig
    type 'a t

    include Binary_searchable.S1 with type 'a t := 'a t

    include
      Indexed_container.S1_with_creators__base
      with type 'a t := 'a t
       and type 'a t__float64 = 'a t
       and type 'a t__bits32 = 'a t
       and type 'a t__bits64 = 'a t
       and type 'a t__word = 'a t
       and type 'a t__immediate = 'a t
       and type 'a t__immediate64 = 'a t

    [%%template:
    [@@@kind k = (value, float64, bits32, bits64, word, immediate, immediate64)]

    [%%rederive:
      type nonrec 'a t = 'a t
      [@@kind k]
      [@@deriving compare ~localize, equal ~localize, sexp ~stackify, globalize]]]

    [%%rederive: type nonrec 'a t = 'a t [@@deriving sexp_grammar]]

    include Indexed_container.S1_with_creators with type 'a t := 'a t
    include Invariant.S1 with type 'a t := 'a t

    val%template map : 'a 'b. 'a t -> f:('a -> 'b) -> 'b t
    [@@kind
      k1 = (value, float64, bits32, bits64, word, immediate, immediate64)
      , k2 = (value, float64, bits32, bits64, word, immediate, immediate64)]

    (** Maximum length of a normal array. The maximum length of a float array is
        [max_length/2] on 32-bit machines and [max_length] on 64-bit machines. *)
    val max_length : int

    (*_ Declared as externals so that the compiler skips the caml_apply_X wrapping even
        when compiling without cross library inlining. *)

    external length : 'a. ('a array[@local_opt]) -> int = "%array_length" [@@layout_poly]

    (** [Array.get a n] returns the element number [n] of array [a]. The first element has
        number 0. The last element has number [Array.length a - 1]. You can also write
        [a.(n)] instead of [Array.get a n].

        Raise [Invalid_argument "index out of bounds"] if [n] is outside the range 0 to
        [(Array.length a - 1)]. *)
    external%template get
      : 'a.
      ('a array[@local_opt]) -> (int[@local_opt]) -> 'a
      = "%array_safe_get"
    [@@layout_poly] [@@mode m = (uncontended, shared)]

    (** Like {!get}, but returns [None] instead of raising. *)
    val%template get_opt : 'a. 'a array -> int -> ('a Option.t[@kind k])
    [@@mode c = (uncontended, shared)]
    [@@kind k = (value, float64, bits32, bits64, word, immediate, immediate64)]
    [@@alloc a @ m = (heap_global, stack_local)]

    (** [Array.set a n x] modifies array [a] in place, replacing element number [n] with
        [x]. You can also write [a.(n) <- x] instead of [Array.set a n x].

        Raise [Invalid_argument "index out of bounds"] if [n] is outside the range 0 to
        [Array.length a - 1]. *)
    external set
      : 'a.
      ('a array[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
      = "%array_safe_set"
    [@@layout_poly]

    (** Unsafe version of [get]. Can cause arbitrary behavior when used for an
        out-of-bounds array access. *)
    external%template unsafe_get
      : 'a.
      ('a array[@local_opt]) -> (int[@local_opt]) -> 'a
      = "%array_unsafe_get"
    [@@layout_poly] [@@mode m = (uncontended, shared)]

    (** Unsafe version of [set]. Can cause arbitrary behavior when used for an
        out-of-bounds array access. *)
    external unsafe_set
      : 'a.
      ('a array[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
      = "%array_unsafe_set"
    [@@layout_poly]

    [%%template:
      val create : len:int -> 'a -> 'a array
      [@@ocaml.doc
        {| [create ~len x] creates an array of length [len] with the value [x] populated in
          each element. |}]
      [@@alloc __ = (heap, stack)]]

    val create_local : len:int -> 'a -> 'a array

    val magic_create_uninitialized : len:int -> 'a array
    [@@ocaml.doc
      {| [magic_create_uninitialized ~len] creates an array of length [len]. All elements
          are magically populated as a tagged [0]. |}]

    (** [create_float_uninitialized ~len] creates a float array of length [len] with
        uninitialized elements -- that is, they may contain arbitrary, nondeterministic
        float values. This can be significantly faster than using [create], when unboxed
        float array representations are enabled. *)
    val create_float_uninitialized : len:int -> float t

    (** [init n ~f] creates an array of length [n] with index [i] set to [f i]. *)
    val%template init : int -> f:(int -> 'a) -> 'a array
    [@@alloc __ @ m = (heap_global, stack_local)]

    (** [Array.make_matrix dimx dimy e] returns a two-dimensional array (an array of
        arrays) with first dimension [dimx] and second dimension [dimy]. All the elements
        of this new matrix are initially physically equal to [e]. The element ([x,y]) of a
        matrix [m] is accessed with the notation [m.(x).(y)].

        Raise [Invalid_argument] if [dimx] or [dimy] is negative or greater than
        [Array.max_length].

        If the value of [e] is a floating-point number, then the maximum size is only
        [Array.max_length / 2]. *)
    val make_matrix : dimx:int -> dimy:int -> 'a -> 'a t t

    (** [Array.copy_matrix t] returns a fresh copy of the array of arrays [t]. This is
        typically used when [t] is a matrix created by [Array.make_matrix]. *)
    val copy_matrix : 'a t t -> 'a t t

    [%%template:
    [@@@kind.default k = (value, float64, bits32, bits64, word, immediate, immediate64)]

    (** Like [Array.append], but concatenates a list of arrays. *)
    val concat : 'a. 'a t list -> 'a t

    (** [Array.copy a] returns a copy of [a], that is, a fresh array containing the same
        elements as [a]. *)
    val copy : 'a. 'a t -> 'a t

    (** [Array.fill a ofs len x] modifies the array [a] in place, storing [x] in elements
        number [ofs] to [ofs + len - 1].

        Raise [Invalid_argument "Array.fill"] if [ofs] and [len] do not designate a valid
        subarray of [a]. *)
    val fill : 'a. 'a t -> pos:int -> len:int -> 'a -> unit]

    (** [Array.blit v1 o1 v2 o2 len] copies [len] elements from array [v1], starting at
        element number [o1], to array [v2], starting at element number [o2]. It works
        correctly even if [v1] and [v2] are the same array, and the source and destination
        chunks overlap.

        Raise [Invalid_argument "Array.blit"] if [o1] and [len] do not designate a valid
        subarray of [v1], or if [o2] and [len] do not designate a valid subarray of [v2].

        [int_blit] and [float_blit] provide fast bound-checked blits for immediate data
        types. The unsafe versions do not bound-check the arguments. *)
    include Blit.S1 with type 'a t := 'a t

    val%template unsafe_blit : 'a. ('a array, 'a array) Blit.blit
    [@@kind k = (float64, bits32, bits64, word, immediate, immediate64)]

    val%template sub : 'a. ('a array, 'a array) Blit.sub
    [@@kind k = (float64, bits32, bits64, word, immediate, immediate64)]

    val%template foldi_right : 'a t -> init:'acc -> f:(int -> 'a -> 'acc -> 'acc) -> 'acc
    [@@alloc a @ m = (stack_local, heap_global)]

    (** [folding_map] is a version of [map] that threads an accumulator through calls to
        [f]. *)
    val folding_map : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) -> 'b t

    val folding_mapi : 'a t -> init:'acc -> f:(int -> 'acc -> 'a -> 'acc * 'b) -> 'b t

    (** [Array.fold_map] is a combination of [Array.fold] and [Array.map] that threads an
        accumulator through calls to [f]. *)
    val fold_map : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) -> 'acc * 'b t

    val fold_mapi : 'a t -> init:'acc -> f:(int -> 'acc -> 'a -> 'acc * 'b) -> 'acc * 'b t

    (** [Array.fold_right f a ~init] computes
        [f a.(0) (f a.(1) ( ... (f a.(n-1) init) ...))], where [n] is the length of the
        array [a]. *)
    val%template fold_right : 'a t -> f:('a -> 'acc -> 'acc) -> init:'acc -> 'acc
    [@@mode m = (uncontended, shared)]

    (** All sort functions in this module sort in increasing order by default. *)

    (** [sort] uses constant heap space. [stable_sort] uses linear heap space.

        To sort only part of the array, specify [pos] to be the index to start sorting
        from and [len] indicating how many elements to sort. *)
    val sort : ?pos:int -> ?len:int -> 'a t -> compare:('a -> 'a -> int) -> unit

    val stable_sort : 'a t -> compare:('a -> 'a -> int) -> unit
    val is_sorted : 'a t -> compare:('a -> 'a -> int) -> bool

    (** [is_sorted_strictly xs ~compare] iff [is_sorted xs ~compare] and no two
        consecutive elements in [xs] are equal according to [compare]. *)
    val is_sorted_strictly : 'a t -> compare:('a -> 'a -> int) -> bool

    (** Merges two arrays: assuming that [a1] and [a2] are sorted according to the
        comparison function [compare], [merge a1 a2 ~compare] will return a sorted array
        containing all the elements of [a1] and [a2]. If several elements compare equal,
        the elements of [a1] will be before the elements of [a2]. *)
    val merge : 'a t -> 'a t -> compare:('a -> 'a -> int) -> 'a t

    val partitioni_tf : 'a t -> f:(int -> 'a -> bool) -> 'a t * 'a t
    val cartesian_product : 'a t -> 'b t -> ('a * 'b) t

    (** [transpose] in the sense of a matrix transpose. It returns [None] if the arrays
        are not all the same length. *)
    val transpose : 'a t t -> 'a t t option

    val transpose_exn : 'a t t -> 'a t t

    (** [filter_opt array] returns a new array where [None] entries are omitted and
        [Some x] entries are replaced with [x]. Note that this changes the index at which
        elements will appear. *)
    val filter_opt : 'a option t -> 'a t

    (** Functions with the 2 suffix raise an exception if the lengths of the two given
        arrays aren't the same. *)

    val iter2_exn : 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit
    val map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
    val fold2_exn : 'a t -> 'b t -> init:'acc -> f:('acc -> 'a -> 'b -> 'acc) -> 'acc

    (** Modifies an array in place, applying [f] to every element of the array *)
    val map_inplace : 'a t -> f:('a -> 'a) -> unit

    [%%template:
    [@@@kind.default k1 = (value, float64, bits32, bits64, word, immediate, immediate64)]

    (** [find_exn f t] returns the first [a] in [t] for which [f t.(i)] is true. It raises
        [Stdlib.Not_found] or [Not_found_s] if there is no such [a]. *)
    val find_exn : 'a. 'a t -> f:('a -> bool) -> 'a

    (** [swap arr i j] swaps the value at index [i] with that at index [j]. *)
    val swap : 'a. 'a t -> int -> int -> unit

    (** [rev_inplace t] reverses [t] in place. *)
    val rev_inplace : 'a. 'a t -> unit

    (** [rev t] returns a reversed copy of [t] *)
    val rev : 'a. 'a t -> 'a t

    (** [of_list_rev l] converts from list then reverses in place. *)
    val of_list_rev : 'a. ('a List.t[@kind k1]) -> 'a t

    [@@@kind.default k2 = (value, float64, bits32, bits64, word, immediate, immediate64)]

    (** Returns the first evaluation of [f] that returns [Some]. Raises [Stdlib.Not_found]
        or [Not_found_s] if [f] always returns [None]. *)
    val find_map_exn : 'a 'b. 'a t -> f:('a -> ('b Option.t[@kind k2])) -> 'b

    (** [find_mapi_exn] is like [find_map_exn] but passes the index as an argument. *)
    val find_mapi_exn : 'a 'b. 'a t -> f:(int -> 'a -> ('b Option.t[@kind k2])) -> 'b

    (** [of_list_map l ~f] is the same as [of_list (List.map l ~f)]. *)
    val of_list_map : 'a 'b. ('a List.t[@kind k1]) -> f:('a -> 'b) -> 'b t

    (** [of_list_mapi l ~f] is the same as [of_list (List.mapi l ~f)]. *)
    val of_list_mapi : 'a 'b. ('a List.t[@kind k1]) -> f:(int -> 'a -> 'b) -> 'b t

    (** [of_list_rev_map l ~f] is the same as [of_list (List.rev_map l ~f)]. *)
    val of_list_rev_map : 'a 'b. ('a List.t[@kind k1]) -> f:('a -> 'b) -> 'b t

    (** [of_list_rev_mapi l ~f] is the same as [of_list (List.rev_mapi l ~f)]. *)
    val of_list_rev_mapi : 'a 'b. ('a List.t[@kind k1]) -> f:(int -> 'a -> 'b) -> 'b t

    [%%template:
    [@@@kind.default k1 k2]
    [@@@mode.default m = (global, local)]

    (** [for_all2_exn t1 t2 ~f] fails if [length t1 <> length t2]. *)
    val for_all2_exn : 'a 'b. 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool

    (** [exists2_exn t1 t2 ~f] fails if [length t1 <> length t2]. *)
    val exists2_exn : 'a 'b. 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool]]

    (** [findi_exn t f] returns the first index [i] of [t] for which [f i t.(i)] is true.
        It raises [Stdlib.Not_found] or [Not_found_s] if there is no such element. *)
    val%template findi_exn : 'a. 'a t -> f:(int -> 'a -> bool) -> int * 'a
    [@@kind k = (float64, bits32, bits64, word, immediate, immediate64)]

    (** For backwards compatibility, we return a boxed product for the value-only version
        of [findi_exn] (instead of a [value & value] product) *)
    val findi_exn : 'a t -> f:(int -> 'a -> bool) -> int * 'a

    (** [find_consecutive_duplicate t ~equal] returns the first pair of consecutive
        elements [(a1, a2)] in [t] such that [equal a1 a2]. They are returned in the same
        order as they appear in [t]. *)
    val find_consecutive_duplicate : 'a t -> equal:('a -> 'a -> bool) -> ('a * 'a) option

    (** [reduce f [a1; ...; an]] is [Some (f (... (f (f a1 a2) a3) ...) an)]. Returns
        [None] on the empty array. *)
    val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a option

    val reduce_exn : 'a t -> f:('a -> 'a -> 'a) -> 'a

    (** [permute ?random_state ?pos ?len t] randomly permutes [t] in place.

        To permute only part of the array, specify [pos] to be the index to start
        permuting from and [len] indicating how many elements to permute.

        [permute] side-effects [random_state] by repeated calls to [Random.State.int]. If
        [random_state] is not supplied, [permute] uses [Random.State.default]. *)
    val permute : ?random_state:Random.State.t -> ?pos:int -> ?len:int -> 'a t -> unit

    (** [random_element ?random_state t] is [None] if [t] is empty, else it is [Some x]
        for some [x] chosen uniformly at random from [t].

        [random_element] side-effects [random_state] by calling [Random.State.int]. If
        [random_state] is not supplied, [random_element] uses [Random.State.default]. *)
    val random_element : ?random_state:Random.State.t -> 'a t -> 'a option

    val random_element_exn : ?random_state:Random.State.t -> 'a t -> 'a

    (** [split_n t n] returns a pair of arrays [(first, second)] where [first] contains
        the first [n] elements of [t] and [second] contains the remaining elements.

        - If [n >= length t], returns [(t, [||])].
        - If [n <= 0], returns [([||], t)]. *)
    val split_n : 'a t -> int -> 'a t * 'a t

    (** [chunks_of t ~length] returns an array of arrays whose concatenation is equal to
        the original array. Every array has [length] elements, except for possibly the
        last array, which may have fewer. [chunks_of] raises if [length <= 0]. *)
    val chunks_of : 'a t -> length:int -> 'a t t

    (** [zip] is like [List.zip], but for arrays. *)
    val zip : 'a t -> 'b t -> ('a * 'b) t option

    val zip_exn : 'a t -> 'b t -> ('a * 'b) t

    (** [unzip] is like [List.unzip], but for arrays. *)
    val unzip : ('a * 'b) t -> 'a t * 'b t

    (** [sorted_copy ar compare] returns a shallow copy of [ar] that is sorted. Similar to
        List.sort *)
    val sorted_copy : 'a t -> compare:('a -> 'a -> int) -> 'a t

    val last : 'a t -> 'a [@@deprecated "[since 2024-07] This was renamed to [last_exn]"]
    val last_exn : 'a t -> 'a

    (** The input array is copied internally so that future modifications of it do not
        change the sequence. *)
    val to_sequence : 'a t -> 'a Sequence.t

    (** The input array is shared with the sequence and modifications of it will result in
        modification of the sequence. *)
    val to_sequence_mutable : 'a t -> 'a Sequence.t
  end
end

module type Array = sig
  include module type of struct
    include Definitions
  end

  include Public with type 'a t = 'a array (** @inline *)

  (**/**)

  (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

    https://opensource.janestreet.com/standards/#private-submodules *)
  module Private : sig
    module Sort : sig
      module type Sort = sig
        val sort : 'a t -> compare:('a -> 'a -> int) -> left:int -> right:int -> unit
      end

      module Insertion_sort : Sort
      module Heap_sort : Sort

      module Intro_sort : sig
        include Sort

        val five_element_sort
          :  'a t
          -> compare:('a -> 'a -> int)
          -> int
          -> int
          -> int
          -> int
          -> int
          -> unit
      end
    end

    module%template.portable
      [@kind
        k = (value, immediate, immediate64, value mod external_, value mod external64)] Sorter (S : sig
        type 'a t

        val get : 'a t -> int -> 'a
        val set : 'a t -> int -> 'a -> unit
        val length : 'a t -> int
      end) : sig
      val sort : ?pos:int -> ?len:int -> 'a S.t -> compare:('a -> 'a -> int) -> unit
    end
  end
end
