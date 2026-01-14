open! Import
module Sexp = Sexp0

module Definitions = struct
  module Or_duplicate = struct
    type 'a t =
      [ `Ok of 'a
      | `Duplicate
      ]
    [@@deriving compare ~localize, equal ~localize, sexp_of]
  end

  module Without_comparator = struct
    type ('key, 'cmp, 'z) t = 'z
  end

  module With_comparator = struct
    type ('key, 'cmp, 'z) t = comparator:('key, 'cmp) Comparator.t -> 'z
  end

  module With_first_class_module = struct
    type ('key, 'cmp, 'z) t = ('key, 'cmp) Comparator.Module.t -> 'z
  end

  module Symmetric_diff_element = struct
    type ('k, 'v) t = 'k * [ `Left of 'v | `Right of 'v | `Unequal of 'v * 'v ]
    [@@deriving compare ~localize, equal ~localize, sexp, sexp_grammar]
  end

  module Merge_element = struct
    type ('left, 'right) t =
      [ `Left of 'left
      | `Right of 'right
      | `Both of 'left * 'right
      ]
    [@@deriving compare ~localize, equal ~localize, sexp_of]
  end

  module When_unmatched = struct
    (** Used for [merge_by_case] for unmatched keys in one map or the other. Using a more
        specific case, like [Drop] instead of [Filter (fun ... -> false)] or [Map f]
        instead of [Filter_map (fun ... -> Some (f ...))], allows a more optimized
        implementation. [Drop] and [Keep], specifically, allow some entire non-overlapping
        subtrees to be handled without traversal. *)
    type ('k, 'a, 'b) t =
      | Drop
      | Keep : (_, 'a, 'a) t
      | Map of (key:'k -> (data:'a -> 'b) @ local)
      | Filter : (key:'k -> (data:'a -> bool) @ local) -> ('k, 'a, 'a) t
      | Filter_map of (key:'k -> (data:'a -> 'b option) @ local)
    [@@deriving sexp_of]
  end

  module When_matched = struct
    (** Used for [merge_by_case] for matching keys in two merged maps. Using a more
        specific case, like [Drop] instead of [Filter_map (fun ~key:_ _ _ -> None)] or
        [Map f] instead of [Filter_map (fun ... -> Some (f ...))], allows a more optimized
        implementation. *)
    type ('k, 'a, 'b, 'c) t =
      | Drop
      | Keep_first : (_, 'a, _, 'a) t
      | Keep_second : (_, _, 'a, 'a) t
      | Map of (key:'k -> ('a -> 'b -> 'c) @ local)
      | Filter_first : (key:'k -> ('a -> 'b -> bool) @ local) -> ('k, 'a, 'b, 'a) t
      | Filter_second : (key:'k -> ('a -> 'b -> bool) @ local) -> ('k, 'a, 'b, 'b) t
      | Filter_map of (key:'k -> ('a -> 'b -> 'c option) @ local)
    [@@deriving sexp_of]
  end

  (** @canonical Base.Map.Continue_or_stop *)
  module Continue_or_stop = struct
    type t =
      | Continue
      | Stop
    [@@deriving compare ~localize, enumerate, equal ~localize, sexp_of]
  end

  (** @canonical Base.Map.Finished_or_unfinished *)
  module Finished_or_unfinished = struct
    type t =
      | Finished
      | Unfinished
    [@@deriving compare ~localize, enumerate, equal ~localize, sexp_of]
  end

  module type Accessors_generic = sig
    type ('a, 'b, 'cmp) t
    type ('a, 'b, 'cmp) tree
    type 'a key
    type 'cmp cmp
    type ('a, 'cmp, 'z) access_options

    (** @inline *)
    include
      Dictionary_immutable.Accessors
      with type 'key key := 'key key
       and type ('key, 'data, 'cmp) t := ('key, 'data, 'cmp) t
       and type ('fn, 'key, _, 'cmp) accessor := ('key, 'cmp, 'fn) access_options

    val invariants : ('k, 'cmp, ('k, 'v, 'cmp) t -> bool) access_options

    val iteri_until
      :  ('k, 'v, _) t
      -> f:(key:'k key -> data:'v -> Continue_or_stop.t) @ local
      -> Finished_or_unfinished.t

    val iter2
      : ( 'k
          , 'cmp
          , ('k, 'v1, 'cmp) t
            -> ('k, 'v2, 'cmp) t
            -> f:(key:'k key -> data:('v1, 'v2) Merge_element.t -> unit) @ local
            -> unit )
          access_options

    val fold_right
      :  ('k, 'v, _) t
      -> init:'acc
      -> f:(key:'k key -> data:'v -> 'acc -> 'acc) @ local
      -> 'acc

    val fold2
      : ( 'k
          , 'cmp
          , ('k, 'v1, 'cmp) t
            -> ('k, 'v2, 'cmp) t
            -> init:'acc
            -> f:(key:'k key -> data:('v1, 'v2) Merge_element.t -> 'acc -> 'acc) @ local
            -> 'acc )
          access_options

    val%template compare_direct
      : ( 'k
          , 'cmp
          , ('v @ m -> 'v @ m -> int)
            -> ('k, 'v, 'cmp) t @ m
            -> ('k, 'v, 'cmp) t @ m
            -> int )
          access_options
    [@@mode m = (local, global)]

    val%template equal
      : ( 'k
          , 'cmp
          , ('v @ m -> 'v @ m -> bool)
            -> ('k, 'v, 'cmp) t @ m
            -> ('k, 'v, 'cmp) t @ m
            -> bool )
          access_options
    [@@mode m = (local, global)]

    val to_alist
      :  ?key_order:[ `Increasing | `Decreasing ]
      -> ('k, 'v, _) t
      -> ('k key * 'v) list

    val fold_range_inclusive
      : ( 'k
          , 'cmp
          , ('k, 'v, 'cmp) t
            -> min:'k key
            -> max:'k key
            -> init:'acc
            -> f:(key:'k key -> data:'v -> 'acc -> 'acc) @ local
            -> 'acc )
          access_options

    val range_to_alist
      : ( 'k
          , 'cmp
          , ('k, 'v, 'cmp) t -> min:'k key -> max:'k key -> ('k key * 'v) list )
          access_options

    val closest_key
      : ( 'k
          , 'cmp
          , ('k, 'v, 'cmp) t
            -> [ `Greater_or_equal_to | `Greater_than | `Less_or_equal_to | `Less_than ]
            -> 'k key
            -> ('k key * 'v) option )
          access_options

    val nth : ('k, 'v, 'cmp) t -> int -> ('k key * 'v) option
    val nth_exn : ('k, 'v, 'cmp) t -> int -> 'k key * 'v
    val rank : ('k, 'cmp, ('k, _, 'cmp) t -> 'k key -> int option) access_options
    val count_lt : ('k, 'cmp, ('k, _, 'cmp) t -> 'k key -> int) access_options
    val count_le : ('k, 'cmp, ('k, _, 'cmp) t -> 'k key -> int) access_options
    val count_gt : ('k, 'cmp, ('k, _, 'cmp) t -> 'k key -> int) access_options
    val count_ge : ('k, 'cmp, ('k, _, 'cmp) t -> 'k key -> int) access_options
    val to_tree : ('k, 'v, 'cmp) t -> ('k key, 'v, 'cmp) tree

    val to_sequence
      : ( 'k
          , 'cmp
          , ?order:[ `Increasing_key | `Decreasing_key ]
            -> ?keys_greater_or_equal_to:'k key
            -> ?keys_less_or_equal_to:'k key
            -> ('k, 'v, 'cmp) t
            -> ('k key * 'v) Sequence.t )
          access_options

    val binary_search
      : ( 'k
          , 'cmp
          , ('k, 'v, 'cmp) t
            -> compare:(key:'k key -> data:'v -> 'key -> int) @ local
            -> Binary_searchable.Which_target_by_key.t
            -> 'key
            -> ('k key * 'v) option )
          access_options

    val binary_search_segmented
      : ( 'k
          , 'cmp
          , ('k, 'v, 'cmp) t
            -> segment_of:(key:'k key -> data:'v -> [ `Left | `Right ]) @ local
            -> Binary_searchable.Which_target_by_segment.t
            -> ('k key * 'v) option )
          access_options

    val binary_search_subrange
      : ( 'k
          , 'cmp
          , ('k, 'v, 'cmp) t
            -> compare:(key:'k key -> data:'v -> 'bound -> int) @ local
            -> lower_bound:'bound Maybe_bound.t
            -> upper_bound:'bound Maybe_bound.t
            -> ('k, 'v, 'cmp) t )
          access_options
  end

  module type Transformers_generic = sig
    type ('a, 'b, 'cmp) t
    type ('a, 'b, 'cmp) tree
    type 'a key
    type 'cmp cmp
    type ('a, 'cmp, 'z) access_options

    (** @inline *)
    include
      Dictionary_immutable.Transformers
      with type 'key key := 'key key
       and type ('key, 'data, 'cmp) t := ('key, 'data, 'cmp) t
       and type ('fn, 'key, _, 'cmp) transformer := ('key, 'cmp, 'fn) access_options

    val split
      : ( 'k
          , 'cmp
          , ('k, 'v, 'cmp) t
            -> 'k key
            -> ('k, 'v, 'cmp) t * ('k key * 'v) option * ('k, 'v, 'cmp) t )
          access_options

    val split_le_gt
      : ( 'k
          , 'cmp
          , ('k, 'v, 'cmp) t -> 'k key -> ('k, 'v, 'cmp) t * ('k, 'v, 'cmp) t )
          access_options

    val split_lt_ge
      : ( 'k
          , 'cmp
          , ('k, 'v, 'cmp) t -> 'k key -> ('k, 'v, 'cmp) t * ('k, 'v, 'cmp) t )
          access_options

    val split_n : ('k, 'v, 'cmp) t -> int -> ('k, 'v, 'cmp) t * ('k, 'v, 'cmp) t

    val append
      : ( 'k
          , 'cmp
          , lower_part:('k, 'v, 'cmp) t
            -> upper_part:('k, 'v, 'cmp) t
            -> [ `Ok of ('k, 'v, 'cmp) t | `Overlapping_key_ranges ] )
          access_options

    val subrange
      : ( 'k
          , 'cmp
          , ('k, 'v, 'cmp) t
            -> lower_bound:'k key Maybe_bound.t
            -> upper_bound:'k key Maybe_bound.t
            -> ('k, 'v, 'cmp) t )
          access_options

    val merge
      : ( 'k
          , 'cmp
          , ('k, 'v1, 'cmp) t
            -> ('k, 'v2, 'cmp) t
            -> f:(key:'k key -> ('v1, 'v2) Merge_element.t -> 'v3 option) @ local
            -> ('k, 'v3, 'cmp) t )
          access_options

    val merge_disjoint_exn
      : ( 'k
          , 'cmp
          , ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t )
          access_options

    val merge_skewed
      : ( 'k
          , 'cmp
          , ('k, 'v, 'cmp) t
            -> ('k, 'v, 'cmp) t
            -> combine:(key:'k key -> 'v -> 'v -> 'v) @ local
            -> ('k, 'v, 'cmp) t )
          access_options

    val merge_by_case
      : ( 'k
          , 'cmp
          , ('k, 'v1, 'cmp) t
            -> ('k, 'v2, 'cmp) t
            -> first:('k key, 'v1, 'v3) When_unmatched.t @ local
            -> second:('k key, 'v2, 'v3) When_unmatched.t @ local
            -> both:('k key, 'v1, 'v2, 'v3) When_matched.t @ local
            -> ('k, 'v3, 'cmp) t )
          access_options

    module%template.portable Make_applicative_traversals
        (A : Applicative.Lazy_applicative) : sig
      val mapi
        :  ('k, 'v1, 'cmp) t
        -> f:(key:'k key -> data:'v1 -> 'v2 A.t)
        -> ('k, 'v2, 'cmp) t A.t

      val filter_mapi
        :  ('k, 'v1, 'cmp) t
        -> f:(key:'k key -> data:'v1 -> 'v2 option A.t)
        -> ('k, 'v2, 'cmp) t A.t
    end
  end

  module type Creators_generic = sig
    type ('k, 'v, 'cmp) t
    type ('k, 'v, 'cmp) map
    type ('k, 'v, 'cmp) tree
    type 'k key
    type 'k map_key
    type ('a, 'cmp, 'z) create_options
    type ('a, 'cmp, 'z) access_options
    type 'cmp cmp

    (** @inline *)
    include
      Dictionary_immutable.Creators
      with type 'key key := 'key key
       and type ('key, 'data, 'cmp) t := ('key, 'data, 'cmp) t
       and type ('fn, 'key, _, 'cmp) creator := ('key, 'cmp, 'fn) create_options

    val map_keys
      : ( 'k2
          , 'cmp2
          , ('k1, 'v, 'cmp1) map
            -> f:('k1 map_key -> 'k2 key) @ local
            -> [ `Ok of ('k2, 'v, 'cmp2) t | `Duplicate_key of 'k2 key ] )
          create_options

    val map_keys_exn
      : ( 'k2
          , 'cmp2
          , ('k1, 'v, 'cmp1) map
            -> f:('k1 map_key -> 'k2 key) @ local
            -> ('k2, 'v, 'cmp2) t )
          create_options

    val transpose_keys
      : ( 'k1
          , 'cmp1
          , ( 'k2
              , 'cmp2
              , ('k1, ('k2, 'a, 'cmp2) t, 'cmp1) map
                -> ('k2, ('k1, 'a, 'cmp1) map, 'cmp2) t )
              create_options )
          access_options

    val of_sorted_array
      : ('k, 'cmp, ('k key * 'v) array -> ('k, 'v, 'cmp) t Or_error.t) create_options

    val of_sorted_array_unchecked
      : ('k, 'cmp, ('k key * 'v) array -> ('k, 'v, 'cmp) t) create_options

    val of_increasing_iterator_unchecked
      : ( 'k
          , 'cmp
          , len:int -> f:(int -> 'k key * 'v) @ local -> ('k, 'v, 'cmp) t )
          create_options

    val of_increasing_sequence
      : ('k, 'cmp, ('k key * 'v) Sequence.t -> ('k, 'v, 'cmp) t Or_error.t) create_options

    val of_list_with_key_fold
      : ( 'k
          , 'cmp
          , 'v list
            -> get_key:('v -> 'k key) @ local
            -> init:'acc
            -> f:('acc -> 'v -> 'acc)
            -> ('k, 'acc, 'cmp) t )
          create_options

    val of_list_with_key_reduce
      : ( 'k
          , 'cmp
          , 'v list
            -> get_key:('v -> 'k key) @ local
            -> f:('v -> 'v -> 'v)
            -> ('k, 'v, 'cmp) t )
          create_options

    val of_tree : ('k, 'cmp, ('k key, 'v, 'cmp) tree -> ('k, 'v, 'cmp) t) create_options
  end

  module type Creators_and_accessors_and_transformers_generic = sig
    type ('a, 'b, 'c) t
    type ('a, 'b, 'c) map
    type ('a, 'b, 'c) tree
    type 'a key
    type 'a map_key
    type 'a cmp
    type ('a, 'b, 'c) create_options
    type ('a, 'b, 'c) access_options

    include
      Creators_generic
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
      with type ('a, 'b, 'c) map := ('a, 'b, 'c) map
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) tree
      with type 'a key := 'a key
      with type 'a map_key := 'a map_key
      with type 'a cmp := 'a cmp
      with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) create_options
      with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) access_options

    include
      Transformers_generic
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) tree
      with type 'a key := 'a key
      with type 'a cmp := 'a cmp
      with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) access_options

    include
      Accessors_generic
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) tree
      with type 'a key := 'a key
      with type 'a cmp := 'a cmp
      with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) access_options
  end

  module type S_poly = sig
    type ('a, 'b) t
    type ('a, 'b) map
    type ('a, 'b) tree
    type comparator_witness

    include
      Creators_and_accessors_and_transformers_generic
      with type ('a, 'b, 'c) t := ('a, 'b) t
      with type ('a, 'b, 'c) map := ('a, 'b) map
      with type ('a, 'b, 'c) tree := ('a, 'b) tree
      with type 'k key := 'k
      with type 'k map_key := 'k
      with type 'c cmp := comparator_witness
      with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) Without_comparator.t
      with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) Without_comparator.t
  end

  module type For_deriving = sig
    type ('a, 'b, 'c) t

    module type%template [@alloc a = (heap, stack)] Sexp_of_m = Sexpable.Sexp_of
    [@alloc a]

    module type M_of_sexp = sig
      type t [@@deriving of_sexp]

      include Comparator.S with type t := t
    end

    module type M_sexp_grammar = sig
      type t [@@deriving sexp_grammar]
    end

    module type Compare_m = sig end
    module type Equal_m = sig end
    module type Hash_fold_m = Hasher.S
    module type Globalize_m = sig end

    val%template sexp_of_m__t
      :  ((module Sexp_of_m with type t = 'k)[@alloc a])
      -> ('v @ m -> Sexp.t @ m)
      -> ('k, 'v, 'cmp) t @ m
      -> Sexp.t @ m
    [@@alloc a @ m = (heap_global, stack_local)]

    val m__t_of_sexp
      :  (module M_of_sexp with type t = 'k and type comparator_witness = 'cmp)
      -> (Sexp.t -> 'v)
      -> Sexp.t
      -> ('k, 'v, 'cmp) t

    val m__t_sexp_grammar
      :  (module M_sexp_grammar with type t = 'k)
      -> 'v Sexplib0.Sexp_grammar.t
      -> ('k, 'v, 'cmp) t Sexplib0.Sexp_grammar.t
      @@ portable

    val compare_m__t
      :  (module Compare_m)
      -> ('v -> 'v -> int)
      -> ('k, 'v, 'cmp) t
      -> ('k, 'v, 'cmp) t
      -> int

    val compare_m__t__local
      :  (module Compare_m)
      -> ('v @ local -> 'v @ local -> int)
      -> ('k, 'v, 'cmp) t @ local
      -> ('k, 'v, 'cmp) t @ local
      -> int

    val equal_m__t
      :  (module Equal_m)
      -> ('v -> 'v -> bool)
      -> ('k, 'v, 'cmp) t
      -> ('k, 'v, 'cmp) t
      -> bool

    val equal_m__t__local
      :  (module Equal_m)
      -> ('v @ local -> 'v @ local -> bool)
      -> ('k, 'v, 'cmp) t @ local
      -> ('k, 'v, 'cmp) t @ local
      -> bool

    val globalize_m__t
      :  (module Globalize_m)
      -> _
      -> ('k, 'v, 'cmp) t @ local
      -> ('k, 'v, 'cmp) t

    val hash_fold_m__t
      :  (module Hash_fold_m with type t = 'k)
      -> (Hash.state -> 'v -> Hash.state)
      -> Hash.state
      -> ('k, 'v, _) t
      -> Hash.state
  end

  (**/**)

  (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

      https://opensource.janestreet.com/standards/#private-submodules *)
  module Private = struct
    module type Enum = sig
      type ('k, 'v, 'cmp) tree

      (** Phantom types, to avoid mixing up enumeration directions. *)

      type increasing
      type decreasing

      (** Enum type *)

      type ('k, 'v, 'cmp, 'direction) nonempty =
        | More of 'k * 'v * ('k, 'v, 'cmp) tree * ('k, 'v, 'cmp, 'direction) t

      and ('k, 'v, 'cmp, 'direction) t = ('k, 'v, 'cmp, 'direction) nonempty or_null

      (** Constructors *)

      val cons
        :  ('k, 'v, 'cmp) tree
        -> ('k, 'v, 'cmp, increasing) t
        -> ('k, 'v, 'cmp, increasing) t

      val cons_right
        :  ('k, 'v, 'cmp) tree
        -> ('k, 'v, 'cmp, decreasing) t
        -> ('k, 'v, 'cmp, decreasing) t

      val of_tree : ('k, 'v, 'cmp) tree -> ('k, 'v, 'cmp, increasing) t
      val of_tree_right : ('k, 'v, 'cmp) tree -> ('k, 'v, 'cmp, decreasing) t

      val starting_at_increasing
        :  ('k, 'v, 'cmp) tree @ local
        -> 'k
        -> ('k -> 'k -> int)
        -> ('k, 'v, 'cmp, increasing) t

      val starting_at_decreasing
        :  ('k, 'v, 'cmp) tree @ local
        -> 'k
        -> ('k -> 'k -> int)
        -> ('k, 'v, 'cmp, decreasing) t

      (** Comparing two enums, or two trees using enums behind the scenes *)

      val drop_phys_equal_prefix
        :  ('k, 'v, 'cmp) tree @ local
        -> ('k, 'v, 'cmp, 'dir) t
        -> ('k, 'v, 'cmp) tree @ local
        -> ('k, 'v, 'cmp, 'dir) t
        -> ('k, 'v, 'cmp, 'dir) t * ('k, 'v, 'cmp, 'dir) t

      val symmetric_diff
        :  ('k, 'v, 'cmp) tree
        -> ('k, 'v, 'cmp) tree
        -> compare_key:('k -> 'k -> int)
        -> data_equal:('v -> 'v -> bool)
        -> ('k, 'v) Symmetric_diff_element.t Sequence.t

      val fold_symmetric_diff
        :  ('k, 'v, 'cmp) tree
        -> ('k, 'v, 'cmp) tree
        -> compare_key:('k -> 'k -> int) @ local
        -> data_equal:('v -> 'v -> bool) @ local
        -> init:'acc
        -> f:('acc -> ('k, 'v) Symmetric_diff_element.t -> 'acc) @ local
        -> 'acc

      val compare
        :  ('k -> 'k -> int)
        -> ('v -> ('v -> int) @ local)
           (** This (otherwise odd) use of [local] makes a single [Enum.compare] usable by
               both [Map.compare_direct] and [Map.compare_direct [@mode local]]. *)
        -> ('k, 'v, 'cmp, increasing) t
        -> ('k, 'v, 'cmp, increasing) t
        -> int

      val equal
        :  ('k -> 'k -> int)
        -> ('v -> ('v -> bool) @ local)
           (** This (otherwise odd) use of [local] makes a single [Enum.equal] usable by
               both [Map.equal] and [Map.equal [@mode local]]. *)
        -> ('k, 'v, 'cmp, increasing) t
        -> ('k, 'v, 'cmp, increasing) t
        -> bool

      (** Traversing two enums *)

      val fold2
        :  ('k -> 'k -> int)
        -> ('k, 'v1, 'cmp, increasing) t
        -> ('k, 'v2, 'cmp, increasing) t
        -> init:'kcc
        -> f:(key:'k -> data:('v1, 'v2) Merge_element.t -> 'kcc -> 'kcc) @ local
        -> 'kcc

      (** Traversing one enum *)

      val fold
        :  init:'acc
        -> f:(key:'k -> data:'v -> 'acc -> 'acc) @ local
        -> ('k, 'v, 'cmp, increasing) t
        -> 'acc
    end
  end
end

module type Map = sig @@ portable
  include module type of struct
    include Definitions
  end

  (** [Map] is a functional data structure (balanced binary tree) implementing finite maps
      over a totally-ordered domain, called a "key". *)

  type (!'key
       , +!'value
       , !'cmp)
       t :
       immutable_data with 'key with 'value with ('key, 'cmp) Comparator.t
  [@@deriving globalize]

  module Finished_or_unfinished : sig
    type t = Finished_or_unfinished.t =
      | Finished
      | Unfinished
    [@@deriving compare ~localize, enumerate, equal ~localize, sexp_of]

    (** Maps [Continue] to [Finished] and [Stop] to [Unfinished]. *)
    val of_continue_or_stop : Continue_or_stop.t -> t

    (** Maps [Finished] to [Continue] and [Unfinished] to [Stop]. *)
    val to_continue_or_stop : t -> Continue_or_stop.t
  end

  module Merge_element : sig
    type ('left, 'right) t =
      [ `Left of 'left
      | `Right of 'right
      | `Both of 'left * 'right
      ]
    [@@deriving compare ~localize, equal ~localize, sexp_of]

    val left : ('left, _) t -> 'left option
    val right : (_, 'right) t -> 'right option
    val left_value : ('left, _) t -> default:'left -> 'left
    val right_value : (_, 'right) t -> default:'right -> 'right

    val values
      :  ('left, 'right) t
      -> left_default:'left
      -> right_default:'right
      -> 'left * 'right
  end

  (** Test if the invariants of the internal weight-balanced search tree hold. *)
  val invariants : (_, _, _) t -> bool

  (** Returns a first-class module that can be used to build other map/set/etc. with the
      same notion of comparison. *)
  val comparator_s : ('a, _, 'cmp) t -> ('a, 'cmp) Comparator.Module.t

  val comparator : ('a, _, 'cmp) t -> ('a, 'cmp) Comparator.t
  val globalize0 : ('key, 'data, 'cmp) t @ local -> ('key, 'data, 'cmp) t

  (** The empty map. *)
  val%template empty
    : 'a 'b ('cmp : value mod p).
    ('a, 'cmp) Comparator.Module.t -> ('a, 'b, 'cmp) t @ p
  [@@mode p = (nonportable, portable)]

  (** A map with one (key, data) pair. *)
  val singleton : ('a, 'cmp) Comparator.Module.t -> 'a -> 'b -> ('a, 'b, 'cmp) t

  (** Creates a map from an association list with unique keys. *)
  val of_alist
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) list
    -> [ `Ok of ('a, 'b, 'cmp) t | `Duplicate_key of 'a ]

  (** Creates a map from an association list with unique keys, returning an error if
      duplicate ['a] keys are found. *)
  val of_alist_or_error
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) list
    -> ('a, 'b, 'cmp) t Or_error.t

  (** Creates a map from an association list with unique keys, raising an exception if
      duplicate ['a] keys are found. *)
  val of_alist_exn : ('a, 'cmp) Comparator.Module.t -> ('a * 'b) list -> ('a, 'b, 'cmp) t

  (** Creates a map from an association list with possibly repeated keys. The values in
      the map for a given key appear in the same order as they did in the association
      list. *)
  val of_alist_multi
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) list
    -> ('a, 'b list, 'cmp) t

  (** Combines an association list into a map, folding together bound values with common
      keys. The accumulator is per-key.

      Example:

      {[
        # (let map =
             String.Map.of_alist_fold
               [ "a", 1; "a", 10; "b", 2; "b", 20; "b", 200 ]
               ~init:Int.Set.empty
               ~f:Set.add
           in
           print_s [%sexp (map : Int.Set.t String.Map.t)]);;
        ((a (1 10)) (b (2 20 200)))
        - : unit = ()
      ]} *)
  val of_alist_fold
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) list
    -> init:'c
    -> f:('c -> 'b -> 'c) @ local
    -> ('a, 'c, 'cmp) t

  (** Combines an association list into a map, reducing together bound values with common
      keys. *)
  val of_alist_reduce
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) list
    -> f:('b -> 'b -> 'b) @ local
    -> ('a, 'b, 'cmp) t

  (** [of_iteri ~iteri] behaves like [of_alist], except that instead of taking a concrete
      data structure, it takes an iteration function. For instance, to convert a string
      table into a map: [of_iteri (module String) ~f:(Hashtbl.iteri table)]. It is faster
      than adding the elements one by one. *)
  val of_iteri
    :  ('a, 'cmp) Comparator.Module.t
    -> iteri:(f:(key:'a -> data:'b -> unit) @ local -> unit) @ local
    -> [ `Ok of ('a, 'b, 'cmp) t | `Duplicate_key of 'a ]

  (** Like [of_iteri] except that it raises an exception if duplicate ['a] keys are found. *)
  val of_iteri_exn
    :  ('a, 'cmp) Comparator.Module.t
    -> iteri:(f:(key:'a -> data:'b -> unit) @ local -> unit) @ local
    -> ('a, 'b, 'cmp) t

  (** Creates a map from a sorted array of key-data pairs. The input array must be sorted
      (either in ascending or descending order), as given by the relevant comparator, and
      must not contain duplicate keys. If either of these conditions does not hold, an
      error is returned. *)
  val of_sorted_array
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) array
    -> ('a, 'b, 'cmp) t Or_error.t

  (** Like [of_sorted_array] except that it returns a map with broken invariants when an
      [Error] would have been returned. *)
  val of_sorted_array_unchecked
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) array
    -> ('a, 'b, 'cmp) t

  (** [of_increasing_iterator_unchecked c ~len ~f] behaves like
      [of_sorted_array_unchecked c (Array.init len ~f)], with the additional restriction
      that a decreasing order is not supported. The advantage is not requiring you to
      allocate an intermediate array. [f] will be called with 0, 1, ... [len - 1], in
      order. *)
  val of_increasing_iterator_unchecked
    :  ('a, 'cmp) Comparator.Module.t
    -> len:int
    -> f:(int -> 'a * 'b) @ local
    -> ('a, 'b, 'cmp) t

  (** [of_increasing_sequence c seq] behaves like
      [of_sorted_array c (Sequence.to_array seq)], but does not allocate the intermediate
      array.

      The sequence will be folded over once, and the additional time complexity is
      {e O(n)}. *)
  val of_increasing_sequence
    :  ('k, 'cmp) Comparator.Module.t
    -> ('k * 'v) Sequence.t
    -> ('k, 'v, 'cmp) t Or_error.t

  (** Creates a map from an association sequence with unique keys.

      [of_sequence c seq] behaves like [of_alist c (Sequence.to_list seq)] but does not
      allocate the intermediate list.

      If your sequence is increasing, use [of_increasing_sequence]. *)
  val of_sequence
    :  ('k, 'cmp) Comparator.Module.t
    -> ('k * 'v) Sequence.t
    -> [ `Ok of ('k, 'v, 'cmp) t | `Duplicate_key of 'k ]

  (** Creates a map from an association sequence with unique keys, returning an error if
      duplicate ['a] keys are found.

      [of_sequence_or_error c seq] behaves like
      [of_alist_or_error c (Sequence.to_list seq)] but does not allocate the intermediate
      list. *)
  val of_sequence_or_error
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) Sequence.t
    -> ('a, 'b, 'cmp) t Or_error.t

  (** Creates a map from an association sequence with unique keys, raising an exception if
      duplicate ['a] keys are found.

      [of_sequence_exn c seq] behaves like [of_alist_exn c (Sequence.to_list seq)] but
      does not allocate the intermediate list. *)
  val of_sequence_exn
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) Sequence.t
    -> ('a, 'b, 'cmp) t

  (** Creates a map from an association sequence with possibly repeated keys. The values
      in the map for a given key appear in the same order as they did in the association
      list.

      [of_sequence_multi c seq] behaves like [of_alist_exn c (Sequence.to_list seq)] but
      does not allocate the intermediate list. *)
  val of_sequence_multi
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) Sequence.t
    -> ('a, 'b list, 'cmp) t

  (** Combines an association sequence into a map, folding together bound values with
      common keys.

      [of_sequence_fold c seq ~init ~f] behaves like
      [of_alist_fold c (Sequence.to_list seq) ~init ~f] but does not allocate the
      intermediate list. *)
  val of_sequence_fold
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) Sequence.t
    -> init:'c
    -> f:('c -> 'b -> 'c) @ local
    -> ('a, 'c, 'cmp) t

  (** Combines an association sequence into a map, reducing together bound values with
      common keys.

      [of_sequence_reduce c seq ~f] behaves like
      [of_alist_reduce c (Sequence.to_list seq) ~f] but does not allocate the intermediate
      list. *)
  val of_sequence_reduce
    :  ('a, 'cmp) Comparator.Module.t
    -> ('a * 'b) Sequence.t
    -> f:('b -> 'b -> 'b) @ local
    -> ('a, 'b, 'cmp) t

  (** Constructs a map from a list of values, where [get_key] extracts a key from a value. *)
  val of_list_with_key
    :  ('k, 'cmp) Comparator.Module.t
    -> 'v list
    -> get_key:('v -> 'k) @ local
    -> [ `Ok of ('k, 'v, 'cmp) t | `Duplicate_key of 'k ]

  (** Like [of_list_with_key]; returns [Error] on duplicate key. *)
  val of_list_with_key_or_error
    :  ('k, 'cmp) Comparator.Module.t
    -> 'v list
    -> get_key:('v -> 'k) @ local
    -> ('k, 'v, 'cmp) t Or_error.t

  (** Like [of_list_with_key]; raises on duplicate key. *)
  val of_list_with_key_exn
    :  ('k, 'cmp) Comparator.Module.t
    -> 'v list
    -> get_key:('v -> 'k) @ local
    -> ('k, 'v, 'cmp) t

  (** Like [of_list_with_key]; produces lists of all values associated with each key. *)
  val of_list_with_key_multi
    :  ('k, 'cmp) Comparator.Module.t
    -> 'v list
    -> get_key:('v -> 'k) @ local
    -> ('k, 'v list, 'cmp) t

  (** Like [of_list_with_key]; resolves duplicate keys the same way [of_alist_fold] does. *)
  val of_list_with_key_fold
    :  ('k, 'cmp) Comparator.Module.t
    -> 'v list
    -> get_key:('v -> 'k) @ local
    -> init:'acc
    -> f:('acc -> 'v -> 'acc)
    -> ('k, 'acc, 'cmp) t

  (** Like [of_list_with_key]; resolves duplicate keys the same way [of_alist_reduce]
      does. *)
  val of_list_with_key_reduce
    :  ('k, 'cmp) Comparator.Module.t
    -> 'v list
    -> get_key:('v -> 'k) @ local
    -> f:('v -> 'v -> 'v)
    -> ('k, 'v, 'cmp) t

  (** Tests whether a map is empty. *)
  val is_empty : (_, _, _) t @ local -> bool

  (** [length map] returns the number of elements in [map]. O(1), but [Tree.length] is
      O(n). *)
  val length : (_, _, _) t @ local -> int

  (** Returns a new map with the specified new binding; if the key was already bound, its
      previous binding disappears. *)
  val set : ('k, 'v, 'cmp) t -> key:'k -> data:'v -> ('k, 'v, 'cmp) t

  (** [add t ~key ~data] adds a new entry to [t] mapping [key] to [data] and returns [`Ok]
      with the new map, or if [key] is already present in [t], returns [`Duplicate]. *)
  val add : ('k, 'v, 'cmp) t -> key:'k -> data:'v -> ('k, 'v, 'cmp) t Or_duplicate.t

  val add_exn : ('k, 'v, 'cmp) t -> key:'k -> data:'v -> ('k, 'v, 'cmp) t

  (** If [key] is not present then add a singleton list, otherwise, cons data onto the
      head of the existing list. *)
  val add_multi : ('k, 'v list, 'cmp) t -> key:'k -> data:'v -> ('k, 'v list, 'cmp) t

  (** If the key is present, then remove its head element; if the result is empty, remove
      the key. *)
  val remove_multi : ('k, 'v list, 'cmp) t -> 'k -> ('k, 'v list, 'cmp) t

  (** Returns the value bound to the given key, or the empty list if there is none. *)
  val find_multi : ('k, 'v list, 'cmp) t -> 'k -> 'v list

  (** [change t key ~f] returns a new map [m] that is the same as [t] on all keys except
      for [key], and whose value for [key] is defined by [f], i.e.,
      [find m key = f (find t key)]. *)
  val change
    :  ('k, 'v, 'cmp) t
    -> 'k
    -> f:('v option -> 'v option) @ local
    -> ('k, 'v, 'cmp) t

  (** [update t key ~f] returns a new map which is the same as [t] but sets the value
      corresponding to [key] to the result of [f].

      [f] is called with Some value if [key] is present in the map, otherwise None.

      [update] can never remove an item from the map. See [change] if you need to remove
      values. *)
  val update : ('k, 'v, 'cmp) t -> 'k -> f:('v option -> 'v) @ local -> ('k, 'v, 'cmp) t

  (** [update_and_return t key ~f] is like [update t key ~f], but also returns the new
      value. *)
  val update_and_return
    :  ('k, 'v, 'cmp) t
    -> 'k
    -> f:('v option -> 'v) @ local
    -> 'v * ('k, 'v, 'cmp) t

  (** Returns [Some value] bound to the given key, or [None] if none exists. *)
  val find : ('k, 'v, 'cmp) t -> 'k -> 'v option

  (** Returns the value bound to the given key, raising [Stdlib.Not_found] or
      [Not_found_s] if none exists. *)
  val find_exn : ('k, 'v, 'cmp) t -> 'k -> 'v

  (** Returns a new map with any binding for the key in question removed. *)
  val remove : ('k, 'v, 'cmp) t -> 'k -> ('k, 'v, 'cmp) t

  (** [mem map key] tests whether [map] contains a binding for [key]. *)
  val mem : ('k, _, 'cmp) t -> 'k -> bool

  val iter_keys : ('k, _, _) t -> f:('k -> unit) @ local -> unit
  val iter : (_, 'v, _) t -> f:('v -> unit) @ local -> unit
  val iteri : ('k, 'v, _) t -> f:(key:'k -> data:'v -> unit) @ local -> unit

  (** Iterates until the first time [f] returns [Stop]. If [f] returns [Stop], the final
      result is [Unfinished]. Otherwise, the final result is [Finished]. *)
  val iteri_until
    :  ('k, 'v, _) t
    -> f:(key:'k -> data:'v -> Continue_or_stop.t) @ local
    -> Finished_or_unfinished.t

  (** Iterates two maps side by side. The complexity of this function is O(M + N). If two
      inputs are [[(0, a); (1, a)]] and [[(1, b); (2, b)]], [f] will be called with
      [[(0, `Left a); (1, `Both (a, b)); (2, `Right b)]]. *)
  val iter2
    :  ('k, 'v1, 'cmp) t
    -> ('k, 'v2, 'cmp) t
    -> f:(key:'k -> data:('v1, 'v2) Merge_element.t -> unit) @ local
    -> unit

  (** Returns a new map with bound values replaced by [f] applied to the bound values. *)
  val map : ('k, 'v1, 'cmp) t -> f:('v1 -> 'v2) @ local -> ('k, 'v2, 'cmp) t

  (** Like [map], but the passed function takes both [key] and [data] as arguments. *)
  val mapi
    :  ('k, 'v1, 'cmp) t
    -> f:(key:'k -> data:'v1 -> 'v2) @ local
    -> ('k, 'v2, 'cmp) t

  (** Convert map with keys of type ['k2] to a map with keys of type ['k2] using [f]. *)
  val map_keys
    :  ('k2, 'cmp2) Comparator.Module.t
    -> ('k1, 'v, 'cmp1) t
    -> f:('k1 -> 'k2) @ local
    -> [ `Ok of ('k2, 'v, 'cmp2) t | `Duplicate_key of 'k2 ]

  (** Like [map_keys], but raises on duplicate key. *)
  val map_keys_exn
    :  ('k2, 'cmp2) Comparator.Module.t
    -> ('k1, 'v, 'cmp1) t
    -> f:('k1 -> 'k2) @ local
    -> ('k2, 'v, 'cmp2) t

  (** Folds over keys and data in the map in increasing order of [key]. *)
  val fold
    :  ('k, 'v, _) t
    -> init:'acc
    -> f:(key:'k -> data:'v -> 'acc -> 'acc) @ local
    -> 'acc

  (** Folds over keys and data in the map in increasing order of [key], until the first
      time that [f] returns [Stop _]. If [f] returns [Stop final], this function returns
      immediately with the value [final]. If [f] never returns [Stop _], and the final
      call to [f] returns [Continue last], this function returns [finish last]. *)
  val fold_until
    :  ('k, 'v, _) t
    -> init:'acc
    -> f:(key:'k -> data:'v -> 'acc -> ('acc, 'final) Container.Continue_or_stop.t)
       @ local
    -> finish:('acc -> 'final) @ local
    -> 'final

  (** Folds over keys and data in the map in decreasing order of [key]. *)
  val fold_right
    :  ('k, 'v, _) t
    -> init:'acc
    -> f:(key:'k -> data:'v -> 'acc -> 'acc) @ local
    -> 'acc

  (** Folds over two maps side by side, like [iter2]. *)
  val fold2
    :  ('k, 'v1, 'cmp) t
    -> ('k, 'v2, 'cmp) t
    -> init:'acc
    -> f:(key:'k -> data:('v1, 'v2) Merge_element.t -> 'acc -> 'acc) @ local
    -> 'acc

  (** [filter], [filteri], [filter_keys], [filter_map], and [filter_mapi] run in O(n)
      time.

      [filter], [filteri], [filter_keys], [partition_tf] and [partitioni_tf] keep a lot of
      sharing between their result and the original map. Dropping or keeping a run of [k]
      consecutive elements costs [O(log(k))] extra memory. Keeping the entire map costs no
      extra memory at all: [filter ~f:(fun _ -> true)] returns the original map. *)
  val filter_keys : ('k, 'v, 'cmp) t -> f:('k -> bool) @ local -> ('k, 'v, 'cmp) t

  val filter : ('k, 'v, 'cmp) t -> f:('v -> bool) @ local -> ('k, 'v, 'cmp) t

  val filteri
    :  ('k, 'v, 'cmp) t
    -> f:(key:'k -> data:'v -> bool) @ local
    -> ('k, 'v, 'cmp) t

  (** Returns a new map with bound values filtered by [f] applied to the bound values. *)
  val filter_map : ('k, 'v1, 'cmp) t -> f:('v1 -> 'v2 option) @ local -> ('k, 'v2, 'cmp) t

  (** Like [filter_map], but the passed function takes both [key] and [data] as arguments. *)
  val filter_mapi
    :  ('k, 'v1, 'cmp) t
    -> f:(key:'k -> data:'v1 -> 'v2 option) @ local
    -> ('k, 'v2, 'cmp) t

  (** [partition_mapi t ~f] returns two new [t]s, with each key in [t] appearing in
      exactly one of the resulting maps depending on its mapping in [f]. *)
  val partition_mapi
    :  ('k, 'v1, 'cmp) t
    -> f:(key:'k -> data:'v1 -> ('v2, 'v3) Either.t) @ local
    -> ('k, 'v2, 'cmp) t * ('k, 'v3, 'cmp) t

  (** [partition_map t ~f = partition_mapi t ~f:(fun ~key:_ ~data -> f data)] *)
  val partition_map
    :  ('k, 'v1, 'cmp) t
    -> f:('v1 -> ('v2, 'v3) Either.t) @ local
    -> ('k, 'v2, 'cmp) t * ('k, 'v3, 'cmp) t

  (** [partition_result t = partition_map t ~f:Result.to_either] *)
  val partition_result
    :  ('k, ('ok, 'error) Result.t, 'cmp) t
    -> ('k, 'ok, 'cmp) t * ('k, 'error, 'cmp) t

  (** {[
        partitioni_tf t ~f
        = partition_mapi t ~f:(fun ~key ~data ->
          if f ~key ~data then First data else Second data)
      ]} *)
  val partitioni_tf
    :  ('k, 'v, 'cmp) t
    -> f:(key:'k -> data:'v -> bool) @ local
    -> ('k, 'v, 'cmp) t * ('k, 'v, 'cmp) t

  (** [partition_tf t ~f = partitioni_tf t ~f:(fun ~key:_ ~data -> f data)] *)
  val partition_tf
    :  ('k, 'v, 'cmp) t
    -> f:('v -> bool) @ local
    -> ('k, 'v, 'cmp) t * ('k, 'v, 'cmp) t

  (** Produces [Ok] of a map including all keys if all data is [Ok], or an [Error]
      including all errors otherwise. *)
  val combine_errors : ('k, 'v Or_error.t, 'cmp) t -> ('k, 'v, 'cmp) t Or_error.t

  (** Given a map of tuples, produces a tuple of maps. Equivalent to:
      [map t ~f:fst, map t ~f:snd] *)
  val unzip : ('k, 'v1 * 'v2, 'cmp) t -> ('k, 'v1, 'cmp) t * ('k, 'v2, 'cmp) t

  (** Returns a total ordering between maps. The first argument is a total ordering used
      to compare data associated with equal keys in the two maps. *)
  val%template compare_direct
    :  ('v @ m -> 'v @ m -> int)
    -> ('k, 'v, 'cmp) t @ m
    -> ('k, 'v, 'cmp) t @ m
    -> int
  [@@mode m = (local, global)]

  (** Hash function: a building block to use when hashing data structures containing maps
      in them. [hash_fold_direct hash_fold_key] is compatible with [compare_direct] iff
      [hash_fold_key] is compatible with [(comparator m).compare] of the map [m] being
      hashed. *)
  val hash_fold_direct : 'k Hash.folder -> 'v Hash.folder -> ('k, 'v, 'cmp) t Hash.folder

  (** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are equal, that is, contain
      the same keys and associate each key with the same value. [cmp] is the equality
      predicate used to compare the values associated with the keys. *)
  val%template equal
    :  ('v @ m -> 'v @ m -> bool)
    -> ('k, 'v, 'cmp) t @ m
    -> ('k, 'v, 'cmp) t @ m
    -> bool
  [@@mode m = (local, global)]

  (** Returns a list of the keys in the given map. *)
  val keys : ('k, _, _) t -> 'k list

  (** Returns a list of the data in the given map. *)
  val data : (_, 'v, _) t -> 'v list

  (** Creates an association list from the given map. *)
  val to_alist
    :  ?key_order:[ `Increasing | `Decreasing ] (** default is [`Increasing] *)
    -> ('k, 'v, _) t
    -> ('k * 'v) list

  (** {2 Additional operations on maps} *)

  (** Merges two maps. The runtime is O(length(t1) + length(t2)).

      The [merge_*] functions immediately below perform better in cases where they are
      applicable. For merging a list of maps especially, use [merge_disjoint_exn] or
      [merge_skewed] instead. If you don't require the full generality of [~f]'s behavior,
      use [merge_by_case]. *)
  val merge
    :  ('k, 'v1, 'cmp) t
    -> ('k, 'v2, 'cmp) t
    -> f:(key:'k -> ('v1, 'v2) Merge_element.t -> 'v3 option) @ local
    -> ('k, 'v3, 'cmp) t

  (** Merges two dictionaries with the same type of data and disjoint sets of keys. Raises
      if any keys overlap. *)
  val merge_disjoint_exn : ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t

  (** A special case of [merge], [merge_skewed t1 t2] is a map containing all the bindings
      of [t1] and [t2]. Bindings that appear in both [t1] and [t2] are combined into a
      single value using the [combine] function. In a call [combine ~key v1 v2], the value
      [v1] comes from [t1] and [v2] from [t2].

      The runtime of [merge_skewed] is [O(min(l1, l2) * log(max(l1, l2)))], where [l1] is
      the length of [t1] and [l2] the length of [t2]. This is likely to be faster than
      [merge] when one of the maps is a lot smaller, or when you merge a list of maps. *)
  val merge_skewed
    :  ('k, 'v, 'cmp) t
    -> ('k, 'v, 'cmp) t
    -> combine:(key:'k -> 'v -> 'v -> 'v) @ local
    -> ('k, 'v, 'cmp) t

  (** An alternative to [merge] that is more efficient when unmatched keys are
      unconditionally dropped or kept unchanged in the result. *)
  val merge_by_case
    :  ('k, 'v1, 'cmp) t
    -> ('k, 'v2, 'cmp) t
    -> first:('k, 'v1, 'v3) When_unmatched.t @ local
    -> second:('k, 'v2, 'v3) When_unmatched.t @ local
    -> both:('k, 'v1, 'v2, 'v3) When_matched.t @ local
    -> ('k, 'v3, 'cmp) t

  (** [symmetric_diff t1 t2 ~data_equal] returns a list of changes between [t1] and [t2].
      It is intended to be efficient in the case where [t1] and [t2] share a large amount
      of structure. The keys in the output sequence will be in sorted order.

      It is assumed that [data_equal] is at least as equating as physical equality: that
      [phys_equal x y] implies [data_equal x y]. Otherwise, [symmetric_diff] may behave in
      unexpected ways. For example, with [~data_equal:(fun _ _ -> false)] it is NOT
      necessarily the case the resulting change sequence will contain an element
      [(k, `Unequal _)] for every key [k] shared by both maps.

      Warning: Float equality violates this property! [phys_equal Float.nan Float.nan] is
      true, but [Float.(=) Float.nan Float.nan] is false. *)
  val symmetric_diff
    :  ('k, 'v, 'cmp) t
    -> ('k, 'v, 'cmp) t
    -> data_equal:('v -> 'v -> bool)
    -> ('k, 'v) Symmetric_diff_element.t Sequence.t

  (** [fold_symmetric_diff t1 t2 ~data_equal] folds across an implicit sequence of changes
      between [t1] and [t2], in sorted order by keys. Equivalent to
      [Sequence.fold (symmetric_diff t1 t2 ~data_equal)], and more efficient. *)
  val fold_symmetric_diff
    :  ('k, 'v, 'cmp) t
    -> ('k, 'v, 'cmp) t
    -> data_equal:('v -> 'v -> bool) @ local
    -> init:'acc
    -> f:('acc -> ('k, 'v) Symmetric_diff_element.t -> 'acc) @ local
    -> 'acc

  (** [min_elt map] returns [Some (key, data)] pair corresponding to the minimum key in
      [map], or [None] if empty. *)
  val min_elt : ('k, 'v, _) t -> ('k * 'v) option

  val min_elt_exn : ('k, 'v, _) t -> 'k * 'v

  (** [max_elt map] returns [Some (key, data)] pair corresponding to the maximum key in
      [map], or [None] if [map] is empty. *)
  val max_elt : ('k, 'v, _) t -> ('k * 'v) option

  val max_elt_exn : ('k, 'v, _) t -> 'k * 'v

  (** Swap the inner and outer keys of nested maps. If [transpose_keys m a = b], then
      [find_exn (find_exn a i) j = find_exn (find_exn b j) i]. *)
  val transpose_keys
    :  ('k2, 'cmp2) Comparator.Module.t
    -> ('k1, ('k2, 'v, 'cmp2) t, 'cmp1) t
    -> ('k2, ('k1, 'v, 'cmp1) t, 'cmp2) t

  (** These functions have the same semantics as similar functions in [List]. *)

  val for_all : ('k, 'v, _) t -> f:('v -> bool) @ local -> bool
  val for_alli : ('k, 'v, _) t -> f:(key:'k -> data:'v -> bool) @ local -> bool
  val exists : ('k, 'v, _) t -> f:('v -> bool) @ local -> bool
  val existsi : ('k, 'v, _) t -> f:(key:'k -> data:'v -> bool) @ local -> bool
  val count : ('k, 'v, _) t -> f:('v -> bool) @ local -> int
  val counti : ('k, 'v, _) t -> f:(key:'k -> data:'v -> bool) @ local -> int

  val sum
    :  (module Container.Summable with type t = 'a)
    -> ('k, 'v, _) t
    -> f:('v -> 'a) @ local
    -> 'a

  val sumi
    :  (module Container.Summable with type t = 'a)
    -> ('k, 'v, _) t
    -> f:(key:'k -> data:'v -> 'a) @ local
    -> 'a

  (** [split t key] returns a map of keys strictly less than [key], the mapping of [key]
      if any, and a map of keys strictly greater than [key]. Runtime is O(log n), where n
      is the size of the input map. *)
  val split
    :  ('k, 'v, 'cmp) t
    -> 'k
    -> ('k, 'v, 'cmp) t * ('k * 'v) option * ('k, 'v, 'cmp) t

  (** [split_le_gt t key] returns a map of keys that are less or equal to [key] and a map
      of keys strictly greater than [key].

      Runtime is O(m + log n), where n is the size of the input map and m is the size of
      the smaller of the two output maps. The O(m) term is due to the need to calculate
      the length of the output maps. *)
  val split_le_gt : ('k, 'v, 'cmp) t -> 'k -> ('k, 'v, 'cmp) t * ('k, 'v, 'cmp) t

  (** [split_lt_ge t key] returns a map of keys strictly less than [key] and a map of keys
      that are greater or equal to [key].

      Runtime is O(m + log n), where n is the size of the input map and m is the size of
      the smaller of the two output maps. The O(m) term is due to the need to calculate
      the length of the output maps. *)
  val split_lt_ge : ('k, 'v, 'cmp) t -> 'k -> ('k, 'v, 'cmp) t * ('k, 'v, 'cmp) t

  (** [split_n t n = l, r] such that [append ~lower_part:l ~upper_part:r = `Ok t] and
      [length l = n], or as close as possible if [n < 0] or [n > length t]. *)
  val split_n : ('k, 'v, 'cmp) t -> int -> ('k, 'v, 'cmp) t * ('k, 'v, 'cmp) t

  (** [count_lt t key] returns the number of keys in [t] that are strictly less than
      [key].

      Runtime is O(log n) where n is the size of the input map. *)
  val count_lt : ('k, 'v, 'cmp) t -> 'k -> int

  (** [count_le t key] returns the number of keys in [t] that are less than or equal to
      [key].

      Runtime is O(log n) where n is the size of the input map. *)
  val count_le : ('k, 'v, 'cmp) t -> 'k -> int

  (** [count_gt t key] returns the number of keys in [t] that are strictly greater than
      [key].

      Runtime is O(log n) where n is the size of the input map. *)
  val count_gt : ('k, 'v, 'cmp) t -> 'k -> int

  (** [count_ge t key] returns the number of keys in [t] that are greater than or equal to
      [key].

      Runtime is O(log n) where n is the size of the input map. *)
  val count_ge : ('k, 'v, 'cmp) t -> 'k -> int

  (** [append ~lower_part ~upper_part] returns [`Ok map] where [map] contains all the
      [(key, value)] pairs from the two input maps if all the keys from [lower_part] are
      less than all the keys from [upper_part]. Otherwise it returns
      [`Overlapping_key_ranges].

      Runtime is O(log n) where n is the size of the larger input map. This can be
      significantly faster than [Map.merge] or repeated [Map.add].

      {[
        assert (
          match Map.append ~lower_part ~upper_part with
          | `Ok whole_map ->
            Map.to_alist whole_map
            = List.append (to_alist lower_part) (to_alist upper_part)
          | `Overlapping_key_ranges -> true)
      ]} *)
  val append
    :  lower_part:('k, 'v, 'cmp) t
    -> upper_part:('k, 'v, 'cmp) t
    -> [ `Ok of ('k, 'v, 'cmp) t | `Overlapping_key_ranges ]

  (** [subrange t ~lower_bound ~upper_bound] returns a map containing all the entries from
      [t] whose keys lie inside the interval indicated by [~lower_bound] and
      [~upper_bound]. If this interval is empty, an empty map is returned.

      Runtime is O(log n), where n is the size of the input map. *)
  val subrange
    :  ('k, 'v, 'cmp) t
    -> lower_bound:'k Maybe_bound.t
    -> upper_bound:'k Maybe_bound.t
    -> ('k, 'v, 'cmp) t

  (** [fold_range_inclusive t ~min ~max ~init ~f] folds [f] (with initial value [~init])
      over all keys (and their associated values) that are in the range [[min, max]]
      (inclusive). *)
  val fold_range_inclusive
    :  ('k, 'v, 'cmp) t
    -> min:'k
    -> max:'k
    -> init:'acc
    -> f:(key:'k -> data:'v -> 'acc -> 'acc) @ local
    -> 'acc

  (** [range_to_alist t ~min ~max] returns an associative list of the elements whose keys
      lie in [[min, max]] (inclusive), with the smallest key being at the head of the
      list. *)
  val range_to_alist : ('k, 'v, 'cmp) t -> min:'k -> max:'k -> ('k * 'v) list

  (** [closest_key t dir k] returns the [(key, value)] pair in [t] with [key] closest to
      [k] that satisfies the given inequality bound.

      For example, [closest_key t `Less_than k] would be the pair with the closest key to
      [k] where [key < k].

      [to_sequence] can be used to get the same results as [closest_key]. It is less
      efficient for individual lookups but more efficient for finding many elements
      starting at some value. *)
  val closest_key
    :  ('k, 'v, 'cmp) t
    -> [ `Greater_or_equal_to | `Greater_than | `Less_or_equal_to | `Less_than ]
    -> 'k
    -> ('k * 'v) option

  (** [nth t n] finds the (key, value) pair of rank n (i.e., such that there are exactly n
      keys strictly less than the found key), if one exists. O(log(length t) + n) time. *)
  val nth : ('k, 'v, _) t -> int -> ('k * 'v) option

  val nth_exn : ('k, 'v, _) t -> int -> 'k * 'v

  (** [rank t k] If [k] is in [t], returns the number of keys strictly less than [k] in
      [t], and [None] otherwise. *)
  val rank : ('k, 'v, 'cmp) t -> 'k -> int option

  (** [to_sequence ?order ?keys_greater_or_equal_to ?keys_less_or_equal_to t] gives a
      sequence of key-value pairs between [keys_less_or_equal_to] and
      [keys_greater_or_equal_to] inclusive, presented in [order]. If
      [keys_greater_or_equal_to > keys_less_or_equal_to], the sequence is empty.

      When neither [keys_greater_or_equal_to] nor [keys_less_or_equal_to] are provided,
      the cost is O(log n) up front and amortized O(1) to produce each element. If either
      is provided (and is used by the order parameter provided), then the the cost is O(n)
      up front, and amortized O(1) to produce each element. *)
  val to_sequence
    :  ?order:[ `Increasing_key (** default *) | `Decreasing_key ]
    -> ?keys_greater_or_equal_to:'k
    -> ?keys_less_or_equal_to:'k
    -> ('k, 'v, 'cmp) t
    -> ('k * 'v) Sequence.t

  (** [binary_search t ~compare which elt] returns the [(key, value)] pair in [t]
      specified by [compare] and [which], if one exists.

      [t] must be sorted in increasing order according to [compare], where [compare] and
      [elt] divide [t] into three (possibly empty) segments:

      {v
        |  < elt  |  = elt  |  > elt  |
      v}

      [binary_search] returns an element on the boundary of segments as specified by
      [which]. See the diagram below next to the [which] variants.

      [binary_search] does not check that [compare] orders [t], and behavior is
      unspecified if [compare] doesn't order [t]. Behavior is also unspecified if
      [compare] mutates [t]. *)
  val binary_search
    :  ('k, 'v, 'cmp) t
    -> compare:(key:'k -> data:'v -> 'key -> int) @ local
    -> [ `Last_strictly_less_than (** [         | < elt X |                       ] *)
       | `Last_less_than_or_equal_to (** [      |      <= elt       X |           ] *)
       | `Last_equal_to (** [                             |   = elt X |           ] *)
       | `First_equal_to (** [                            | X = elt   |           ] *)
       | `First_greater_than_or_equal_to (** [            | X       >= elt      | ] *)
       | `First_strictly_greater_than (** [                           | X > elt | ] *)
       ]
    -> 'key
    -> ('k * 'v) option

  (** [binary_search_segmented t ~segment_of which] takes a [segment_of] function that
      divides [t] into two (possibly empty) segments:

      {v
        | segment_of elt = `Left | segment_of elt = `Right |
      v}

      [binary_search_segmented] returns the [(key, value)] pair on the boundary of the
      segments as specified by [which]: [`Last_on_left] yields the last element of the
      left segment, while [`First_on_right] yields the first element of the right segment.
      It returns [None] if the segment is empty.

      [binary_search_segmented] does not check that [segment_of] segments [t] as in the
      diagram, and behavior is unspecified if [segment_of] doesn't segment [t]. Behavior
      is also unspecified if [segment_of] mutates [t]. *)
  val binary_search_segmented
    :  ('k, 'v, 'cmp) t
    -> segment_of:(key:'k -> data:'v -> [ `Left | `Right ]) @ local
    -> [ `Last_on_left | `First_on_right ]
    -> ('k * 'v) option

  (** [binary_search_subrange] takes a [compare] function that divides [t] into three
      (possibly empty) segments with respect to [lower_bound] and [upper_bound]:

      {v
        | Below_lower_bound | In_range | Above_upper_bound |
      v}

      and returns a map of the [In_range] segment.

      Runtime is O(log m + n) where [m] is the length of the input map and [n] is the
      length of the output. The linear term in [n] is to compute the length of the output.

      Behavior is undefined if [compare] does not segment [t] as shown above, or if
      [compare] mutates its inputs. *)
  val binary_search_subrange
    :  ('k, 'v, 'cmp) t
    -> compare:(key:'k -> data:'v -> 'bound -> int) @ local
    -> lower_bound:'bound Maybe_bound.t
    -> upper_bound:'bound Maybe_bound.t
    -> ('k, 'v, 'cmp) t

  (** Creates traversals to reconstruct a map within an applicative. Uses
      [Lazy_applicative] so that the map can be traversed within the applicative, rather
      than needing to be traversed all at once, outside the applicative. *)
  module%template.portable Make_applicative_traversals
      (A : Applicative.Lazy_applicative) : sig
    val mapi
      :  ('k, 'v1, 'cmp) t
      -> f:(key:'k -> data:'v1 -> 'v2 A.t)
      -> ('k, 'v2, 'cmp) t A.t

    val filter_mapi
      :  ('k, 'v1, 'cmp) t
      -> f:(key:'k -> data:'v1 -> 'v2 option A.t)
      -> ('k, 'v2, 'cmp) t A.t
  end

  (** [M] is meant to be used in combination with OCaml applicative functor types:

      {[
        type string_to_int_map = int Map.M(String).t
      ]}

      which stands for:

      {[
        type string_to_int_map = (String.t, int, String.comparator_witness) Map.t
      ]}

      The point is that [int Map.M(String).t] supports deriving, whereas the second syntax
      doesn't (because there is no such thing as, say, [String.sexp_of_comparator_witness]
      -- instead you would want to pass the comparator directly).

      In addition, when using [@@deriving], the requirements on the key module are only
      those needed to satisfy what you are trying to derive on the map itself. Say you
      write:

      {[
        type t = int Map.M(X).t [@@deriving hash]
      ]}

      then this will be well typed exactly if [X] contains at least:
      - a type [t] with no parameters
      - a comparator witness
      - a [hash_fold_t] function with the right type *)
  module M (K : sig
      type t
      type comparator_witness
    end) : sig
    type nonrec 'v t = (K.t, 'v, K.comparator_witness) t
  end

  include For_deriving with type ('key, 'value, 'cmp) t := ('key, 'value, 'cmp) t

  module Tree : sig
    type weight : immutable_data

    type (+'k, +'v, 'cmp) t : immutable_data with 'k with 'v = private
      | Empty
      | Leaf of
          { global_ key : 'k
          ; global_ data : 'v
          }
      | Node of
          { global_ left : ('k, 'v, 'cmp) t
          ; global_ key : 'k
          ; global_ data : 'v
          ; global_ right : ('k, 'v, 'cmp) t
          ; weight : weight
          }
    [@@deriving sexp_of]

    val t_of_sexp_direct
      :  comparator:('k, 'cmp) Comparator.t
      -> (Sexp.t -> 'k)
      -> (Sexp.t -> 'v)
      -> Sexp.t
      -> ('k, 'v, 'cmp) t

    val globalize : _ -> _ -> _ -> ('k, 'v, 'cmp) t @ local -> ('k, 'v, 'cmp) t

    include
      Creators_and_accessors_and_transformers_generic
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
      with type ('a, 'b, 'c) map := ('a, 'b, 'c) t
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) t
      with type 'k key := 'k
      with type 'k map_key := 'k
      with type 'c cmp := 'c
      with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) With_comparator.t
      with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) With_comparator.t

    val empty_without_value_restriction : (_, _, _) t

    (** [Build_increasing] can be used to construct a map incrementally from a sequence
        that is known to be increasing.

        The total time complexity of constructing a map this way is O(n), which is more
        efficient than using [Map.add] by a logarithmic factor.

        This interface can be thought of as a dual of [to_sequence], but we don't have an
        equally neat idiom for the duals of sequences ([of_sequence] is much less general
        because it does not allow the sequence to be produced asynchronously). *)
    module Build_increasing : sig
      type ('a, 'b, 'c) tree := ('a, 'b, 'c) t
      type ('k, 'v, 'w) t

      val empty : ('k, 'v, 'w) t

      (** Time complexity of [add_exn] is amortized constant-time (if [t] is used
          linearly), with a worst-case O(log(n)) time. *)
      val add_exn
        :  ('k, 'v, 'w) t
        -> comparator:('k, 'w) Comparator.t
        -> key:'k
        -> data:'v
        -> ('k, 'v, 'w) t

      (** Time complexity is O(log(n)). *)
      val to_tree : ('k, 'v, 'w) t -> ('k, 'v, 'w) tree
    end

    (** Low-level constructors for balanced trees. If not carefully used, the results
        might violate the normal invariants of a tree. *)
    module Expert : sig
      (** Sexp prints the internal node structure. *)
      type nonrec ('k, 'v, 'cmp) t = ('k, 'v, 'cmp) t [@@deriving sexp_of]

      (** Just the tree balance checks from [invariants]. Excludes the checks in
          [order_invariants]. *)
      val balance_invariants : (_, _, _) t -> bool

      (** Just the key ordering checks from [invariants]. Excludes the checks in
          [balance_invariants]. *)
      val order_invariants
        :  comparator:('k, 'cmp) Comparator.t
        -> ('k, 'v, 'cmp) t
        -> bool

      (** Reports whether two trees are sufficiently balanced for
          [create_assuming_balanced_unchecked]. Two trees with the same or mirrored shape
          are guaranteed to be balanced. The left and right subtrees of a [Node]
          constructor are also guaranteed to be balanced.

          We do not describe our balance invariants in detail in this interface, as they
          have changed in the past and may change again in the future. *)
      val are_balanced : ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> bool

      (** Reports whether two trees are sufficiently balanced for
          [create_and_rebalance_at_most_once_unchecked].

          If two trees satisfy [are_balanced], at most a single key is added or removed
          from one of them, and the tree is rebuilt via
          [create_and_rebalance_at_most_once_unchecked], then the result should satisfy
          [need_rebalance_at_most_once].

          The preceding operations are equivalent to a single call to most single-key
          update functions, e.g. [add], [remove], [change], etc. *)
      val need_rebalance_at_most_once : ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> bool

      (** [create_assuming_balanced_unchecked left key data right] constructs a single
          [Node]. Given keys must be unique and strictly sorted, and
          [are_balanced left right] must be true. Otherwise map/tree behavior will be
          unspecified. *)
      val create_assuming_balanced_unchecked
        :  ('k, 'v, 'cmp) t
        -> 'k
        -> 'v
        -> ('k, 'v, 'cmp) t
        -> ('k, 'v, 'cmp) t

      (** [create_and_rebalance_at_most_once_unchecked left key data right] constructs a
          [Node], possibly rebalancing [left] and [right] once. Given keys must be unique
          and strictly sorted, and [need_rebalance_at_most_once left right] must be true.
          Otherwise map/tree behavior will be unspecified. *)
      val create_and_rebalance_at_most_once_unchecked
        :  ('k, 'v, 'cmp) t
        -> 'k
        -> 'v
        -> ('k, 'v, 'cmp) t
        -> ('k, 'v, 'cmp) t

      (** [create_and_rebalance_unchecked left key data right] constructs a [Node],
          possibly rebalancing [left] and [right] recursively. Given keys must be unique
          and strictly sorted. Otherwise map/tree behavior will be unspecified. The
          subtrees may be arbitrarily imbalanced with respect to each other. *)
      val create_and_rebalance_unchecked
        :  ('k, 'v, 'cmp) t
        -> 'k
        -> 'v
        -> ('k, 'v, 'cmp) t
        -> ('k, 'v, 'cmp) t

      (** [concat_and_rebalance_at_most_once_unchecked left right] appends [left] and
          [right] in that order, possibly rebalancing the result once. Given keys must be
          unique and strictly sorted, and [are_balanced left right] must be true.
          Otherwise map/tree behavior will be unspecified. *)
      val concat_and_rebalance_at_most_once_unchecked
        :  ('k, 'v, 'cmp) t
        -> ('k, 'v, 'cmp) t
        -> ('k, 'v, 'cmp) t

      (** [concat_and_rebalance_unchecked left right] appends [left] and [right] in that
          order, rebalancing the result recursively. Given keys must be unique and
          strictly sorted. Otherwise map/tree behavior will be unspecified. The subtrees
          may be arbitrarily imbalanced with respect to each other. *)
      val concat_and_rebalance_unchecked
        :  ('k, 'v, 'cmp) t
        -> ('k, 'v, 'cmp) t
        -> ('k, 'v, 'cmp) t

      (** Like [Tree.singleton], but does not require a comparator. *)
      val singleton : 'k -> 'v -> ('k, 'v, 'cmp) t

      (** Equivalent to [empty_without_value_restriction]. *)
      val empty : ('k, 'v, 'cmp) t

      (** Compute a tree's length from its weight field. *)
      val length_of_weight : weight -> int
    end
  end

  (** [Using_comparator] is a similar interface as the toplevel of [Map], except the
      functions take a [~comparator:('k, 'cmp) Comparator.t], whereas the functions at the
      toplevel of [Map] take a [('k, 'cmp) comparator]. *)
  module Using_comparator : sig
    type nonrec ('k, +'v, 'cmp) t = ('k, 'v, 'cmp) t [@@deriving sexp_of]

    val t_of_sexp_direct
      :  comparator:('k, 'cmp) Comparator.t
      -> (Sexp.t -> 'k)
      -> (Sexp.t -> 'v)
      -> Sexp.t
      -> ('k, 'v, 'cmp) t

    include
      Creators_and_accessors_and_transformers_generic
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
      with type ('a, 'b, 'c) map := ('a, 'b, 'c) t
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Tree.t
      with type 'k key := 'k
      with type 'k map_key := 'k
      with type 'c cmp := 'c
      with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) Without_comparator.t
      with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) With_comparator.t

    val comparator : ('a, _, 'cmp) t -> ('a, 'cmp) Comparator.t

    val hash_fold_direct
      :  'k Hash.folder
      -> 'v Hash.folder
      -> ('k, 'v, 'cmp) t Hash.folder

    (** To get around the value restriction, apply the functor and include it. You can see
        an example of this in the [Poly] submodule below. *)
    module%template.portable Empty_without_value_restriction (K : Comparator.S1) : sig
      val empty : ('a K.t, 'v, K.comparator_witness) t
    end

    module Tree = Tree
  end

  (** A polymorphic Map. *)
  module Poly :
    S_poly
    with type ('key, +'value) t = ('key, 'value, Comparator.Poly.comparator_witness) t
     and type ('key, +'value) map := ('key, 'value, Comparator.Poly.comparator_witness) t
     and type ('key, +'value) tree =
      ('key, 'value, Comparator.Poly.comparator_witness) Using_comparator.Tree.t
     and type comparator_witness = Comparator.Poly.comparator_witness

  (** Create a map from a tree using the given comparator. *)
  val of_tree
    :  ('k, 'cmp) Comparator.Module.t
    -> ('k, 'v, 'cmp) Using_comparator.Tree.t
    -> ('k, 'v, 'cmp) t

  (** Extract a tree from a map. *)
  val to_tree : ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) Using_comparator.Tree.t

  (**/**)

  (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

      https://opensource.janestreet.com/standards/#private-submodules *)
  module Private : sig
    module Enum : Private.Enum with type ('k, 'v, 'cmp) tree := ('k, 'v, 'cmp) Tree.t
  end
end
