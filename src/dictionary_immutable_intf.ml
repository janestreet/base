(** Interfaces for immutable dictionary types, such as [Map.t].

    We define separate interfaces for [Accessors] and [Creators], along with [S] combining
    both. These interfaces are written once in their most general form, which involves
    extra type definitions and type parameters that most instances do not need.

    We then provide instantiations of these interfaces with 1, 2, and 3 type parameters
    for [t]. These cover more common usage patterns for the interfaces. *)

open! Import

(** These definitions are re-exported by [Dictionary_immutable]. *)
module Definitions = struct
  module type Accessors = sig
    (** The type of keys. This will be ['key] for polymorphic dictionaries, or some fixed
        type for dictionaries with monomorphic keys. *)
    type 'key key

    (** Dictionaries. Their keys have type ['key key]. Each key's associated value has
        type ['data]. The dictionary may be distinguished by a ['phantom] type. *)
    type ('key, 'data, 'phantom) t

    (** The type of accessor functions ['fn] that operate on [('key, 'data, 'phantom) t].
        May take extra arguments before ['fn], such as a comparison function. *)
    type ('fn, 'key, 'data, 'phantom) accessor

    (** Whether the dictionary is empty. *)
    val is_empty : (_, _, _) t -> bool

    (** How many key/value pairs the dictionary contains. *)
    val length : (_, _, _) t -> int

    (** All key/value pairs. *)
    val to_alist : ('key, 'data, _) t -> ('key key * 'data) list

    (** All keys in the dictionary, in the same order as [to_alist]. *)
    val keys : ('key, _, _) t -> 'key key list

    (** All values in the dictionary, in the same order as [to_alist]. *)
    val data : (_, 'data, _) t -> 'data list

    (** Like [to_alist]. Produces a sequence. *)
    val to_sequence : ('key, 'data, 'phantom) t -> ('key key * 'data) Sequence.t

    (** Whether [key] has a value. *)
    val mem : (('key, _, 'phantom) t -> 'key key -> bool, 'key, 'data, 'phantom) accessor

    (** Produces the current value, or absence thereof, for a given key. *)
    val find
      : ( ('key, 'data, 'phantom) t -> 'key key -> 'data option
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Like [find]. Raises if there is no value for the given key. *)
    val find_exn
      : (('key, 'data, 'phantom) t -> 'key key -> 'data, 'key, 'data, 'phantom) accessor

    (** Adds a key/value pair for a key the dictionary does not contain, or reports a
        duplicate. *)
    val add
      : ( ('key, 'data, 'phantom) t
          -> key:'key key
          -> data:'data
          -> [ `Ok of ('key, 'data, 'phantom) t | `Duplicate ]
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Like [add]. Raises on duplicates. *)
    val add_exn
      : ( ('key, 'data, 'phantom) t
          -> key:'key key
          -> data:'data
          -> ('key, 'data, 'phantom) t
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Adds or replaces a key/value pair in the dictionary. *)
    val set
      : ( ('key, 'data, 'phantom) t
          -> key:'key key
          -> data:'data
          -> ('key, 'data, 'phantom) t
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Removes any value for the given key. *)
    val remove
      : ( ('key, 'data, 'phantom) t -> 'key key -> ('key, 'data, 'phantom) t
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Adds, replaces, or removes the value for a given key, depending on its current
        value or lack thereof. *)
    val change
      : ( ('key, 'data, 'phantom) t
          -> 'key key
          -> f:('data option -> 'data option)
          -> ('key, 'data, 'phantom) t
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Adds or replaces the value for a given key, depending on its current value or
        lack thereof. *)
    val update
      : ( ('key, 'data, 'phantom) t
          -> 'key key
          -> f:('data option -> 'data)
          -> ('key, 'data, 'phantom) t
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Adds [data] to the existing key/value pair for [key]. Interprets a missing key as
        having an empty list. *)
    val add_multi
      : ( ('key, 'data list, 'phantom) t
          -> key:'key key
          -> data:'data
          -> ('key, 'data list, 'phantom) t
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Removes one element from the existing key/value pair for [key]. Removes the key
        entirely if the new list is empty. *)
    val remove_multi
      : ( ('key, 'data list, 'phantom) t -> 'key key -> ('key, 'data list, 'phantom) t
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Produces the list associated with the corresponding key. Interprets a missing
        key as having an empty list. *)
    val find_multi
      : ( ('key, 'data list, 'phantom) t -> 'key key -> 'data list
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Combines every value in the dictionary. *)
    val fold
      :  ('key, 'data, _) t
      -> init:'acc
      -> f:(key:'key key -> data:'data -> 'acc -> 'acc)
      -> 'acc

    (** Like [fold]. May stop before completing the iteration. *)
    val fold_until
      :  ('key, 'data, _) t
      -> init:'acc
      -> f:
           (key:'key key
            -> data:'data
            -> 'acc
            -> ('acc, 'final) Container.Continue_or_stop.t)
      -> finish:('acc -> 'final)
      -> 'final

    (** Whether every value satisfies [f]. *)
    val for_all : ('key, 'data, _) t -> f:('data -> bool) -> bool

    (** Like [for_all]. The predicate may also depend on the associated key. *)
    val for_alli : ('key, 'data, _) t -> f:(key:'key key -> data:'data -> bool) -> bool

    (** Whether at least one value satisfies [f]. *)
    val exists : ('key, 'data, _) t -> f:('data -> bool) -> bool

    (** Like [exists]. The predicate may also depend on the associated key. *)
    val existsi : ('key, 'data, _) t -> f:(key:'key key -> data:'data -> bool) -> bool

    (** How many values satisfy [f]. *)
    val count : ('key, 'data, _) t -> f:('data -> bool) -> int

    (** Like [count]. The predicate may also depend on the associated key. *)
    val counti : ('key, 'data, _) t -> f:(key:'key key -> data:'data -> bool) -> int

    (** Sum up [f data] for all data in the dictionary. *)
    val sum
      :  (module Container.Summable with type t = 'a)
      -> ('key, 'data, _) t
      -> f:('data -> 'a)
      -> 'a

    (** Like [sum]. The function may also depend on the associated key. *)
    val sumi
      :  (module Container.Summable with type t = 'a)
      -> ('key, 'data, _) t
      -> f:(key:'key -> data:'data -> 'a)
      -> 'a

    (** Produces the key/value pair with the smallest key if non-empty. *)
    val min_elt : ('key, 'data, _) t -> ('key key * 'data) option

    (** Like [min_elt]. Raises if empty. *)
    val min_elt_exn : ('key, 'data, _) t -> 'key key * 'data

    (** Produces the key/value pair with the largest key if non-empty. *)
    val max_elt : ('key, 'data, _) t -> ('key key * 'data) option

    (** Like [max_elt]. Raises if empty. *)
    val max_elt_exn : ('key, 'data, _) t -> 'key key * 'data

    (** Calls [f] for every key. *)
    val iter_keys : ('key, _, _) t -> f:('key key -> unit) -> unit

    (** Calls [f] for every value. *)
    val iter : (_, 'data, _) t -> f:('data -> unit) -> unit

    (** Calls [f] for every key/value pair. *)
    val iteri : ('key, 'data, _) t -> f:(key:'key key -> data:'data -> unit) -> unit

    (** Transforms every value. *)
    val map
      :  ('key, 'data1, 'phantom) t
      -> f:('data1 -> 'data2)
      -> ('key, 'data2, 'phantom) t

    (** Like [map]. The transformation may also depend on the associated key. *)
    val mapi
      :  ('key, 'data1, 'phantom) t
      -> f:(key:'key key -> data:'data1 -> 'data2)
      -> ('key, 'data2, 'phantom) t

    (** Produces only those key/value pairs whose key satisfies [f]. *)
    val filter_keys
      :  ('key, 'data, 'phantom) t
      -> f:('key key -> bool)
      -> ('key, 'data, 'phantom) t

    (** Produces only those key/value pairs whose value satisfies [f]. *)
    val filter
      :  ('key, 'data, 'phantom) t
      -> f:('data -> bool)
      -> ('key, 'data, 'phantom) t

    (** Produces only those key/value pairs which satisfy [f]. *)
    val filteri
      :  ('key, 'data, 'phantom) t
      -> f:(key:'key key -> data:'data -> bool)
      -> ('key, 'data, 'phantom) t

    (** Produces key/value pairs for which [f] produces [Some]. *)
    val filter_map
      :  ('key, 'data1, 'phantom) t
      -> f:('data1 -> 'data2 option)
      -> ('key, 'data2, 'phantom) t

    (** Like [filter_map]. The new value may also depend on the associated key. *)
    val filter_mapi
      :  ('key, 'data1, 'phantom) t
      -> f:(key:'key key -> data:'data1 -> 'data2 option)
      -> ('key, 'data2, 'phantom) t

    (** Splits one dictionary into two. The first contains key/value pairs for which the
        value satisfies [f]. The second contains the remainder. *)
    val partition_tf
      :  ('key, 'data, 'phantom) t
      -> f:('data -> bool)
      -> ('key, 'data, 'phantom) t * ('key, 'data, 'phantom) t

    (** Like [partition_tf]. The predicate may also depend on the associated key. *)
    val partitioni_tf
      :  ('key, 'data, 'phantom) t
      -> f:(key:'key key -> data:'data -> bool)
      -> ('key, 'data, 'phantom) t * ('key, 'data, 'phantom) t

    (** Splits one dictionary into two, corresponding respectively to [First _] and
        [Second _] results from [f]. *)
    val partition_map
      :  ('key, 'data1, 'phantom) t
      -> f:('data1 -> ('data2, 'data3) Either.t)
      -> ('key, 'data2, 'phantom) t * ('key, 'data3, 'phantom) t

    (** Like [partition_map]. The split may also depend on the associated key. *)
    val partition_mapi
      :  ('key, 'data1, 'phantom) t
      -> f:(key:'key key -> data:'data1 -> ('data2, 'data3) Either.t)
      -> ('key, 'data2, 'phantom) t * ('key, 'data3, 'phantom) t

    (** Produces an error combining all error messages from key/value pairs, or a
        dictionary of all [Ok] values if none are [Error]. *)
    val combine_errors
      : ( ('key, 'data Or_error.t, 'phantom) t -> ('key, 'data, 'phantom) t Or_error.t
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Splits the [fst] and [snd] components of values associated with keys into separate
        dictionaries. *)
    val unzip
      :  ('key, 'data1 * 'data2, 'phantom) t
      -> ('key, 'data1, 'phantom) t * ('key, 'data2, 'phantom) t

    (** Merges two dictionaries by fully traversing both. Not suitable for efficiently
        merging lists of dictionaries. See [merge_disjoint_exn] and [merge_skewed]
        instead. *)
    val merge
      : ( ('key, 'data1, 'phantom) t
          -> ('key, 'data2, 'phantom) t
          -> f:
               (key:'key key
                -> [ `Left of 'data1 | `Right of 'data2 | `Both of 'data1 * 'data2 ]
                -> 'data3 option)
          -> ('key, 'data3, 'phantom) t
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Merges two dictionaries with the same type of data and disjoint sets of keys.
        Raises if any keys overlap. *)
    val merge_disjoint_exn
      : ( ('key, 'data, 'phantom) t
          -> ('key, 'data, 'phantom) t
          -> ('key, 'data, 'phantom) t
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Merges two dictionaries by traversing only the smaller of the two. Adds key/value
        pairs missing from the larger dictionary, and [combine]s duplicate values. *)
    val merge_skewed
      : ( ('key, 'data, 'phantom) t
          -> ('key, 'data, 'phantom) t
          -> combine:(key:'key key -> 'data -> 'data -> 'data)
          -> ('key, 'data, 'phantom) t
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Computes a sequence of differences between two dictionaries. *)
    val symmetric_diff
      : ( ('key, 'data, 'phantom) t
          -> ('key, 'data, 'phantom) t
          -> data_equal:('data -> 'data -> bool)
          -> ('key key * [ `Left of 'data | `Right of 'data | `Unequal of 'data * 'data ])
             Sequence.t
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Folds over the result of [symmetric_diff]. May be more performant. *)
    val fold_symmetric_diff
      : ( ('key, 'data, 'phantom) t
          -> ('key, 'data, 'phantom) t
          -> data_equal:('data -> 'data -> bool)
          -> init:'acc
          -> f:
               ('acc
                -> 'key key
                   * [ `Left of 'data | `Right of 'data | `Unequal of 'data * 'data ]
                -> 'acc)
          -> 'acc
        , 'key
        , 'data
        , 'phantom )
        accessor
  end

  module type Accessors1 = sig
    type key
    type 'data t

    (** @inline *)
    include
      Accessors
        with type (_, 'data, _) t := 'data t
         and type _ key := key
         and type ('fn, _, _, _) accessor := 'fn
  end

  module type Accessors2 = sig
    type ('key, 'data) t
    type ('fn, 'key, 'data) accessor

    (** @inline *)
    include
      Accessors
        with type ('key, 'data, _) t := ('key, 'data) t
         and type 'key key := 'key
         and type ('fn, 'key, 'data, _) accessor := ('fn, 'key, 'data) accessor
  end

  module type Accessors3 = sig
    type ('key, 'data, 'phantom) t
    type ('fn, 'key, 'data, 'phantom) accessor

    (** @inline *)
    include
      Accessors
        with type ('key, 'data, 'phantom) t := ('key, 'data, 'phantom) t
         and type 'key key := 'key
         and type ('fn, 'key, 'data, 'phantom) accessor :=
          ('fn, 'key, 'data, 'phantom) accessor
  end

  module type Creators = sig
    (** The type of keys. This will be ['key] for polymorphic dictionaries, or some fixed
        type for dictionaries with monomorphic keys. *)
    type 'key key

    (** Dictionaries. Their keys have type ['key key]. Each key's associated value has
        type ['data]. The dictionary may be distinguished by a ['phantom] type. *)
    type ('key, 'data, 'phantom) t

    (** The type of creator functions ['fn] that operate on [('key, 'data, 'phantom) t].
        May take extra arguments before ['fn], such as a comparison function. *)
    type ('fn, 'key, 'data, 'phantom) creator

    (** The empty dictionary. *)
    val empty : (('key, 'data, 'phantom) t, 'key, 'data, 'phantom) creator

    (** Dictionary with a single key/value pair. *)
    val singleton
      : ('key key -> 'data -> ('key, 'data, 'phantom) t, 'key, 'data, 'phantom) creator

    (** Dictionary containing the given key/value pairs. Fails if there are duplicate
        keys. *)
    val of_alist
      : ( ('key key * 'data) list
          -> [ `Ok of ('key, 'data, 'phantom) t | `Duplicate_key of 'key key ]
        , 'key
        , 'data
        , 'phantom )
        creator

    (** Like [of_alist]. Returns a [Result.t]. *)
    val of_alist_or_error
      : ( ('key key * 'data) list -> ('key, 'data, 'phantom) t Or_error.t
        , 'key
        , 'data
        , 'phantom )
        creator

    (** Like [of_alist]. Raises on duplicates. *)
    val of_alist_exn
      : ( ('key key * 'data) list -> ('key, 'data, 'phantom) t
        , 'key
        , 'data
        , 'phantom )
        creator

    (** Produces a dictionary mapping each key to a list of associated values. *)
    val of_alist_multi
      : ( ('key key * 'data) list -> ('key, 'data list, 'phantom) t
        , 'key
        , 'data
        , 'phantom )
        creator

    (** Produces a dictionary using each key/value pair. Combines all values for a given
        key with [init] using [f]. *)
    val of_alist_fold
      : ( ('key key * 'data) list
          -> init:'acc
          -> f:('acc -> 'data -> 'acc)
          -> ('key, 'acc, 'phantom) t
        , 'key
        , 'data
        , 'phantom )
        creator

    (** Produces a dictionary using each key/value pair. Combines multiple values for a
        given key using [f]. *)
    val of_alist_reduce
      : ( ('key key * 'data) list
          -> f:('data -> 'data -> 'data)
          -> ('key, 'data, 'phantom) t
        , 'key
        , 'data
        , 'phantom )
        creator

    (** Like [of_alist]. Consumes a sequence. *)
    val of_sequence
      : ( ('key key * 'data) Sequence.t
          -> [ `Ok of ('key, 'data, 'phantom) t | `Duplicate_key of 'key key ]
        , 'key
        , 'data
        , 'phantom )
        creator

    (** Like [of_alist_or_error]. Consumes a sequence. *)
    val of_sequence_or_error
      : ( ('key key * 'data) Sequence.t -> ('key, 'data, 'phantom) t Or_error.t
        , 'key
        , 'data
        , 'phantom )
        creator

    (** Like [of_alist_exn]. Consumes a sequence. *)
    val of_sequence_exn
      : ( ('key key * 'data) Sequence.t -> ('key, 'data, 'phantom) t
        , 'key
        , 'data
        , 'phantom )
        creator

    (** Like [of_alist_multi]. Consumes a sequence. *)
    val of_sequence_multi
      : ( ('key key * 'data) Sequence.t -> ('key, 'data list, 'phantom) t
        , 'key
        , 'data
        , 'phantom )
        creator

    (** Like [of_alist_fold]. Consumes a sequence. *)
    val of_sequence_fold
      : ( ('key key * 'data) Sequence.t
          -> init:'c
          -> f:('c -> 'data -> 'c)
          -> ('key, 'c, 'phantom) t
        , 'key
        , 'data
        , 'phantom )
        creator

    (** Like [of_alist_reduce]. Consumes a sequence. *)
    val of_sequence_reduce
      : ( ('key key * 'data) Sequence.t
          -> f:('data -> 'data -> 'data)
          -> ('key, 'data, 'phantom) t
        , 'key
        , 'data
        , 'phantom )
        creator

    (** Like [of_alist]. Consume values for which keys can be computed. *)
    val of_list_with_key
      : ( 'data list
          -> get_key:('data -> 'key key)
          -> [ `Ok of ('key, 'data, 'phantom) t | `Duplicate_key of 'key key ]
        , 'key
        , 'data
        , 'phantom )
        creator

    (** Like [of_alist_or_error]. Consume values for which keys can be computed. *)
    val of_list_with_key_or_error
      : ( 'data list -> get_key:('data -> 'key key) -> ('key, 'data, 'phantom) t Or_error.t
        , 'key
        , 'data
        , 'phantom )
        creator

    (** Like [of_alist_exn]. Consume values for which keys can be computed. *)
    val of_list_with_key_exn
      : ( 'data list -> get_key:('data -> 'key key) -> ('key, 'data, 'phantom) t
        , 'key
        , 'data
        , 'phantom )
        creator

    (** Like [of_alist_multi]. Consume values for which keys can be computed. *)
    val of_list_with_key_multi
      : ( 'data list -> get_key:('data -> 'key key) -> ('key, 'data list, 'phantom) t
        , 'key
        , 'data
        , 'phantom )
        creator

    (** Produces a dictionary of all key/value pairs that [iteri] passes to [~f]. Fails if
        a duplicate key is found. *)
    val of_iteri
      : ( iteri:(f:(key:'key key -> data:'data -> unit) -> unit)
          -> [ `Ok of ('key, 'data, 'phantom) t | `Duplicate_key of 'key key ]
        , 'key
        , 'data
        , 'phantom )
        creator

    (** Like [of_iteri]. Raises on duplicate key. *)
    val of_iteri_exn
      : ( iteri:(f:(key:'key key -> data:'data -> unit) -> unit)
          -> ('key, 'data, 'phantom) t
        , 'key
        , 'data
        , 'phantom )
        creator
  end

  module type Creators1 = sig
    type key
    type 'data t

    (** @inline *)
    include
      Creators
        with type (_, 'data, _) t := 'data t
         and type _ key := key
         and type ('fn, _, _, _) creator := 'fn
  end

  module type Creators2 = sig
    type ('key, 'data) t
    type ('fn, 'key, 'data) creator

    (** @inline *)
    include
      Creators
        with type ('key, 'data, _) t := ('key, 'data) t
         and type 'key key := 'key
         and type ('fn, 'key, 'data, _) creator := ('fn, 'key, 'data) creator
  end

  module type Creators3 = sig
    type ('key, 'data, 'phantom) t
    type ('fn, 'key, 'data, 'phantom) creator

    (** @inline *)
    include
      Creators
        with type ('key, 'data, 'phantom) t := ('key, 'data, 'phantom) t
         and type 'key key := 'key
         and type ('fn, 'key, 'data, 'phantom) creator :=
          ('fn, 'key, 'data, 'phantom) creator
  end

  module type S = sig
    type 'key key
    type ('key, 'data, 'phantom) t
    type ('fn, 'key, 'data, 'phantom) accessor
    type ('fn, 'key, 'data, 'phantom) creator

    (** @inline *)
    include
      Accessors
        with type ('key, 'data, 'phantom) t := ('key, 'data, 'phantom) t
         and type 'key key := 'key key
         and type ('fn, 'key, 'data, 'phantom) accessor :=
          ('fn, 'key, 'data, 'phantom) accessor

    (** @inline *)
    include
      Creators
        with type ('key, 'data, 'phantom) t := ('key, 'data, 'phantom) t
         and type 'key key := 'key key
         and type ('fn, 'key, 'data, 'phantom) creator :=
          ('fn, 'key, 'data, 'phantom) creator
  end

  module type S1 = sig
    type key
    type 'data t

    (** @inline *)
    include
      S
        with type (_, 'data, _) t := 'data t
         and type _ key := key
         and type ('fn, _, _, _) accessor := 'fn
         and type ('fn, _, _, _) creator := 'fn
  end

  module type S2 = sig
    type ('key, 'data) t
    type ('fn, 'key, 'data) accessor
    type ('fn, 'key, 'data) creator

    (** @inline *)
    include
      S
        with type ('key, 'data, _) t := ('key, 'data) t
         and type 'key key := 'key
         and type ('fn, 'key, 'data, _) accessor := ('fn, 'key, 'data) accessor
         and type ('fn, 'key, 'data, _) creator := ('fn, 'key, 'data) creator
  end

  module type S3 = sig
    type ('key, 'data, 'phantom) t
    type ('fn, 'key, 'data, 'phantom) accessor
    type ('fn, 'key, 'data, 'phantom) creator

    (** @inline *)
    include
      S
        with type ('key, 'data, 'phantom) t := ('key, 'data, 'phantom) t
         and type 'key key := 'key
         and type ('fn, 'key, 'data, 'phantom) accessor :=
          ('fn, 'key, 'data, 'phantom) accessor
         and type ('fn, 'key, 'data, 'phantom) creator :=
          ('fn, 'key, 'data, 'phantom) creator
  end
end

module type Dictionary_immutable = sig
  (** @inline *)
  include module type of struct
    include Definitions (** @inline *)
  end
end
