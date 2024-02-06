(** Interfaces for mutable dictionary types, such as [Hashtbl.t].

    We define separate interfaces for [Accessors] and [Creators], along with [S] combining
    both. These interfaces are written once in their most general form, which involves
    extra type definitions and type parameters that most instances do not need.

    We then provide instantiations of these interfaces with 1, 2, and 3 type parameters
    for [t]. These cover more common usage patterns for the interfaces. *)

open! Import

(** These definitions are re-exported by [Dictionary_mutable]. *)
module Definitions = struct
  (** @canonical Base.Dictionary_mutable.Merge_into_action *)
  module Merge_into_action = struct
    type 'data t =
      | Remove
      | Set_to of 'data
  end

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
    val is_empty : (_, _, 'phantom) t -> bool

    (** How many key/value pairs the dictionary contains. *)
    val length : (_, _, 'phantom) t -> int

    (** All key/value pairs. *)
    val to_alist : ('key, 'data, 'phantom) t -> ('key key * 'data) list

    (** All keys in the dictionary, in the same order as [to_alist]. *)
    val keys : ('key, _, 'phantom) t -> 'key key list

    (** All values in the dictionary, in the same order as [to_alist]. *)
    val data : (_, 'data, 'phantom) t -> 'data list

    (** Removes all key/value pairs from the dictionary. *)
    val clear : (_, _, 'phantom) t -> unit

    (** A new dictionary containing the same key/value pairs. *)
    val copy : ('key, 'data, 'phantom) t -> ('key, 'data, 'phantom) t

    (** Whether [key] has a value. *)
    val mem
      : (('key, 'data, 'phantom) t -> 'key key -> bool, 'key, 'data, 'phantom) accessor

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

    (** Like [find]. Adds the value [default ()] if none exists, then returns it. *)
    val find_or_add
      : ( ('key, 'data, 'phantom) t -> 'key key -> default:(unit -> 'data) -> 'data
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Like [find]. Adds [default key] if no value exists. *)
    val findi_or_add
      : ( ('key, 'data, 'phantom) t -> 'key key -> default:('key key -> 'data) -> 'data
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Like [find]. Calls [if_found data] if a value exists, or [if_not_found key]
        otherwise. Avoids allocation [Some]. *)
    val find_and_call
      : ( ('key, 'data, 'phantom) t
          -> 'key key
          -> if_found:('data -> 'c)
          -> if_not_found:('key key -> 'c)
          -> 'c
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Like [findi]. Calls [if_found ~key ~data] if a value exists. *)
    val findi_and_call
      : ( ('key, 'data, 'phantom) t
          -> 'key key
          -> if_found:(key:'key key -> data:'data -> 'c)
          -> if_not_found:('key key -> 'c)
          -> 'c
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Like [find]. Removes the value for [key], if any, from the dictionary before
        returning it. *)
    val find_and_remove
      : ( ('key, 'data, 'phantom) t -> 'key key -> 'data option
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Adds a key/value pair for a key the dictionary does not contain, or reports a
        duplicate. *)
    val add
      : ( ('key, 'data, 'phantom) t -> key:'key key -> data:'data -> [ `Ok | `Duplicate ]
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Like [add]. Raises on duplicates. *)
    val add_exn
      : ( ('key, 'data, 'phantom) t -> key:'key key -> data:'data -> unit
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Adds or replaces a key/value pair in the dictionary. *)
    val set
      : ( ('key, 'data, 'phantom) t -> key:'key key -> data:'data -> unit
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Removes any value for the given key. *)
    val remove
      : (('key, 'data, 'phantom) t -> 'key key -> unit, 'key, 'data, 'phantom) accessor

    (** Adds, replaces, or removes the value for a given key, depending on its current
        value or lack thereof. *)
    val change
      : ( ('key, 'data, 'phantom) t -> 'key key -> f:('data option -> 'data option) -> unit
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Adds or replaces the value for a given key, depending on its current value or
        lack thereof. *)
    val update
      : ( ('key, 'data, 'phantom) t -> 'key key -> f:('data option -> 'data) -> unit
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Like [update]. Returns the new value. *)
    val update_and_return
      :  ('key, 'data, 'phantom) t
      -> 'key key
      -> f:('data option -> 'data)
      -> 'data

    (** Adds [by] to the value for [key], default 0 if [key] is absent. May remove [key]
        if the result is [0], depending on [remove_if_zero]. *)
    val incr
      : ( ?by:int (** default: 1 *)
          -> ?remove_if_zero:bool (** default: false *)
          -> ('key, int, 'phantom) t
          -> 'key key
          -> unit
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Subtracts [by] from the value for [key], default 0 if [key] is absent. May remove
        [key] if the result is [0], depending on [remove_if_zero]. *)
    val decr
      : ( ?by:int (** default: 1 *)
          -> ?remove_if_zero:bool (** default: false *)
          -> ('key, int, 'phantom) t
          -> 'key key
          -> unit
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Adds [data] to the existing key/value pair for [key]. Interprets a missing key as
        having an empty list. *)
    val add_multi
      : ( ('key, 'data list, 'phantom) t -> key:'key key -> data:'data -> unit
        , 'key
        , 'data
        , 'phantom )
        accessor

    (** Removes one element from the existing key/value pair for [key]. Removes the key
        entirely if the new list is empty. *)
    val remove_multi
      : (('key, _ list, 'phantom) t -> 'key key -> unit, 'key, 'data, 'phantom) accessor

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
      :  ('key, 'data, 'phantom) t
      -> init:'acc
      -> f:(key:'key key -> data:'data -> 'acc -> 'acc)
      -> 'acc

    (** Whether every value satisfies [f]. *)
    val for_all : (_, 'data, 'phantom) t -> f:('data -> bool) -> bool

    (** Like [for_all]. The predicate may also depend on the associated key. *)
    val for_alli
      :  ('key, 'data, 'phantom) t
      -> f:(key:'key key -> data:'data -> bool)
      -> bool

    (** Whether at least one value satisfies [f]. *)
    val exists : (_, 'data, 'phantom) t -> f:('data -> bool) -> bool

    (** Like [exists]. The predicate may also depend on the associated key. *)
    val existsi
      :  ('key, 'data, 'phantom) t
      -> f:(key:'key key -> data:'data -> bool)
      -> bool

    (** How many values satisfy [f]. *)
    val count : (_, 'data, 'phantom) t -> f:('data -> bool) -> int

    (** Like [count]. The predicate may also depend on the associated key. *)
    val counti
      :  ('key, 'data, 'phantom) t
      -> f:(key:'key key -> data:'data -> bool)
      -> int

    (** Arbitrary, deterministic key/value pair if non-empty. *)
    val choose : ('key, 'data, 'phantom) t -> ('key key * 'data) option

    (** Like [choose]. Raises if empty. *)
    val choose_exn : ('key, 'data, 'phantom) t -> 'key key * 'data

    (** Arbitrary, pseudo-random key/value pair if non-empty. *)
    val choose_randomly
      :  ?random_state:Random.State.t
      -> ('key, 'data, 'phantom) t
      -> ('key key * 'data) option

    (** Like [choose_randomly]. Raises if empty. *)
    val choose_randomly_exn
      :  ?random_state:Random.State.t
      -> ('key, 'data, 'phantom) t
      -> 'key key * 'data

    (** Calls [f] for every key. *)
    val iter_keys : ('key, _, 'phantom) t -> f:('key key -> unit) -> unit

    (** Calls [f] for every value. *)
    val iter : (_, 'data, 'phantom) t -> f:('data -> unit) -> unit

    (** Calls [f] for every key/value pair. *)
    val iteri
      :  ('key, 'data, 'phantom) t
      -> f:(key:'key key -> data:'data -> unit)
      -> unit

    (** Transforms every value. *)
    val map : ('key, 'data, 'phantom) t -> f:('data -> 'c) -> ('key, 'c, 'phantom) t

    (** Like [map]. The transformation may also depend on the associated key. *)
    val mapi
      :  ('key, 'data, 'phantom) t
      -> f:(key:'key key -> data:'data -> 'c)
      -> ('key, 'c, 'phantom) t

    (** Like [map]. Modifies the input. *)
    val map_inplace : (_, 'data, 'phantom) t -> f:('data -> 'data) -> unit

    (** Like [mapi]. Modifies the input. *)
    val mapi_inplace
      :  ('key, 'data, 'phantom) t
      -> f:(key:'key key -> data:'data -> 'data)
      -> unit

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

    (** Like [filter_keys]. Modifies the input. *)
    val filter_keys_inplace : ('key, _, 'phantom) t -> f:('key key -> bool) -> unit

    (** Like [filter]. Modifies the input. *)
    val filter_inplace : (_, 'data, 'phantom) t -> f:('data -> bool) -> unit

    (** Like [filteri]. Modifies the input. *)
    val filteri_inplace
      :  ('key, 'data, 'phantom) t
      -> f:(key:'key key -> data:'data -> bool)
      -> unit

    (** Produces key/value pairs for which [f] produces [Some]. *)
    val filter_map
      :  ('key, 'data, 'phantom) t
      -> f:('data -> 'c option)
      -> ('key, 'c, 'phantom) t

    (** Like [filter_map]. The new value may also depend on the associated key. *)
    val filter_mapi
      :  ('key, 'data, 'phantom) t
      -> f:(key:'key key -> data:'data -> 'c option)
      -> ('key, 'c, 'phantom) t

    (** Like [filter_map]. Modifies the input. *)
    val filter_map_inplace : (_, 'data, 'phantom) t -> f:('data -> 'data option) -> unit

    (** Like [filter_mapi]. Modifies the input. *)
    val filter_mapi_inplace
      :  ('key, 'data, 'phantom) t
      -> f:(key:'key key -> data:'data -> 'data option)
      -> unit

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
      :  ('key, 'data, 'phantom) t
      -> f:('data -> ('c, 'd) Either.t)
      -> ('key, 'c, 'phantom) t * ('key, 'd, 'phantom) t

    (** Like [partition_map]. The split may also depend on the associated key. *)
    val partition_mapi
      :  ('key, 'data, 'phantom) t
      -> f:(key:'key key -> data:'data -> ('c, 'd) Either.t)
      -> ('key, 'c, 'phantom) t * ('key, 'd, 'phantom) t

    (** Merges two dictionaries by fully traversing both. Not suitable for efficiently
        merging lists of dictionaries. See [merge_into] instead. *)
    val merge
      : ( ('key, 'data1, 'phantom) t
          -> ('key, 'data2, 'phantom) t
          -> f:
               (key:'key key
                -> [ `Left of 'data1 | `Right of 'data2 | `Both of 'data1 * 'data2 ]
                -> 'data3 option)
          -> ('key, 'data3, 'phantom) t
        , 'key
        , 'data3
        , 'phantom )
        accessor

    (** Merges two dictionaries by traversing [src] and adding to [dst]. Computes the
        effect on [dst] of each key/value pair in [src] using [f]. *)
    val merge_into
      : ( src:('key, 'data1, 'phantom) t
          -> dst:('key, 'data2, 'phantom) t
          -> f:(key:'key key -> 'data1 -> 'data2 option -> 'data2 Merge_into_action.t)
          -> unit
        , 'key
        , 'data
        , 'phantom )
        accessor
  end

  module type Accessors1 = sig
    type key
    type 'data t

    include
      Accessors
        with type (_, 'data, _) t := 'data t
         and type _ key := key
         and type ('fn, _, _, _) accessor := 'fn
  end

  module type Accessors2 = sig
    type ('key, 'data) t
    type ('fn, 'key, 'data) accessor

    include
      Accessors
        with type ('key, 'data, _) t := ('key, 'data) t
         and type 'key key := 'key
         and type ('fn, 'key, 'data, _) accessor := ('fn, 'key, 'data) accessor
  end

  module type Accessors3 = sig
    type ('key, 'data, 'phantom) t
    type ('fn, 'key, 'data, 'phantom) accessor

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

    (** Creates a new empty dictionary. *)
    val create : (unit -> ('key, 'data, 'phantom) t, 'key, 'data, 'phantom) creator

    (** Dictionary containing the given key/value pairs. Fails if there are duplicate
        keys. *)
    val of_alist
      : ( ('key key * 'data) list
          -> [ `Ok of ('key, 'data, 'phantom) t | `Duplicate_key of 'key key ]
        , 'key
        , 'data
        , 'phantom )
        creator

    (** Like [of_alist]. On failure, provides all duplicate keys instead of a single
        representative. *)
    val of_alist_report_all_dups
      : ( ('key key * 'data) list
          -> [ `Ok of ('key, 'data, 'phantom) t | `Duplicate_keys of 'key key list ]
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
        , 'data list
        , 'phantom )
        creator

    (** Like [of_alist]. Consume a list of elements for which key/value pairs can be
        computed. *)
    val create_mapped
      : ( get_key:('a -> 'key key)
          -> get_data:('a -> 'data)
          -> 'a list
          -> [ `Ok of ('key, 'data, 'phantom) t | `Duplicate_keys of 'key key list ]
        , 'key
        , 'data
        , 'phantom )
        creator

    (** Like [of_alist]. Consume values for which keys can be computed. *)
    val create_with_key
      : ( get_key:('data -> 'key key)
          -> 'data list
          -> [ `Ok of ('key, 'data, 'phantom) t | `Duplicate_keys of 'key key list ]
        , 'key
        , 'data
        , 'phantom )
        creator

    (** Like [of_alist_or_error]. Consume values for which keys can be computed. *)
    val create_with_key_or_error
      : ( get_key:('data -> 'key key) -> 'data list -> ('key, 'data, 'phantom) t Or_error.t
        , 'key
        , 'data
        , 'phantom )
        creator

    (** Like [of_alist_exn]. Consume values for which keys can be computed. *)
    val create_with_key_exn
      : ( get_key:('data -> 'key key) -> 'data list -> ('key, 'data, 'phantom) t
        , 'key
        , 'data
        , 'phantom )
        creator

    (** Like [create_mapped]. Multiple values for a key are [combine]d rather than
        producing an error. *)
    val group
      : ( get_key:('a -> 'key key)
          -> get_data:('a -> 'data)
          -> combine:('data -> 'data -> 'data)
          -> 'a list
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

module type Dictionary_mutable = sig
  (** @inline *)
  include module type of struct
    include Definitions (** @inline *)
  end
end
