open! Import

module type Key = sig
  type t [@@deriving_inline compare, sexp_of]
  include
  sig
    [@@@ocaml.warning "-32"]
    val compare : t -> t -> int
    val sexp_of_t : t -> Sexplib.Sexp.t
  end
  [@@@end]

  (** Values returned by [hash] must be non-negative.  An exception will be raised in the
      case that [hash] returns a negative value. *)
  val hash : t -> int
end

module Hashable = struct
  type 'a t =
    { hash : 'a -> int;
      compare : 'a -> 'a -> int;
      sexp_of_t : 'a -> Sexp.t;
    }

  let equal a b =
    phys_equal a b
    || (phys_equal a.hash b.hash
        && phys_equal a.compare b.compare
        && phys_equal a.sexp_of_t b.sexp_of_t)
  ;;

  let hash_param = Caml.Hashtbl.hash_param
  let hash       = Caml.Hashtbl.hash

  let poly = { hash;
               compare = Poly.compare;
               sexp_of_t = (fun _ -> Sexp.Atom "_");
             }

  let of_key (type a) (module Key : Key with type t = a) =
    { hash = Key.hash;
      compare = Key.compare;
      sexp_of_t = Key.sexp_of_t;
    }
  ;;

end

module type Hashable = sig
  type 'a t = 'a Hashable.t =
    { hash : 'a -> int;
      compare : 'a -> 'a -> int;
      sexp_of_t : 'a -> Sexp.t;
    }

  val poly : 'a t

  val of_key : (module Key with type t = 'a) -> 'a t

  val hash_param : int -> int -> 'a -> int

  val hash : 'a -> int
end

module type Accessors = sig
  type ('a, 'b) t
  type 'a key

  val sexp_of_key : ('a, _) t -> 'a key -> Sexp.t
  val clear : (_, _) t -> unit
  val copy : ('a, 'b) t -> ('a, 'b) t

  (** Attempting to modify ([set], [remove], etc.) the hashtable during iteration ([fold],
      [iter], [iter_keys], [iteri]) will raise an exception. *)
  val fold : ('a, 'b) t -> init:'c -> f:(key:'a key -> data:'b -> 'c -> 'c) -> 'c

  val iter_keys : ('a,  _) t -> f:(    'a key            -> unit) -> unit
  val iter      : ( _, 'b) t -> f:(                   'b -> unit) -> unit
  val iteri     : ('a, 'b) t -> f:(key:'a key -> data:'b -> unit) -> unit

  val existsi  : ('a, 'b) t -> f:(key:'a key -> data:'b -> bool) -> bool
  val exists   : (_ , 'b) t -> f:(                   'b -> bool) -> bool
  val for_alli : ('a, 'b) t -> f:(key:'a key -> data:'b -> bool) -> bool
  val for_all  : (_ , 'b) t -> f:(                   'b -> bool) -> bool
  val counti   : ('a, 'b) t -> f:(key:'a key -> data:'b -> bool) -> int
  val count    : (_ , 'b) t -> f:(                   'b -> bool) -> int

  val length : (_, _) t -> int
  val is_empty : (_, _) t -> bool
  val mem : ('a, _) t -> 'a key -> bool
  val remove : ('a, _) t -> 'a key -> unit

  val set          : ('a, 'b) t -> key:'a key -> data:'b -> unit

  (** [add] and [add_exn] leave the table unchanged if the key was already present. *)
  val add          : ('a, 'b) t -> key:'a key -> data:'b -> [ `Ok | `Duplicate ]
  val add_exn      : ('a, 'b) t -> key:'a key -> data:'b -> unit

  (** [change t key ~f] changes [t]'s value for [key] to be [f (find t key)]. *)
  val change : ('a, 'b) t -> 'a key -> f:('b option -> 'b option) -> unit

  (** [update t key ~f] is [change t key ~f:(fun o -> Some (f o))]. *)
  val update : ('a, 'b) t -> 'a key -> f:('b option -> 'b) -> unit

  (** [map t f] returns new table with bound values replaced by
      [f] applied to the bound values *)
  val map : ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t

  (** like [map], but function takes both key and data as arguments *)
  val mapi : ('a, 'b) t -> f:(key:'a key -> data:'b -> 'c) -> ('a, 'c) t

  (** returns new table with bound values filtered by f applied to the bound
      values *)
  val filter_map : ('a, 'b) t -> f:('b -> 'c option) -> ('a, 'c) t

  (** like [filter_map], but function takes both key and data as arguments*)
  val filter_mapi
    : ('a, 'b) t -> f:(key:'a key -> data:'b -> 'c option) -> ('a, 'c) t

  val filter_keys : ('a, 'b) t -> f:('a key -> bool) -> ('a, 'b) t
  val filter      : ('a, 'b) t -> f:('b -> bool) -> ('a, 'b) t
  val filteri     : ('a, 'b) t -> f:(key:'a key -> data:'b -> bool) -> ('a, 'b) t

  (** returns new tables with bound values partitioned by f applied to the bound values *)
  val partition_map
    :  ('a, 'b) t
    -> f:('b -> [`Fst of 'c | `Snd of 'd])
    -> ('a, 'c) t * ('a, 'd) t

  (** like [partition_map], but function takes both key and data as arguments*)
  val partition_mapi
    :  ('a, 'b) t
    -> f:(key:'a key -> data:'b -> [`Fst of 'c | `Snd of 'd])
    -> ('a, 'c) t * ('a, 'd) t

  (** returns a pair of tables [(t1, t2)], where [t1] contains all the elements of the
      initial table which satisfy the predicate [f], and [t2] contains all the elements
      which do not satisfy [f]. *)
  val partition_tf : ('a, 'b) t -> f:('b -> bool) -> ('a, 'b) t * ('a, 'b) t

  (** like [partition_tf], but function takes both key and data as arguments *)
  val partitioni_tf : ('a, 'b) t -> f:(key:'a key -> data:'b -> bool) -> ('a, 'b) t * ('a, 'b) t

  (** [find_or_add t k ~default] returns the data associated with key k if it
      is in the table t, otherwise it lets d = default() and adds it to the
      table. *)
  val find_or_add : ('a, 'b) t -> 'a key -> default:(unit -> 'b) -> 'b

  (** [find t k] returns Some (the current binding) of k in t, or None if no
      such binding exists *)
  val find : ('a, 'b) t -> 'a key -> 'b option

  (** [find_exn t k] returns the current binding of k in t, or raises Not_found
      if no such binding exists.*)
  val find_exn : ('a, 'b) t -> 'a key -> 'b

  (** [find_and_call t k ~if_found ~if_not_found]

      is equivalent to:

      [match find t k with Some v -> if_found v | None -> if_not_found k]

      except that it doesn't allocate the option. *)
  val find_and_call
    :  ('a, 'b) t
    -> 'a key
    -> if_found:('b -> 'c)
    -> if_not_found:('a key -> 'c)
    -> 'c

  (** [find_and_remove t k] returns Some (the current binding) of k in t and removes
      it, or None is no such binding exists *)
  val find_and_remove : ('a, 'b) t -> 'a key -> 'b option

  (** Merge two hashtables.

      The result of [merge f h1 h2] has as keys the set of all [k] in the
      union of the sets of keys of [h1] and [h2] for which [d(k)] is not
      None, where:

      d(k) =
      - f ~key:k (Some d1) None
      if [k] in [h1] is to d1, and [h2] does not map [k];

      - f ~key:k None (Some d2)
      if [k] in [h2] is to d2, and [h1] does not map [k];

      - f ~key:k (Some d1) (Some d2)
      otherwise, where [k] in [h1] is to [d1] and [k] in [h2] is to [d2].

      Each key [k] is mapped to a single piece of data x, where [d(k)] = Some x. *)
  val merge
    :  ('k, 'a) t
    -> ('k, 'b) t
    -> f:(key:'k key -> [ `Left of 'a | `Right of 'b | `Both of 'a * 'b ] -> 'c option)
    -> ('k, 'c) t

  (** Every [key] in [src] will be removed or set in [dst] according to the return value
      of [f]. *)
  type 'a merge_into_action = Remove | Set_to of 'a

  val merge_into
    :  src:('k, 'a) t
    -> dst:('k, 'b) t
    -> f:(key:'k key -> 'a -> 'b option -> 'b merge_into_action)
    -> unit

  (** Returns the list of all keys for given hashtable. *)
  val keys : ('a, _) t -> 'a key list

  (** Returns the list of all data for given hashtable. *)
  val data : (_, 'b) t -> 'b list

  (** [filter_inplace t ~f] removes all the elements from [t] that don't satisfy [f]. *)
  val filter_keys_inplace : ('a,  _) t -> f:('a key -> bool) -> unit
  val filter_inplace      : ( _, 'b) t -> f:('b -> bool) -> unit
  val filteri_inplace     : ('a, 'b) t -> f:(key:'a key -> data:'b -> bool) -> unit

  (** [map_inplace t ~f] applies f to all elements in [t], transforming them in place *)
  val map_inplace  : (_,  'b) t -> f:(                   'b -> 'b) -> unit
  val mapi_inplace : ('a, 'b) t -> f:(key:'a key -> data:'b -> 'b) -> unit

  (** [filter_map_inplace] combines the effects of [map_inplace] and [filter_inplace] *)
  val filter_map_inplace  : (_,  'b) t -> f:(                   'b -> 'b option) -> unit
  val filter_mapi_inplace : ('a, 'b) t -> f:(key:'a key -> data:'b -> 'b option) -> unit

  (** [equal t1 t2 f] and [similar t1 t2 f] both return true iff [t1] and [t2] have the
      same keys and for all keys [k], [f (find_exn t1 k) (find_exn t2 k)].  [equal] and
      [similar] only differ in their types. *)
  val equal   : ('a, 'b ) t -> ('a, 'b ) t -> ('b  -> 'b  -> bool) -> bool
  val similar : ('a, 'b1) t -> ('a, 'b2) t -> ('b1 -> 'b2 -> bool) -> bool

  (** Returns the list of all (key,data) pairs for given hashtable. *)
  val to_alist : ('a, 'b) t -> ('a key * 'b) list

  val validate
    :  name:('a key -> string)
    -> 'b Validate.check
    -> ('a, 'b) t Validate.check

  (** [remove_if_zero]'s default is [false]. *)
  val incr : ?by:int -> ?remove_if_zero:bool -> ('a, int) t -> 'a key -> unit
  val decr : ?by:int -> ?remove_if_zero:bool -> ('a, int) t -> 'a key -> unit
end

module type Multi = sig
  type ('a, 'b) t
  type 'a key

  (** [add_multi t ~key ~data] if [key] is present in the table then cons
      [data] on the list, otherwise add [key] with a single element list. *)
  val add_multi : ('a, 'b list) t -> key:'a key -> data:'b -> unit

  (** [remove_multi t key] updates the table, removing the head of the list bound to
      [key]. If the list has only one element (or is empty) then the binding is
      removed. *)
  val remove_multi : ('a, _ list) t -> 'a key -> unit

  (** [find_multi t key] returns the empty list if [key] is not present in the table,
      returns [t]'s values for [key] otherwise *)
  val find_multi : ('a, 'b list) t -> 'a key -> 'b list
end

module type Deprecated = sig
  type ('key, 'data) t
  type 'key key

  val iter_vals : ( _, 'b) t -> f:(                   'b -> unit) -> unit
  [@@deprecated "[since 2016-04] Use iter instead"]

  val replace      : ('a, 'b) t -> key:'a key -> data:'b -> unit
  [@@deprecated "[since 2015-10] Use set instead"]

  val replace_all : (_, 'b) t -> f:('b -> 'b) -> unit
  [@@deprecated "[since 2016-02] Use map_inplace instead"]
  val replace_alli : ('a, 'b) t -> f:(key:'a key -> data:'b -> 'b) -> unit
  [@@deprecated "[since 2016-02] Use mapi_inplace instead"]
  val filter_replace_all : (_, 'b) t -> f:('b -> 'b option) -> unit
  [@@deprecated "[since 2016-02] Use filter_map_inplace instead"]
  val filter_replace_alli : ('a, 'b) t -> f:(key:'a key -> data:'b -> 'b option) -> unit
  [@@deprecated "[since 2016-02] Use filter_mapi_inplace instead"]
end

type ('key, 'data, 'z) create_options_without_hashable =
  ?growth_allowed:bool (** defaults to [true] *)
  -> ?size:int (** initial size -- default 128 *)
  -> 'z

type ('key, 'data, 'z) create_options_with_hashable =
  ?growth_allowed:bool (** defaults to [true] *)
  -> ?size:int (** initial size -- default 128 *)
  -> hashable:'key Hashable.t
  -> 'z

type ('key, 'data, 'z) create_options_with_first_class_module =
  (module Key with type t = 'key)
  -> ?growth_allowed:bool (** defaults to [true] *)
  -> ?size:int (** initial size -- default 128 *)
  -> 'z

module type Creators = sig
  type ('a, 'b) t
  type 'a key
  type ('key, 'data, 'z) create_options

  val create : ('a key, 'b, unit -> ('a, 'b) t) create_options

  val of_alist
    :  ('a key,
        'b,
        ('a key * 'b) list
        -> [ `Ok of ('a, 'b) t
           | `Duplicate_key of 'a key
           ]) create_options

  val of_alist_report_all_dups
    : ('a key,
       'b,
       ('a key * 'b) list
       -> [ `Ok of ('a, 'b) t
          | `Duplicate_keys of 'a key list
          ]) create_options

  val of_alist_or_error : ('a key, 'b, ('a key * 'b) list -> ('a, 'b) t Or_error.t) create_options

  val of_alist_exn : ('a key, 'b, ('a key * 'b) list -> ('a, 'b) t) create_options

  val of_alist_multi : ('a key, 'b list, ('a key * 'b) list -> ('a, 'b list) t) create_options


  (** {[ create_mapped get_key get_data [x1,...,xn]
         = of_alist [get_key x1, get_data x1; ...; get_key xn, get_data xn]
     ]} *)
  val create_mapped
    : ('a key,
       'b,
       get_key:('r -> 'a key)
       -> get_data:('r -> 'b)
       -> 'r list
       -> [ `Ok of ('a, 'b) t
          | `Duplicate_keys of 'a key list ]) create_options

  (** {[ create_with_key ~get_key [x1,...,xn]
         = of_alist [get_key x1, x1; ...; get_key xn, xn] ]} *)
  val create_with_key
    : ('a key,
       'r,
       get_key:('r -> 'a key)
       -> 'r list
       -> [ `Ok of ('a, 'r) t
          | `Duplicate_keys of 'a key list ]) create_options

  val create_with_key_or_error
    : ('a key,
       'r,
       get_key:('r -> 'a key)
       -> 'r list
       -> ('a, 'r) t Or_error.t) create_options

  val create_with_key_exn
    : ('a key,
       'r,
       get_key:('r -> 'a key)
       -> 'r list
       -> ('a, 'r) t) create_options

  val group
    : ('a key,
       'b,
       get_key:('r -> 'a key)
       -> get_data:('r -> 'b)
       -> combine:('b -> 'b -> 'b)
       -> 'r list
       -> ('a, 'b) t) create_options
end

module type S_without_submodules = sig

  module Hashable : Hashable

  val hash : 'a -> int
  val hash_param : int -> int -> 'a -> int

  (** We use [[@@deriving_inline sexp_of][@@@end]] but not [[@@deriving sexp]] because we want people to
      be explicit about the hash and comparison functions used when creating hashtables.
      One can use [Hashtbl.Poly.t], which does have [[@@deriving_inline sexp][@@@end]], to use
      polymorphic comparison and hashing. *)
  type ('a, 'b) t [@@deriving_inline sexp_of]
  include
  sig
    [@@@ocaml.warning "-32"]
    val sexp_of_t :
      ('a -> Sexplib.Sexp.t) ->
      ('b -> Sexplib.Sexp.t) -> ('a,'b) t -> Sexplib.Sexp.t
  end
  [@@@end]

  include Invariant.S2 with type ('a, 'b) t := ('a, 'b) t

  include Creators
    with type ('a, 'b) t  := ('a, 'b) t
    with type 'a key = 'a
    with type ('a, 'b, 'z) create_options :=
      ('a, 'b, 'z) create_options_with_first_class_module
  (** @open *)

  include Accessors
    with type ('a, 'b) t := ('a, 'b) t
    with type 'a key := 'a key
  (** @open *)

  include Multi
    with type ('a, 'b) t := ('a, 'b) t
    with type 'a key := 'a key
  (** @open *)

  include Deprecated
    with type ('a, 'b) t := ('a, 'b) t
    with type 'a key := 'a key

  val hashable : ('key, _) t -> 'key Hashable.t

end

module type S_using_hashable = sig
  type 'a key
  type ('a, 'b) t [@@deriving_inline sexp_of]
  include
  sig
    [@@@ocaml.warning "-32"]
    val sexp_of_t :
      ('a -> Sexplib.Sexp.t) ->
      ('b -> Sexplib.Sexp.t) -> ('a,'b) t -> Sexplib.Sexp.t
  end
  [@@@end]
  include Creators
    with type ('a, 'b) t  := ('a, 'b) t
    with type 'a key := 'a key
    with type ('a, 'b, 'z) create_options := ('a, 'b, 'z) create_options_with_hashable
  include Accessors
    with type ('a, 'b) t := ('a, 'b) t
    with type 'a key := 'a key
  include Multi
    with type ('a, 'b) t := ('a, 'b) t
    with type 'a key := 'a key
  include Deprecated
    with type ('a, 'b) t := ('a, 'b) t
    with type 'a key := 'a key
end

module type S_poly = sig

  type ('a, 'b) t [@@deriving_inline sexp]
  include
  sig
    [@@@ocaml.warning "-32"]
    val t_of_sexp :
      (Sexplib.Sexp.t -> 'a) ->
      (Sexplib.Sexp.t -> 'b) -> Sexplib.Sexp.t -> ('a,'b) t
    val sexp_of_t :
      ('a -> Sexplib.Sexp.t) ->
      ('b -> Sexplib.Sexp.t) -> ('a,'b) t -> Sexplib.Sexp.t
  end
  [@@@end]

  val hashable : 'a Hashable.t

  include Invariant.S2 with type ('a, 'b) t := ('a, 'b) t

  include Creators
    with type ('a, 'b) t  := ('a, 'b) t
    with type 'a key = 'a
    with type ('key, 'data, 'z) create_options
    := ('key, 'data, 'z) create_options_without_hashable

  include Accessors
    with type ('a, 'b) t := ('a, 'b) t
    with type 'a key := 'a key

  include Multi
    with type ('a, 'b) t := ('a, 'b) t
    with type 'a key := 'a key

  include Deprecated
    with type ('a, 'b) t := ('a, 'b) t
    with type 'a key := 'a key
end

module type Hashtbl = sig
  include S_without_submodules (** @inline *)

  module Using_hashable : S_using_hashable
    with type ('a, 'b) t = ('a, 'b) t
    with type 'a key := 'a key
    with type 'a merge_into_action = 'a merge_into_action

  module Creators (Key : sig type 'a t val hashable : 'a t Hashable.t end) : sig
    type ('a, 'b) t_ = ('a Key.t, 'b) t
    val t_of_sexp : (Sexp.t -> 'a Key.t) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t_
    include Creators
      with type ('a, 'b) t := ('a, 'b) t_
      with type 'a key := 'a Key.t
      with type ('key, 'data, 'a) create_options :=
        ('key, 'data, 'a) create_options_without_hashable
  end

  module Poly : S_poly with type ('a, 'b) t = ('a, 'b) t

  (** [M] is meant to be used in combination with OCaml applicative functor types:

      {[
        type string_to_int_table = int Hashtbl.M(String).t
      ]}

      which stands for:

      {[
        type string_to_int_table = (String.t, int) Hashtbl.t
      ]}

      The point is that [int Hashtbl.M(String).t] supports deriving, whereas the second
      syntax doesn't (because [t_of_sexp] doesn't know what comparison/hash function to
      use). *)
  module M (K : T.T) : sig
    type nonrec 'v t = (K.t, 'v) t
  end
  module type Sexp_of_m = sig type t [@@deriving_inline sexp_of]
    include sig [@@@ocaml.warning "-32"] val sexp_of_t : t -> Sexplib.Sexp.t end
    [@@@end] end
  module type M_of_sexp = sig
    type t [@@deriving_inline of_sexp]
    include sig [@@@ocaml.warning "-32"] val t_of_sexp : Sexplib.Sexp.t -> t end
    [@@@end] include Key with type t := t
  end
  val sexp_of_m__t
    :  (module Sexp_of_m with type t = 'k)
    -> ('v -> Sexp.t)
    -> ('k, 'v) t
    -> Sexp.t
  val m__t_of_sexp
    :  (module M_of_sexp with type t = 'k)
    -> (Sexp.t -> 'v)
    -> Sexp.t
    -> ('k, 'v) t
end
