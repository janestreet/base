open! Import
module Sexp = Sexp0

module Definitions = struct
  (** @canonical Base.Hashtbl.Key *)
  module%template Key = struct
    [@@@kind.default k = (value, bits64, float64)]
    [@@@mode.default m = (local, global), p = (nonportable, portable)]

    module type S = sig
      type t [@@deriving sexp_of]

      val compare : t -> t -> int [@@mode m = (global, m)]

      (** Two [t]s that [compare] equal must have equal hashes for the hashtable to behave
          properly. *)
      val hash : t -> int
    end

    type 'a t = ((module S with type t = 'a)[@kind k] [@mode m p])
  end

  module Merge_into_action = Dictionary_mutable.Merge_into_action

  module type Accessors = sig
    (** {2 Accessors} *)

    type ('a, 'b) t
    type 'a key

    (** Attempting to modify ([set], [remove], etc.) the hashtable during iteration
        ([fold], [iter], [iter_keys], [iteri]) will raise an exception. *)

    (** @inline *)
    include
      Dictionary_mutable.Accessors
      with type 'key key := 'key key
       and type ('key, 'data, _) t := ('key, 'data) t
       and type ('fn, _, _, _) accessor := 'fn

    val sexp_of_key : ('a, _) t -> 'a key -> Sexp.t
    val capacity : _ t -> int
    val growth_allowed : _ t -> bool

    (** We redeclare [choose*] below to add implementation-specific notes on performance. *)

    (** Choose an arbitrary key/value pair of a hash table. Returns [None] if [t] is
        empty.

        The choice is deterministic. Calling [choose] multiple times on the same table
        returns the same key/value pair, so long as the table is not mutated in between.
        Beyond determinism, no guarantees are made about how the choice is made. Expect
        bias toward certain hash values.

        This hash bias can lead to degenerate performance in some cases, such as clearing
        a hash table using repeated [choose] and [remove]. At each iteration, finding the
        next element may have to scan farther from its initial hash value. *)
    val%template choose : ('a, 'b) t -> ('a key * 'b) option
    [@@mode m = global]

    (** Like [choose], but returns a local pair option *)
    val%template choose : ('a, 'b) t -> ('a key Modes.Global.t * 'b Modes.Global.t) option
    [@@mode m = local]

    (** Like [choose]. Raises if [t] is empty. *)
    val choose_exn : ('a, 'b) t -> 'a key * 'b

    (** Chooses a random key/value pair of a hash table. Returns [None] if [t] is empty.

        The choice is distributed uniformly across hash values, rather than across keys
        themselves. As a consequence, the closer the keys are to evenly spaced out in the
        table, the closer this function will be to a uniform choice of keys.

        This function may be preferable to [choose] when nondeterministic choice is
        acceptable, and bias toward certain hash values is undesirable. *)
    val choose_randomly
      :  ?random_state:Random.State.t (** default: [Random.State.default] *)
      -> ('a, 'b) t
      -> ('a key * 'b) option

    (** Like [choose_randomly]. Raises if [t] is empty. *)
    val choose_randomly_exn
      :  ?random_state:Random.State.t (** default: [Random.State.default] *)
      -> ('a, 'b) t
      -> 'a key * 'b

    (** [find_or_null] returns [This data] if the key is found, [Null] if not. *)
    val find_or_null : ('a, 'b) t -> 'a key -> 'b or_null

    (** Just like [find_and_call], but takes an extra argument which is passed to
        [if_found] and [if_not_found], so that the client code can avoid allocating
        closures or using refs to pass this additional information. This function is only
        useful in code which tries to minimize heap allocation. *)
    val find_and_call1
      : 'a 'b 'c 'd.
      ('a, 'b) t
      -> 'a key
      -> a:'d
      -> if_found:('b -> 'd -> 'c)
      -> if_not_found:('a key -> 'd -> 'c)
      -> 'c

    val find_and_call2
      : 'a 'b 'c 'd 'e.
      ('a, 'b) t
      -> 'a key
      -> a:'d
      -> b:'e
      -> if_found:('b -> 'd -> 'e -> 'c)
      -> if_not_found:('a key -> 'd -> 'e -> 'c)
      -> 'c

    val findi_and_call1
      : 'a 'b 'c 'd.
      ('a, 'b) t
      -> 'a key
      -> a:'d
      -> if_found:(key:'a key -> data:'b -> 'd -> 'c)
      -> if_not_found:('a key -> 'd -> 'c)
      -> 'c

    val findi_and_call2
      : 'a 'b 'c 'd 'e.
      ('a, 'b) t
      -> 'a key
      -> a:'d
      -> b:'e
      -> if_found:(key:'a key -> data:'b -> 'd -> 'e -> 'c)
      -> if_not_found:('a key -> 'd -> 'e -> 'c)
      -> 'c

    (** [equal f t1 t2] and [similar f t1 t2] both return true iff [t1] and [t2] have the
        same keys and for all keys [k], [f (find_exn t1 k) (find_exn t2 k)]. [equal] and
        [similar] only differ in their types. *)
    val equal : ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool

    val similar : ('b1 -> 'b2 -> bool) -> ('a, 'b1) t -> ('a, 'b2) t -> bool
  end

  module type Multi = sig
    type ('a, 'b) t
    type 'a key

    (** [add_multi t ~key ~data] if [key] is present in the table then cons [data] on the
        list, otherwise add [key] with a single element list. *)
    val add_multi : ('a, 'b list) t -> key:'a key -> data:'b -> unit

    (** [remove_multi t key] updates the table, removing the head of the list bound to
        [key]. If the list has only one element (or is empty) then the binding is removed. *)
    val remove_multi : ('a, _ list) t -> 'a key -> unit

    (** [find_multi t key] returns the empty list if [key] is not present in the table,
        returns [t]'s values for [key] otherwise. *)
    val find_multi : ('a, 'b list) t -> 'a key -> 'b list
  end

  type ('key, 'data, 'z) create_options =
    ?growth_allowed:bool (** defaults to [true] *)
    -> ?size:int (** initial size -- default 0 *)
    -> 'key Key.t
    -> 'z

  type ('key, 'data, 'z) create_options_without_first_class_module =
    ?growth_allowed:bool (** defaults to [true] *)
    -> ?size:int (** initial size -- default 0 *)
    -> 'z

  module type Creators_generic = sig
    type ('a, 'b) t
    type 'a key
    type ('key, 'data, 'z) create_options

    (** @inline *)
    include
      Dictionary_mutable.Creators
      with type 'key key := 'key key
       and type ('key, 'data, _) t := ('key, 'data) t
       and type ('fn, 'key, 'data, _) creator := ('key key, 'data, 'fn) create_options
  end

  module type Creators = sig
    type ('a, 'b) t

    (** {2 Creators} *)

    (** The module you pass to [create] must have a type that is hashable, sexpable, and
        comparable.

        Example:

        {v
        Hashtbl.create (module Int);;
        - : (int, '_a) Hashtbl.t = <abstr>;;
        v} *)
    val create
      :  ?growth_allowed:bool (** defaults to [true] *)
      -> ?size:int (** initial size -- default 0 *)
      -> 'a Key.t
      -> ('a, 'b) t

    (** Example:

        {v
         Hashtbl.of_alist (module Int) [(3, "something"); (2, "whatever")]
         - : [ `Duplicate_key of int | `Ok of (int, string) Hashtbl.t ] = `Ok <abstr>
        v} *)
    val of_alist
      :  ?growth_allowed:bool (** defaults to [true] *)
      -> ?size:int (** initial size -- default 0 *)
      -> 'a Key.t
      -> ('a * 'b) list
      -> [ `Ok of ('a, 'b) t | `Duplicate_key of 'a ]

    (** Whereas [of_alist] will report [Duplicate_key] no matter how many dups there are
        in your list, [of_alist_report_all_dups] will report each and every duplicate
        entry.

        For example:

        {v
        Hashtbl.of_alist (module Int) [(1, "foo"); (1, "bar"); (2, "foo"); (2, "bar")];;
        - : [ `Duplicate_key of int | `Ok of (int, string) Hashtbl.t ] = `Duplicate_key 1

        Hashtbl.of_alist_report_all_dups (module Int) [(1, "foo"); (1, "bar"); (2, "foo"); (2, "bar")];;
        - : [ `Duplicate_keys of int list | `Ok of (int, string) Hashtbl.t ] = `Duplicate_keys [1; 2]
        v} *)
    val of_alist_report_all_dups
      :  ?growth_allowed:bool (** defaults to [true] *)
      -> ?size:int (** initial size -- default 0 *)
      -> 'a Key.t
      -> ('a * 'b) list
      -> [ `Ok of ('a, 'b) t | `Duplicate_keys of 'a list ]

    val of_alist_or_error
      :  ?growth_allowed:bool (** defaults to [true] *)
      -> ?size:int (** initial size -- default 0 *)
      -> 'a Key.t
      -> ('a * 'b) list
      -> ('a, 'b) t Or_error.t

    val of_alist_exn
      :  ?growth_allowed:bool (** defaults to [true] *)
      -> ?size:int (** initial size -- default 0 *)
      -> 'a Key.t
      -> ('a * 'b) list
      -> ('a, 'b) t

    (** Creates a {{!Multi} "multi"} hashtable, i.e., a hashtable where each key points to
        a list potentially containing multiple values. So instead of short-circuiting with
        a [`Duplicate_key] variant on duplicates, as in [of_alist], [of_alist_multi] folds
        those values into a list for the given key:

        {v
      let h = Hashtbl.of_alist_multi (module Int) [(1, "a"); (1, "b"); (2, "c"); (2, "d")];;
      val h : (int, string list) Hashtbl.t = <abstr>

      Hashtbl.find_exn h 1;;
      - : string list = ["b"; "a"]
        v} *)
    val of_alist_multi
      :  ?growth_allowed:bool (** defaults to [true] *)
      -> ?size:int (** initial size -- default 0 *)
      -> 'a Key.t
      -> ('a * 'b) list
      -> ('a, 'b list) t

    (** Applies the [get_key] and [get_data] functions to the ['r list] to create the
        initial keys and values, respectively, for the new hashtable.

        {[
          create_mapped get_key get_data [x1;...;xn]
          = of_alist [get_key x1, get_data x1; ...; get_key xn, get_data xn]
        ]}

        Example:

        {v
        let h =
          Hashtbl.create_mapped (module Int)
            ~get_key:(local_ (fun x -> x))
            ~get_data:(local_ (fun x -> x + 1))
           [1; 2; 3];;
        val h : [ `Duplicate_keys of int list | `Ok of (int, int) Hashtbl.t ] = `Ok <abstr>

        let h =
          match h with
          | `Ok x -> x
          | `Duplicate_keys _ -> failwith ""
        in
        Hashtbl.find_exn h 1;;
        - : int = 2
        v} *)
    val create_mapped
      :  ?growth_allowed:bool (** defaults to [true] *)
      -> ?size:int (** initial size -- default 0 *)
      -> 'a Key.t
      -> get_key:('r -> 'a)
      -> get_data:('r -> 'b)
      -> 'r list
      -> [ `Ok of ('a, 'b) t | `Duplicate_keys of 'a list ]

    (** {[
          create_with_key ~get_key [x1;...;xn]
          = of_alist [get_key x1, x1; ...; get_key xn, xn]
        ]} *)
    val create_with_key
      :  ?growth_allowed:bool (** defaults to [true] *)
      -> ?size:int (** initial size -- default 0 *)
      -> 'a Key.t
      -> get_key:('r -> 'a)
      -> 'r list
      -> [ `Ok of ('a, 'r) t | `Duplicate_keys of 'a list ]

    val create_with_key_or_error
      :  ?growth_allowed:bool (** defaults to [true] *)
      -> ?size:int (** initial size -- default 0 *)
      -> 'a Key.t
      -> get_key:('r -> 'a)
      -> 'r list
      -> ('a, 'r) t Or_error.t

    val create_with_key_exn
      :  ?growth_allowed:bool (** defaults to [true] *)
      -> ?size:int (** initial size -- default 0 *)
      -> 'a Key.t
      -> get_key:('r -> 'a)
      -> 'r list
      -> ('a, 'r) t

    (** Like [create_mapped], applies the [get_key] and [get_data] functions to the
        ['r list] to create the initial keys and values, respectively, for the new
        hashtable -- and then, like [add_multi], folds together values belonging to the
        same keys. Here, though, the function used for the folding is given by [combine]
        (instead of just being a [cons]).

        Example:

        {v
         Hashtbl.group (module Int)
           ~get_key:(local_ (fun x -> x / 2))
           ~get_data:(local_ (fun x -> x))
           ~combine:(local_ (fun x y -> x * y))
            [ 1; 2; 3; 4]
         |> Hashtbl.to_alist;;
         - : (int * int) list = [(2, 4); (1, 6); (0, 1)]
        v} *)
    val group
      :  ?growth_allowed:bool (** defaults to [true] *)
      -> ?size:int (** initial size -- default 0 *)
      -> 'a Key.t
      -> get_key:('r -> 'a)
      -> get_data:('r -> 'b)
      -> combine:('b -> 'b -> 'b)
      -> 'r list
      -> ('a, 'b) t
  end

  module type S_without_submodules = sig
    val hash : 'a -> int
    val hash_param : int -> int -> 'a -> int

    type (!'a, !'b) t

    (** We provide a [sexp_of_t] but not a [t_of_sexp] for this type because one needs to
        be explicit about the hash and comparison functions used when creating a
        hashtable. Note that [Hashtbl.Poly.t] does have [[@@deriving sexp]], and uses
        OCaml's built-in polymorphic comparison and and polymorphic hashing. *)
    val sexp_of_t : ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) t -> Sexp.t

    include Creators with type ('a, 'b) t := ('a, 'b) t (** @inline *)

    include Accessors with type ('a, 'b) t := ('a, 'b) t with type 'a key = 'a
    (** @inline *)

    include Multi with type ('a, 'b) t := ('a, 'b) t with type 'a key := 'a key
    (** @inline *)

    val hashable_s : ('key, _) t -> 'key Key.t

    include Invariant.S2 with type ('a, 'b) t := ('a, 'b) t
  end

  module type S_poly = sig
    type ('a, 'b) t [@@deriving sexp, sexp_grammar]

    val hashable : 'a Hashable.t

    include Invariant.S2 with type ('a, 'b) t := ('a, 'b) t

    include
      Creators_generic
      with type ('a, 'b) t := ('a, 'b) t
      with type 'a key = 'a
      with type ('key, 'data, 'z) create_options :=
        ('key, 'data, 'z) create_options_without_first_class_module

    include Accessors with type ('a, 'b) t := ('a, 'b) t with type 'a key := 'a key
    include Multi with type ('a, 'b) t := ('a, 'b) t with type 'a key := 'a key
  end

  module type For_deriving = sig
    type ('k, 'v) t

    module type Sexp_of_m = sig
      type t [@@deriving sexp_of]
    end

    module type M_of_sexp = sig
      type t [@@deriving of_sexp]

      include Key.S with type t := t
    end

    module type M_sexp_grammar = sig
      type t [@@deriving sexp_grammar]
    end

    module type Equal_m = sig end

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

    val m__t_sexp_grammar
      :  (module M_sexp_grammar with type t = 'k)
      -> 'v Sexplib0.Sexp_grammar.t
      -> ('k, 'v) t Sexplib0.Sexp_grammar.t

    val equal_m__t
      :  (module Equal_m)
      -> ('v -> 'v -> bool)
      -> ('k, 'v) t
      -> ('k, 'v) t
      -> bool
  end

  module type%template Non_value = sig
    include sig
      type (!'a, !'b) t
      [@@deriving sexp_of]
      [@@kind k = (float64, bits64, value_or_null), v = (float64, bits64, value_or_null)]

      [@@@kind k = (float64, bits64, value_or_null), v = (float64, bits64, value_or_null)]

      type (!'a, !'b) t := (('a, 'b) t[@kind k v])
      type 'a key := ('a Key.t[@kind k] [@mode p]) [@@mode p = (nonportable, portable)]

      [@@@kind.default k v]

      val create
        :  ?growth_allowed:bool (** defaults to [true] *)
        -> ?size:int (** initial size -- default 0 *)
        -> ('a key[@mode p])
        -> ('a, 'b) t
      [@@mode p = (nonportable, portable)]

      val singleton
        :  ?growth_allowed:bool
        -> ?size:int
        -> 'k key
        -> 'k
        -> 'v
        -> ('k, 'v) t

      val length : ('k, 'v) t -> int [@@mode c = (uncontended, shared)]
      val capacity : ('k, 'v) t -> int [@@mode c = (uncontended, shared)]
      val growth_allowed : ('k, 'v) t -> bool [@@mode c = (uncontended, contended)]
      val is_empty : ('k, 'v) t -> bool [@@mode c = (uncontended, shared)]
      val add_exn : ('k, 'v) t -> key:'k -> data:'v -> unit
      val add : ('k, 'v) t -> key:'k -> data:'v -> [ `Ok | `Duplicate ]

      val find : ('k, 'v) t -> 'k -> ('v Option.t[@kind v])
      [@@mode c = (uncontended, shared)]

      val find_exn : ('k, 'v) t -> 'k -> 'v
      val find_and_remove : ('k, 'v) t -> 'k -> ('v Option.t[@kind v])
      val remove : ('k, 'v) t -> 'k -> unit
      val mem : 'k 'v. ('k, 'v) t -> 'k -> bool [@@mode c = (uncontended, shared)]
      val set : ('k, 'v) t -> key:'k -> data:'v -> unit
      val clear : _ t -> unit

      val find_and_call
        : 'k 'v 'a.
        ('k, 'v) t -> 'k -> if_found:('v -> 'a) -> if_not_found:('k -> 'a) -> 'a
      [@@kind k = k, v = v, r = (value_or_null, bits64, float64)]
      [@@mode c = (uncontended, shared)]

      val find_and_call1
        : 'k 'v 'a 'r.
        ('k, 'v) t
        -> 'k
        -> a:'a
        -> if_found:('v -> 'a -> 'r)
        -> if_not_found:('k -> 'a -> 'r)
        -> 'r
      [@@kind
        k = k
        , v = v
        , a = (value_or_null, bits64, float64)
        , r = (value_or_null, bits64, float64)]
      [@@mode c = (uncontended, shared)]

      val update : ('k, 'v) t -> 'k -> f:(('v Option.t[@kind v]) -> 'v) -> unit
      val update_and_return : ('k, 'v) t -> 'k -> f:(('v Option.t[@kind v]) -> 'v) -> 'v

      val change
        :  ('k, 'v) t
        -> 'k
        -> f:(('v Option.t[@kind v]) -> ('v Option.t[@kind v]))
        -> unit

      val fold : ('k, 'v) t -> init:'acc -> f:(key:'k -> data:'v -> 'acc -> 'acc) -> 'acc
      val iteri : ('k, 'v) t -> f:(key:'k -> data:'v -> unit) -> unit
      val find_or_add : ('k, 'v) t -> 'k -> default:(unit -> 'v) -> 'v
      val findi_or_add : ('k, 'v) t -> 'k -> default:('k -> 'v) -> 'v
      val keys : ('k, 'v) t -> ('k List.t[@kind k])
      val data : ('k, 'v) t -> ('v List.t[@kind v])
      val choose_exn : ('k, 'v) t -> 'k * 'v
    end

    include sig
      [@@@kind.default
        k = (value_or_null, bits64, float64), v = (value_or_null, bits64, float64)]

      val add_multi
        :  (('k, ('v List.t[@kind v])) t[@kind k value_or_null])
        -> key:'k
        -> data:'v
        -> unit

      val remove_multi
        :  (('k, ('v List.t[@kind v])) t[@kind k value_or_null])
        -> 'k
        -> unit

      val find_multi
        :  (('k, ('v List.t[@kind v])) t[@kind k value_or_null])
        -> 'k
        -> ('v List.t[@kind v])

      val map : (('k, 'v) t[@kind k v]) -> f:('v -> 'w) -> (('k, 'w) t[@kind k v'])
      [@@kind k = k, v = v, v' = (value_or_null, bits64, float64)]
    end
  end

  module type Hashtbl_equality = sig
    type ('a, 'b) t

    val%template equal : ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
    [@@mode m = local]

    val%template similar : ('b -> 'c -> bool) -> ('a, 'b) t -> ('a, 'c) t -> bool
    [@@mode m = local]
  end
end

module type Hashtbl = sig
  include module type of struct
    include Definitions
  end

  (** A hash table is a mutable data structure implementing a map between keys and values.
      It supports constant-time lookup and in-place modification.

      {1 Usage}

      As a simple example, we'll create a hash table with string keys using the {{!create}
      [create]} constructor, which expects a module defining the key's type:

      {[
        let h = Hashtbl.create (module String);;
        val h : (string, '_a) Hashtbl.t = <abstr>
      ]}

      We can set the values of individual keys with {{!set} [set]}. If the key already has
      a value, it will be overwritten.

      {v
      Hashtbl.set h ~key:"foo" ~data:5;;
      - : unit = ()

      Hashtbl.set h ~key:"foo" ~data:6;;
      - : unit = ()

      Hashtbl.set h ~key:"bar" ~data:6;;
      - : unit = ()
      v}

      We can access values by key, or dump all of the hash table's data:

      {v
      Hashtbl.find h "foo";;
      - : int option = Some 6

      Hashtbl.find_exn h "foo";;
      - : int = 6

      Hashtbl.to_alist h;;
      - : (string * int) list = [("foo", 6); ("bar", 6)]
      v}

      {{!change} [change]} lets us change a key's value by applying the given function:

      {v
      Hashtbl.change h "foo" (fun x ->
       match x with
       | Some x -> Some (x * 2)
       | None -> None
      );;
      - : unit = ()

      Hashtbl.to_alist h;;
      - : (string * int) list = [("foo", 12); ("bar", 6)]
      v}

      We can use {{!merge} [merge]} to merge two hashtables with fine-grained control over
      how we choose values when a key is present in the first ("left") hashtable, the
      second ("right"), or both. Here, we'll cons the values when both hashtables have a
      key:

      {v
      let h1 = Hashtbl.of_alist_exn (module Int) [(1, 5); (2, 3232)] in
      let h2 = Hashtbl.of_alist_exn (module Int) [(1, 3)] in
      Hashtbl.merge h1 h2 ~f:(fun ~key:_ -> function
        | `Left x -> Some (`Left x)
        | `Right x -> Some (`Right x)
        | `Both (x, y) -> if x=y then None else Some (`Both (x,y))
      ) |> Hashtbl.to_alist;;
      - : (int * [> `Both of int * int | `Left of int | `Right of int ]) list =
      [(2, `Left 3232); (1, `Both (5, 3))]
      v}

      {1 Interface} *)

  type (!'a, !'b) t

  include S_without_submodules with type ('k, 'v) t := ('k, 'v) t (** @inline *)

  include Hashtbl_equality with type ('k, 'v) t := ('k, 'v) t (** @inline *)

  include Non_value with type ('k, 'v) t := ('k, 'v) t (** @inline *)

  module%template.portable Creators (Key : sig
      type 'a t

      val hashable : 'a t Hashable.t
    end) : sig
    type ('a, 'b) t_ = ('a Key.t, 'b) t

    val t_of_sexp : (Sexp.t -> 'a Key.t) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t_

    include
      Creators_generic
      with type ('a, 'b) t := ('a, 'b) t_
      with type 'a key := 'a Key.t
      with type ('key, 'data, 'a) create_options :=
        ('key, 'data, 'a) create_options_without_first_class_module
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

  include For_deriving with type ('a, 'b) t := ('a, 'b) t

  (**/**)

  (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

    https://opensource.janestreet.com/standards/#private-submodules *)
  module Private : sig
    module type Creators_generic = Creators_generic

    type nonrec ('key, 'data, 'z) create_options_without_first_class_module =
      ('key, 'data, 'z) create_options_without_first_class_module

    val hashable : ('key, _) t -> 'key Hashable.t
  end
end
