open! Import
include Hashtbl_intf.Definitions
module Subatomic = Basement.Subatomic

module type Key = Key.S

let with_return = With_return.with_return
let hash_param = Hashable.hash_param
let hash = Hashable.hash
let raise_s = Error.raise_s

type 'a key = 'a

(** Internally use a maximum size that is a power of 2. Reverses the above to find the
    floor power of 2 below the system max array length *)
let max_table_length = Int.floor_pow2 Array.max_length

(** Supplemental hash. This may not be necessary, it is intended as a defense against poor
    hash functions, for which the power of 2 sized table will be especially sensitive.
    With some testing we may choose to add it, but this table is designed to be robust to
    collisions, and in most of my testing this degrades performance. *)
let _supplemental_hash h =
  let h = h lxor ((h lsr 20) lxor (h lsr 12)) in
  h lxor (h lsr 7) lxor (h lsr 4)
;;

[%%template
[@@@kind_set.define all = (float64, bits64, value_or_null)]

[%%template
[@@@kind.default k = all, v = all]

include struct
  type ('k : k, 'v : v) t =
    { mutable table : (('k, 'v) Avltree.t[@kind k v]) array
    ; mutable length : int
    ; growth_allowed : bool
    ; global_ hashable : 'k Hashable.t
    ; mutable num_iterators__do_not_read_write_directly : int [@atomic]
    }
  [@@kind k v]

  let construct ~table ~length ~growth_allowed ~hashable ~num_iterators =
    { table
    ; length
    ; growth_allowed
    ; hashable
    ; num_iterators__do_not_read_write_directly = num_iterators
    }
  ;;

  let num_iterators t = exclave_
    Subatomic.Loc.unsafe_of_atomic_loc
      [%atomic.loc t.num_iterators__do_not_read_write_directly]
  [@@synchro unsync]
  ;;

  let num_iterators t = exclave_
    Subatomic.Loc.Shared.unsafe_of_atomic_loc
      [%atomic.loc t.num_iterators__do_not_read_write_directly]
  [@@synchro sync]
  ;;

  let ensure_mutation_allowed t =
    let num_iterators = num_iterators t in
    if Subatomic.Loc.get num_iterators > 0
    then failwith "Hashtbl: mutation not allowed during iteration"
  ;;

  let incr x = Subatomic.Loc.set x (Subatomic.Loc.get x + 1) [@@synchro unsync]
  let incr x = Subatomic.Loc.Shared.incr x [@@synchro sync]
  let decr x = Subatomic.Loc.set x (Subatomic.Loc.get x - 1) [@@synchro unsync]
  let decr x = Subatomic.Loc.Shared.decr x [@@synchro sync]

  let without_mutating t f =
    let num_iterators = (num_iterators [@synchro s]) t in
    (incr [@synchro s]) num_iterators;
    match f () with
    | x ->
      (decr [@synchro s]) num_iterators;
      x
    | exception exn ->
      (decr [@synchro s]) num_iterators;
      raise exn
  [@@synchro s = (unsync, sync)]
  ;;
end

let sexp_of_key t = t.hashable.Hashable.sexp_of_t
let compare_key t = t.hashable.Hashable.compare

include struct
  let[@mode nonportable] construct = construct

  let[@mode portable] construct
    : ('k : k mod portable) ('v : v mod portable).
    table:(('k, 'v) Avltree.t[@kind k v]) array
    -> length:int
    -> growth_allowed:bool
    -> hashable:'k Hashable.t @ portable
    -> num_iterators:int
    -> (('k, 'v) t[@kind k v]) @ portable
    =
    fun ~table ~length ~growth_allowed ~hashable ~num_iterators ->
    let _table = Modes.Portable.cross table in
    let _length = Modes.Portable.cross length in
    let _growth_allowed @ portable = growth_allowed in
    let _hashable @ portable = hashable in
    let _num_iterators = Modes.Portable.cross num_iterators in
    Stdlib.Obj.magic_portable
      (construct ~table ~length ~growth_allowed ~hashable ~num_iterators)
  ;;
end

(* The default size is chosen to be 0 (as opposed to 128 as it was before) because:
   - 128 can create substantial memory overhead (x10) when creating many tables, most of
     which are not big (say, if you have a hashtbl of hashtbl). And memory overhead is not
     that easy to profile.
   - if a hashtbl is going to grow, it's not clear why 128 is markedly better than other
     sizes (if you going to stick 1000 elements, you're going to grow the hashtable once
     or twice anyway)
   - in other languages (like rust, python, and apparently go), the default is also a
     small size. *)
let create ?(growth_allowed = true) ?(size = 0) ~hashable () =
  let size = Int.min (Int.max 1 size) max_table_length in
  let size = Int.ceil_pow2 size in
  (construct [@mode p])
    ~table:(Array.create ~len:size ((Avltree.get_empty [@kind k v]) ()))
    ~length:0
    ~growth_allowed
    ~hashable
    ~num_iterators:0
[@@mode p = (nonportable, portable)]
;;

let slot t key =
  let hash = t.hashable.Hashable.hash key in
  (* this is always non-negative because we do [land] with non-negative number *)
  hash land (Array.length t.table - 1)
;;

let add_worker t ~replace ~key ~data =
  let i = (slot [@kind k v]) t key in
  let root = t.table.(i) in
  let local_ added = ref false in
  let new_root =
    (* The avl tree might replace the value [replace=true] or do nothing [replace=false]
       to the entry, in that case the table did not get bigger, so we should not increment
       length, we pass in the bool ref t.added so that it can tell us whether it added or
       replaced. We do it this way to avoid extra allocation. Since the bool is an
       immediate it does not go through the write barrier. *)
    (Avltree.add [@kind k v])
      ~replace
      root
      ~compare:((compare_key [@kind k v]) t)
      ~added
      ~key
      ~data
  in
  if !added then t.length <- t.length + 1;
  (* This little optimization saves a caml_modify when the tree hasn't been rebalanced. *)
  if not (phys_equal new_root root) then t.table.(i) <- new_root;
  !added
;;

let maybe_resize_table t =
  let len = Array.length t.table in
  let should_grow = t.length > len in
  if should_grow && t.growth_allowed
  then (
    let new_array_length = Int.min (len * 2) max_table_length in
    if new_array_length > len
    then (
      let new_table =
        Array.create ~len:new_array_length ((Avltree.get_empty [@kind k v]) ())
      in
      let old_table = t.table in
      t.table <- new_table;
      t.length <- 0;
      let f ~key ~data =
        ignore ((add_worker [@kind k v]) ~replace:true t ~key ~data : bool)
      in
      for i = 0 to Array.length old_table - 1 do
        (Avltree.iter [@kind k v]) old_table.(i) ~f
      done))
;;

let capacity t = Array.length t.table [@@mode c = (uncontended, shared)]
let growth_allowed t = t.growth_allowed [@@mode c = (uncontended, contended)]

let set t ~key ~data =
  ensure_mutation_allowed t;
  ignore ((add_worker [@kind k v]) ~replace:true t ~key ~data : bool);
  (maybe_resize_table [@kind k v]) t
;;

let add t ~key ~data =
  ensure_mutation_allowed t;
  let added = (add_worker [@kind k v]) ~replace:false t ~key ~data in
  if added
  then (
    (maybe_resize_table [@kind k v]) t;
    `Ok)
  else `Duplicate
;;

let singleton ?growth_allowed ?size ~hashable key data =
  let t = (create [@kind k v] [@mode p]) ?growth_allowed ?size ~hashable () in
  (set [@kind k v]) t ~key ~data;
  t
[@@mode p = (nonportable, portable)]
;;

let add_exn t ~key ~data =
  match (add [@kind k v]) t ~key ~data with
  | `Ok -> ()
  | `Duplicate ->
    let sexp_of_key = (sexp_of_key [@kind k v]) t in
    let error =
      (Error.create [@kind k]) "Hashtbl.add_exn got key already present" key sexp_of_key
    in
    Error.raise error
;;

let clear t =
  if t.length = 0
  then ()
  else (
    ensure_mutation_allowed t;
    for i = 0 to Array.length t.table - 1 do
      t.table.(i) <- (Avltree.get_empty [@kind k v]) ()
    done;
    t.length <- 0)
;;

let[@kind k = k, v = v, r = all] [@mode __ = local, c = (uncontended, shared)] find_and_call
  (type k : k mod c)
  (t @ c)
  (key : k)
  ~if_found
  ~if_not_found
  : (_ : r)
  =
  (* with a good hash function these first two cases will be the overwhelming majority,
     and Avltree.find is recursive, so it can't be inlined, so doing this avoids a
     function call in most cases. *)
  match (Array.get [@mode c]) t.table ((slot [@kind k v]) t key) with
  | Avltree.Empty -> if_not_found key
  | Avltree.Leaf { key = k; value = v } ->
    if t.hashable.compare k key = 0 then if_found v else if_not_found key
  | tree ->
    (Avltree.find_and_call [@kind k v r] [@mode c])
      tree
      ~compare:t.hashable.compare
      key
      ~if_found
      ~if_not_found
;;

let[@kind k = k, v = v, r = all] [@mode __ = global, c = (uncontended, shared)] [@inline] find_and_call
  t
  key
  ~if_found
  ~if_not_found
  =
  (find_and_call [@mode local c] [@kind k v r]) t key ~if_found ~if_not_found
;;

let[@mode __ = local, c = (uncontended, shared)] find =
  let if_found v : ((_ Option.t[@kind v]) Modes.t[@mode c]) = { modal = Some v } in
  let if_not_found _ : ((_ Option.t[@kind v]) Modes.t[@mode c]) = { modal = None } in
  fun t key ->
    ((find_and_call [@mode local c] [@kind k v value_or_null])
       t
       key
       ~if_found
       ~if_not_found)
      .modal
;;

let[@mode __ = global, c = (uncontended, shared)] find =
  [%eta2 find [@mode local c] [@kind k v]]
;;

let mem (type k : k mod c) t (key : k) =
  match (Array.get [@mode c]) t.table ((slot [@kind k v]) t key) with
  | Avltree.Empty -> false
  | Avltree.Leaf { key = k; value = _ } -> (compare_key [@kind k v]) t k key = 0
  | tree ->
    (Avltree.mem [@kind k v] [@mode c]) tree ~compare:((compare_key [@kind k v]) t) key
[@@mode c = (uncontended, shared)]
;;

let remove t key =
  ensure_mutation_allowed t;
  let i = (slot [@kind k v]) t key in
  let root = t.table.(i) in
  let local_ removed = ref false in
  let new_root =
    (Avltree.remove [@kind k v]) root ~removed ~compare:((compare_key [@kind k v]) t) key
  in
  if not (phys_equal root new_root) then t.table.(i) <- new_root;
  if !removed then t.length <- t.length - 1
;;

let length t = t.length [@@mode c = (uncontended, shared)]
let is_empty t = (length [@kind k v] [@mode c]) t = 0 [@@mode c = (uncontended, shared)]

let fold t ~init ~f =
  if (length [@kind k v]) t = 0
  then init
  else
    (without_mutating [@synchro s]) t (fun () ->
      let n = Array.length t.table in
      let acc = ref init in
      for i = 0 to n - 1 do
        match (Array.unsafe_get [@mode c]) t.table i with
        | Avltree.Empty -> ()
        | Avltree.Leaf { key; value = data } -> acc := f ~key ~data !acc
        | bucket -> acc := (Avltree.fold [@kind k v] [@mode c]) bucket ~init:!acc ~f
      done;
      !acc)
    [@nontail]
[@@synchro s @ c = (unsync_uncontended, sync_shared)]
;;

let[@mode local] iteri t ~f =
  if t.length = 0
  then ()
  else
    (without_mutating [@synchro s]) t (fun () ->
      let n = Array.length t.table in
      for i = 0 to n - 1 do
        match (Array.unsafe_get [@mode c]) t.table i with
        | Avltree.Empty -> ()
        | Avltree.Leaf { key; value = data } -> f ~key ~data
        | bucket -> (Avltree.iter [@kind k v] [@mode c]) bucket ~f
      done)
    [@nontail]
[@@synchro s @ c = (unsync_uncontended, sync_shared)]
;;

let[@mode global] [@inline] iteri t ~f =
  (iteri [@kind k v] [@mode local] [@synchro s]) t ~f
[@@synchro s = (unsync, sync)]
;;

let iter t ~f =
  (iteri [@kind k v] [@mode l] [@synchro s]) t ~f:(fun ~key:_ ~data -> f data) [@nontail]
[@@mode l = (local, global)] [@@synchro s @ c = (unsync_uncontended, sync_shared)]
;;

let find_or_add t id ~(local_ default) =
  (find_and_call [@kind k v v])
    t
    id
    ~if_found:(fun data -> data)
    ~if_not_found:(fun key ->
      let default = default () in
      (set [@kind k v]) t ~key ~data:default;
      default) [@nontail]
;;

let findi_or_add t id ~default =
  (find_and_call [@kind k v v])
    t
    id
    ~if_found:(fun data -> data)
    ~if_not_found:(fun key ->
      let default = default key in
      (set [@kind k v]) t ~key ~data:default;
      default) [@nontail]
;;]

[%%template
[@@@mode.default c = (uncontended, shared)]

let find_and_call1
  (t : (_ t[@kind k v]) @ c)
  (key : (_ : k mod c))
  ~a
  ~if_found
  ~if_not_found
  : (_ : r)
  =
  match (Array.get [@mode c]) t.table ((slot [@kind k v]) t key) with
  | Avltree.Empty -> if_not_found key a
  | Avltree.Leaf { key = k; value = v } ->
    if (compare_key [@kind k v]) t k key = 0 then if_found v a else if_not_found key a
  | tree ->
    (Avltree.find_and_call1 [@kind k v a r] [@mode c])
      tree
      ~compare:((compare_key [@kind k v]) t)
      key
      ~a
      ~if_found
      ~if_not_found
[@@kind k = all, v = all, a = all, r = all]
;;

let find_and_call2 (type k : value mod c) t (key : k) ~a ~b ~if_found ~if_not_found =
  match (Array.get [@mode c]) t.table (slot t key) with
  | Avltree.Empty -> if_not_found key a b
  | Avltree.Leaf { key = k; value = v } ->
    if compare_key t k key = 0 then if_found v a b else if_not_found key a b
  | tree ->
    (Avltree.find_and_call2 [@mode c])
      tree
      ~compare:(compare_key t)
      key
      ~a
      ~b
      ~if_found
      ~if_not_found
;;

let findi_and_call (type k : value mod c) t (key : k) ~if_found ~if_not_found =
  (* with a good hash function these first two cases will be the overwhelming majority,
     and Avltree.find is recursive, so it can't be inlined, so doing this avoids a
     function call in most cases. *)
  match (Array.get [@mode c]) t.table (slot t key) with
  | Avltree.Empty -> if_not_found key
  | Avltree.Leaf { key = k; value = v } ->
    if compare_key t k key = 0 then if_found ~key:k ~data:v else if_not_found key
  | tree ->
    (Avltree.findi_and_call [@mode c])
      tree
      ~compare:(compare_key t)
      key
      ~if_found
      ~if_not_found
;;

let findi_and_call1 (type k : value mod c) t (key : k) ~a ~if_found ~if_not_found =
  match (Array.get [@mode c]) t.table (slot t key) with
  | Avltree.Empty -> if_not_found key a
  | Avltree.Leaf { key = k; value = v } ->
    if compare_key t k key = 0 then if_found ~key:k ~data:v a else if_not_found key a
  | tree ->
    (Avltree.findi_and_call1 [@mode c])
      tree
      ~compare:(compare_key t)
      key
      ~a
      ~if_found
      ~if_not_found
;;

let findi_and_call2 (type k : value mod c) t (key : k) ~a ~b ~if_found ~if_not_found =
  match (Array.get [@mode c]) t.table (slot t key) with
  | Avltree.Empty -> if_not_found key a b
  | Avltree.Leaf { key = k; value = v } ->
    if compare_key t k key = 0 then if_found ~key:k ~data:v a b else if_not_found key a b
  | tree ->
    (Avltree.findi_and_call2 [@mode c])
      tree
      ~compare:(compare_key t)
      key
      ~a
      ~b
      ~if_found
      ~if_not_found
;;]

let iter_keys t ~f = iteri t ~f:(fun ~key ~data:_ -> f key) [@nontail]

[%%template
[@@@kind.default k = all, v = all]
[@@@mode.default c = (uncontended, shared)]

let rec choose_nonempty table i =
  let avltree = (Array.unsafe_get [@mode c]) table i in
  if (Avltree.is_empty [@kind k v]) avltree
  then (
    let i = (i + 1) land (Array.length table - 1) in
    (choose_nonempty [@kind k v] [@mode c]) table i)
  else (Avltree.choose_exn [@kind k v] [@mode c]) avltree
;;

let choose_exn (t : (_ t[@kind k v])) =
  if t.length = 0 then raise_s (Sexp.message "[Hashtbl.choose_exn] of empty hashtbl" []);
  (choose_nonempty [@kind k v] [@mode c]) t.table 0
;;]

[%%template
[@@@mode.default c = (uncontended, shared)]

let[@inline] [@mode global] wrap_pair #(k, v) = #(k, v)

let[@inline] [@mode local] wrap_pair #(k, v) =
  #({ Modes.Global.global = k }, { Modes.Global.global = v })
;;

let[@mode m = (global, local), c = c] choose t =
  if is_empty t
  then None
  else (
    let #(k, v) = (wrap_pair [@mode m]) ((choose_nonempty [@mode c]) t.table 0) in
    Some (k, v) [@exclave_if_local m])
;;

let choose_randomly_nonempty ~random_state t =
  let start_idx = Random.State.int random_state (Array.length t.table) in
  (choose_nonempty [@mode c]) t.table start_idx
;;

let choose_randomly ?(random_state = Random.State.get_default ()) t =
  if is_empty t
  then None
  else (
    let #(k, v) = (choose_randomly_nonempty [@mode c]) ~random_state t in
    Some (k, v))
;;

let choose_randomly_exn ?(random_state = Random.State.get_default ()) t =
  if t.length = 0
  then raise_s (Sexp.message "[Hashtbl.choose_randomly_exn] of empty hashtbl" []);
  (choose_randomly_nonempty [@mode c]) ~random_state t
;;]

let invariant invariant_key invariant_data t =
  for i = 0 to Array.length t.table - 1 do
    Avltree.invariant t.table.(i) ~compare:(compare_key t)
  done;
  let real_len =
    fold t ~init:0 ~f:(fun ~key ~data i ->
      invariant_key key;
      invariant_data data;
      i + 1)
  in
  assert (real_len = t.length)
;;

[%%template
[@@@mode.default c = (uncontended, shared)]

let find_or_null =
  let if_found v : (_ Modes.t[@modality c]) = { modal = This v } in
  let if_not_found _ : (_ Modes.t[@modality c]) = { modal = Null } in
  fun t key -> ((find_and_call [@mode c]) t key ~if_found ~if_not_found).modal
;;

let find_exn =
  let if_found v _ : (_ Modes.t[@modality c] [@kind v]) = { modal = v } in
  let if_not_found k (t : ((_ t[@kind k v]) Modes.t[@modality c])) =
    (raise [@kind v])
      (Not_found_s
         (List [ Atom "Hashtbl.find_exn: not found"; t.modal.hashable.sexp_of_t k ]))
  in
  let find_exn t key =
    ((find_and_call1 [@kind k v value_or_null v] [@mode c])
       t
       key
       ~a:({ modal = t } : (_ Modes.t[@modality c]))
       ~if_found
       ~if_not_found)
      .modal
  in
  (* named to preserve symbol in compiled binary *)
  find_exn
[@@kind k = all, v = all]
;;]

let existsi t ~f =
  with_return (fun r ->
    iteri t ~f:(fun ~key ~data -> if f ~key ~data then r.return true);
    false)
  [@nontail]
;;

let exists t ~f = existsi t ~f:(fun ~key:_ ~data -> f data) [@nontail]
let for_alli t ~f = not (existsi t ~f:(fun ~key ~data -> not (f ~key ~data)))
let for_all t ~f = not (existsi t ~f:(fun ~key:_ ~data -> not (f data)))

let counti t ~f =
  fold t ~init:0 ~f:(fun ~key ~data acc -> if f ~key ~data then acc + 1 else acc)
  [@nontail]
;;

let count t ~f =
  fold t ~init:0 ~f:(fun ~key:_ ~data acc -> if f data then acc + 1 else acc) [@nontail]
;;

[%%template
[@@@kind.default k = all, v = all, v' = all]

let mapi (t : (_ t[@kind k v])) ~f =
  let new_t =
    (create [@kind k v'])
      ~growth_allowed:t.growth_allowed
      ~hashable:t.hashable
      ~size:t.length
      ()
  in
  (iteri [@kind k v]) t ~f:(fun ~key ~data ->
    (set [@kind k v']) new_t ~key ~data:(f ~key ~data));
  new_t
;;

let map t ~f = (mapi [@kind k v v']) t ~f:(fun ~key:_ ~data -> f data) [@nontail]]

let copy t = map t ~f:Fn.id

let filter_mapi t ~f =
  let new_t =
    create ~growth_allowed:t.growth_allowed ~hashable:t.hashable ~size:t.length ()
  in
  iteri t ~f:(fun ~key ~data ->
    match f ~key ~data with
    | Some new_data -> set new_t ~key ~data:new_data
    | None -> ());
  new_t
;;

let filter_map t ~f = filter_mapi t ~f:(fun ~key:_ ~data -> f data) [@nontail]

let filteri t ~f =
  filter_mapi t ~f:(fun ~key ~data -> if f ~key ~data then Some data else None) [@nontail]
;;

let filter t ~f = filteri t ~f:(fun ~key:_ ~data -> f data) [@nontail]
let filter_keys t ~f = filteri t ~f:(fun ~key ~data:_ -> f key) [@nontail]

let partition_mapi t ~f =
  let t0 =
    create ~growth_allowed:t.growth_allowed ~hashable:t.hashable ~size:t.length ()
  in
  let t1 =
    create ~growth_allowed:t.growth_allowed ~hashable:t.hashable ~size:t.length ()
  in
  iteri t ~f:(fun ~key ~data ->
    match (f ~key ~data : _ Either.t) with
    | First new_data -> set t0 ~key ~data:new_data
    | Second new_data -> set t1 ~key ~data:new_data);
  t0, t1
;;

let partition_map t ~f = partition_mapi t ~f:(fun ~key:_ ~data -> f data) [@nontail]

let partitioni_tf t ~f =
  partition_mapi t ~f:(fun ~key ~data -> if f ~key ~data then First data else Second data)
  [@nontail]
;;

let partition_tf t ~f = partitioni_tf t ~f:(fun ~key:_ ~data -> f data) [@nontail]

(* Some hashtbl implementations may be able to perform this more efficiently than two
   separate lookups *)
let[@kind k = all, v = all] find_and_remove t id =
  let result = (find [@kind k v]) t id in
  if (Option.is_some [@kind v]) result then (remove [@kind k v]) t id;
  result
;;

[%%template
[@@@kind.default k = all, v = all]

let change t id ~f =
  match (f ((find [@kind k v]) t id) : (_ Option.t[@kind v])) with
  | None -> (remove [@kind k v]) t id
  | Some data -> (set [@kind k v]) t ~key:id ~data
;;

let update_and_return t id ~f =
  let data = f ((find [@kind k v]) t id) in
  (set [@kind k v]) t ~key:id ~data;
  data
;;

let update t id ~f =
  let (_ : _) = ((update_and_return [@kind k v]) t id ~f : _) in
  ()
;;]

let incr_by ~remove_if_zero t key by =
  if remove_if_zero
  then
    change t key ~f:(fun opt ->
      match by + Option.value opt ~default:0 with
      | 0 -> None
      | n -> Some n)
  else
    update t key ~f:(function
      | None -> by
      | Some i -> by + i)
;;

let incr ?(by = 1) ?(remove_if_zero = false) t key = incr_by ~remove_if_zero t key by
let decr ?(by = 1) ?(remove_if_zero = false) t key = incr_by ~remove_if_zero t key (-by)

[%%template
[@@@kind k = all, v = all]

type ('k : k, 'v : v) l = (('k, ('v List.t[@kind v])) t[@kind k value_or_null])

[@@@kind.default k v]

let add_multi (t : _ l) ~key ~data =
  (update [@kind k value_or_null]) t key ~f:(function
    | None -> ([ data ] : (_ List.t[@kind v]))
    | Some l -> data :: l)
;;

let remove_multi (t : _ l) key =
  match (find [@kind k value_or_null]) t key with
  | None -> ()
  | Some [] | Some [ _ ] -> (remove [@kind k value_or_null]) t key
  | Some (_ :: tl) -> (set [@kind k value_or_null]) t ~key ~data:tl
;;

let find_multi t key : (_ List.t[@kind v]) =
  match (find [@kind k value_or_null] [@mode c]) t key with
  | None -> []
  | Some l -> l
[@@mode c = (uncontended, shared)]
;;]

[%%template
[@@@mode.default p = (nonportable, portable)]

let create_mapped ?growth_allowed ?size ~hashable ~get_key ~get_data rows =
  let size =
    match size with
    | Some s -> s
    | None -> List.length rows
  in
  let res = (create [@mode p]) ?growth_allowed ~hashable ~size () in
  let dupes = ref [] in
  List.iter rows ~f:(fun r ->
    let key = get_key r in
    let data = get_data r in
    if mem res key then dupes := key :: !dupes else set res ~key ~data);
  match !dupes with
  | [] -> `Ok res
  | keys -> `Duplicate_keys (List.dedup_and_sort ~compare:hashable.Hashable.compare keys)
;;

let create_mapped_multi
  (type b : value mod p)
  ?growth_allowed
  ?size
  ~hashable
  ~get_key
  ~get_data
  rows
  =
  let size =
    match size with
    | Some s -> s
    | None -> List.length rows
  in
  let res : (_, b list) t = (create [@mode p]) ?growth_allowed ~size ~hashable () in
  List.iter rows ~f:(fun r ->
    let key = get_key r in
    let data = get_data r in
    add_multi res ~key ~data);
  res
;;

let of_alist ?growth_allowed ?size ~hashable lst =
  match
    (create_mapped [@mode p])
      ?growth_allowed
      ?size
      ~hashable
      ~get_key:fst
      ~get_data:snd
      lst
  with
  | `Ok t -> `Ok t
  | `Duplicate_keys k -> `Duplicate_key (List.hd_exn k)
;;

let of_alist_report_all_dups ?growth_allowed ?size ~hashable lst =
  (create_mapped [@mode p]) ?growth_allowed ?size ~hashable ~get_key:fst ~get_data:snd lst
;;

let of_alist_or_error ?growth_allowed ?size ~hashable lst =
  match (of_alist [@mode p]) ?growth_allowed ?size ~hashable lst with
  | `Ok v -> Result.Ok v
  | `Duplicate_key key ->
    let sexp_of_key = hashable.Hashable.sexp_of_t in
    (Or_error.error [@mode p]) "Hashtbl.of_alist_exn: duplicate key" key sexp_of_key
;;

let of_alist_exn ?growth_allowed ?size ~hashable lst =
  match (of_alist_or_error [@mode p]) ?growth_allowed ?size ~hashable lst with
  | Result.Ok v -> v
  | Result.Error e -> Error.raise e
;;

let of_alist_multi ?growth_allowed ?size ~hashable lst =
  (create_mapped_multi [@mode p])
    ?growth_allowed
    ?size
    ~hashable
    ~get_key:fst
    ~get_data:snd
    lst
;;]

(* A hack around the fact we can't have boxed tuples with unboxed types. *)
module KV = struct
  [%%template
  [@@@kind.default k = all, v = all]

  type ('k : k, 'v : v) t =
    { key : 'k
    ; data : 'v
    }

  let sexp_of_t sexp_of_key sexp_of_data ({ key; data } : (_ t[@kind k v])) =
    Sexp.List [ sexp_of_key key; sexp_of_data data ]
  ;;]

  external to_pairs : ('k, 'v) t list -> ('k * 'v) list @@ portable = "%identity"
end

[%%template
[@@@kind.default k = all, v = all]

let to_alist (type (k : k) (v : v)) (t : ((k, v) t[@kind k v]))
  : ((k, v) KV.t[@kind k v]) List.t
  =
  (fold [@kind k v])
    ~f:(fun ~key ~data list -> ({ key; data } : (_ KV.t[@kind k v])) :: list)
    ~init:[]
    t
;;

let sexp_of_t (type (k : k) (v : v)) sexp_of_key sexp_of_data (t : ((k, v) t[@kind k v]))
  : Sexp.t
  =
  t
  |> (to_alist [@kind k v])
  |> List.sort ~compare:(fun ({ KV.key = k1; _ } : (_ KV.t[@kind k v])) { key = k2; _ } ->
    t.hashable.compare k1 k2)
  |> sexp_of_list [%sexp_of: ((key, data) KV.t[@kind k v])]
;;]

let to_alist t = to_alist t |> KV.to_pairs

let[@mode p = (nonportable, portable)] t_of_sexp ~(hashable @ p) k_of_sexp d_of_sexp sexp =
  let alist = list_of_sexp (pair_of_sexp k_of_sexp d_of_sexp) sexp in
  match of_alist ~hashable alist ~size:(List.length alist) with
  | `Ok v -> v
  | `Duplicate_key k ->
    (* find the sexp of a duplicate key, so the error is narrowed to a key and not the
       whole map *)
    let alist_sexps = list_of_sexp (pair_of_sexp Fn.id Fn.id) sexp in
    let found_first_k = ref false in
    List.iter2_exn alist alist_sexps ~f:(fun (k2, _) (k2_sexp, _) ->
      if hashable.compare k k2 = 0
      then
        if !found_first_k
        then of_sexp_error "Hashtbl.t_of_sexp: duplicate key" k2_sexp
        else found_first_k := true);
    assert false
;;

let t_sexp_grammar
  (type k v)
  (k_grammar : k Sexplib0.Sexp_grammar.t)
  (v_grammar : v Sexplib0.Sexp_grammar.t)
  : (k, v) t Sexplib0.Sexp_grammar.t
  =
  Sexplib0.Sexp_grammar.coerce (List.Assoc.t_sexp_grammar k_grammar v_grammar)
;;

[%%template
[@@@kind.default k = all, v = all]
[@@@synchro.default s @ c = (unsync_uncontended, sync_shared)]

open struct
  let[@inline] get_all t ~f =
    ((fold [@kind k v] [@synchro s])
       t
       ~init:({ modal = [] } : ((_ List.t[@kind a]) Modes.t[@modality c]))
       ~f:(fun ~key ~data { modal = acc } : ((_ List.t[@kind a]) Modes.t[@modality c]) ->
         { modal = f ~key ~data :: acc }))
      .modal
  [@@kind k = k, v = v, a = (k, v)]
  ;;
end

let keys t = (get_all [@kind k v k]) t ~f:(fun ~key ~data:_ -> key)
let data t = (get_all [@kind k v v]) t ~f:(fun ~key:_ ~data -> data)]

let add_to_groups groups ~get_key ~get_data ~combine ~rows =
  List.iter rows ~f:(fun row ->
    let key = get_key row in
    let data = get_data row in
    let data =
      match find groups key with
      | None -> data
      | Some old -> combine old data
    in
    set groups ~key ~data)
  [@nontail]
;;

[%%template
[@@@mode.default p = (nonportable, portable)]

let group ?growth_allowed ?size ~hashable ~get_key ~get_data ~combine rows =
  let res = (create [@mode p]) ?growth_allowed ?size ~hashable () in
  add_to_groups res ~get_key ~get_data ~combine ~rows;
  res
;;

let create_with_key ?growth_allowed ?size ~hashable ~get_key rows =
  (create_mapped [@mode p]) ?growth_allowed ?size ~hashable ~get_key ~get_data:Fn.id rows
;;

let create_with_key_or_error ?growth_allowed ?size ~hashable ~get_key rows =
  match (create_with_key [@mode p]) ?growth_allowed ?size ~hashable ~get_key rows with
  | `Ok t -> Result.Ok t
  | `Duplicate_keys keys ->
    let sexp_of_key = hashable.Hashable.sexp_of_t in
    Or_error.error_s
      (Sexp.message
         "Hashtbl.create_with_key: duplicate keys"
         [ "keys", sexp_of_list sexp_of_key keys ])
;;

let create_with_key_exn ?growth_allowed ?size ~hashable ~get_key rows =
  match
    (create_with_key_or_error [@mode p]) ?growth_allowed ?size ~hashable ~get_key rows
  with
  | Ok res -> res
  | Error error -> Error.raise error
;;]

let merge =
  let maybe_set t ~key ~f d =
    match f ~key d with
    | None -> ()
    | Some v -> add_exn t ~key ~data:v
  in
  fun t_left t_right ~f ->
    let new_t =
      create
        ~growth_allowed:t_left.growth_allowed
        ~hashable:t_left.hashable
        ~size:t_left.length
        ()
    in
    without_mutating t_left (fun () ->
      without_mutating t_right (fun () ->
        iteri t_left ~f:(fun ~key ~data:left ->
          match find t_right key with
          | None -> maybe_set new_t ~key ~f (`Left left)
          | Some right -> maybe_set new_t ~key ~f (`Both (left, right)));
        iteri t_right ~f:(fun ~key ~data:right ->
          match find t_left key with
          | None -> maybe_set new_t ~key ~f (`Right right)
          | Some _ -> ()
          (* already done above *))
        [@nontail])
      [@nontail]);
    new_t
;;

let merge_into ~src ~dst ~f =
  iteri src ~f:(fun ~key ~data ->
    let dst_data = find dst key in
    let action = without_mutating dst (fun () -> f ~key data dst_data) in
    match (action : _ Merge_into_action.t) with
    | Remove -> remove dst key
    | Set_to data ->
      (match dst_data with
       | None -> set dst ~key ~data
       | Some dst_data -> if not (phys_equal dst_data data) then set dst ~key ~data))
  [@nontail]
;;

let filteri_inplace t ~f =
  let to_remove =
    fold t ~init:[] ~f:(fun ~key ~data ac -> if f ~key ~data then ac else key :: ac)
  in
  List.iter to_remove ~f:(fun key -> remove t key)
;;

let filter_inplace t ~f = filteri_inplace t ~f:(fun ~key:_ ~data -> f data) [@nontail]
let filter_keys_inplace t ~f = filteri_inplace t ~f:(fun ~key ~data:_ -> f key) [@nontail]

let filter_mapi_inplace t ~f =
  let map_results = fold t ~init:[] ~f:(fun ~key ~data ac -> (key, f ~key ~data) :: ac) in
  List.iter map_results ~f:(fun (key, result) ->
    match result with
    | None -> remove t key
    | Some data -> set t ~key ~data)
;;

let filter_map_inplace t ~f =
  filter_mapi_inplace t ~f:(fun ~key:_ ~data -> f data) [@nontail]
;;

let mapi_inplace t ~f =
  ensure_mutation_allowed t;
  without_mutating t (fun () ->
    Array.iter t.table ~f:(Avltree.mapi_inplace ~f) [@nontail])
  [@nontail]
;;

let map_inplace t ~f = mapi_inplace t ~f:(fun ~key:_ ~data -> f data) [@nontail]

let[@mode local] equal equal t t' =
  length t = length t'
  && (with_return (fun r ->
        without_mutating t' (fun () ->
          (iteri [@mode local]) t ~f:(fun ~key ~data ->
            match (find [@mode local]) t' key with
            | None -> r.return false
            | Some data' -> if not (equal data data') then r.return false)
          [@nontail]);
        true)
  [@nontail])
;;

let[@mode global] [@inline] equal equal t t' = (equal [@mode local]) [%eta2 equal] t t'
let similar = (equal [@mode m]) [@@mode m = (local, global)]

module Accessors = struct
  let invariant = invariant

  [%%template
  [@@@mode.default c = (uncontended, shared)]

  let choose = (choose [@mode m c]) [@@mode m = (local, global), c = c]
  let choose_exn = (choose_exn [@mode c])
  let choose_randomly = (choose_randomly [@mode c])
  let choose_randomly_exn = (choose_randomly_exn [@mode c])]

  let clear = clear
  let copy = copy
  let remove = remove
  let set = set
  let add = add
  let add_exn = add_exn
  let change = change
  let update = update
  let update_and_return = update_and_return
  let add_multi = add_multi
  let remove_multi = remove_multi
  let%template find_multi = (find_multi [@mode c]) [@@mode c = (uncontended, shared)]
  let mem = mem
  let iter_keys = iter_keys
  let iter = iter
  let iteri = iteri
  let exists = exists
  let existsi = existsi
  let for_all = for_all
  let for_alli = for_alli
  let count = count
  let counti = counti
  let fold = fold
  let length = length
  let is_empty = is_empty
  let map = map
  let mapi = mapi
  let filter_map = filter_map
  let filter_mapi = filter_mapi
  let filter_keys = filter_keys
  let filter = filter
  let filteri = filteri
  let partition_map = partition_map
  let partition_mapi = partition_mapi
  let partition_tf = partition_tf
  let partitioni_tf = partitioni_tf
  let find_or_add = find_or_add
  let findi_or_add = findi_or_add

  [%%template
  [@@@mode.default c = (uncontended, shared)]

  let find = (find [@mode c])
  let find_or_null = (find_or_null [@mode c])
  let find_exn = (find_exn [@mode c])
  let find_and_call = (find_and_call [@mode c])
  let find_and_call1 = (find_and_call1 [@mode c])
  let find_and_call2 = (find_and_call2 [@mode c])
  let findi_and_call = (findi_and_call [@mode c])
  let findi_and_call1 = (findi_and_call1 [@mode c])
  let findi_and_call2 = (findi_and_call2 [@mode c])]

  let find_and_remove = find_and_remove
  let to_alist = to_alist
  let merge = merge
  let merge_into = merge_into
  let keys = keys
  let data = data
  let filter_keys_inplace = filter_keys_inplace
  let filter_inplace = filter_inplace
  let filteri_inplace = filteri_inplace
  let map_inplace = map_inplace
  let mapi_inplace = mapi_inplace
  let filter_map_inplace = filter_map_inplace
  let filter_mapi_inplace = filter_mapi_inplace
  let equal = equal
  let similar = similar
  let incr = incr
  let decr = decr
  let sexp_of_key = sexp_of_key
end

module%template.portable
  [@modality p] Creators (Key : sig
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
end = struct
  let hashable = Key.hashable

  type ('a, 'b) t_ = ('a Key.t, 'b) t

  let create ?growth_allowed ?size () = create ?growth_allowed ?size ~hashable ()
  let of_alist ?growth_allowed ?size l = of_alist ?growth_allowed ~hashable ?size l

  let of_alist_report_all_dups ?growth_allowed ?size l =
    of_alist_report_all_dups ?growth_allowed ~hashable ?size l
  ;;

  let of_alist_or_error ?growth_allowed ?size l =
    of_alist_or_error ?growth_allowed ~hashable ?size l
  ;;

  let of_alist_exn ?growth_allowed ?size l =
    of_alist_exn ?growth_allowed ~hashable ?size l
  ;;

  let t_of_sexp k_of_sexp d_of_sexp sexp =
    (t_of_sexp [@mode p]) ~hashable k_of_sexp d_of_sexp sexp
  ;;

  let of_alist_multi ?growth_allowed ?size l =
    of_alist_multi ?growth_allowed ~hashable ?size l
  ;;

  let create_mapped ?growth_allowed ?size ~get_key ~get_data l =
    create_mapped ?growth_allowed ~hashable ?size ~get_key ~get_data l
  ;;

  let create_with_key ?growth_allowed ?size ~get_key l =
    create_with_key ?growth_allowed ~hashable ?size ~get_key l
  ;;

  let create_with_key_or_error ?growth_allowed ?size ~get_key l =
    create_with_key_or_error ?growth_allowed ~hashable ?size ~get_key l
  ;;

  let create_with_key_exn ?growth_allowed ?size ~get_key l =
    create_with_key_exn ?growth_allowed ~hashable ?size ~get_key l
  ;;

  let group ?growth_allowed ?size ~get_key ~get_data ~combine l =
    group ?growth_allowed ~hashable ?size ~get_key ~get_data ~combine l
  ;;
end

module Poly = struct
  type nonrec ('a, 'b) t = ('a, 'b) t
  type 'a key = 'a

  let hashable = Hashable.poly
  let capacity = capacity
  let growth_allowed = growth_allowed

  include Creators [@modality portable] (struct
      type 'a t = 'a

      let hashable = hashable
    end)

  include Accessors

  let sexp_of_t = sexp_of_t
  let t_sexp_grammar = t_sexp_grammar
end

module Private = struct
  module type Creators_generic = Creators_generic

  type nonrec ('key, 'data, 'z) create_options_without_first_class_module =
    ('key, 'data, 'z) create_options_without_first_class_module

  let hashable t = t.hashable
end

[%%template
[@@@kind.default k = all, v = all]
[@@@mode.default p = (nonportable, portable)]

let create ?growth_allowed ?size m =
  (create [@kind k v] [@mode p])
    ~hashable:((Hashable.of_key [@kind k] [@mode p]) m)
    ?growth_allowed
    ?size
    ()
;;

let singleton ?growth_allowed ?size m k v =
  (singleton [@kind k v] [@mode p])
    ~hashable:((Hashable.of_key [@kind k] [@mode p]) m)
    ?growth_allowed
    ?size
    k
    v
;;]]

[%%template
[@@@mode p = (nonportable, portable)]

let hashable = (Hashable.of_key [@mode p])

[@@@mode.default p]

let of_alist ?growth_allowed ?size m l =
  (of_alist [@mode p]) ~hashable:(hashable m) ?growth_allowed ?size l
;;

let of_alist_report_all_dups ?growth_allowed ?size m l =
  (of_alist_report_all_dups [@mode p]) ~hashable:(hashable m) ?growth_allowed ?size l
;;

let of_alist_or_error ?growth_allowed ?size m l =
  (of_alist_or_error [@mode p]) ~hashable:(hashable m) ?growth_allowed ?size l
;;

let of_alist_exn ?growth_allowed ?size m l =
  (of_alist_exn [@mode p]) ~hashable:(hashable m) ?growth_allowed ?size l
;;

let of_alist_multi ?growth_allowed ?size m l =
  (of_alist_multi [@mode p]) ~hashable:(hashable m) ?growth_allowed ?size l
;;

let create_mapped ?growth_allowed ?size m ~get_key ~get_data l =
  (create_mapped [@mode p])
    ~hashable:(hashable m)
    ?growth_allowed
    ?size
    ~get_key
    ~get_data
    l
;;

let create_with_key ?growth_allowed ?size m ~get_key l =
  (create_with_key [@mode p]) ~hashable:(hashable m) ?growth_allowed ?size ~get_key l
;;

let create_with_key_or_error ?growth_allowed ?size m ~get_key l =
  (create_with_key_or_error [@mode p])
    ~hashable:(hashable m)
    ?growth_allowed
    ?size
    ~get_key
    l
;;

let create_with_key_exn ?growth_allowed ?size m ~get_key l =
  (create_with_key_exn [@mode p]) ~hashable:(hashable m) ?growth_allowed ?size ~get_key l
;;

let group ?growth_allowed ?size m ~get_key ~get_data ~combine l =
  (group [@mode p])
    ~hashable:(hashable m)
    ?growth_allowed
    ?size
    ~get_key
    ~get_data
    ~combine
    l
;;]

let hashable_s t = Hashable.to_key t.hashable

module M (K : T.T) = struct
  type nonrec 'v t = (K.t, 'v) t
end

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

let sexp_of_m__t (type k) (module K : Sexp_of_m with type t = k) sexp_of_v t =
  sexp_of_t K.sexp_of_t sexp_of_v t
;;

let m__t_of_sexp (type k) (module K : M_of_sexp with type t = k) v_of_sexp sexp =
  t_of_sexp ~hashable:(Hashable.of_key (module K)) K.t_of_sexp v_of_sexp sexp
;;

let m__t_sexp_grammar (type k) (module K : M_sexp_grammar with type t = k) v_grammar =
  t_sexp_grammar K.t_sexp_grammar v_grammar
;;

let equal_m__t (module _ : Equal_m) equal_v t1 t2 = equal equal_v t1 t2
