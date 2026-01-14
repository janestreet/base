(***********************************************************************
 *
 *                           Objective Caml
 *
 *            Xavier Leroy, projet Cristal, INRIA Rocquencourt
 *
 *  Copyright 1996 Institut National de Recherche en Informatique et
 *  en Automatique.  All rights reserved.  This file is distributed
 *  under the terms of the Apache 2.0 license. See ../THIRD-PARTY.txt
 *  for details.
 *
 ***********************************************************************)

open! Import
module List = List0
include Map_intf.Definitions

module Finished_or_unfinished = struct
  include Finished_or_unfinished

  (* These two functions are tested in [test_map.ml] to make sure our use of
     [Stdlib.Obj.magic] is correct and safe. *)
  let of_continue_or_stop : Continue_or_stop.t -> t = Stdlib.Obj.magic
  let to_continue_or_stop : t -> Continue_or_stop.t = Stdlib.Obj.magic
end

module Merge_element = struct
  include Merge_element

  let left = function
    | `Right _ -> None
    | `Left left | `Both (left, _) -> Some left
  ;;

  let right = function
    | `Left _ -> None
    | `Right right | `Both (_, right) -> Some right
  ;;

  let left_value t ~default =
    match t with
    | `Right _ -> default
    | `Left left | `Both (left, _) -> left
  ;;

  let right_value t ~default =
    match t with
    | `Left _ -> default
    | `Right right | `Both (_, right) -> right
  ;;

  let values t ~left_default ~right_default =
    match t with
    | `Left left -> left, right_default
    | `Right right -> left_default, right
    | `Both (left, right) -> left, right
  ;;
end

let with_return = With_return.with_return

exception Duplicate [@@deriving sexp]

module Tree0 = struct
  (* Weight-balanced trees, where the weight of each tree is [length t + 1]. We store
     [weight] at each node to make balance operations cheap, and computing length just
     requires subtracting 1. Conveniently, for the node N=(L,k,R), wt(N)=wt(L)+wt(R).

     Weight-balanced trees of this form are originally defined in [1]. The balancing
     algorithm depends on two parameters: delta and gamma. Delta controls the balance
     invariant: for sibling subtrees A and B, [wt(A) <= wt(B) * delta] and vice versa.
     Gamma is used during rotation to decide whether to use a single or double rotation. A
     single left-rotation suffices for (_, (A, B)) if [wt(A) < wt(B) * gamma]. The valid
     bounds for these parameters and their performance impact are analyzed in [2].

     Of the options presented in [2], we choose (delta, gamma) = (5/2, 3/2). This choice
     is good by three criteria. One, it has the best performance in the paper's
     benchmarks. Two, it yields average tree heights in our own tests comparable to our
     previous implementation based on AVL trees. Three, we can implement these comparisons
     efficiently with just two bit shifts and one addition each.

     We define the weight comparisons below as [is_too_heavy] and [may_rotate_just_once].

     [1] Binary search trees of bounded balance, Nievergelt and Reingold, SIAM Journal on
         Computing Vol. 2, Iss. 1 (1973). https://dl.acm.org/doi/pdf/10.1145/800152.804906

     [2] Balancing weight-balanced trees, Hirai and Yamamoto, JFP 21 (3): 287–307, 2011.
         https://yoichihirai.com/bst.pdf *)

  type ('k, 'v, 'cmp) t =
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
        ; weight : int
        }

  type ('k, 'v, 'cmp) tree = ('k, 'v, 'cmp) t

  let globalize0 : 'k 'v 'cmp. ('k, 'v, 'cmp) t @ local -> ('k, 'v, 'cmp) t = function
    | Empty -> Empty
    | Leaf { key; data } -> Leaf { key; data }
    | Node { left; key; data; right; weight } -> Node { left; key; data; right; weight }
  ;;

  (* Checks for failure of the balance invariant in one direction:

     {[
       not (wt A <= wt B * 5 / 2)
     ]}

     We negate it by changing [<=] to [>].

     {[
       wt A > wt B * 5 / 2
     ]}

     We avoid division by multiplying both sides by two.

     {[
       wt A * 2 > wt B * 5
     ]}

     We stick to powers of two by changing [x * 5] to [x * 4 + x].

     {[
       wt A * 2 > (wt B * 4) + wt B
     ]}

     And we avoid multiplication by using shifts for multiplication by 2 and by 4.

     {[
       wt A << 1 > wt B << 2 + wt B
     ]}
  *)
  let is_too_heavy ~weight:wtA ~for_weight:wtB =
    (* See? Just like above! *)
    wtA lsl 1 > (wtB lsl 2) + wtB
  [@@inline always]
  ;;

  (* Checks if we can use a single rotation for the currently-lower siblings:

     {[
       wt A < wt B * 3 / 2
     ]}

     We avoid division by multiplying both sides by two.

     {[
       wt A * 2 < wt B * 3
     ]}

     We stick to powers of two by changing [x * 3] to [x * 2 + x].

     {[
       wt A * 2 < (wt B * 2) + wt B
     ]}

     We incur one fewer multiplication by moving [wt(B) * 2] to the left.

     {[
       (wt A - wt B) * 2 < wt B
     ]}

     And we avoid multiplication by using shift for multiplication by 2.

     {[
       wt A - wt B << 1 < wt B
     ]}
  *)
  let may_rotate_just_once ~inner_sibling_weight:wtA ~outer_sibling_weight:wtB =
    (* See? Just like above! *)
    (wtA - wtB) lsl 1 < wtB
  [@@inline always]
  ;;

  let weight = function
    | Empty -> 1
    | Leaf _ -> 2
    | Node { left = _; key = _; data = _; right = _; weight = w } -> w
  ;;

  let order_invariants =
    let in_range ~lower ~upper compare_key k =
      (match lower with
       | None -> true
       | Some lower -> compare_key lower k < 0)
      &&
      match upper with
      | None -> true
      | Some upper -> compare_key k upper < 0
    in
    let rec loop ~lower ~upper compare_key t =
      match t with
      | Empty -> true
      | Leaf { key = k; data = _ } -> in_range ~lower ~upper compare_key k
      | Node { left = l; key = k; data = _; right = r; weight = _ } ->
        in_range ~lower ~upper compare_key k
        && loop ~lower ~upper:(Some k) compare_key l
        && loop ~lower:(Some k) ~upper compare_key r
    in
    fun t ~compare_key -> loop ~lower:None ~upper:None compare_key t
  ;;

  let rec balance_invariants t =
    match t with
    | Empty | Leaf _ -> true
    | Node { left = l; key = _; data = _; right = r; weight = w } ->
      let wl = weight l
      and wr = weight r in
      w = wl + wr
      && w > 2
      && (not (is_too_heavy ~weight:wl ~for_weight:wr))
      && (not (is_too_heavy ~weight:wr ~for_weight:wl))
      && balance_invariants l
      && balance_invariants r
  ;;

  let invariants t ~compare_key = order_invariants t ~compare_key && balance_invariants t

  (* preconditions: wl = weight(l), wr = weight(r), and neither [is_too_heavy] *)
  let[@inline] create_with_weights ~wl ~wr l x d r =
    if wl = 1 && wr = 1
    then Leaf { key = x; data = d }
    else Node { left = l; key = x; data = d; right = r; weight = wl + wr }
  ;;

  (* precondition: neither weight(l) nor weight(r) [is_too_heavy]. *)
  let create l x d r = create_with_weights ~wl:(weight l) ~wr:(weight r) l x d r
  let singleton key data = Leaf { key; data }

  (* We must call [f] with increasing indexes, because the bin_prot reader in Core.Map
     needs it. *)
  let of_increasing_iterator_unchecked ~len ~f =
    let rec loop n ~f i : (_, _, _) t =
      match n with
      | 0 -> Empty
      | 1 ->
        let k, v = f i in
        Leaf { key = k; data = v }
      | 2 ->
        let kl, vl = f i in
        let k, v = f (i + 1) in
        Node
          { left = Leaf { key = kl; data = vl }
          ; key = k
          ; data = v
          ; right = Empty
          ; weight = 3
          }
      | 3 ->
        let kl, vl = f i in
        let k, v = f (i + 1) in
        let kr, vr = f (i + 2) in
        Node
          { left = Leaf { key = kl; data = vl }
          ; key = k
          ; data = v
          ; right = Leaf { key = kr; data = vr }
          ; weight = 4
          }
      | n ->
        let left_length = n lsr 1 in
        let right_length = n - left_length - 1 in
        let left = loop left_length ~f i in
        let k, v = f (i + left_length) in
        let right = loop right_length ~f (i + left_length + 1) in
        create left k v right
    in
    loop len ~f 0
  ;;

  let of_sorted_array_unchecked array ~compare_key =
    let array_length = Array.length array in
    let next =
      if array_length < 2
         ||
         let k0, _ = array.(0) in
         let k1, _ = array.(1) in
         compare_key k0 k1 < 0
      then fun i -> array.(i)
      else fun i -> array.(array_length - 1 - i)
    in
    of_increasing_iterator_unchecked ~len:array_length ~f:next
  ;;

  let of_sorted_array array ~compare_key =
    match array with
    | [||] | [| _ |] -> Result.Ok (of_sorted_array_unchecked array ~compare_key)
    | _ ->
      with_return (fun r ->
        let increasing =
          match compare_key (fst array.(0)) (fst array.(1)) with
          | 0 -> r.return (Or_error.error_string "of_sorted_array: duplicated elements")
          | i -> i < 0
        in
        for i = 1 to Array.length array - 2 do
          match compare_key (fst array.(i)) (fst array.(i + 1)) with
          | 0 -> r.return (Or_error.error_string "of_sorted_array: duplicated elements")
          | i ->
            if Poly.( <> ) (i < 0) increasing
            then
              r.return (Or_error.error_string "of_sorted_array: elements are not ordered")
        done;
        Result.Ok (of_sorted_array_unchecked array ~compare_key))
  ;;

  (* precondition: balanced, or one side [is_too_heavy] by at most 1 *)
  let[@inline] bal l x d r =
    let wl = weight l in
    let wr = weight r in
    if is_too_heavy ~weight:wl ~for_weight:wr
    then (
      match l with
      | Empty -> invalid_arg "Map.bal"
      | Leaf _ -> assert false (* a leaf never [is_too_heavy] *)
      | Node { left = ll; key = lv; data = ld; right = lr; weight = _ } ->
        if may_rotate_just_once
             ~inner_sibling_weight:(weight lr)
             ~outer_sibling_weight:(weight ll)
        then create ll lv ld (create lr x d r)
        else (
          match lr with
          | Empty -> invalid_arg "Map.bal"
          | Leaf { key = lrv; data = lrd } ->
            create (create ll lv ld Empty) lrv lrd (create Empty x d r)
          | Node { left = lrl; key = lrv; data = lrd; right = lrr; weight = _ } ->
            create (create ll lv ld lrl) lrv lrd (create lrr x d r)))
    else if is_too_heavy ~weight:wr ~for_weight:wl
    then (
      match r with
      | Empty -> invalid_arg "Map.bal"
      | Leaf _ -> assert false (* a leaf never [is_too_heavy] *)
      | Node { left = rl; key = rv; data = rd; right = rr; weight = _ } ->
        if may_rotate_just_once
             ~inner_sibling_weight:(weight rl)
             ~outer_sibling_weight:(weight rr)
        then create (create l x d rl) rv rd rr
        else (
          match rl with
          | Empty -> invalid_arg "Map.bal"
          | Leaf { key = rlv; data = rld } ->
            create (create l x d Empty) rlv rld (create Empty rv rd rr)
          | Node { left = rll; key = rlv; data = rld; right = rlr; weight = _ } ->
            create (create l x d rll) rlv rld (create rlr rv rd rr)))
    else create_with_weights ~wl ~wr l x d r
  ;;

  let is_empty = function
    | Empty -> true
    | _ -> false
  ;;

  let raise_key_already_present ~key ~sexp_of_key =
    Error.raise_s
      (Sexp.message "[Map.add_exn] got key already present" [ "key", key |> sexp_of_key ])
  ;;

  module Add_or_set = struct
    type t =
      | Add_exn_internal
      | Add_exn
      | Set
  end

  let rec find_and_add_or_set
    t
    ~key:x
    ~data
    ~compare_key
    ~sexp_of_key
    ~(add_or_set : Add_or_set.t)
    =
    match t with
    | Empty -> Leaf { key = x; data }
    | Leaf { key = v; data = d } ->
      let c = compare_key x v in
      if c = 0
      then (
        match add_or_set with
        | Add_exn_internal -> Exn.raise_without_backtrace Duplicate
        | Add_exn -> raise_key_already_present ~key:x ~sexp_of_key
        | Set -> Leaf { key = x; data })
      else if c < 0
      then
        Node
          { left = Leaf { key = x; data }; key = v; data = d; right = Empty; weight = 3 }
      else
        Node
          { left = Empty; key = v; data = d; right = Leaf { key = x; data }; weight = 3 }
    | Node { left = l; key = v; data = d; right = r; weight = w } ->
      let c = compare_key x v in
      if c = 0
      then (
        match add_or_set with
        | Add_exn_internal -> Exn.raise_without_backtrace Duplicate
        | Add_exn -> raise_key_already_present ~key:x ~sexp_of_key
        | Set -> Node { left = l; key = x; data; right = r; weight = w })
      else (
        let l, r =
          if c < 0
          then (
            let l =
              find_and_add_or_set ~key:x ~data l ~compare_key ~sexp_of_key ~add_or_set
            in
            l, r)
          else (
            let r =
              find_and_add_or_set ~key:x ~data r ~compare_key ~sexp_of_key ~add_or_set
            in
            l, r)
        in
        bal l v d r)
  ;;

  (* specialization of [set'] for the case when [key] is less than all the existing keys *)
  let rec set_min key data t =
    match t with
    | Empty -> Leaf { key; data }
    | Leaf { key = v; data = d } ->
      Node { left = Leaf { key; data }; key = v; data = d; right = Empty; weight = 3 }
    | Node { left = l; key = v; data = d; right = r; weight = _ } ->
      let l = set_min key data l in
      bal l v d r
  ;;

  (* specialization of [set'] for the case when [key] is greater than all the existing
     keys *)
  let rec set_max t key data =
    match t with
    | Empty -> Leaf { key; data }
    | Leaf { key = v; data = d } ->
      Node { left = Empty; key = v; data = d; right = Leaf { key; data }; weight = 3 }
    | Node { left = l; key = v; data = d; right = r; weight = _ } ->
      let r = set_max r key data in
      bal l v d r
  ;;

  let add_exn t ~key ~data ~compare_key ~sexp_of_key =
    find_and_add_or_set t ~key ~data ~compare_key ~sexp_of_key ~add_or_set:Add_exn
  ;;

  let add_exn_internal t ~key ~data ~compare_key ~sexp_of_key =
    find_and_add_or_set
      t
      ~key
      ~data
      ~compare_key
      ~sexp_of_key
      ~add_or_set:Add_exn_internal
  ;;

  let set t ~key ~data ~compare_key =
    find_and_add_or_set
      t
      ~key
      ~data
      ~compare_key
      ~sexp_of_key:(fun _ -> List [])
      ~add_or_set:Set
  ;;

  module Build_increasing : sig @@ portable
    type ('k, 'd, 'cmp) t

    val empty : ('k, 'd, 'cmp) t
    val get_empty : unit -> ('k, 'd, 'cmp) t
    val max_key : ('k, 'd, 'cmp) t -> 'k option
    val add_unchecked : ('k, 'd, 'cmp) t -> key:'k -> data:'d -> ('k, 'd, 'cmp) t
    val to_tree_unchecked : ('k, 'd, 'cmp) t -> ('k, 'd, 'cmp) tree
  end = struct
    type ('k, 'd, 'cmp) t = ('k * 'd) list

    let empty = []
    let get_empty () = []

    let max_key = function
      | [] -> None
      | (key, _) :: _ -> Some key
    ;;

    let add_unchecked t ~key ~data = (key, data) :: t

    let to_tree_unchecked = function
      | [] -> Empty
      | [ (key, data) ] -> Leaf { key; data }
      | list ->
        let len = List.length list in
        let local_ list = ref list in
        let rec local_ loop len =
          match len, !list with
          | 1, (key, data) :: tail ->
            list := tail;
            Leaf { key; data }
          | 2, (k2, d2) :: (k1, d1) :: tail ->
            list := tail;
            Node
              { left = Empty
              ; key = k1
              ; data = d1
              ; right = Leaf { key = k2; data = d2 }
              ; weight = 3
              }
          | 3, (k3, d3) :: (k2, d2) :: (k1, d1) :: tail ->
            list := tail;
            Node
              { left = Leaf { key = k1; data = d1 }
              ; key = k2
              ; data = d2
              ; right = Leaf { key = k3; data = d3 }
              ; weight = 4
              }
          | _, _ ->
            let nr = len / 2 in
            let nl = len - nr - 1 in
            let r = loop nr in
            (match !list with
             | [] -> assert false
             | (k, d) :: tail ->
               list := tail;
               let l = loop nl in
               create l k d r)
        in
        loop len [@nontail]
    ;;
  end

  let of_increasing_sequence seq ~compare_key =
    with_return (fun { return } ->
      let builder =
        Sequence.fold
          seq
          ~init:(Build_increasing.get_empty ())
          ~f:(fun builder (key, data) ->
            match Build_increasing.max_key builder with
            | Some prev_key when compare_key prev_key key >= 0 ->
              return (Or_error.error_string "of_increasing_sequence: non-increasing key")
            | _ -> Build_increasing.add_unchecked builder ~key ~data)
      in
      Ok (Build_increasing.to_tree_unchecked builder))
  ;;

  (* The [join] algorithm is taken from the github implementation[3] of [4]. It is like
     [create] and [bal], and works on trees of arbitrary weight.

     We adapt our functions from [include/pam/balance_utils.h]. We use the name [join] for
     [node_join], [join_rotating_right] for [right_join], and [join_rotating_left] for
     [left_join]. We use our [may_rotate_just_once] where they use [is_single_rotation].

     In the two recursive helpers, we've moved the initial balance check to just outside
     the recursive call instead of just inside the function definition, since the
     condition is known the first time we call it.

     [3] https://github.com/cmuparlay/PAM/tree/2a30a856a55698c7aa7e7ebf86ee826864bbcf86

     [4] Just Join for Parallel Ordered Sets; Blelloch, Ferizovic, and Sun; SPAA ’16, July
     11-13, 20. https://www.cs.cmu.edu/~guyb/papers/BFS16.pdf *)
  include struct
    open struct
      (* These helpers are intended only to be called from [join]. *)

      (* For [join_rotating_right l lk ld m rk rd r], the arguments correspond to an
         unbalanced tree with the shape:
         {v
               (rk,rd)
                /   \
            (lk,ld)  r
              / \
             l   m
         v}

         Precondition: [create l lk ld m] [is_too_heavy] for [r], and [r = Node _]. *)
      let rec join_rotating_right l lk ld m rk rd r =
        let mr =
          (* Recur down [m]'s right side until [create m rk rd r] is balanced. *)
          match m with
          | Node { left = ml; key = mk; data = md; right = mr; weight = mw }
            when is_too_heavy ~weight:mw ~for_weight:(weight r) ->
            join_rotating_right ml mk md mr rk rd r
          | _ -> create m rk rd r
        in
        (* Since [r] is a [Node], [mr] must be a [Node]. *)
        match mr with
        | Empty -> assert false
        | Leaf _ -> assert false
        | Node { left = m; key = rk; data = rd; right = r; weight = mrw } ->
          (* Now re-add [l] with 0-2 rotations. Proven sufficient in literature above. *)
          let lw = weight l in
          (match m with
           | Node { left = ml; key = mk; data = md; right = mr; weight = mw }
             when is_too_heavy ~weight:mrw ~for_weight:lw ->
             if may_rotate_just_once
                  ~inner_sibling_weight:mw
                  ~outer_sibling_weight:(weight r)
             then create (create l lk ld m) rk rd r
             else create (create l lk ld ml) mk md (create mr rk rd r)
           | _ -> create l lk ld mr)
      ;;

      (* Rotating left proceeds symmetrically to rotating right. *)

      (* For [join_rotating_left l lk ld m rk rd r], the arguments correspond to an
         unbalanced tree with the shape:
         {v
               (lk,ld)
                /   \
               l  (rk,rd)
                    / \
                   m   r
         v}

         Precondition: [create m rk rd r] [is_too_heavy] for [l], and [l = Node _]. *)
      let rec join_rotating_left l lk ld m rk rd r =
        let lm =
          (* Recur down [m]'s left side until [create l lk ld m] is balanced. *)
          match m with
          | Node { left = ml; key = mk; data = md; right = mr; weight = mw }
            when is_too_heavy ~weight:mw ~for_weight:(weight l) ->
            join_rotating_left l lk ld ml mk md mr
          | _ -> create l lk ld m
        in
        (* Since [l] is a [Node], [lm] must be a [Node]. *)
        match lm with
        | Empty -> assert false
        | Leaf _ -> assert false
        | Node { left = l; key = lk; data = ld; right = m; weight = lmw } ->
          (* Now re-add [r] with 0-2 rotations. Proven sufficient in literature above. *)
          let rw = weight r in
          (match m with
           | Node { left = ml; key = mk; data = md; right = mr; weight = mw }
             when is_too_heavy ~weight:lmw ~for_weight:rw ->
             if may_rotate_just_once
                  ~inner_sibling_weight:mw
                  ~outer_sibling_weight:(weight l)
             then create l lk ld (create m rk rd r)
             else create (create l lk ld ml) mk md (create mr rk rd r)
           | _ -> create lm rk rd r)
      ;;
    end

    (* Like [create] and [bal], for arbitrary height differences. See more detailed
       comment at start of [include struct ...] above. *)
    let join l k d r =
      (* Cases adding just one or two keys are straightforward. *)
      match l, r with
      | Empty, _ -> set_min k d r
      | _, Empty -> set_max l k d
      | Leaf { key = lk; data = ld }, _ -> set_min lk ld (set_min k d r)
      | _, Leaf { key = rk; data = rd } -> set_max (set_max l k d) rk rd
      | ( Node { left = ll; key = lk; data = ld; right = lr; weight = lw }
        , Node { left = rl; key = rk; data = rd; right = rr; weight = rw } ) ->
        (* Otherwise, recur down the heavier side and rotate toward the lighter side. *)
        if is_too_heavy ~weight:lw ~for_weight:rw
        then join_rotating_right ll lk ld lr k d r
        else if is_too_heavy ~weight:rw ~for_weight:lw
        then join_rotating_left l k d rl rk rd rr
        else create l k d r
    ;;
  end

  let[@inline] rec split_gen t x ~compare_key =
    match t with
    | Empty -> Empty, None, Empty
    | Leaf { key = k; data = d } ->
      let cmp = compare_key k in
      if cmp = 0
      then Empty, Some (k, d), Empty
      else if cmp < 0
      then Empty, None, t
      else t, None, Empty
    | Node { left = l; key = k; data = d; right = r; weight = _ } ->
      let cmp = compare_key k in
      if cmp = 0
      then l, Some (k, d), r
      else if cmp < 0
      then (
        let ll, maybe, lr = split_gen l x ~compare_key in
        ll, maybe, join lr k d r)
      else (
        let rl, maybe, rr = split_gen r x ~compare_key in
        join l k d rl, maybe, rr)
  ;;

  let split t x ~compare_key = split_gen t x ~compare_key:(fun y -> compare_key x y)

  (* This function does not really reinsert [x], but just arranges so that [split]
     produces the equivalent tree in the first place. *)
  let split_and_reinsert_boundary t ~into x ~compare_key =
    let left, boundary_opt, right =
      split_gen
        t
        x
        ~compare_key:
          (match into with
           | `Left ->
             fun y ->
               (match compare_key x y with
                | 0 -> 1
                | res -> res)
           | `Right ->
             fun y ->
               (match compare_key x y with
                | 0 -> -1
                | res -> res))
    in
    assert (Option.is_none boundary_opt);
    left, right
  ;;

  let split_range
    t
    ~(lower_bound : 'a Maybe_bound.t)
    ~(upper_bound : 'a Maybe_bound.t)
    ~compare_key
    =
    if Maybe_bound.bounds_crossed
         ~compare:compare_key
         ~lower:lower_bound
         ~upper:upper_bound
    then Empty, Empty, Empty
    else (
      let left, mid_and_right =
        match lower_bound with
        | Unbounded -> Empty, t
        | Incl lb -> split_and_reinsert_boundary ~into:`Right t lb ~compare_key
        | Excl lb -> split_and_reinsert_boundary ~into:`Left t lb ~compare_key
      in
      let mid, right =
        match upper_bound with
        | Unbounded -> mid_and_right, Empty
        | Incl lb -> split_and_reinsert_boundary ~into:`Left mid_and_right lb ~compare_key
        | Excl lb ->
          split_and_reinsert_boundary ~into:`Right mid_and_right lb ~compare_key
      in
      left, mid, right)
  ;;

  let length = function
    | Empty -> 0
    | Leaf _ -> 1
    | Node { left = _; key = _; data = _; right = _; weight = w } -> w - 1
  ;;

  let count_lt_eq_gt t k ~compare_key = exclave_
    let rec local_ loop t ~lt ~gt = exclave_
      match t with
      | Empty -> lt, 0, gt
      | Leaf { key; data = _ } ->
        (match compare_key key k with
         | c when c < 0 -> lt + 1, 0, gt
         | c when c > 0 -> lt, 0, gt + 1
         | _ -> lt, 1, gt)
      | Node { left; key; data = _; right; weight = _ } ->
        (match compare_key key k with
         | c when c < 0 -> loop right ~lt:(lt + 1 + length left) ~gt
         | c when c > 0 -> loop left ~lt ~gt:(gt + 1 + length right)
         | _ -> lt + length left, 1, gt + length right)
    in
    loop t ~lt:0 ~gt:0
  ;;

  let count_lt t k ~compare_key =
    let lt, _, _ = count_lt_eq_gt t k ~compare_key in
    lt
  ;;

  let count_le t k ~compare_key =
    let lt, eq, _ = count_lt_eq_gt t k ~compare_key in
    lt + eq
  ;;

  let count_gt t k ~compare_key =
    let _, _, gt = count_lt_eq_gt t k ~compare_key in
    gt
  ;;

  let count_ge t k ~compare_key =
    let _, eq, gt = count_lt_eq_gt t k ~compare_key in
    eq + gt
  ;;

  let rec find t x ~compare_key =
    match t with
    | Empty -> None
    | Leaf { key = v; data = d } -> if compare_key x v = 0 then Some d else None
    | Node { left = l; key = v; data = d; right = r; weight = _ } ->
      let c = compare_key x v in
      if c = 0 then Some d else find (if c < 0 then l else r) x ~compare_key
  ;;

  let add_multi t ~key ~data ~compare_key =
    let data = data :: Option.value (find t key ~compare_key) ~default:[] in
    set ~key ~data t ~compare_key
  ;;

  let find_multi t x ~compare_key =
    match find t x ~compare_key with
    | None -> []
    | Some l -> l
  ;;

  let find_exn =
    let if_not_found key ~sexp_of_key =
      raise (Not_found_s (List [ Atom "Map.find_exn: not found"; sexp_of_key key ]))
    in
    let rec find_exn t x ~compare_key ~sexp_of_key =
      match t with
      | Empty -> if_not_found x ~sexp_of_key
      | Leaf { key = v; data = d } ->
        if compare_key x v = 0 then d else if_not_found x ~sexp_of_key
      | Node { left = l; key = v; data = d; right = r; weight = _ } ->
        let c = compare_key x v in
        if c = 0 then d else find_exn (if c < 0 then l else r) x ~compare_key ~sexp_of_key
    in
    (* named to preserve symbol in compiled binary *)
    find_exn
  ;;

  let mem t x ~compare_key = Option.is_some (find t x ~compare_key)

  let rec min_elt = function
    | Empty -> None
    | Leaf { key = k; data = d } -> Some (k, d)
    | Node { left = Empty; key = k; data = d; right = _; weight = _ } -> Some (k, d)
    | Node { left = l; key = _; data = _; right = _; weight = _ } -> min_elt l
  ;;

  exception Map_min_elt_exn_of_empty_map [@@deriving sexp]
  exception Map_max_elt_exn_of_empty_map [@@deriving sexp]

  let min_elt_exn t =
    match min_elt t with
    | None -> raise Map_min_elt_exn_of_empty_map
    | Some v -> v
  ;;

  let rec max_elt = function
    | Empty -> None
    | Leaf { key = k; data = d } -> Some (k, d)
    | Node { left = _; key = k; data = d; right = Empty; weight = _ } -> Some (k, d)
    | Node { left = _; key = _; data = _; right = r; weight = _ } -> max_elt r
  ;;

  let max_elt_exn t =
    match max_elt t with
    | None -> raise Map_max_elt_exn_of_empty_map
    | Some v -> v
  ;;

  let rec remove_min_elt t =
    match t with
    | Empty -> invalid_arg "Map.remove_min_elt"
    | Leaf _ -> Empty
    | Node { left = Empty; key = _; data = _; right = r; weight = _ } -> r
    | Node { left = l; key = x; data = d; right = r; weight = _ } ->
      bal (remove_min_elt l) x d r
  ;;

  let append ~lower_part ~upper_part ~compare_key =
    match max_elt lower_part, min_elt upper_part with
    | None, _ -> `Ok upper_part
    | _, None -> `Ok lower_part
    | Some (max_lower, _), Some (min_upper, v) when compare_key max_lower min_upper < 0 ->
      let upper_part_without_min = remove_min_elt upper_part in
      `Ok (join lower_part min_upper v upper_part_without_min)
    | _ -> `Overlapping_key_ranges
  ;;

  let fold_range_inclusive =
    (* This assumes that min <= max, which is checked by the outer function. *)
    let rec go t ~min ~max ~init ~f ~compare_key =
      match t with
      | Empty -> init
      | Leaf { key = k; data = d } ->
        if compare_key k min < 0 || compare_key k max > 0
        then (*=k < min || k > max *)
          init
        else f ~key:k ~data:d init
      | Node { left = l; key = k; data = d; right = r; weight = _ } ->
        let c_min = compare_key k min in
        if c_min < 0
        then
          (* if k < min, then this node and its left branch are outside our range *)
          go r ~min ~max ~init ~f ~compare_key
        else if c_min = 0
        then
          (* if k = min, then this node's left branch is outside our range *)
          go r ~min ~max ~init:(f ~key:k ~data:d init) ~f ~compare_key
        else (
          (* k > min *)
          let z = go l ~min ~max ~init ~f ~compare_key in
          let c_max = compare_key k max in
          (* if k > max, we're done *)
          if c_max > 0
          then z
          else (
            let z = f ~key:k ~data:d z in
            (* if k = max, then we fold in this one last value and we're done *)
            if c_max = 0 then z else go r ~min ~max ~init:z ~f ~compare_key))
    in
    fun t ~min ~max ~init ~f ~compare_key ->
      if compare_key min max <= 0 then go t ~min ~max ~init ~f ~compare_key else init
  ;;

  let range_to_alist t ~min ~max ~compare_key =
    List.rev
      (fold_range_inclusive
         t
         ~min
         ~max
         ~init:[]
         ~f:(fun ~key ~data l -> (key, data) :: l)
         ~compare_key)
  ;;

  (* preconditions:
     - all elements in t1 are less than elements in t2
     - neither [is_too_heavy] for the other *)
  let concat_unchecked t1 t2 =
    match t1, t2 with
    | Empty, t -> t
    | t, Empty -> t
    | _, _ ->
      let x, d = min_elt_exn t2 in
      bal t1 x d (remove_min_elt t2)
  ;;

  (* similar to [concat_unchecked], and balances trees of arbitrary weight differences *)
  let concat_and_balance_unchecked t1 t2 =
    match t1, t2 with
    | Empty, t -> t
    | t, Empty -> t
    | _, _ ->
      let x, d = min_elt_exn t2 in
      join t1 x d (remove_min_elt t2)
  ;;

  let rec remove t x ~compare_key =
    match t with
    | Empty -> t
    | Leaf { key = v; data = _ } -> if compare_key x v = 0 then Empty else t
    | Node { left = l; key = v; data = d; right = r; weight = w } ->
      let c = compare_key x v in
      if c = 0
      then concat_unchecked l r
      else (
        let l, r =
          if c < 0
          then (
            let l = remove l x ~compare_key in
            l, r)
          else (
            let r = remove r x ~compare_key in
            l, r)
        in
        if w = weight l + weight r then t else bal l v d r)
  ;;

  let rec change t key ~f ~compare_key =
    match t with
    | Empty ->
      (match f None with
       | None -> Empty
       | Some data -> Leaf { key; data })
    | Leaf { key = v; data = d } ->
      let c = compare_key key v in
      if c = 0
      then (
        match f (Some d) with
        | None -> Empty
        | Some d' -> Leaf { key = v; data = d' })
      else if c < 0
      then (
        let l' = change Empty key ~f ~compare_key in
        if phys_equal l' t then t else bal l' v d Empty)
      else (
        let r' = change Empty key ~f ~compare_key in
        if phys_equal r' t then t else bal Empty v d r')
    | Node { left = l; key = v; data = d; right = r; weight = w } ->
      let c = compare_key key v in
      if c = 0
      then (
        match f (Some d) with
        | None -> concat_unchecked l r
        | Some data -> Node { left = l; key; data; right = r; weight = w })
      else if c < 0
      then (
        let l' = change l key ~f ~compare_key in
        if phys_equal l' l then t else bal l' v d r)
      else (
        let r' = change r key ~f ~compare_key in
        if phys_equal r' r then t else bal l v d r')
  ;;

  let rec update t key ~f ~compare_key =
    match t with
    | Empty ->
      let data = f None in
      Leaf { key; data }
    | Leaf { key = v; data = d } ->
      let c = compare_key key v in
      if c = 0
      then (
        let d' = f (Some d) in
        Leaf { key = v; data = d' })
      else if c < 0
      then (
        let l = update Empty key ~f ~compare_key in
        bal l v d Empty)
      else (
        let r = update Empty key ~f ~compare_key in
        bal Empty v d r)
    | Node { left = l; key = v; data = d; right = r; weight = w } ->
      let c = compare_key key v in
      if c = 0
      then (
        let data = f (Some d) in
        Node { left = l; key; data; right = r; weight = w })
      else if c < 0
      then (
        let l = update l key ~f ~compare_key in
        bal l v d r)
      else (
        let r = update r key ~f ~compare_key in
        bal l v d r)
  ;;

  let update_and_return t key ~f ~compare_key =
    let result_ref = ref None in
    let new_t =
      update t key ~compare_key ~f:(fun elt ->
        let res = f elt in
        result_ref := Some res;
        res)
    in
    (* This [Option.value_exn] is safe because it is guaranteed by [update] that [f] will
       be called exactly once. *)
    !result_ref |> Option.value_exn |> Modes.Global.wrap, new_t
  ;;

  let remove_multi t key ~compare_key =
    change t key ~compare_key ~f:(function
      | None | Some ([] | [ _ ]) -> None
      | Some (_ :: (_ :: _ as non_empty_tail)) -> Some non_empty_tail)
  ;;

  let rec iter_keys t ~f =
    match t with
    | Empty -> ()
    | Leaf { key = v; data = _ } -> f v
    | Node { left = l; key = v; data = _; right = r; weight = _ } ->
      iter_keys ~f l;
      f v;
      iter_keys ~f r
  ;;

  let rec iter t ~f =
    match t with
    | Empty -> ()
    | Leaf { key = _; data = d } -> f d
    | Node { left = l; key = _; data = d; right = r; weight = _ } ->
      iter ~f l;
      f d;
      iter ~f r
  ;;

  let rec iteri t ~f =
    match t with
    | Empty -> ()
    | Leaf { key = v; data = d } -> f ~key:v ~data:d
    | Node { left = l; key = v; data = d; right = r; weight = _ } ->
      iteri ~f l;
      f ~key:v ~data:d;
      iteri ~f r
  ;;

  let iteri_until =
    let rec iteri_until_loop t ~f : Continue_or_stop.t =
      match t with
      | Empty -> Continue
      | Leaf { key = v; data = d } -> f ~key:v ~data:d
      | Node { left = l; key = v; data = d; right = r; weight = _ } ->
        (match iteri_until_loop ~f l with
         | Stop -> Stop
         | Continue ->
           (match f ~key:v ~data:d with
            | Stop -> Stop
            | Continue -> iteri_until_loop ~f r))
    in
    fun t ~f -> Finished_or_unfinished.of_continue_or_stop (iteri_until_loop t ~f)
  ;;

  let rec map t ~f =
    match t with
    | Empty -> Empty
    | Leaf { key = v; data = d } -> Leaf { key = v; data = f d }
    | Node { left = l; key = v; data = d; right = r; weight = w } ->
      let l' = map ~f l in
      let d' = f d in
      let r' = map ~f r in
      Node { left = l'; key = v; data = d'; right = r'; weight = w }
  ;;

  let rec mapi t ~f =
    match t with
    | Empty -> Empty
    | Leaf { key = v; data = d } -> Leaf { key = v; data = f ~key:v ~data:d }
    | Node { left = l; key = v; data = d; right = r; weight = w } ->
      let l' = mapi ~f l in
      let d' = f ~key:v ~data:d in
      let r' = mapi ~f r in
      Node { left = l'; key = v; data = d'; right = r'; weight = w }
  ;;

  let rec fold t ~init:accu ~f =
    match t with
    | Empty -> accu
    | Leaf { key = v; data = d } -> f ~key:v ~data:d accu
    | Node { left = l; key = v; data = d; right = r; weight = _ } ->
      fold ~f r ~init:(f ~key:v ~data:d (fold ~f l ~init:accu))
  ;;

  let fold_until t ~init ~f ~finish =
    let rec fold_until_loop t ~acc ~f : (_, _) Container.Continue_or_stop.t =
      match t with
      | Empty -> Continue acc
      | Leaf { key = v; data = d } -> f ~key:v ~data:d acc
      | Node { left = l; key = v; data = d; right = r; weight = _ } ->
        (match fold_until_loop l ~acc ~f with
         | Stop final -> Stop final
         | Continue acc ->
           (match f ~key:v ~data:d acc with
            | Stop final -> Stop final
            | Continue acc -> fold_until_loop r ~acc ~f))
    in
    match fold_until_loop t ~acc:init ~f with
    | Continue acc -> finish acc [@nontail]
    | Stop stop -> stop
  ;;

  let%template[@mode m = (global, local)] fold_right t ~init ~f =
    (let rec loop accu t =
       match[@exclave_if_local m ~reasons:[ May_return_local ]] t with
       | Empty -> accu
       | Leaf { key = v; data = d } -> f ~key:v ~data:d accu
       | Node { left = l; key = v; data = d; right = r; weight = _ } ->
         loop (f ~key:v ~data:d (loop accu r)) l
     in
     loop init t [@nontail])
    [@exclave_if_local m ~reasons:[ May_return_local ]]
  ;;

  let rec filter_mapi t ~f =
    match t with
    | Empty -> Empty
    | Leaf { key = v; data = d } ->
      (match f ~key:v ~data:d with
       | Some new_data -> Leaf { key = v; data = new_data }
       | None -> Empty)
    | Node { left = l; key = v; data = d; right = r; weight = _ } ->
      let l' = filter_mapi l ~f in
      let new_data = f ~key:v ~data:d in
      let r' = filter_mapi r ~f in
      (match new_data with
       | Some new_data -> join l' v new_data r'
       | None -> concat_and_balance_unchecked l' r')
  ;;

  let rec filteri t ~f =
    match t with
    | Empty -> Empty
    | Leaf { key = v; data = d } ->
      (match f ~key:v ~data:d with
       | true -> t
       | false -> Empty)
    | Node { left = l; key = v; data = d; right = r; weight = _ } ->
      let l' = filteri l ~f in
      let keep_data = f ~key:v ~data:d in
      let r' = filteri r ~f in
      if phys_equal l l' && keep_data && phys_equal r r'
      then t
      else (
        match keep_data with
        | true -> join l' v d r'
        | false -> concat_and_balance_unchecked l' r')
  ;;

  let filter t ~f = filteri t ~f:(fun ~key:_ ~data -> f data) [@nontail]
  let filter_keys t ~f = filteri t ~f:(fun ~key ~data:_ -> f key) [@nontail]
  let filter_map t ~f = filter_mapi t ~f:(fun ~key:_ ~data -> f data) [@nontail]

  let partition_mapi t ~f =
    let t1, t2 =
      fold
        t
        ~init:(Build_increasing.get_empty (), Build_increasing.get_empty ())
        ~f:(fun ~key ~data (t1, t2) ->
          match (f ~key ~data : _ Either.t) with
          | First x -> Build_increasing.add_unchecked t1 ~key ~data:x, t2
          | Second y -> t1, Build_increasing.add_unchecked t2 ~key ~data:y)
    in
    Build_increasing.to_tree_unchecked t1, Build_increasing.to_tree_unchecked t2
  ;;

  let partition_map t ~f = partition_mapi t ~f:(fun ~key:_ ~data -> f data) [@nontail]
  let partition_result t = partition_map t ~f:Result.to_either [@nontail]

  let partitioni_tf t ~f =
    let rec loop t ~f =
      match t with
      | Empty -> Empty, Empty
      | Leaf { key = v; data = d } ->
        (match f ~key:v ~data:d with
         | true -> t, Empty
         | false -> Empty, t)
      | Node { left = l; key = v; data = d; right = r; weight = _ } ->
        let l't, l'f = loop l ~f in
        let keep_data_t = f ~key:v ~data:d in
        let r't, r'f = loop r ~f in
        let mk l' keep_data r' =
          if phys_equal l l' && keep_data && phys_equal r r'
          then t
          else (
            match keep_data with
            | true -> join l' v d r'
            | false -> concat_and_balance_unchecked l' r')
        in
        mk l't keep_data_t r't, mk l'f (not keep_data_t) r'f
    in
    loop t ~f
  ;;

  let partition_tf t ~f = partitioni_tf t ~f:(fun ~key:_ ~data -> f data) [@nontail]

  module Enum : Private.Enum with type ('k, 'v, 'cmp) tree := ('k, 'v, 'cmp) tree = struct
    type increasing
    type decreasing

    type ('k, 'v, 'cmp, 'direction) nonempty =
      | More of 'k * 'v * ('k, 'v, 'cmp) tree * ('k, 'v, 'cmp, 'direction) t

    and ('k, 'v, 'cmp, 'direction) t = ('k, 'v, 'cmp, 'direction) nonempty or_null

    let rec cons t (e : (_, _, _, increasing) t) : (_, _, _, increasing) t =
      match t with
      | Empty -> e
      | Leaf { key = v; data = d } -> This (More (v, d, Empty, e))
      | Node { left = l; key = v; data = d; right = r; weight = _ } ->
        cons l (This (More (v, d, r, e)))
    ;;

    let rec cons_right t (e : (_, _, _, decreasing) t) : (_, _, _, decreasing) t =
      match t with
      | Empty -> e
      | Leaf { key = v; data = d } -> This (More (v, d, Empty, e))
      | Node { left = l; key = v; data = d; right = r; weight = _ } ->
        cons_right r (This (More (v, d, l, e)))
    ;;

    let of_tree tree : (_, _, _, increasing) t = cons tree Null
    let of_tree_right tree : (_, _, _, decreasing) t = cons_right tree Null

    let starting_at_increasing t key compare : (_, _, _, increasing) t =
      let rec loop t e =
        match t with
        | Empty -> e
        | Leaf { key = v; data = d } ->
          if compare v key < 0 then e else This (More (v, d, Empty, e))
        | Node { left = l; key = v; data = d; right = r; weight = _ } ->
          if compare v key < 0 then loop r e else loop l (This (More (v, d, r, e)))
      in
      loop t Null
    ;;

    let starting_at_decreasing t key compare : (_, _, _, decreasing) t =
      let rec loop t e =
        match t with
        | Empty -> e
        | Leaf { key = v; data = d } ->
          if compare v key > 0 then e else This (More (v, d, Empty, e))
        | Node { left = l; key = v; data = d; right = r; weight = _ } ->
          if compare v key > 0 then loop l e else loop r (This (More (v, d, l, e)))
      in
      loop t Null
    ;;

    let step_deeper_exn tree e =
      match tree with
      | Empty -> assert false
      | Leaf { key = v; data = d } -> Empty, This (More (v, d, Empty, e))
      | Node { left = l; key = v; data = d; right = r; weight = _ } ->
        l, This (More (v, d, r, e))
    ;;

    (* [drop_phys_equal_prefix tree1 acc1 tree2 acc2] drops the largest physically-equal
       prefix of tree1 and tree2 that they share, and then prepends the remaining data
       into acc1 and acc2, respectively. This can be asymptotically faster than [cons]
       even if it skips a small proportion of the tree because [cons] is always O(log(n))
       in the size of the tree, while this function is O(log(n/m)) where [m] is the size
       of the part of the tree that is skipped. *)
    let rec drop_phys_equal_prefix tree1 acc1 tree2 acc2 =
      if phys_equal tree1 tree2
      then (* Trees are equal, drop them *)
        acc1, acc2
      else (
        let w2 = weight tree2 in
        let w1 = weight tree1 in
        if w2 = w1
        then (
          (* Neither tree can be a subtree of the other, descend into both *)
          let tree1, acc1 = step_deeper_exn tree1 acc1 in
          let tree2, acc2 = step_deeper_exn tree2 acc2 in
          drop_phys_equal_prefix tree1 acc1 tree2 acc2)
        else if w2 > w1
        then (
          (* [tree1] could be a subtree of [tree2], step into [tree2] only *)
          let tree2, acc2 = step_deeper_exn tree2 acc2 in
          drop_phys_equal_prefix tree1 acc1 tree2 acc2)
        else (
          (* [tree2] could be a subtree of [tree1], step into [tree1] only *)
          let tree1, acc1 = step_deeper_exn tree1 acc1 in
          drop_phys_equal_prefix tree1 acc1 tree2 acc2))
    ;;

    let compare compare_key compare_data t1 t2 =
      let rec loop t1 t2 =
        match t1, t2 with
        | Null, Null -> 0
        | Null, _ -> -1
        | _, Null -> 1
        | This (More (v1, d1, r1, e1)), This (More (v2, d2, r2, e2)) ->
          let c = compare_key v1 v2 in
          if c <> 0
          then c
          else (
            let c = compare_data d1 d2 in
            if c <> 0
            then c
            else (
              let e1, e2 = drop_phys_equal_prefix r1 e1 r2 e2 in
              loop e1 e2))
      in
      loop t1 t2
    ;;

    let equal compare_key data_equal t1 t2 =
      let rec loop t1 t2 =
        match t1, t2 with
        | Null, Null -> true
        | Null, _ | _, Null -> false
        | This (More (v1, d1, r1, e1)), This (More (v2, d2, r2, e2)) ->
          compare_key v1 v2 = 0
          && data_equal d1 d2
          &&
          let e1, e2 = drop_phys_equal_prefix r1 e1 r2 e2 in
          loop e1 e2
      in
      loop t1 t2
    ;;

    let rec fold ~init ~f = function
      | Null -> init
      | This (More (key, data, tree, enum)) ->
        let next = f ~key ~data init in
        fold (cons tree enum) ~init:next ~f
    ;;

    let fold2 compare_key t1 t2 ~init ~f =
      let rec loop t1 t2 curr =
        match t1, t2 with
        | Null, Null -> curr
        | Null, _ ->
          fold t2 ~init:curr ~f:(fun ~key ~data acc -> f ~key ~data:(`Right data) acc)
          [@nontail]
        | _, Null ->
          fold t1 ~init:curr ~f:(fun ~key ~data acc -> f ~key ~data:(`Left data) acc)
          [@nontail]
        | This (More (k1, v1, tree1, enum1)), This (More (k2, v2, tree2, enum2)) ->
          let compare_result = compare_key k1 k2 in
          if compare_result = 0
          then (
            let next = f ~key:k1 ~data:(`Both (v1, v2)) curr in
            loop (cons tree1 enum1) (cons tree2 enum2) next)
          else if compare_result < 0
          then (
            let next = f ~key:k1 ~data:(`Left v1) curr in
            loop (cons tree1 enum1) t2 next)
          else (
            let next = f ~key:k2 ~data:(`Right v2) curr in
            loop t1 (cons tree2 enum2) next)
      in
      loop t1 t2 init [@nontail]
    ;;

    let symmetric_diff t1 t2 ~compare_key ~data_equal =
      let step state =
        match state with
        | Null, Null -> Sequence.Step.Done
        | Null, This (More (key, data, tree, enum)) ->
          Sequence.Step.Yield { value = key, `Right data; state = Null, cons tree enum }
        | This (More (key, data, tree, enum)), Null ->
          Sequence.Step.Yield { value = key, `Left data; state = cons tree enum, Null }
        | ( (This (More (k1, v1, tree1, enum1)) as left)
          , (This (More (k2, v2, tree2, enum2)) as right) ) ->
          let compare_result = compare_key k1 k2 in
          if compare_result = 0
          then (
            let next_state = drop_phys_equal_prefix tree1 enum1 tree2 enum2 in
            if data_equal v1 v2
            then Sequence.Step.Skip { state = next_state }
            else Sequence.Step.Yield { value = k1, `Unequal (v1, v2); state = next_state })
          else if compare_result < 0
          then
            Sequence.Step.Yield { value = k1, `Left v1; state = cons tree1 enum1, right }
          else
            Sequence.Step.Yield { value = k2, `Right v2; state = left, cons tree2 enum2 }
      in
      Sequence.unfold_step ~init:(drop_phys_equal_prefix t1 Null t2 Null) ~f:step
    ;;

    let fold_symmetric_diff t1 t2 ~compare_key ~data_equal ~init ~f =
      let add acc k v = f acc (k, `Right v) in
      let remove acc k v = f acc (k, `Left v) in
      let rec loop left right acc =
        match left, right with
        | Null, enum ->
          fold enum ~init:acc ~f:(fun ~key ~data acc -> add acc key data) [@nontail]
        | enum, Null ->
          fold enum ~init:acc ~f:(fun ~key ~data acc -> remove acc key data) [@nontail]
        | ( (This (More (k1, v1, tree1, enum1)) as left)
          , (This (More (k2, v2, tree2, enum2)) as right) ) ->
          let compare_result = compare_key k1 k2 in
          if compare_result = 0
          then (
            let acc = if data_equal v1 v2 then acc else f acc (k1, `Unequal (v1, v2)) in
            let enum1, enum2 = drop_phys_equal_prefix tree1 enum1 tree2 enum2 in
            loop enum1 enum2 acc)
          else if compare_result < 0
          then (
            let acc = remove acc k1 v1 in
            loop (cons tree1 enum1) right acc)
          else (
            let acc = add acc k2 v2 in
            loop left (cons tree2 enum2) acc)
      in
      let left, right = drop_phys_equal_prefix t1 Null t2 Null in
      loop left right init [@nontail]
    ;;
  end

  let to_sequence_increasing comparator ~from_key t =
    let next enum =
      match enum with
      | Null -> Sequence.Step.Done
      | This (Enum.More (k, v, t, e)) ->
        Sequence.Step.Yield { value = k, v; state = Enum.cons t e }
    in
    let init =
      match from_key with
      | None -> Enum.of_tree t
      | Some key -> Enum.starting_at_increasing t key (Comparator.compare comparator)
    in
    Sequence.unfold_step ~init ~f:next
  ;;

  let to_sequence_decreasing comparator ~from_key t =
    let next enum =
      match enum with
      | Null -> Sequence.Step.Done
      | This (Enum.More (k, v, t, e)) ->
        Sequence.Step.Yield { value = k, v; state = Enum.cons_right t e }
    in
    let init =
      match from_key with
      | None -> Enum.of_tree_right t
      | Some key -> Enum.starting_at_decreasing t key (Comparator.compare comparator)
    in
    Sequence.unfold_step ~init ~f:next
  ;;

  let to_sequence
    comparator
    ?(order = `Increasing_key)
    ?keys_greater_or_equal_to
    ?keys_less_or_equal_to
    t
    =
    let inclusive_bound side t bound =
      let compare_key = Comparator.compare comparator in
      let l, maybe, r = split t bound ~compare_key in
      let t = side (l, r) in
      match maybe with
      | None -> t
      | Some (key, data) -> set t ~key ~data ~compare_key
    in
    match order with
    | `Increasing_key ->
      let t = Option.fold keys_less_or_equal_to ~init:t ~f:(inclusive_bound fst) in
      to_sequence_increasing comparator ~from_key:keys_greater_or_equal_to t
    | `Decreasing_key ->
      let t = Option.fold keys_greater_or_equal_to ~init:t ~f:(inclusive_bound snd) in
      to_sequence_decreasing comparator ~from_key:keys_less_or_equal_to t
  ;;

  let compare compare_key compare_data t1 t2 =
    let e1, e2 = Enum.drop_phys_equal_prefix t1 Null t2 Null in
    Enum.compare compare_key compare_data e1 e2
  ;;

  let equal compare_key data_equal t1 t2 =
    let e1, e2 = Enum.drop_phys_equal_prefix t1 Null t2 Null in
    Enum.equal compare_key data_equal e1 e2
  ;;

  let iter2 t1 t2 ~f ~compare_key =
    Enum.fold2
      compare_key
      (Enum.of_tree t1)
      (Enum.of_tree t2)
      ~init:()
      ~f:(fun ~key ~data () -> f ~key ~data)
    [@nontail]
  ;;

  let fold2 t1 t2 ~init ~f ~compare_key =
    Enum.fold2 compare_key (Enum.of_tree t1) (Enum.of_tree t2) ~f ~init
  ;;

  let symmetric_diff = Enum.symmetric_diff

  let fold_symmetric_diff t1 t2 ~compare_key ~data_equal ~init ~f =
    (* [Enum.fold_diffs] is a correct implementation of this function, but is considerably
       slower, as we have to allocate quite a lot of state to track enumeration of a tree.
       Avoid if we can.
    *)
    let slow x y ~init = Enum.fold_symmetric_diff x y ~compare_key ~data_equal ~f ~init in
    let add acc k v = f acc (k, `Right v) in
    let remove acc k v = f acc (k, `Left v) in
    let delta acc k v v' = if data_equal v v' then acc else f acc (k, `Unequal (v, v')) in
    (* If two trees have the same structure at the root (and the same key, if they're
       [Node]s) we can trivially diff each subpart in obvious ways. *)
    let rec loop t t' acc =
      if phys_equal t t'
      then acc
      else (
        match t, t' with
        | Empty, new_vals ->
          fold new_vals ~init:acc ~f:(fun ~key ~data acc -> add acc key data) [@nontail]
        | old_vals, Empty ->
          fold old_vals ~init:acc ~f:(fun ~key ~data acc -> remove acc key data)
          [@nontail]
        | Leaf { key = k; data = v }, Leaf { key = k'; data = v' } ->
          (match compare_key k k' with
           | x when x = 0 -> delta acc k v v'
           | x when x < 0 ->
             let acc = remove acc k v in
             add acc k' v'
           | _ (* when x > 0 *) ->
             let acc = add acc k' v' in
             remove acc k v)
        | ( Node { left = l; key = k; data = v; right = r; weight = _ }
          , Node { left = l'; key = k'; data = v'; right = r'; weight = _ } )
          when compare_key k k' = 0 ->
          let acc = loop l l' acc in
          let acc = delta acc k v v' in
          loop r r' acc
        (* Our roots aren't the same key. Fallback to the slow mode. Trees with small
           diffs will only do this on very small parts of the tree (hopefully - if the
           overall root is rebalanced, we'll eat the whole cost, unfortunately.) *)
        | Node _, Node _ | Node _, Leaf _ | Leaf _, Node _ -> slow t t' ~init:acc)
    in
    loop t1 t2 init [@nontail]
  ;;

  let hash_fold_t_ignoring_structure hash_fold_key hash_fold_data state t =
    fold
      t
      ~init:(hash_fold_int state (length t))
      ~f:(fun ~key ~data state -> hash_fold_data (hash_fold_key state key) data)
  ;;

  let keys t = fold_right ~f:(fun ~key ~data:_ list -> key :: list) t ~init:[]
  let data t = fold_right ~f:(fun ~key:_ ~data list -> data :: list) t ~init:[]

  module type Foldable = sig @@ portable
    val name : string

    type 'a t

    val fold : 'a t -> init:'acc -> f:local_ ('acc -> 'a -> 'acc) -> 'acc
  end

  let[@inline always] of_foldable' ~fold foldable ~init ~f ~compare_key =
    (fold [@inlined hint]) foldable ~init:Empty ~f:(fun accum (key, data) ->
      let prev_data =
        match find accum key ~compare_key with
        | None -> init
        | Some prev -> prev
      in
      let data = f prev_data data in
      set accum ~key ~data ~compare_key [@nontail])
    [@nontail]
  ;;

  module Of_foldable (M : Foldable) = struct
    let of_foldable_fold foldable ~init ~f ~compare_key =
      of_foldable' ~fold:M.fold foldable ~init ~f ~compare_key
    ;;

    let of_foldable_reduce foldable ~f ~compare_key =
      M.fold foldable ~init:Empty ~f:(fun accum (key, data) ->
        let new_data =
          match find accum key ~compare_key with
          | None -> data
          | Some prev -> f prev data
        in
        set accum ~key ~data:new_data ~compare_key [@nontail])
      [@nontail]
    ;;

    let of_foldable foldable ~compare_key =
      with_return (fun r ->
        let map =
          M.fold foldable ~init:Empty ~f:(fun t (key, data) ->
            let acc = set ~key ~data t ~compare_key in
            if length t = length acc then r.return (`Duplicate_key key) else acc)
        in
        `Ok map)
    ;;

    let of_foldable_or_error foldable ~comparator =
      match of_foldable foldable ~compare_key:(Comparator.compare comparator) with
      | `Ok x -> Result.Ok x
      | `Duplicate_key key ->
        Or_error.error
          ("Map.of_" ^ M.name ^ "_or_error: duplicate key")
          key
          (Comparator.sexp_of_t comparator)
    ;;

    let of_foldable_exn foldable ~comparator =
      match of_foldable foldable ~compare_key:(Comparator.compare comparator) with
      | `Ok x -> x
      | `Duplicate_key key ->
        Error.create
          ("Map.of_" ^ M.name ^ "_exn: duplicate key")
          key
          (Comparator.sexp_of_t comparator)
        |> Error.raise
    ;;

    (* Reverse the input, then fold from left to right. The resulting map uses the first
       instance of each key from the input list. The relative ordering of elements in each
       output list is the same as in the input list. *)
    let of_foldable_multi foldable ~compare_key =
      let alist = M.fold foldable ~init:[] ~f:(fun l x -> x :: l) in
      of_foldable' alist ~fold:List.fold ~init:[] ~f:(fun l x -> x :: l) ~compare_key
    ;;
  end

  module Of_alist = Of_foldable (struct
      let name = "alist"

      type 'a t = 'a list

      let fold = List.fold
    end)

  let of_alist_fold = Of_alist.of_foldable_fold
  let of_alist_reduce = Of_alist.of_foldable_reduce
  let of_alist = Of_alist.of_foldable
  let of_alist_or_error = Of_alist.of_foldable_or_error
  let of_alist_exn = Of_alist.of_foldable_exn
  let of_alist_multi = Of_alist.of_foldable_multi

  module Of_sequence = Of_foldable (struct
      let name = "sequence"

      type 'a t = 'a Sequence.t

      let fold = Sequence.fold
    end)

  let of_sequence_fold = Of_sequence.of_foldable_fold
  let of_sequence_reduce = Of_sequence.of_foldable_reduce
  let of_sequence = Of_sequence.of_foldable
  let of_sequence_or_error = Of_sequence.of_foldable_or_error
  let of_sequence_exn = Of_sequence.of_foldable_exn
  let of_sequence_multi = Of_sequence.of_foldable_multi

  let of_list_with_key list ~get_key ~compare_key =
    with_return (fun r ->
      let map =
        List.fold list ~init:Empty ~f:(fun t data ->
          let key = get_key data in
          let acc = set ~key ~data t ~compare_key in
          if length t = length acc then r.return (`Duplicate_key key) else acc)
      in
      `Ok map)
    [@nontail]
  ;;

  let of_list_with_key_or_error list ~get_key ~comparator =
    match of_list_with_key list ~get_key ~compare_key:(Comparator.compare comparator) with
    | `Ok x -> Result.Ok x
    | `Duplicate_key key ->
      Or_error.error
        "Map.of_list_with_key_or_error: duplicate key"
        key
        (Comparator.sexp_of_t comparator)
  ;;

  let of_list_with_key_exn list ~get_key ~comparator =
    match of_list_with_key list ~get_key ~compare_key:(Comparator.compare comparator) with
    | `Ok x -> x
    | `Duplicate_key key ->
      Error.create
        "Map.of_list_with_key_exn: duplicate key"
        key
        (Comparator.sexp_of_t comparator)
      |> Error.raise
  ;;

  let of_list_with_key_multi list ~get_key ~compare_key =
    let list = List.rev list in
    List.fold list ~init:Empty ~f:(fun t data ->
      let key = get_key data in
      update t key ~compare_key ~f:(fun option ->
        let list = Option.value option ~default:[] in
        data :: list)
      [@nontail])
    [@nontail]
  ;;

  let of_list_with_key_fold list ~get_key ~init ~f ~compare_key =
    List.fold list ~init:Empty ~f:(fun t data ->
      let key = get_key data in
      update t key ~compare_key ~f:(function
        | None -> f init data
        | Some prev -> f prev data)
        [@nontail])
    [@nontail]
  ;;

  let of_list_with_key_reduce list ~get_key ~f ~compare_key =
    List.fold list ~init:Empty ~f:(fun t data ->
      let key = get_key data in
      update t key ~compare_key ~f:(function
        | None -> data
        | Some prev -> f prev data)
        [@nontail])
    [@nontail]
  ;;

  let for_all t ~f =
    with_return (fun r ->
      iter t ~f:(fun data -> if not (f data) then r.return false);
      true)
    [@nontail]
  ;;

  let for_alli t ~f =
    with_return (fun r ->
      iteri t ~f:(fun ~key ~data -> if not (f ~key ~data) then r.return false);
      true)
    [@nontail]
  ;;

  let exists t ~f =
    with_return (fun r ->
      iter t ~f:(fun data -> if f data then r.return true);
      false)
    [@nontail]
  ;;

  let existsi t ~f =
    with_return (fun r ->
      iteri t ~f:(fun ~key ~data -> if f ~key ~data then r.return true);
      false)
    [@nontail]
  ;;

  let count t ~f =
    fold t ~init:0 ~f:(fun ~key:_ ~data acc -> if f data then acc + 1 else acc) [@nontail]
  ;;

  let counti t ~f =
    fold t ~init:0 ~f:(fun ~key ~data acc -> if f ~key ~data then acc + 1 else acc)
    [@nontail]
  ;;

  let sum (type a) (module M : Container.Summable with type t = a) t ~f =
    fold t ~init:M.zero ~f:(fun ~key:_ ~data acc -> M.( + ) (f data) acc) [@nontail]
  ;;

  let sumi (type a) (module M : Container.Summable with type t = a) t ~f =
    fold t ~init:M.zero ~f:(fun ~key ~data acc -> M.( + ) (f ~key ~data) acc) [@nontail]
  ;;

  let to_alist ?(key_order = `Increasing) t =
    match key_order with
    | `Increasing -> fold_right t ~init:[] ~f:(fun ~key ~data x -> (key, data) :: x)
    | `Decreasing -> fold t ~init:[] ~f:(fun ~key ~data x -> (key, data) :: x)
  ;;

  module When_unmatched = struct
    include When_unmatched

    let apply_to_node
      : type k a b c.
        (k, a, b) t @ local
        -> (key:k
            -> data:a
            -> left:(k, b, c) tree
            -> right:(k, b, c) tree
            -> (k, b, c) tree)
           @ local
      =
      fun t -> exclave_
      match t with
      | Drop -> fun ~key:_ ~data:_ ~left ~right -> concat_and_balance_unchecked left right
      | Keep -> fun ~key ~data ~left ~right -> join left key data right
      | Map f -> fun ~key ~data ~left ~right -> join left key (f ~key ~data) right
      | Filter f ->
        fun ~key ~data ~left ~right ->
          if f ~key ~data
          then join left key data right
          else concat_and_balance_unchecked left right
      | Filter_map f ->
        fun ~key ~data ~left ~right ->
          (match f ~key ~data with
           | None -> concat_and_balance_unchecked left right
           | Some data -> join left key data right)
    ;;

    let apply_to_subtree
      : type k a b c. (k, a, b) t @ local -> ((k, a, c) tree -> (k, b, c) tree) @ local
      =
      fun t -> exclave_
      match t with
      | Drop -> fun _ -> Empty
      | Keep -> fun tree -> tree
      | Map f -> fun tree -> mapi tree ~f
      | Filter f -> fun tree -> filteri tree ~f
      | Filter_map f -> fun tree -> filter_mapi tree ~f
    ;;
  end

  module When_matched_internal = struct
    (* Like [When_matched], but special cased to always traverse the left tree. Allows us
       to avoid allocation in [Keep] and [Filter] cases by reusing unmodified trees. *)
    type ('k, 'a, 'b, 'c) t =
      | Drop
      | Keep_first : (_, 'a, _, 'a) t
      | Map of (key:'k -> local_ ('a -> 'b -> 'c))
      | Filter_first : (key:'k -> local_ ('a -> 'b -> bool)) -> ('k, 'a, 'b, 'a) t
      | Filter_map of (key:'k -> local_ ('a -> 'b -> 'c option))

    (* In cases where type ['a] = ['c], we can sometimes reuse our input tree. *)
    let join_or_reuse ~key data ~left ~right ~tree1 ~tree1_left ~tree1_right =
      if phys_equal left tree1_left && phys_equal right tree1_right
      then tree1
      else join left key data right
    ;;

    let apply_to_node
      : type k a b c d.
        (k, a, b, c) t @ local
        -> (key:k
            -> a
            -> b
            -> left:(k, c, d) tree
            -> right:(k, c, d) tree
            -> tree1:(k, a, d) tree
            -> tree1_left:(k, a, d) tree
            -> tree1_right:(k, a, d) tree
            -> (k, c, d) tree)
           @ local
      =
      (* Match on [t] eagerly and return a function that can be reused on each node. *)
      fun t -> exclave_
      match t with
      | Drop ->
        fun ~key:_ _ _ ~left ~right ~tree1:_ ~tree1_left:_ ~tree1_right:_ ->
          concat_and_balance_unchecked left right
      | Keep_first ->
        fun ~key a _ ~left ~right ~tree1 ~tree1_left ~tree1_right ->
          join_or_reuse ~key a ~left ~right ~tree1 ~tree1_left ~tree1_right
      | Map f ->
        fun ~key a b ~left ~right ~tree1:_ ~tree1_left:_ ~tree1_right:_ ->
          join left key (f ~key a b) right
      | Filter_first f ->
        fun ~key a b ~left ~right ~tree1 ~tree1_left ~tree1_right ->
          if f ~key a b
          then join_or_reuse ~key a ~left ~right ~tree1 ~tree1_left ~tree1_right
          else concat_and_balance_unchecked left right
      | Filter_map f ->
        fun ~key a b ~left ~right ~tree1:_ ~tree1_left:_ ~tree1_right:_ ->
          (match f ~key a b with
           | None -> concat_and_balance_unchecked left right
           | Some data -> join left key data right)
    ;;
  end

  (* Workhorse of [merge_by_case], specialized to use [When_matched_internal]. *)
  let merge_by_case_internal
    (type k a b c d)
    (tree1 : (k, a, d) tree)
    (tree2 : (k, b, d) tree)
    ~(first : (k, a, c) When_unmatched.t @ local)
    ~(second : (k, b, c) When_unmatched.t @ local)
    ~(both : (k, a, b, c) When_matched_internal.t @ local)
    ~(compare_key : k -> k -> int @ local)
    =
    (* Perform all our matching on GADTs once, before recurring. *)
    let when_unmatched_first_key = When_unmatched.apply_to_node first in
    let when_unmatched_first = When_unmatched.apply_to_subtree first in
    let when_unmatched_second = When_unmatched.apply_to_subtree second in
    let when_matched = When_matched_internal.apply_to_node both in
    let rec merge (tree1 : (k, a, d) tree) (tree2 : (k, b, d) tree) =
      (* Walk [tree1] recursively. *)
      match
        (* The extra [Empty] is a trick so we can combine [Leaf] and [Node] clauses, using
           the [Empty] for the fields of [Node] that are not present in a [Leaf]. *)
        Empty, tree1, tree2
      with
      | _, Empty, Empty -> Empty
      | _, Empty, _ -> when_unmatched_second tree2
      | _, _, Empty -> when_unmatched_first tree1
      | (_ as tree1_left as tree1_right), Leaf { key; data = tree1_data }, _
      | ( _
        , Node
            { left = tree1_left; key; data = tree1_data; right = tree1_right; weight = _ }
        , _ ) ->
        (* Find the corresponding key in [tree2] and merge. *)
        let tree2_left, tree2_maybe_data, tree2_right = split tree2 key ~compare_key in
        let left = merge tree1_left tree2_left in
        let right = merge tree1_right tree2_right in
        (match tree2_maybe_data with
         | None -> when_unmatched_first_key ~key ~data:tree1_data ~left ~right
         | Some (_, tree2_data) ->
           when_matched
             ~key
             tree1_data
             tree2_data
             ~left
             ~right
             ~tree1
             ~tree1_left
             ~tree1_right)
    in
    merge tree1 tree2 [@nontail]
  ;;

  (* Dispatch to [merge_by_case_internal], case by case. Determine which tree to recur on
     first by [both], then based on [weight tree1] and [weight tree2] if a choice remains.
     We prefer to walk the larger tree so we don't have to repeat [split] on a large tree
     more often than necessary in [merge_by_case_internal] above. *)
  let merge_by_case
    (type k a b c d)
    (tree1 : (k, a, d) tree)
    (tree2 : (k, b, d) tree)
    ~(first : (k, a, c) When_unmatched.t @ local)
    ~(second : (k, b, c) When_unmatched.t @ local)
    ~(both : (k, a, b, c) When_matched.t @ local)
    ~(compare_key : k -> k -> int @ local)
    =
    match (both : _ When_matched.t) with
    | Drop ->
      (* dispatch based on weight *)
      if weight tree1 >= weight tree2
      then merge_by_case_internal tree1 tree2 ~first ~second ~both:Drop ~compare_key
      else
        merge_by_case_internal
          tree2
          tree1
          ~first:second
          ~second:first
          ~both:Drop
          ~compare_key
    | Keep_first ->
      (* traverse first tree to keep unmodified nodes *)
      merge_by_case_internal tree1 tree2 ~first ~second ~both:Keep_first ~compare_key
    | Keep_second ->
      (* traverse second tree to keep unmodified nodes *)
      merge_by_case_internal
        tree2
        tree1
        ~first:second
        ~second:first
        ~both:Keep_first
        ~compare_key
    | Map f ->
      (* dispatch based on weight *)
      if weight tree1 >= weight tree2
      then
        merge_by_case_internal
          tree1
          tree2
          ~first
          ~second
          ~both:(Map f)
          ~compare_key [@nontail]
      else
        merge_by_case_internal
          tree2
          tree1
          ~first:second
          ~second:first
          ~both:(Map (fun ~key a b -> f ~key b a))
          ~compare_key [@nontail]
    | Filter_first f ->
      (* traverse first tree to keep unmodified nodes *)
      merge_by_case_internal
        tree1
        tree2
        ~first
        ~second
        ~both:(Filter_first f)
        ~compare_key [@nontail]
    | Filter_second f ->
      (* traverse second tree to keep unmodified nodes *)
      merge_by_case_internal
        tree2
        tree1
        ~first:second
        ~second:first
        ~both:(Filter_first (fun ~key a b -> f ~key b a))
        ~compare_key [@nontail]
    | Filter_map f ->
      (* dispatch based on weight *)
      if weight tree1 >= weight tree2
      then
        merge_by_case_internal
          tree1
          tree2
          ~first
          ~second
          ~both:(Filter_map f)
          ~compare_key [@nontail]
      else
        merge_by_case_internal
          tree2
          tree1
          ~first:second
          ~second:first
          ~both:(Filter_map (fun ~key a b -> f ~key b a))
          ~compare_key [@nontail]
  ;;

  let merge t1 t2 ~f ~compare_key =
    let elts = Uniform_array.unsafe_create_uninitialized ~len:(length t1 + length t2) in
    let i = ref 0 in
    iter2 t1 t2 ~compare_key ~f:(fun ~key ~data:values ->
      match f ~key values with
      | Some value ->
        Uniform_array.set elts !i (key, value);
        incr i
      | None -> ());
    let len = !i in
    let get i = Uniform_array.get elts i in
    of_increasing_iterator_unchecked ~len ~f:get
  ;;

  let merge_skewed =
    let merge_large_first t_large t_small ~call ~combine ~compare_key =
      fold t_small ~init:t_large ~f:(fun ~key ~data:data' t ->
        update t key ~compare_key ~f:(function
          | None -> data'
          | Some data -> call combine ~key data data')
          [@nontail])
      [@nontail]
    in
    let call f ~key x y = f ~key x y in
    let swap f ~key x y = f ~key y x in
    fun t1 t2 ~combine ~compare_key ->
      if length t2 <= length t1
      then merge_large_first t1 t2 ~call ~combine ~compare_key
      else merge_large_first t2 t1 ~call:swap ~combine ~compare_key
  ;;

  let merge_disjoint_exn t1 t2 ~(comparator : _ Comparator.t) =
    merge_skewed
      t1
      t2
      ~compare_key:(Comparator.compare comparator)
      ~combine:(fun ~key _ _ ->
        Error.create
          "Map.merge_disjoint_exn: duplicate key"
          key
          (Comparator.sexp_of_t comparator)
        |> Error.raise)
  ;;

  module Closest_key_impl = struct
    (* [marker] and [repackage] allow us to create "logical" options without actually
       allocating any options. Passing [Found key value] to a function is equivalent to
       passing [Some (key, value)]; passing [Missing () ()] is equivalent to passing
       [None]. *)
    type ('k, 'v, 'k_opt, 'v_opt) marker =
      | Missing : ('k, 'v, unit, unit) marker
      | Found : ('k, 'v, 'k, 'v) marker

    let repackage
      (type k v k_opt v_opt)
      (marker : (k, v, k_opt, v_opt) marker)
      (k : k_opt)
      (v : v_opt)
      : (k * v) option
      =
      match marker with
      | Missing -> None
      | Found -> Some (k, v)
    ;;

    (* The type signature is explicit here to allow polymorphic recursion. *)
    let rec loop
      : 'k 'v 'cmp 'k_opt 'v_opt.
      ('k, 'v, 'cmp) tree
      -> [ `Greater_or_equal_to | `Greater_than | `Less_or_equal_to | `Less_than ]
      -> 'k
      -> compare_key:('k -> 'k -> int)
      -> ('k, 'v, 'k_opt, 'v_opt) marker
      -> 'k_opt
      -> 'v_opt
      -> ('k * 'v) option
      =
      fun t dir k ~compare_key found_marker found_key found_value ->
      match t with
      | Empty -> repackage found_marker found_key found_value
      | Leaf { key = k'; data = v' } ->
        let c = compare_key k' k in
        if match dir with
           | `Greater_or_equal_to -> c >= 0
           | `Greater_than -> c > 0
           | `Less_or_equal_to -> c <= 0
           | `Less_than -> c < 0
        then Some (k', v')
        else repackage found_marker found_key found_value
      | Node { left = l; key = k'; data = v'; right = r; weight = _ } ->
        let c = compare_key k' k in
        if c = 0
        then (
          (* This is a base case (no recursive call). *)
          match dir with
          | `Greater_or_equal_to | `Less_or_equal_to -> Some (k', v')
          | `Greater_than ->
            if is_empty r then repackage found_marker found_key found_value else min_elt r
          | `Less_than ->
            if is_empty l then repackage found_marker found_key found_value else max_elt l)
        else (
          (* We are guaranteed here that k' <> k. *)
          (* This is the only recursive case. *)
          match dir with
          | `Greater_or_equal_to | `Greater_than ->
            if c > 0
            then loop l dir k ~compare_key Found k' v'
            else loop r dir k ~compare_key found_marker found_key found_value
          | `Less_or_equal_to | `Less_than ->
            if c < 0
            then loop r dir k ~compare_key Found k' v'
            else loop l dir k ~compare_key found_marker found_key found_value)
    ;;

    let closest_key t dir k ~compare_key = loop t dir k ~compare_key Missing () ()
  end

  let closest_key = Closest_key_impl.closest_key

  let rank t k ~compare_key =
    let rec local_ loop t acc =
      match t with
      | Empty -> None
      | Leaf { key = k'; data = _ } -> if compare_key k' k = 0 then Some acc else None
      | Node { left = l; key = k'; data = _; right = r; weight = _ } ->
        let c = compare_key k' k in
        if c = 0
        then Some (acc + length l)
        else if c > 0
        then loop l acc
        else loop r (acc + length l + 1)
    in
    loop t 0 [@nontail]
  ;;

  let nth t n =
    let rec local_ loop t n =
      match t with
      | Empty -> assert false
      | Leaf { key; data } -> key, data
      | Node { left; key; data; right; weight = _ } ->
        let l = length left in
        if n < l then loop left n else if n = l then key, data else loop right (n - l - 1)
    in
    if n < 0 || n >= length t then None else Some (loop t n)
  ;;

  let split_n t n =
    if n <= 0
    then Empty, t
    else if n >= length t
    then t, Empty
    else (
      (* Only call this when [0 < n && n < length t]. *)
      let rec split_n_nontrivial t n =
        match t with
        | Empty | Leaf _ ->
          (* Precondition cannot be met in these cases. *)
          assert false
        | Node { left; key; data; right; weight = _ } ->
          let left_len = length left in
          if n < left_len
          then (
            let prefix, suffix = split_n_nontrivial left n in
            prefix, join suffix key data right)
          else if n = left_len
          then left, join Empty key data right
          else if n = left_len + 1
          then join left key data Empty, right
          else (
            let prefix, suffix = split_n_nontrivial right (n - left_len - 1) in
            join left key data prefix, suffix)
      in
      split_n_nontrivial t n)
  ;;

  let rec find_first_satisfying t ~f =
    match t with
    | Empty -> None
    | Leaf { key = k; data = v } -> if f ~key:k ~data:v then Some (k, v) else None
    | Node { left = l; key = k; data = v; right = r; weight = _ } ->
      if f ~key:k ~data:v
      then (
        match find_first_satisfying l ~f with
        | None -> Some (k, v)
        | Some _ as x -> x)
      else find_first_satisfying r ~f
  ;;

  let rec find_last_satisfying t ~f =
    match t with
    | Empty -> None
    | Leaf { key = k; data = v } -> if f ~key:k ~data:v then Some (k, v) else None
    | Node { left = l; key = k; data = v; right = r; weight = _ } ->
      if f ~key:k ~data:v
      then (
        match find_last_satisfying r ~f with
        | None -> Some (k, v)
        | Some _ as x -> x)
      else find_last_satisfying l ~f
  ;;

  let binary_search t ~compare how v =
    match how with
    | `Last_strictly_less_than ->
      find_last_satisfying t ~f:(fun ~key ~data -> compare ~key ~data v < 0) [@nontail]
    | `Last_less_than_or_equal_to ->
      find_last_satisfying t ~f:(fun ~key ~data -> compare ~key ~data v <= 0) [@nontail]
    | `First_equal_to ->
      (match find_first_satisfying t ~f:(fun ~key ~data -> compare ~key ~data v >= 0) with
       | Some (key, data) as pair when compare ~key ~data v = 0 -> pair
       | None | Some _ -> None)
    | `Last_equal_to ->
      (match find_last_satisfying t ~f:(fun ~key ~data -> compare ~key ~data v <= 0) with
       | Some (key, data) as pair when compare ~key ~data v = 0 -> pair
       | None | Some _ -> None)
    | `First_greater_than_or_equal_to ->
      find_first_satisfying t ~f:(fun ~key ~data -> compare ~key ~data v >= 0) [@nontail]
    | `First_strictly_greater_than ->
      find_first_satisfying t ~f:(fun ~key ~data -> compare ~key ~data v > 0) [@nontail]
  ;;

  let binary_search_segmented t ~segment_of how =
    let is_left ~key ~data =
      match segment_of ~key ~data with
      | `Left -> true
      | `Right -> false
    in
    let is_right ~key ~data = not (is_left ~key ~data) in
    match how with
    | `Last_on_left -> find_last_satisfying t ~f:is_left [@nontail]
    | `First_on_right -> find_first_satisfying t ~f:is_right [@nontail]
  ;;

  (* [binary_search_one_sided_bound] finds the key in [t] which satisfies [maybe_bound]
     and the relevant one of [if_exclusive] or [if_inclusive], as judged by [compare]. *)
  let binary_search_one_sided_bound t maybe_bound ~compare ~if_exclusive ~if_inclusive =
    let find_bound t how bound ~compare : _ Maybe_bound.t option =
      match binary_search t how bound ~compare with
      | Some (bound, _) -> Some (Incl bound)
      | None -> None
    in
    match (maybe_bound : _ Maybe_bound.t) with
    | Excl bound -> find_bound t if_exclusive bound ~compare
    | Incl bound -> find_bound t if_inclusive bound ~compare
    | Unbounded -> Some Unbounded
  ;;

  (* [binary_search_two_sided_bounds] finds the (not necessarily distinct) keys in [t]
     which most closely approach (but do not cross) [lower_bound] and [upper_bound], as
     judged by [compare]. It returns [None] if no keys in [t] are within that range. *)
  let binary_search_two_sided_bounds t ~compare ~lower_bound ~upper_bound =
    let find_lower_bound t maybe_bound ~compare =
      binary_search_one_sided_bound
        t
        maybe_bound
        ~compare
        ~if_exclusive:`First_strictly_greater_than
        ~if_inclusive:`First_greater_than_or_equal_to
    in
    let find_upper_bound t maybe_bound ~compare =
      binary_search_one_sided_bound
        t
        maybe_bound
        ~compare
        ~if_exclusive:`Last_strictly_less_than
        ~if_inclusive:`Last_less_than_or_equal_to
    in
    match find_lower_bound t lower_bound ~compare with
    | None -> None
    | Some lower_bound ->
      (match find_upper_bound t upper_bound ~compare with
       | None -> None
       | Some upper_bound -> Some (lower_bound, upper_bound))
  ;;

  type ('k, 'v, 'c) acc =
    { mutable bad_key : 'k option
    ; mutable map : ('k, 'v, 'c) t
    }

  let of_iteri ~iteri ~compare_key =
    let acc = { bad_key = None; map = Empty } in
    iteri ~f:(local_ fun ~key ~data ->
      let new_map = set ~key ~data acc.map ~compare_key in
      if length acc.map = length new_map && Option.is_none acc.bad_key
      then acc.bad_key <- Some key
      else acc.map <- new_map);
    match acc.bad_key with
    | None -> `Ok acc.map
    | Some key -> `Duplicate_key key
  ;;

  let of_iteri_exn ~iteri ~(comparator : _ Comparator.t) =
    match of_iteri ~iteri ~compare_key:(Comparator.compare comparator) with
    | `Ok v -> v
    | `Duplicate_key key ->
      Error.create "Map.of_iteri_exn: duplicate key" key (Comparator.sexp_of_t comparator)
      |> Error.raise
  ;;

  let t_of_sexp_direct key_of_sexp value_of_sexp sexp ~(comparator : _ Comparator.t) =
    let alist = list_of_sexp (pair_of_sexp key_of_sexp value_of_sexp) sexp in
    let compare_key = Comparator.compare comparator in
    match of_alist alist ~compare_key with
    | `Ok v -> v
    | `Duplicate_key k ->
      (* find the sexp of a duplicate key, so the error is narrowed to a key and not the
         whole map *)
      let alist_sexps = list_of_sexp (pair_of_sexp Fn.id Fn.id) sexp in
      let found_first_k = ref false in
      List.iter2_ok alist alist_sexps ~f:(fun (k2, _) (k2_sexp, _) ->
        if compare_key k k2 = 0
        then
          if !found_first_k
          then of_sexp_error "Map.t_of_sexp_direct: duplicate key" k2_sexp
          else found_first_k := true);
      assert false
  ;;

  let%template[@alloc a @ m = (heap_global, stack_local)] sexp_of_t
    sexp_of_key
    sexp_of_value
    t
    =
    (let f ~key ~data acc =
       Sexp.List [ sexp_of_key key; sexp_of_value data ] :: acc [@exclave_if_stack a]
     in
     Sexp.List ((fold_right [@mode m]) ~f t ~init:[]))
    [@exclave_if_stack a]
  ;;

  let combine_errors t ~sexp_of_key =
    let oks, errors = partition_result t in
    if is_empty errors
    then Ok oks
    else Or_error.error_s (sexp_of_t sexp_of_key Error.sexp_of_t errors)
  ;;

  let unzip t = map t ~f:fst, map t ~f:snd

  let map_keys t1 ~f ~comparator =
    let compare_key = Comparator.compare comparator in
    let sexp_of_key = Comparator.sexp_of_t comparator in
    with_return (fun { return } ->
      `Ok
        (fold t1 ~init:Empty ~f:(fun ~key ~data t2 ->
           let key = f key in
           try add_exn_internal t2 ~key ~data ~compare_key ~sexp_of_key with
           | Duplicate -> return (`Duplicate_key key))))
    [@nontail]
  ;;

  let map_keys_exn t ~f ~comparator =
    match map_keys t ~f ~comparator with
    | `Ok result -> result
    | `Duplicate_key key ->
      let sexp_of_key = Comparator.sexp_of_t comparator in
      Error.raise_s
        (Sexp.message "Map.map_keys_exn: duplicate key" [ "key", key |> sexp_of_key ])
  ;;

  let transpose_keys ~outer_comparator ~inner_comparator outer_t =
    fold outer_t ~init:Empty ~f:(fun ~key:outer_key ~data:inner_t acc ->
      fold inner_t ~init:acc ~f:(fun ~key:inner_key ~data acc ->
        update
          acc
          inner_key
          ~compare_key:(Comparator.compare inner_comparator)
          ~f:(function
          | None -> singleton outer_key data
          | Some elt ->
            set
              elt
              ~key:outer_key
              ~data
              ~compare_key:(Comparator.compare outer_comparator) [@nontail])
          [@nontail]))
  ;;

  module%template.portable Make_applicative_traversals (A : Applicative.Lazy_applicative) =
  struct
    let rec mapi t ~f =
      match t with
      | Empty -> A.return Empty
      | Leaf { key = v; data = d } ->
        A.map (f ~key:v ~data:d) ~f:(fun new_data -> Leaf { key = v; data = new_data })
      | Node { left = l; key = v; data = d; right = r; weight = w } ->
        let l' = A.of_thunk (fun () -> mapi ~f l) in
        let d' = f ~key:v ~data:d in
        let r' = A.of_thunk (fun () -> mapi ~f r) in
        A.map3 l' d' r' ~f:(fun l' d' r' ->
          Node { left = l'; key = v; data = d'; right = r'; weight = w })
    ;;

    (* In theory the computation of length on-the-fly is not necessary here because it can
       be done by wrapping the applicative [A] with length-computing logic. However,
       introducing an applicative transformer like that makes the map benchmarks in
       async_kernel/bench/src/bench_deferred_map.ml noticeably slower. *)
    let filter_mapi t ~f =
      let rec tree_filter_mapi t ~f =
        match t with
        | Empty -> A.return Empty
        | Leaf { key = v; data = d } ->
          A.map (f ~key:v ~data:d) ~f:(function
            | Some new_data -> Leaf { key = v; data = new_data }
            | None -> Empty)
        | Node { left = l; key = v; data = d; right = r; weight = _ } ->
          A.map3
            (A.of_thunk (fun () -> tree_filter_mapi l ~f))
            (f ~key:v ~data:d)
            (A.of_thunk (fun () -> tree_filter_mapi r ~f))
            ~f:(fun l' new_data r' ->
              match new_data with
              | Some new_data -> join l' v new_data r'
              | None -> concat_and_balance_unchecked l' r')
      in
      tree_filter_mapi t ~f
    ;;
  end
end

type ('k, 'v, 'comparator) t =
  { (* [comparator] is the first field so that polymorphic equality fails on a map due to
       the functional value in the comparator. Note that this does not affect polymorphic
       [compare]: that still produces nonsense. *)
    global_ comparator : ('k, 'comparator) Comparator.t
  ; tree : ('k, 'v, 'comparator) Tree0.t
  }

let globalize0 (local_ { comparator; tree }) =
  { comparator; tree = Tree0.globalize0 tree }
;;

let compare_key t = Comparator.compare t.comparator
let like { tree = _; comparator } tree = { tree; comparator }

let like_maybe_no_op ({ tree = old_tree; comparator } as old_t) tree =
  if phys_equal old_tree tree then old_t else { tree; comparator }
;;

let with_same_length { tree = _; comparator } tree = { tree; comparator }
let of_tree ~comparator tree = { tree; comparator }

module Accessors = struct
  let comparator t = t.comparator
  let to_tree t = t.tree
  let invariants t = Tree0.invariants t.tree ~compare_key:(compare_key t)
  let is_empty t = Tree0.is_empty t.tree
  let length t = Tree0.length t.tree

  let set t ~key ~data =
    like t (Tree0.set t.tree ~key ~data ~compare_key:(compare_key t)) [@nontail]
  ;;

  let add_exn t ~key ~data =
    like
      t
      (Tree0.add_exn
         t.tree
         ~key
         ~data
         ~compare_key:(compare_key t)
         ~sexp_of_key:(Comparator.sexp_of_t t.comparator)) [@nontail]
  ;;

  let add_exn_internal t ~key ~data =
    like
      t
      (Tree0.add_exn_internal
         t.tree
         ~key
         ~data
         ~compare_key:(compare_key t)
         ~sexp_of_key:(Comparator.sexp_of_t t.comparator)) [@nontail]
  ;;

  let add t ~key ~data =
    match add_exn_internal t ~key ~data with
    | result -> `Ok result
    | exception Duplicate -> `Duplicate
  ;;

  let add_multi t ~key ~data =
    like t (Tree0.add_multi t.tree ~key ~data ~compare_key:(compare_key t)) [@nontail]
  ;;

  let remove_multi t key =
    like t (Tree0.remove_multi t.tree key ~compare_key:(compare_key t)) [@nontail]
  ;;

  let find_multi t key = Tree0.find_multi t.tree key ~compare_key:(compare_key t)

  let change t key ~f =
    like t (Tree0.change t.tree key ~f ~compare_key:(compare_key t)) [@nontail]
  ;;

  let update t key ~f =
    like t (Tree0.update t.tree key ~f ~compare_key:(compare_key t)) [@nontail]
  ;;

  let update_and_return t key ~f =
    let result, new_t =
      Tree0.update_and_return t.tree key ~f ~compare_key:(compare_key t)
    in
    Modes.Global.unwrap result, like t new_t
  ;;

  let find_exn t key =
    Tree0.find_exn
      t.tree
      key
      ~compare_key:(compare_key t)
      ~sexp_of_key:(Comparator.sexp_of_t t.comparator)
  ;;

  let find t key = Tree0.find t.tree key ~compare_key:(compare_key t)

  let remove t key =
    like_maybe_no_op t (Tree0.remove t.tree key ~compare_key:(compare_key t)) [@nontail]
  ;;

  let mem t key = Tree0.mem t.tree key ~compare_key:(compare_key t)
  let iter_keys t ~f = Tree0.iter_keys t.tree ~f
  let iter t ~f = Tree0.iter t.tree ~f
  let iteri t ~f = Tree0.iteri t.tree ~f
  let iteri_until t ~f = Tree0.iteri_until t.tree ~f
  let iter2 t1 t2 ~f = Tree0.iter2 t1.tree t2.tree ~f ~compare_key:(compare_key t1)
  let map t ~f = with_same_length t (Tree0.map t.tree ~f)
  let mapi t ~f = with_same_length t (Tree0.mapi t.tree ~f)
  let fold t ~init ~f = Tree0.fold t.tree ~f ~init
  let fold_until t ~init ~f ~finish = Tree0.fold_until t.tree ~init ~f ~finish
  let fold_right t ~init ~f = Tree0.fold_right t.tree ~f ~init

  let fold2 t1 t2 ~init ~f =
    Tree0.fold2 t1.tree t2.tree ~init ~f ~compare_key:(compare_key t1)
  ;;

  let filter_keys t ~f =
    let tree = Tree0.filter_keys t.tree ~f in
    like_maybe_no_op t tree [@nontail]
  ;;

  let filter t ~f =
    let tree = Tree0.filter t.tree ~f in
    like_maybe_no_op t tree [@nontail]
  ;;

  let filteri t ~f =
    let tree = Tree0.filteri t.tree ~f in
    like_maybe_no_op t tree [@nontail]
  ;;

  let filter_map t ~f =
    let tree = Tree0.filter_map t.tree ~f in
    like t tree [@nontail]
  ;;

  let filter_mapi t ~f =
    let tree = Tree0.filter_mapi t.tree ~f in
    like t tree [@nontail]
  ;;

  let like2 t (t1, t2) = like t t1, like t t2
  let like2_maybe_no_op t (t1, t2) = like_maybe_no_op t t1, like_maybe_no_op t t2
  let partition_mapi t ~f = like2 t (Tree0.partition_mapi t.tree ~f)
  let partition_map t ~f = like2 t (Tree0.partition_map t.tree ~f)
  let partition_result t = like2 t (Tree0.partition_result t.tree)
  let partitioni_tf t ~f = like2_maybe_no_op t (Tree0.partitioni_tf t.tree ~f)
  let partition_tf t ~f = like2_maybe_no_op t (Tree0.partition_tf t.tree ~f)

  let combine_errors t =
    Or_error.map
      ~f:(like t)
      (Tree0.combine_errors t.tree ~sexp_of_key:(Comparator.sexp_of_t t.comparator))
  ;;

  let unzip t = like2 t (Tree0.unzip t.tree)

  let%template[@mode m = (local, global)] compare_direct compare_data t1 t2 =
    Tree0.compare (compare_key t1) compare_data t1.tree t2.tree
  ;;

  let%template[@mode m = (local, global)] equal data_equal t1 t2 =
    Tree0.equal (compare_key t1) data_equal t1.tree t2.tree
  ;;

  let keys t = Tree0.keys t.tree
  let data t = Tree0.data t.tree
  let to_alist ?key_order t = Tree0.to_alist ?key_order t.tree

  let symmetric_diff t1 t2 ~data_equal =
    Tree0.symmetric_diff t1.tree t2.tree ~compare_key:(compare_key t1) ~data_equal
  ;;

  let fold_symmetric_diff t1 t2 ~data_equal ~init ~f =
    Tree0.fold_symmetric_diff
      t1.tree
      t2.tree
      ~compare_key:(compare_key t1)
      ~data_equal
      ~init
      ~f
  ;;

  let merge_by_case t1 t2 ~(local_ first) ~(local_ second) ~both =
    (Tree0.merge_by_case
       t1.tree
       t2.tree
       ~first
       ~second
       ~both
       ~compare_key:(compare_key t1)
     |> like t1)
    [@nontail]
  ;;

  let merge t1 t2 ~f =
    like t1 (Tree0.merge t1.tree t2.tree ~f ~compare_key:(compare_key t1)) [@nontail]
  ;;

  let merge_disjoint_exn t1 t2 =
    like
      t1
      (Tree0.merge_disjoint_exn t1.tree t2.tree ~comparator:t1.comparator) [@nontail]
  ;;

  let merge_skewed t1 t2 ~combine =
    (* This is only a no-op in the case where at least one of the maps is empty. *)
    like_maybe_no_op
      (if length t2 <= length t1 then t1 else t2)
      (Tree0.merge_skewed t1.tree t2.tree ~combine ~compare_key:(compare_key t1))
  ;;

  let min_elt t = Tree0.min_elt t.tree
  let min_elt_exn t = Tree0.min_elt_exn t.tree
  let max_elt t = Tree0.max_elt t.tree
  let max_elt_exn t = Tree0.max_elt_exn t.tree
  let for_all t ~f = Tree0.for_all t.tree ~f
  let for_alli t ~f = Tree0.for_alli t.tree ~f
  let exists t ~f = Tree0.exists t.tree ~f
  let existsi t ~f = Tree0.existsi t.tree ~f
  let count t ~f = Tree0.count t.tree ~f
  let counti t ~f = Tree0.counti t.tree ~f
  let sum m t ~f = Tree0.sum m t.tree ~f
  let sumi m t ~f = Tree0.sumi m t.tree ~f

  let split t k =
    let l, maybe, r = Tree0.split t.tree k ~compare_key:(compare_key t) in
    like t l, maybe, like t r
  ;;

  let split_and_reinsert_boundary t ~into k =
    Tree0.split_and_reinsert_boundary t.tree ~into k ~compare_key:(compare_key t)
    |> like2 t
  ;;

  let split_le_gt t k = split_and_reinsert_boundary t ~into:`Left k
  let split_lt_ge t k = split_and_reinsert_boundary t ~into:`Right k
  let count_lt t k = Tree0.count_lt t.tree k ~compare_key:(compare_key t)
  let count_le t k = Tree0.count_le t.tree k ~compare_key:(compare_key t)
  let count_gt t k = Tree0.count_gt t.tree k ~compare_key:(compare_key t)
  let count_ge t k = Tree0.count_ge t.tree k ~compare_key:(compare_key t)

  let subrange t ~lower_bound ~upper_bound =
    let _, mid, _ =
      Tree0.split_range t.tree ~lower_bound ~upper_bound ~compare_key:(compare_key t)
    in
    like t mid
  ;;

  let append ~lower_part ~upper_part =
    match
      Tree0.append
        ~compare_key:(compare_key lower_part)
        ~lower_part:lower_part.tree
        ~upper_part:upper_part.tree
    with
    | `Ok tree -> `Ok (of_tree tree ~comparator:(comparator lower_part))
    | `Overlapping_key_ranges -> `Overlapping_key_ranges
  ;;

  let fold_range_inclusive t ~min ~max ~init ~f =
    Tree0.fold_range_inclusive t.tree ~min ~max ~init ~f ~compare_key:(compare_key t)
  ;;

  let range_to_alist t ~min ~max =
    Tree0.range_to_alist t.tree ~min ~max ~compare_key:(compare_key t)
  ;;

  let closest_key t dir key =
    Tree0.closest_key t.tree dir key ~compare_key:(compare_key t)
  ;;

  let nth t n = Tree0.nth t.tree n
  let nth_exn t n = Option.value_exn (nth t n)
  let rank t key = Tree0.rank t.tree key ~compare_key:(compare_key t)
  let split_n t n = Tree0.split_n t.tree n |> like2 t

  let%template[@alloc a = (heap, stack)] sexp_of_t sexp_of_k sexp_of_v _ t =
    (Tree0.sexp_of_t [@alloc a]) sexp_of_k sexp_of_v t.tree [@exclave_if_stack a]
  ;;

  let to_sequence ?order ?keys_greater_or_equal_to ?keys_less_or_equal_to t =
    Tree0.to_sequence
      t.comparator
      ?order
      ?keys_greater_or_equal_to
      ?keys_less_or_equal_to
      t.tree
  ;;

  let binary_search t ~compare how v = Tree0.binary_search t.tree ~compare how v

  let binary_search_segmented t ~segment_of how =
    Tree0.binary_search_segmented t.tree ~segment_of how
  ;;

  let hash_fold_direct hash_fold_key hash_fold_data state t =
    Tree0.hash_fold_t_ignoring_structure hash_fold_key hash_fold_data state t.tree
  ;;

  let binary_search_subrange t ~compare ~lower_bound ~upper_bound =
    match
      Tree0.binary_search_two_sided_bounds t.tree ~compare ~lower_bound ~upper_bound
    with
    | Some (lower_bound, upper_bound) -> subrange t ~lower_bound ~upper_bound
    | None -> like_maybe_no_op t Tree0.Empty [@nontail]
  ;;

  module%template.portable
    [@modality p] Make_applicative_traversals
      (A : Applicative.Lazy_applicative) =
  struct
    module Tree_traversals = Tree0.Make_applicative_traversals [@modality p] (A)

    let mapi t ~f =
      A.map (Tree_traversals.mapi t.tree ~f) ~f:(fun new_tree ->
        with_same_length t new_tree)
    ;;

    let filter_mapi t ~f =
      A.map (Tree_traversals.filter_mapi t.tree ~f) ~f:(fun new_tree_with_length ->
        like t new_tree_with_length)
    ;;
  end
end

(* [0] is used as the [length] argument everywhere in this module, since trees do not have
   their lengths stored at the root, unlike maps. The values are discarded always. *)
module Tree = struct
  type weight = int

  type ('k, 'v, 'cmp) t = ('k, 'v, 'cmp) Tree0.t =
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
        ; weight : int
        }

  let globalize0 = Tree0.globalize0
  let globalize _ _ _ t = globalize0 t
  let empty_without_value_restriction = Tree0.Empty
  let empty ~comparator:_ = Tree0.Empty
  let of_tree ~comparator:_ tree = tree
  let singleton ~comparator:_ k v = Tree0.singleton k v

  let of_sorted_array_unchecked ~comparator array =
    Tree0.of_sorted_array_unchecked array ~compare_key:(Comparator.compare comparator)
  ;;

  let of_sorted_array ~comparator array =
    Tree0.of_sorted_array array ~compare_key:(Comparator.compare comparator)
  ;;

  let of_alist ~comparator alist =
    Tree0.of_alist alist ~compare_key:(Comparator.compare comparator)
  ;;

  let of_alist_or_error ~comparator alist = Tree0.of_alist_or_error alist ~comparator
  let of_alist_exn ~comparator alist = Tree0.of_alist_exn alist ~comparator

  let of_alist_multi ~comparator alist =
    Tree0.of_alist_multi alist ~compare_key:(Comparator.compare comparator)
  ;;

  let of_alist_fold ~comparator alist ~init ~f =
    Tree0.of_alist_fold alist ~init ~f ~compare_key:(Comparator.compare comparator)
  ;;

  let of_alist_reduce ~comparator alist ~f =
    Tree0.of_alist_reduce alist ~f ~compare_key:(Comparator.compare comparator)
  ;;

  let of_iteri ~comparator ~iteri =
    Tree0.of_iteri ~iteri ~compare_key:(Comparator.compare comparator)
  ;;

  let of_iteri_exn ~comparator ~iteri = Tree0.of_iteri_exn ~iteri ~comparator

  let of_increasing_iterator_unchecked ~comparator:_required_by_intf ~len ~f =
    Tree0.of_increasing_iterator_unchecked ~len ~f
  ;;

  let of_increasing_sequence ~comparator seq =
    Tree0.of_increasing_sequence seq ~compare_key:(Comparator.compare comparator)
  ;;

  let of_sequence ~comparator seq =
    Tree0.of_sequence seq ~compare_key:(Comparator.compare comparator)
  ;;

  let of_sequence_or_error ~comparator seq = Tree0.of_sequence_or_error seq ~comparator
  let of_sequence_exn ~comparator seq = Tree0.of_sequence_exn seq ~comparator

  let of_sequence_multi ~comparator seq =
    Tree0.of_sequence_multi seq ~compare_key:(Comparator.compare comparator)
  ;;

  let of_sequence_fold ~comparator seq ~init ~f =
    Tree0.of_sequence_fold seq ~init ~f ~compare_key:(Comparator.compare comparator)
  ;;

  let of_sequence_reduce ~comparator seq ~f =
    Tree0.of_sequence_reduce seq ~f ~compare_key:(Comparator.compare comparator)
  ;;

  let of_list_with_key ~comparator list ~get_key =
    Tree0.of_list_with_key list ~get_key ~compare_key:(Comparator.compare comparator)
  ;;

  let of_list_with_key_or_error ~comparator list ~get_key =
    Tree0.of_list_with_key_or_error list ~get_key ~comparator
  ;;

  let of_list_with_key_exn ~comparator list ~get_key =
    Tree0.of_list_with_key_exn list ~get_key ~comparator
  ;;

  let of_list_with_key_multi ~comparator list ~get_key =
    Tree0.of_list_with_key_multi
      list
      ~get_key
      ~compare_key:(Comparator.compare comparator)
  ;;

  let of_list_with_key_fold ~comparator list ~get_key ~init ~f =
    Tree0.of_list_with_key_fold
      list
      ~get_key
      ~init
      ~f
      ~compare_key:(Comparator.compare comparator)
  ;;

  let of_list_with_key_reduce ~comparator list ~get_key ~f =
    Tree0.of_list_with_key_reduce
      list
      ~get_key
      ~f
      ~compare_key:(Comparator.compare comparator)
  ;;

  let to_tree t = t

  let invariants ~comparator t =
    Tree0.invariants t ~compare_key:(Comparator.compare comparator)
  ;;

  let is_empty t = Tree0.is_empty t
  let length t = Tree0.length t

  let set ~comparator t ~key ~data =
    Tree0.set t ~key ~data ~compare_key:(Comparator.compare comparator)
  ;;

  let add_exn ~comparator t ~key ~data =
    Tree0.add_exn
      t
      ~key
      ~data
      ~compare_key:(Comparator.compare comparator)
      ~sexp_of_key:(Comparator.sexp_of_t comparator)
  ;;

  let add_exn_internal ~comparator t ~key ~data =
    Tree0.add_exn_internal
      t
      ~key
      ~data
      ~compare_key:(Comparator.compare comparator)
      ~sexp_of_key:(Comparator.sexp_of_t comparator)
  ;;

  let add ~comparator t ~key ~data =
    try `Ok (add_exn_internal t ~comparator ~key ~data) with
    | _ -> `Duplicate
  ;;

  let add_multi ~comparator t ~key ~data =
    Tree0.add_multi t ~key ~data ~compare_key:(Comparator.compare comparator)
  ;;

  let remove_multi ~comparator t key =
    Tree0.remove_multi t key ~compare_key:(Comparator.compare comparator)
  ;;

  let find_multi ~comparator t key =
    Tree0.find_multi t key ~compare_key:(Comparator.compare comparator)
  ;;

  let change ~comparator t key ~f =
    Tree0.change t key ~f ~compare_key:(Comparator.compare comparator)
  ;;

  let update ~comparator t key ~f =
    Tree0.update t key ~f ~compare_key:(Comparator.compare comparator)
  ;;

  let update_and_return ~comparator t key ~f =
    let result, tree =
      Tree0.update_and_return t key ~f ~compare_key:(Comparator.compare comparator)
    in
    Modes.Global.unwrap result, tree
  ;;

  let find_exn ~comparator t key =
    Tree0.find_exn
      t
      key
      ~compare_key:(Comparator.compare comparator)
      ~sexp_of_key:(Comparator.sexp_of_t comparator)
  ;;

  let find ~comparator t key =
    Tree0.find t key ~compare_key:(Comparator.compare comparator)
  ;;

  let remove ~comparator t key =
    Tree0.remove t key ~compare_key:(Comparator.compare comparator)
  ;;

  let mem ~comparator t key = Tree0.mem t key ~compare_key:(Comparator.compare comparator)
  let iter_keys t ~f = Tree0.iter_keys t ~f
  let iter t ~f = Tree0.iter t ~f
  let iteri t ~f = Tree0.iteri t ~f
  let iteri_until t ~f = Tree0.iteri_until t ~f

  let iter2 ~comparator t1 t2 ~f =
    Tree0.iter2 t1 t2 ~f ~compare_key:(Comparator.compare comparator)
  ;;

  let map t ~f = Tree0.map t ~f
  let mapi t ~f = Tree0.mapi t ~f
  let fold t ~init ~f = Tree0.fold t ~f ~init
  let fold_until t ~init ~f ~finish = Tree0.fold_until t ~f ~init ~finish
  let fold_right t ~init ~f = Tree0.fold_right t ~f ~init

  let fold2 ~comparator t1 t2 ~init ~f =
    Tree0.fold2 t1 t2 ~init ~f ~compare_key:(Comparator.compare comparator)
  ;;

  let filter_keys t ~f = Tree0.filter_keys t ~f
  let filter t ~f = Tree0.filter t ~f
  let filteri t ~f = Tree0.filteri t ~f
  let filter_map t ~f = Tree0.filter_map t ~f
  let filter_mapi t ~f = Tree0.filter_mapi t ~f
  let partition_mapi t ~f = Tree0.partition_mapi t ~f
  let partition_map t ~f = Tree0.partition_map t ~f
  let partition_result t = Tree0.partition_result t
  let partitioni_tf t ~f = Tree0.partitioni_tf t ~f
  let partition_tf t ~f = Tree0.partition_tf t ~f

  let combine_errors ~comparator t =
    Tree0.combine_errors t ~sexp_of_key:(Comparator.sexp_of_t comparator)
  ;;

  let unzip = Tree0.unzip

  let%template[@mode m = (local, global)] compare_direct ~comparator compare_data t1 t2 =
    Tree0.compare (Comparator.compare comparator) compare_data t1 t2
  ;;

  let%template[@mode m = (local, global)] equal ~comparator data_equal t1 t2 =
    Tree0.equal (Comparator.compare comparator) data_equal t1 t2
  ;;

  let keys t = Tree0.keys t
  let data t = Tree0.data t
  let to_alist ?key_order t = Tree0.to_alist ?key_order t

  let symmetric_diff ~comparator t1 t2 ~data_equal =
    Tree0.symmetric_diff t1 t2 ~compare_key:(Comparator.compare comparator) ~data_equal
  ;;

  let fold_symmetric_diff ~comparator t1 t2 ~data_equal ~init ~f =
    Tree0.fold_symmetric_diff
      t1
      t2
      ~compare_key:(Comparator.compare comparator)
      ~data_equal
      ~init
      ~f
  ;;

  let merge_by_case ~comparator t1 t2 ~first ~second ~both =
    let compare_key = Comparator.compare comparator in
    Tree0.merge_by_case t1 t2 ~first ~second ~both ~compare_key
  ;;

  let merge ~comparator t1 t2 ~f =
    Tree0.merge t1 t2 ~f ~compare_key:(Comparator.compare comparator)
  ;;

  let merge_disjoint_exn ~comparator t1 t2 = Tree0.merge_disjoint_exn t1 t2 ~comparator

  let merge_skewed ~comparator t1 t2 ~combine =
    Tree0.merge_skewed t1 t2 ~combine ~compare_key:(Comparator.compare comparator)
  ;;

  let min_elt t = Tree0.min_elt t
  let min_elt_exn t = Tree0.min_elt_exn t
  let max_elt t = Tree0.max_elt t
  let max_elt_exn t = Tree0.max_elt_exn t
  let for_all t ~f = Tree0.for_all t ~f
  let for_alli t ~f = Tree0.for_alli t ~f
  let exists t ~f = Tree0.exists t ~f
  let existsi t ~f = Tree0.existsi t ~f
  let count t ~f = Tree0.count t ~f
  let counti t ~f = Tree0.counti t ~f
  let sum m t ~f = Tree0.sum m t ~f
  let sumi m t ~f = Tree0.sumi m t ~f
  let split ~comparator t k = Tree0.split t k ~compare_key:(Comparator.compare comparator)

  let split_le_gt ~comparator t k =
    Tree0.split_and_reinsert_boundary
      t
      ~into:`Left
      k
      ~compare_key:(Comparator.compare comparator)
  ;;

  let split_lt_ge ~comparator t k =
    Tree0.split_and_reinsert_boundary
      t
      ~into:`Right
      k
      ~compare_key:(Comparator.compare comparator)
  ;;

  let count_lt ~comparator t k =
    Tree0.count_lt t k ~compare_key:(Comparator.compare comparator)
  ;;

  let count_le ~comparator t k =
    Tree0.count_le t k ~compare_key:(Comparator.compare comparator)
  ;;

  let count_gt ~comparator t k =
    Tree0.count_gt t k ~compare_key:(Comparator.compare comparator)
  ;;

  let count_ge ~comparator t k =
    Tree0.count_ge t k ~compare_key:(Comparator.compare comparator)
  ;;

  let append ~comparator ~lower_part ~upper_part =
    Tree0.append ~lower_part ~upper_part ~compare_key:(Comparator.compare comparator)
  ;;

  let subrange ~comparator t ~lower_bound ~upper_bound =
    let _, ret, _ =
      Tree0.split_range
        t
        ~lower_bound
        ~upper_bound
        ~compare_key:(Comparator.compare comparator)
    in
    ret
  ;;

  let fold_range_inclusive ~comparator t ~min ~max ~init ~f =
    Tree0.fold_range_inclusive
      t
      ~min
      ~max
      ~init
      ~f
      ~compare_key:(Comparator.compare comparator)
  ;;

  let range_to_alist ~comparator t ~min ~max =
    Tree0.range_to_alist t ~min ~max ~compare_key:(Comparator.compare comparator)
  ;;

  let closest_key ~comparator t dir key =
    Tree0.closest_key t dir key ~compare_key:(Comparator.compare comparator)
  ;;

  let nth t n = Tree0.nth t n
  let nth_exn t n = Option.value_exn (nth t n)

  let rank ~comparator t key =
    Tree0.rank t key ~compare_key:(Comparator.compare comparator)
  ;;

  let split_n t n = Tree0.split_n t n
  let sexp_of_t sexp_of_k sexp_of_v _ t = Tree0.sexp_of_t sexp_of_k sexp_of_v t

  let t_of_sexp_direct ~comparator k_of_sexp v_of_sexp sexp =
    Tree0.t_of_sexp_direct k_of_sexp v_of_sexp sexp ~comparator
  ;;

  let to_sequence ~comparator ?order ?keys_greater_or_equal_to ?keys_less_or_equal_to t =
    Tree0.to_sequence comparator ?order ?keys_greater_or_equal_to ?keys_less_or_equal_to t
  ;;

  let binary_search ~comparator:_ t ~compare how v = Tree0.binary_search t ~compare how v

  let binary_search_segmented ~comparator:_ t ~segment_of how =
    Tree0.binary_search_segmented t ~segment_of how
  ;;

  let binary_search_subrange ~comparator t ~compare ~lower_bound ~upper_bound =
    match Tree0.binary_search_two_sided_bounds t ~compare ~lower_bound ~upper_bound with
    | Some (lower_bound, upper_bound) -> subrange ~comparator t ~lower_bound ~upper_bound
    | None -> Empty
  ;;

  module%template.portable [@modality p] Make_applicative_traversals =
    Tree0.Make_applicative_traversals
    [@modality p]

  let map_keys ~comparator t ~f = Tree0.map_keys ~comparator t ~f
  let map_keys_exn ~comparator t ~f = Tree0.map_keys_exn ~comparator t ~f

  (* This calling convention of [~comparator ~comparator] is confusing. It is required
     because [access_options] and [create_options] both demand a [~comparator] argument in
     [Map.Using_comparator.Tree].

     Making it less confusing would require some unnecessary complexity in signatures.
     Better to just live with an undesirable interface in a function that will probably
     never be called directly. *)
  let transpose_keys ~comparator:outer_comparator ~comparator:inner_comparator t =
    Tree0.transpose_keys ~outer_comparator ~inner_comparator t
  ;;

  module Build_increasing = struct
    type ('k, 'v, 'w) t = ('k, 'v, 'w) Tree0.Build_increasing.t

    let empty = Tree0.Build_increasing.empty

    let add_exn t ~comparator ~key ~data =
      match Tree0.Build_increasing.max_key t with
      | Some prev_key when (Comparator.compare comparator) prev_key key >= 0 ->
        Error.raise_s (Sexp.Atom "Map.Build_increasing.add: non-increasing key")
      | _ -> Tree0.Build_increasing.add_unchecked t ~key ~data
    ;;

    let to_tree t = Tree0.Build_increasing.to_tree_unchecked t
  end

  module Expert = struct
    type ('k, 'v, 'cmp) t = ('k, 'v, 'cmp) Tree0.t =
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
          ; weight : int
          }
    [@@deriving sexp_of]

    let balance_invariants t = Tree0.balance_invariants t

    let order_invariants ~comparator t =
      Tree0.order_invariants t ~compare_key:(Comparator.compare comparator)
    ;;

    let are_balanced t1 t2 =
      let w1 = Tree0.weight t1
      and w2 = Tree0.weight t2 in
      (not (Tree0.is_too_heavy ~weight:w1 ~for_weight:w2))
      && not (Tree0.is_too_heavy ~weight:w2 ~for_weight:w1)
    ;;

    let need_rebalance_at_most_once t1 t2 =
      let w1 = Tree0.weight t1
      and w2 = Tree0.weight t2 in
      match t1, t2 with
      | Node node, _ when Tree0.is_too_heavy ~weight:w1 ~for_weight:w2 ->
        (not (Tree0.is_too_heavy ~weight:(w1 - 1) ~for_weight:w2))
        && Tree0.may_rotate_just_once
             ~inner_sibling_weight:(Tree0.weight node.right)
             ~outer_sibling_weight:(Tree0.weight node.left)
      | _, Node node when Tree0.is_too_heavy ~weight:w2 ~for_weight:w1 ->
        (not (Tree0.is_too_heavy ~weight:(w2 - 1) ~for_weight:w1))
        && Tree0.may_rotate_just_once
             ~inner_sibling_weight:(Tree0.weight node.left)
             ~outer_sibling_weight:(Tree0.weight node.right)
      | _ -> true
    ;;

    let create_assuming_balanced_unchecked = Tree0.create
    let create_and_rebalance_at_most_once_unchecked = Tree0.bal
    let create_and_rebalance_unchecked = Tree0.join
    let concat_and_rebalance_at_most_once_unchecked = Tree0.concat_unchecked
    let concat_and_rebalance_unchecked = Tree0.concat_and_balance_unchecked
    let singleton = Tree0.singleton
    let empty = Empty
    let length_of_weight = Int.pred
  end
end

module Using_comparator = struct
  type nonrec ('k, 'v, 'cmp) t = ('k, 'v, 'cmp) t

  include Accessors

  let%template empty ~(comparator @ p) = { tree = Tree0.Empty; comparator }
  [@@mode p = (portable, nonportable)]
  ;;

  let singleton ~comparator k v = { comparator; tree = Tree0.singleton k v }
  let of_tree ~comparator tree = { comparator; tree }
  let to_tree = to_tree

  let of_sorted_array_unchecked ~comparator array =
    of_tree
      ~comparator
      (Tree0.of_sorted_array_unchecked array ~compare_key:(Comparator.compare comparator))
    [@nontail]
  ;;

  let of_sorted_array ~comparator array =
    Or_error.map
      (Tree0.of_sorted_array array ~compare_key:(Comparator.compare comparator))
      ~f:(fun tree -> of_tree ~comparator tree)
  ;;

  let of_alist ~comparator alist =
    match Tree0.of_alist alist ~compare_key:(Comparator.compare comparator) with
    | `Ok tree -> `Ok (of_tree ~comparator tree)
    | `Duplicate_key _ as z -> z
  ;;

  let of_alist_or_error ~comparator alist =
    Result.map (Tree0.of_alist_or_error alist ~comparator) ~f:(fun tree ->
      of_tree ~comparator tree)
  ;;

  let of_alist_exn ~comparator alist =
    of_tree ~comparator (Tree0.of_alist_exn alist ~comparator)
  ;;

  let of_alist_multi ~comparator alist =
    of_tree
      ~comparator
      (Tree0.of_alist_multi alist ~compare_key:(Comparator.compare comparator))
  ;;

  let of_alist_fold ~comparator alist ~init ~f =
    of_tree
      ~comparator
      (Tree0.of_alist_fold alist ~init ~f ~compare_key:(Comparator.compare comparator))
  ;;

  let of_alist_reduce ~comparator alist ~f =
    of_tree
      ~comparator
      (Tree0.of_alist_reduce alist ~f ~compare_key:(Comparator.compare comparator))
  ;;

  let of_iteri ~comparator ~iteri =
    match Tree0.of_iteri ~compare_key:(Comparator.compare comparator) ~iteri with
    | `Ok tree_length -> `Ok (of_tree ~comparator tree_length)
    | `Duplicate_key _ as z -> z
  ;;

  let of_iteri_exn ~comparator ~iteri =
    of_tree ~comparator (Tree0.of_iteri_exn ~comparator ~iteri)
  ;;

  let of_increasing_iterator_unchecked ~comparator ~len ~f =
    of_tree ~comparator (Tree0.of_increasing_iterator_unchecked ~len ~f) [@nontail]
  ;;

  let of_increasing_sequence ~comparator seq =
    Or_error.map
      ~f:(fun x -> of_tree ~comparator x)
      (Tree0.of_increasing_sequence seq ~compare_key:(Comparator.compare comparator))
  ;;

  let of_sequence ~comparator seq =
    match Tree0.of_sequence seq ~compare_key:(Comparator.compare comparator) with
    | `Ok tree -> `Ok (of_tree ~comparator tree)
    | `Duplicate_key _ as z -> z
  ;;

  let of_sequence_or_error ~comparator seq =
    Result.map (Tree0.of_sequence_or_error seq ~comparator) ~f:(fun tree ->
      of_tree ~comparator tree)
  ;;

  let of_sequence_exn ~comparator seq =
    of_tree ~comparator (Tree0.of_sequence_exn seq ~comparator)
  ;;

  let of_sequence_multi ~comparator seq =
    of_tree
      ~comparator
      (Tree0.of_sequence_multi seq ~compare_key:(Comparator.compare comparator))
  ;;

  let of_sequence_fold ~comparator seq ~init ~f =
    of_tree
      ~comparator
      (Tree0.of_sequence_fold seq ~init ~f ~compare_key:(Comparator.compare comparator))
  ;;

  let of_sequence_reduce ~comparator seq ~f =
    of_tree
      ~comparator
      (Tree0.of_sequence_reduce seq ~f ~compare_key:(Comparator.compare comparator))
  ;;

  let of_list_with_key ~comparator list ~get_key =
    match
      Tree0.of_list_with_key list ~get_key ~compare_key:(Comparator.compare comparator)
    with
    | `Ok tree -> `Ok (of_tree ~comparator tree)
    | `Duplicate_key _ as z -> z
  ;;

  let of_list_with_key_or_error ~comparator list ~get_key =
    Result.map (Tree0.of_list_with_key_or_error list ~get_key ~comparator) ~f:(fun tree ->
      of_tree ~comparator tree)
  ;;

  let of_list_with_key_exn ~comparator list ~get_key =
    of_tree ~comparator (Tree0.of_list_with_key_exn list ~get_key ~comparator)
  ;;

  let of_list_with_key_multi ~comparator list ~get_key =
    Tree0.of_list_with_key_multi
      list
      ~get_key
      ~compare_key:(Comparator.compare comparator)
    |> of_tree ~comparator
  ;;

  let of_list_with_key_fold ~comparator list ~get_key ~init ~f =
    Tree0.of_list_with_key_fold
      list
      ~get_key
      ~init
      ~f
      ~compare_key:(Comparator.compare comparator)
    |> of_tree ~comparator
  ;;

  let of_list_with_key_reduce ~comparator list ~get_key ~f =
    Tree0.of_list_with_key_reduce
      list
      ~get_key
      ~f
      ~compare_key:(Comparator.compare comparator)
    |> of_tree ~comparator
  ;;

  let t_of_sexp_direct ~comparator k_of_sexp v_of_sexp sexp =
    of_tree ~comparator (Tree0.t_of_sexp_direct k_of_sexp v_of_sexp sexp ~comparator)
  ;;

  let map_keys ~comparator t ~f =
    match Tree0.map_keys t.tree ~f ~comparator with
    | `Ok pair -> `Ok (of_tree ~comparator pair)
    | `Duplicate_key _ as dup -> dup
  ;;

  let map_keys_exn ~comparator t ~f =
    of_tree ~comparator (Tree0.map_keys_exn t.tree ~f ~comparator)
  ;;

  let transpose_keys ~comparator:inner_comparator t =
    let outer_comparator = t.comparator in
    Tree0.transpose_keys ~outer_comparator ~inner_comparator (Tree0.map t.tree ~f:to_tree)
    |> of_tree ~comparator:inner_comparator
    |> map ~f:(fun x -> of_tree ~comparator:outer_comparator x)
  ;;

  module%template.portable Empty_without_value_restriction (K : Comparator.S1) = struct
    let empty = { tree = Tree0.Empty; comparator = K.comparator }
  end

  module Tree = Tree
end

include Accessors

let comparator_s t = Comparator.to_module t.comparator
let to_comparator = Comparator.of_module
let of_tree m tree = of_tree ~comparator:(to_comparator m) tree

let%template empty (type cw : value mod p) (m : (_, cw) Comparator.Module.t) =
  (Using_comparator.empty [@mode p]) ~comparator:(to_comparator m)
[@@mode p = (nonportable, portable)]
;;

let singleton m a = Using_comparator.singleton ~comparator:(to_comparator m) a
let of_alist m a = Using_comparator.of_alist ~comparator:(to_comparator m) a

let of_alist_or_error m a =
  Using_comparator.of_alist_or_error ~comparator:(to_comparator m) a
;;

let of_alist_exn m a = Using_comparator.of_alist_exn ~comparator:(to_comparator m) a
let of_alist_multi m a = Using_comparator.of_alist_multi ~comparator:(to_comparator m) a

let of_alist_fold m a ~init ~f =
  Using_comparator.of_alist_fold ~comparator:(to_comparator m) a ~init ~f
;;

let of_alist_reduce m a ~f =
  Using_comparator.of_alist_reduce ~comparator:(to_comparator m) a ~f
;;

let of_sorted_array_unchecked m a =
  Using_comparator.of_sorted_array_unchecked ~comparator:(to_comparator m) a
;;

let of_sorted_array m a = Using_comparator.of_sorted_array ~comparator:(to_comparator m) a
let of_iteri m ~iteri = Using_comparator.of_iteri ~iteri ~comparator:(to_comparator m)

let of_iteri_exn m ~iteri =
  Using_comparator.of_iteri_exn ~iteri ~comparator:(to_comparator m)
;;

let of_increasing_iterator_unchecked m ~len ~f =
  Using_comparator.of_increasing_iterator_unchecked ~len ~f ~comparator:(to_comparator m)
;;

let of_increasing_sequence m seq =
  Using_comparator.of_increasing_sequence ~comparator:(to_comparator m) seq
;;

let of_sequence m s = Using_comparator.of_sequence ~comparator:(to_comparator m) s

let of_sequence_or_error m s =
  Using_comparator.of_sequence_or_error ~comparator:(to_comparator m) s
;;

let of_sequence_exn m s = Using_comparator.of_sequence_exn ~comparator:(to_comparator m) s

let of_sequence_multi m s =
  Using_comparator.of_sequence_multi ~comparator:(to_comparator m) s
;;

let of_sequence_fold m s ~init ~f =
  Using_comparator.of_sequence_fold ~comparator:(to_comparator m) s ~init ~f
;;

let of_sequence_reduce m s ~f =
  Using_comparator.of_sequence_reduce ~comparator:(to_comparator m) s ~f
;;

let of_list_with_key m l ~get_key =
  Using_comparator.of_list_with_key ~comparator:(to_comparator m) l ~get_key
;;

let of_list_with_key_or_error m l ~get_key =
  Using_comparator.of_list_with_key_or_error ~comparator:(to_comparator m) l ~get_key
;;

let of_list_with_key_exn m l ~get_key =
  Using_comparator.of_list_with_key_exn ~comparator:(to_comparator m) l ~get_key
;;

let of_list_with_key_multi m l ~get_key =
  Using_comparator.of_list_with_key_multi ~comparator:(to_comparator m) l ~get_key
;;

let of_list_with_key_fold m l ~get_key ~init ~f =
  Using_comparator.of_list_with_key_fold ~comparator:(to_comparator m) l ~get_key ~init ~f
;;

let of_list_with_key_reduce m l ~get_key ~f =
  Using_comparator.of_list_with_key_reduce ~comparator:(to_comparator m) l ~get_key ~f
;;

let map_keys m t ~f = Using_comparator.map_keys ~comparator:(to_comparator m) t ~f
let map_keys_exn m t ~f = Using_comparator.map_keys_exn ~comparator:(to_comparator m) t ~f
let transpose_keys m t = Using_comparator.transpose_keys ~comparator:(to_comparator m) t

module M (K : sig
    type t
    type comparator_witness
  end) =
struct
  type nonrec 'v t = (K.t, 'v, K.comparator_witness) t
end

module type%template [@alloc a = (heap, stack)] Sexp_of_m = Sexpable.Sexp_of [@alloc a]

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

let%template[@alloc a @ m = (heap_global, stack_local)] sexp_of_m__t
  (type k)
  (module K : Sexp_of_m with type t = k[@alloc a])
  sexp_of_v
  t
  =
  (sexp_of_t [@alloc a])
    (K.sexp_of_t [@alloc a])
    sexp_of_v
    (fun _ -> Sexp.Atom "_")
    t [@exclave_if_stack a]
;;

let m__t_of_sexp
  (type k cmp)
  (module K : M_of_sexp with type t = k and type comparator_witness = cmp)
  v_of_sexp
  sexp
  =
  Using_comparator.t_of_sexp_direct ~comparator:K.comparator K.t_of_sexp v_of_sexp sexp
;;

let m__t_sexp_grammar
  (type k)
  (module K : M_sexp_grammar with type t = k)
  (v_grammar : _ Sexplib0.Sexp_grammar.t)
  : _ Sexplib0.Sexp_grammar.t
  =
  { untyped =
      Tagged
        { key = Sexplib0.Sexp_grammar.assoc_tag
        ; value = List []
        ; grammar =
            List
              (Many
                 (List
                    (Cons
                       ( Tagged
                           { key = Sexplib0.Sexp_grammar.assoc_key_tag
                           ; value = List []
                           ; grammar = K.t_sexp_grammar.untyped
                           }
                       , Cons
                           ( Tagged
                               { key = Sexplib0.Sexp_grammar.assoc_value_tag
                               ; value = List []
                               ; grammar = v_grammar.untyped
                               }
                           , Empty ) ))))
        }
  }
;;

let compare_m__t (module _ : Compare_m) compare_v t1 t2 = compare_direct compare_v t1 t2
let equal_m__t (module _ : Equal_m) equal_v t1 t2 = equal equal_v t1 t2

let compare_m__t__local (module _ : Compare_m) compare_v t1 t2 =
  compare_direct__local compare_v t1 t2
;;

let equal_m__t__local (module _ : Equal_m) equal_v t1 t2 = equal__local equal_v t1 t2
let globalize_m__t (module _ : Globalize_m) _ (local_ t) = globalize0 t

let hash_fold_m__t (type k) (module K : Hash_fold_m with type t = k) hash_fold_v state =
  hash_fold_direct K.hash_fold_t hash_fold_v state
;;

let globalize _ _ _ t = globalize0 t

module Poly = struct
  type nonrec ('k, 'v) t = ('k, 'v, Comparator.Poly.comparator_witness) t
  type nonrec ('k, 'v) tree = ('k, 'v, Comparator.Poly.comparator_witness) Tree0.t
  type comparator_witness = Comparator.Poly.comparator_witness

  include Accessors

  let comparator = Comparator.Poly.comparator
  let of_tree tree = { tree; comparator }

  include%template
    Using_comparator.Empty_without_value_restriction [@modality portable] (Comparator.Poly)

  let singleton a = Using_comparator.singleton ~comparator a
  let of_alist a = Using_comparator.of_alist ~comparator a
  let of_alist_or_error a = Using_comparator.of_alist_or_error ~comparator a
  let of_alist_exn a = Using_comparator.of_alist_exn ~comparator a
  let of_alist_multi a = Using_comparator.of_alist_multi ~comparator a
  let of_alist_fold a ~init ~f = Using_comparator.of_alist_fold ~comparator a ~init ~f
  let of_alist_reduce a ~f = Using_comparator.of_alist_reduce ~comparator a ~f

  let of_sorted_array_unchecked a =
    Using_comparator.of_sorted_array_unchecked ~comparator a
  ;;

  let of_sorted_array a = Using_comparator.of_sorted_array ~comparator a
  let of_iteri ~iteri = Using_comparator.of_iteri ~iteri ~comparator
  let of_iteri_exn ~iteri = Using_comparator.of_iteri_exn ~iteri ~comparator

  let of_increasing_iterator_unchecked ~len ~f =
    Using_comparator.of_increasing_iterator_unchecked ~len ~f ~comparator
  ;;

  let of_increasing_sequence seq = Using_comparator.of_increasing_sequence ~comparator seq
  let of_sequence s = Using_comparator.of_sequence ~comparator s
  let of_sequence_or_error s = Using_comparator.of_sequence_or_error ~comparator s
  let of_sequence_exn s = Using_comparator.of_sequence_exn ~comparator s
  let of_sequence_multi s = Using_comparator.of_sequence_multi ~comparator s

  let of_sequence_fold s ~init ~f =
    Using_comparator.of_sequence_fold ~comparator s ~init ~f
  ;;

  let of_sequence_reduce s ~f = Using_comparator.of_sequence_reduce ~comparator s ~f

  let of_list_with_key l ~get_key =
    Using_comparator.of_list_with_key ~comparator l ~get_key
  ;;

  let of_list_with_key_or_error l ~get_key =
    Using_comparator.of_list_with_key_or_error ~comparator l ~get_key
  ;;

  let of_list_with_key_exn l ~get_key =
    Using_comparator.of_list_with_key_exn ~comparator l ~get_key
  ;;

  let of_list_with_key_multi l ~get_key =
    Using_comparator.of_list_with_key_multi ~comparator l ~get_key
  ;;

  let of_list_with_key_fold l ~get_key ~init ~f =
    Using_comparator.of_list_with_key_fold ~comparator l ~get_key ~init ~f
  ;;

  let of_list_with_key_reduce l ~get_key ~f =
    Using_comparator.of_list_with_key_reduce ~comparator l ~get_key ~f
  ;;

  let map_keys t ~f = Using_comparator.map_keys ~comparator t ~f
  let map_keys_exn t ~f = Using_comparator.map_keys_exn ~comparator t ~f
  let transpose_keys t = Using_comparator.transpose_keys ~comparator t
end

module Private = struct
  module Enum = Tree0.Enum
end
