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

(* Sets over ordered types *)

open! Import
include Set_intf.Definitions

let with_return = With_return.with_return

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

  type ('a, 'cmp) t =
    | Empty
    (* Leaf is the same as Node with empty children but uses less space. *)
    | Leaf of { global_ elt : 'a }
    | Node of
        { global_ left : ('a, 'cmp) t
        ; global_ elt : 'a
        ; global_ right : ('a, 'cmp) t
        ; weight : int
        }

  type ('a, 'cmp) tree = ('a, 'cmp) t

  let globalize0 : 'a 'cmp. local_ ('a, 'cmp) t -> ('a, 'cmp) t = function
    | Empty -> Empty
    | Leaf { elt } -> Leaf { elt }
    | Node { left; elt; right; weight } -> Node { left; elt; right; weight }
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

  let[@inline always] weight = function
    | Empty -> 1
    | Leaf { elt = _ } -> 2
    | Node { left = _; elt = _; right = _; weight = w } -> w
  ;;

  let[@inline always] length = function
    | Empty -> 0
    | Leaf { elt = _ } -> 1
    | Node { left = _; elt = _; right = _; weight = w } -> w - 1
  ;;

  let order_invariants =
    let in_range ~lower ~upper compare_elt v =
      (match lower with
       | None -> true
       | Some lower -> compare_elt lower v < 0)
      &&
      match upper with
      | None -> true
      | Some upper -> compare_elt v upper < 0
    in
    let rec loop ~lower ~upper compare_elt t =
      match t with
      | Empty -> true
      | Leaf { elt = v } -> in_range ~lower ~upper compare_elt v
      | Node { left = l; elt = v; right = r; weight = _ } ->
        in_range ~lower ~upper compare_elt v
        && loop ~lower ~upper:(Some v) compare_elt l
        && loop ~lower:(Some v) ~upper compare_elt r
    in
    fun t ~compare_elt -> loop ~lower:None ~upper:None compare_elt t
  ;;

  let rec balance_invariants t =
    match t with
    | Empty | Leaf _ -> true
    | Node { left = l; elt = _; right = r; weight = w } ->
      let wl = weight l
      and wr = weight r in
      w = wl + wr
      && w > 2
      && (not (is_too_heavy ~weight:wl ~for_weight:wr))
      && (not (is_too_heavy ~weight:wr ~for_weight:wl))
      && balance_invariants l
      && balance_invariants r
  ;;

  let invariants t ~compare_elt = order_invariants t ~compare_elt && balance_invariants t

  let is_empty = function
    | Empty -> true
    | Leaf { elt = _ } | Node _ -> false
  ;;

  (* Creates a new node with left son l, value v and right son r. We must have all
     elements of l < v < all elements of r. l and r must be balanced and neither
     [is_too_heavy] for the other. *)

  let[@inline always] create l v r =
    let wl = (weight [@inlined]) l in
    let wr = (weight [@inlined]) r in
    let w = wl + wr in
    if w = 2 then Leaf { elt = v } else Node { left = l; elt = v; right = r; weight = w }
  ;;

  (* We must call [f] with increasing indexes, because the bin_prot reader in Core.Set
     needs it. *)
  let of_increasing_iterator_unchecked ~len ~f =
    let rec loop n ~f i =
      match n with
      | 0 -> Empty
      | 1 ->
        let k = f i in
        Leaf { elt = k }
      | 2 ->
        let kl = f i in
        let k = f (i + 1) in
        create (Leaf { elt = kl }) k Empty
      | 3 ->
        let kl = f i in
        let k = f (i + 1) in
        let kr = f (i + 2) in
        create (Leaf { elt = kl }) k (Leaf { elt = kr })
      | n ->
        let left_length = n lsr 1 in
        let right_length = n - left_length - 1 in
        let left = loop left_length ~f i in
        let k = f (i + left_length) in
        let right = loop right_length ~f (i + left_length + 1) in
        create left k right
    in
    loop len ~f 0
  ;;

  let of_sorted_array_unchecked array ~compare_elt =
    let array_length = Array.length array in
    let next =
      (* We don't check if the array is sorted or keys are duplicated, because that
         checking is slower than the whole [of_sorted_array] function *)
      if array_length < 2 || compare_elt array.(0) array.(1) < 0
      then fun i -> array.(i)
      else fun i -> array.(array_length - 1 - i)
    in
    of_increasing_iterator_unchecked ~len:array_length ~f:next
  ;;

  let of_sorted_array array ~compare_elt =
    match array with
    | [||] | [| _ |] -> Result.Ok (of_sorted_array_unchecked array ~compare_elt)
    | _ ->
      with_return (fun r ->
        let increasing =
          match compare_elt array.(0) array.(1) with
          | 0 -> r.return (Or_error.error_string "of_sorted_array: duplicated elements")
          | i -> i < 0
        in
        for i = 1 to Array.length array - 2 do
          match compare_elt array.(i) array.(i + 1) with
          | 0 -> r.return (Or_error.error_string "of_sorted_array: duplicated elements")
          | i ->
            if Poly.( <> ) (i < 0) increasing
            then
              r.return (Or_error.error_string "of_sorted_array: elements are not ordered")
        done;
        Result.Ok (of_sorted_array_unchecked array ~compare_elt))
  ;;

  (* Same as create, but performs one step of rebalancing if necessary. Assumes l and r
     balanced and either [is_too_heavy] by at most 1. *)

  let bal l v r =
    let wl = (weight [@inlined]) l in
    let wr = (weight [@inlined]) r in
    if is_too_heavy ~weight:wl ~for_weight:wr
    then (
      match l with
      | Empty -> assert false
      | Leaf { elt = _ } -> assert false (* a leaf never [is_too_heavy] *)
      | Node { left = ll; elt = lv; right = lr; weight = _ } ->
        if may_rotate_just_once
             ~inner_sibling_weight:(weight lr)
             ~outer_sibling_weight:(weight ll)
        then create ll lv (create lr v r)
        else (
          match lr with
          | Empty -> assert false
          | Leaf { elt = lrv } -> create (create ll lv Empty) lrv (create Empty v r)
          | Node { left = lrl; elt = lrv; right = lrr; weight = _ } ->
            create (create ll lv lrl) lrv (create lrr v r)))
    else if is_too_heavy ~weight:wr ~for_weight:wl
    then (
      match r with
      | Empty -> assert false
      | Leaf { elt = _ } -> assert false (* a leaf never [is_too_heavy] *)
      | Node { left = rl; elt = rv; right = rr; weight = _ } ->
        if may_rotate_just_once
             ~inner_sibling_weight:(weight rl)
             ~outer_sibling_weight:(weight rr)
        then create (create l v rl) rv rr
        else (
          match rl with
          | Empty -> assert false
          | Leaf { elt = rlv } -> create (create l v Empty) rlv (create Empty rv rr)
          | Node { left = rll; elt = rlv; right = rlr; weight = _ } ->
            create (create l v rll) rlv (create rlr rv rr)))
    else (create [@inlined]) l v r
  ;;

  (* Insertion of one element *)

  exception Same

  let add t x ~compare_elt =
    let rec aux = function
      | Empty -> Leaf { elt = x }
      | Leaf { elt = v } ->
        let c = compare_elt x v in
        if c = 0
        then Exn.raise_without_backtrace Same
        else if c < 0
        then create (Leaf { elt = x }) v Empty
        else create Empty v (Leaf { elt = x })
      | Node { left = l; elt = v; right = r; weight = _ } ->
        let c = compare_elt x v in
        if c = 0
        then Exn.raise_without_backtrace Same
        else if c < 0
        then bal (aux l) v r
        else bal l v (aux r)
    in
    try aux t with
    | Same -> t
  ;;

  (* specialization of [add] that assumes that [x] is less than all existing elements *)
  let rec add_min x t =
    match t with
    | Empty -> Leaf { elt = x }
    | Leaf { elt = _ } -> Node { left = Empty; elt = x; right = t; weight = 3 }
    | Node { left = l; elt = v; right = r; weight = _ } -> bal (add_min x l) v r
  ;;

  (* specialization of [add] that assumes that [x] is greater than all existing elements *)
  let rec add_max t x =
    match t with
    | Empty -> Leaf { elt = x }
    | Leaf { elt = _ } -> Node { left = t; elt = x; right = Empty; weight = 3 }
    | Node { left = l; elt = v; right = r; weight = _ } -> bal l v (add_max r x)
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

      (* For [join_rotating_right l lv m rv r], the arguments correspond to an unbalanced
         tree with the shape:
         {v
                 rv
                /  \
              lv    r
             /  \
            l    m
         v}

         Precondition: [create l lv m] [is_too_heavy] for [r], and [r = Node _]. *)
      let rec join_rotating_right l lv m rv r =
        let mr =
          (* Recur down [m]'s right side until [create m rv r] is balanced. *)
          match m with
          | Node { left = ml; elt = mv; right = mr; weight = mw }
            when is_too_heavy ~weight:mw ~for_weight:(weight r) ->
            join_rotating_right ml mv mr rv r
          | _ -> create m rv r
        in
        (* Since [r] is a [Node], [mr] must be a [Node]. *)
        match mr with
        | Empty -> assert false
        | Leaf _ -> assert false
        | Node { left = m; elt = rv; right = r; weight = mrw } ->
          (* Now re-add [l] with 0-2 rotations. Proven sufficient in literature above. *)
          let lw = weight l in
          (match m with
           | Node { left = ml; elt = mv; right = mr; weight = mw }
             when is_too_heavy ~weight:mrw ~for_weight:lw ->
             if may_rotate_just_once
                  ~inner_sibling_weight:mw
                  ~outer_sibling_weight:(weight r)
             then create (create l lv m) rv r
             else create (create l lv ml) mv (create mr rv r)
           | _ -> create l lv mr)
      ;;

      (* Rotating left proceeds symmetrically to rotating right. *)

      (* For [join_rotating_left l lv m rv r], the arguments correspond to an unbalanced
         tree with the shape:
         {v
                 lv
                /  \
               l   rv
                  /  \
                 m    r
         v}

         Precondition: [create m rv r] [is_too_heavy] for [l], and [l = Node _]. *)
      let rec join_rotating_left l lv m rv r =
        let lm =
          (* Recur down [m]'s left side until [create l lv m] is balanced. *)
          match m with
          | Node { left = ml; elt = mv; right = mr; weight = mw }
            when is_too_heavy ~weight:mw ~for_weight:(weight l) ->
            join_rotating_left l lv ml mv mr
          | _ -> create l lv m
        in
        (* Since [l] is a [Node], [lm] must be a [Node]. *)
        match lm with
        | Empty -> assert false
        | Leaf _ -> assert false
        | Node { left = l; elt = lv; right = m; weight = lmw } ->
          (* Now re-add [r] with 0-2 rotations. Proven sufficient in literature above. *)
          let rw = weight r in
          (match m with
           | Node { left = ml; elt = mv; right = mr; weight = mw }
             when is_too_heavy ~weight:lmw ~for_weight:rw ->
             if may_rotate_just_once
                  ~inner_sibling_weight:mw
                  ~outer_sibling_weight:(weight l)
             then create l lv (create m rv r)
             else create (create l lv ml) mv (create mr rv r)
           | _ -> create lm rv r)
      ;;
    end

    (* Like [create] and [bal], for arbitrary height differences. See more detailed
       comment at start of [include struct ...] above. *)
    let join l v r =
      (* Cases adding just one or two values are straightforward. *)
      match l, r with
      | Empty, _ -> add_min v r
      | _, Empty -> add_max l v
      | Leaf { elt = lv }, _ -> add_min lv (add_min v r)
      | _, Leaf { elt = rv } -> add_max (add_max l v) rv
      | ( Node { left = ll; elt = lv; right = lr; weight = lw }
        , Node { left = rl; elt = rv; right = rr; weight = rw } ) ->
        (* Otherwise, recur down the heavier side and rotate toward the lighter side. *)
        if is_too_heavy ~weight:lw ~for_weight:rw
        then join_rotating_right ll lv lr v r
        else if is_too_heavy ~weight:rw ~for_weight:lw
        then join_rotating_left l v rl rv rr
        else create l v r
    ;;
  end

  let return_none () = None

  (* Smallest and greatest element of a set *)
  let rec call_with_min_elt ~none ~some = function
    | Empty -> none ()
    | Leaf { elt = v } | Node { left = Empty; elt = v; right = _; weight = _ } -> some v
    | Node { left = l; elt = _; right = _; weight = _ } -> call_with_min_elt l ~none ~some
  ;;

  let raise_min_elt_exn () = Error.raise_s (Atom "Set.min_elt_exn: empty set")
  let min_elt t = call_with_min_elt t ~none:return_none ~some:Option.return
  let min_elt_exn t = call_with_min_elt t ~none:raise_min_elt_exn ~some:Fn.id

  let fold_until t ~init ~f ~finish =
    let rec fold_until_helper ~f t acc =
      match t with
      | Empty -> Container.Continue_or_stop.Continue acc
      | Leaf { elt = value } -> f acc value [@nontail]
      | Node { left; elt = value; right; weight = _ } ->
        (match fold_until_helper ~f left acc with
         | Stop _a as x -> x
         | Continue acc ->
           (match f acc value with
            | Stop _a as x -> x
            | Continue a -> fold_until_helper ~f right a))
    in
    match fold_until_helper ~f t init with
    | Continue x -> finish x [@nontail]
    | Stop x -> x
  ;;

  let rec call_with_max_elt ~none ~some = function
    | Empty -> none ()
    | Leaf { elt = v } | Node { left = _; elt = v; right = Empty; weight = _ } -> some v
    | Node { left = _; elt = _; right = r; weight = _ } -> call_with_max_elt r ~none ~some
  ;;

  let raise_max_elt_exn () = Error.raise_s (Atom "Set.max_elt_exn: empty set")
  let max_elt t = call_with_max_elt t ~none:return_none ~some:Option.return
  let max_elt_exn t = call_with_max_elt t ~none:raise_max_elt_exn ~some:Fn.id

  (* Remove the smallest element of the given set *)

  let rec remove_min_elt = function
    | Empty -> invalid_arg "Set.remove_min_elt"
    | Leaf { elt = _ } -> Empty
    | Node { left = Empty; elt = _; right = r; weight = _ } -> r
    | Node { left = l; elt = v; right = r; weight = _ } -> bal (remove_min_elt l) v r
  ;;

  (* Merge two trees l and r into one. All elements of l must precede the elements of r.
     Assume either l or r [is_too_heavy] by at most 1. *)
  let merge t1 t2 =
    match t1, t2 with
    | Empty, t -> t
    | t, Empty -> t
    | _, _ -> bal t1 (min_elt_exn t2) (remove_min_elt t2)
  ;;

  (* Merge two trees l and r into one. All elements of l must precede the elements of r.
     No assumption on the weights of l and r. *)
  let concat t1 t2 =
    match t1, t2 with
    | Empty, t | t, Empty -> t
    | _, _ -> join t1 (min_elt_exn t2) (remove_min_elt t2)
  ;;

  let split t x ~compare_elt =
    let rec split t =
      match t with
      | Empty -> Empty, None, Empty
      | Leaf { elt = v } ->
        let c = compare_elt x v in
        if c = 0
        then Empty, Some v, Empty
        else if c < 0
        then Empty, None, Leaf { elt = v }
        else Leaf { elt = v }, None, Empty
      | Node { left = l; elt = v; right = r; weight = _ } ->
        let c = compare_elt x v in
        if c = 0
        then l, Some v, r
        else if c < 0
        then (
          let ll, maybe_elt, rl = split l in
          ll, maybe_elt, join rl v r)
        else (
          let lr, maybe_elt, rr = split r in
          join l v lr, maybe_elt, rr)
    in
    split t
  ;;

  let rec split_le_gt t x ~compare_elt =
    match t with
    | Empty -> Empty, Empty
    | Leaf { elt = v } ->
      if compare_elt x v >= 0 then Leaf { elt = v }, Empty else Empty, Leaf { elt = v }
    | Node { left = l; elt = v; right = r; weight = _ } ->
      let c = compare_elt x v in
      if c = 0
      then add_max l v, r
      else if c < 0
      then (
        let ll, rl = split_le_gt l x ~compare_elt in
        ll, join rl v r)
      else (
        let lr, rr = split_le_gt r x ~compare_elt in
        join l v lr, rr)
  ;;

  let rec split_lt_ge t x ~compare_elt =
    match t with
    | Empty -> Empty, Empty
    | Leaf { elt = v } ->
      if compare_elt x v > 0 then Leaf { elt = v }, Empty else Empty, Leaf { elt = v }
    | Node { left = l; elt = v; right = r; weight = _ } ->
      let c = compare_elt x v in
      if c = 0
      then l, add_min v r
      else if c < 0
      then (
        let ll, rl = split_lt_ge l x ~compare_elt in
        ll, join rl v r)
      else (
        let lr, rr = split_lt_ge r x ~compare_elt in
        join l v lr, rr)
  ;;

  (* Implementation of the set operations *)

  let rec mem t x ~compare_elt =
    match t with
    | Empty -> false
    | Leaf { elt = v } ->
      let c = compare_elt x v in
      c = 0
    | Node { left = l; elt = v; right = r; weight = _ } ->
      let c = compare_elt x v in
      c = 0 || mem (if c < 0 then l else r) x ~compare_elt
  ;;

  let singleton x = Leaf { elt = x }

  let remove t x ~compare_elt =
    let rec aux t =
      match t with
      | Empty -> Exn.raise_without_backtrace Same
      | Leaf { elt = v } ->
        if compare_elt x v = 0 then Empty else Exn.raise_without_backtrace Same
      | Node { left = l; elt = v; right = r; weight = _ } ->
        let c = compare_elt x v in
        if c = 0 then merge l r else if c < 0 then bal (aux l) v r else bal l v (aux r)
    in
    try aux t with
    | Same -> t
  ;;

  let remove_index t i ~compare_elt:_ =
    let rec aux t i =
      match t with
      | Empty -> Exn.raise_without_backtrace Same
      | Leaf { elt = _ } -> if i = 0 then Empty else Exn.raise_without_backtrace Same
      | Node { left = l; elt = v; right = r; weight = _ } ->
        let l_size = length l in
        let c = Poly.compare i l_size in
        if c = 0
        then merge l r
        else if c < 0
        then bal (aux l i) v r
        else bal l v (aux r (i - l_size - 1))
    in
    try aux t i with
    | Same -> t
  ;;

  let union s1 s2 ~compare_elt =
    let rec union s1 s2 =
      if phys_equal s1 s2
      then s1
      else (
        match s1, s2 with
        | Empty, t | t, Empty -> t
        | Leaf { elt = v1 }, _ -> add s2 v1 ~compare_elt
        | _, Leaf { elt = v2 } -> add s1 v2 ~compare_elt
        | ( Node { left = l1; elt = v1; right = r1; weight = w1 }
          , Node { left = l2; elt = v2; right = r2; weight = w2 } ) ->
          if w1 >= w2
          then (
            let l2, _, r2 = split s2 v1 ~compare_elt in
            join (union l1 l2) v1 (union r1 r2))
          else (
            let l1, _, r1 = split s1 v2 ~compare_elt in
            join (union l1 l2) v2 (union r1 r2)))
    in
    union s1 s2
  ;;

  let union_list ~comparator ~to_tree xs =
    let compare_elt = Comparator.compare comparator in
    List.fold xs ~init:Empty ~f:(fun ac x -> union ac (to_tree x) ~compare_elt)
  ;;

  let inter s1 s2 ~compare_elt =
    let rec inter s1 s2 =
      if phys_equal s1 s2
      then s1
      else (
        match s1, s2 with
        | Empty, _ | _, Empty -> Empty
        | (Leaf { elt } as singleton), other_set | other_set, (Leaf { elt } as singleton)
          -> if mem other_set elt ~compare_elt then singleton else Empty
        | Node { left = l1; elt = v1; right = r1; weight = _ }, t2 ->
          (match split t2 v1 ~compare_elt with
           | l2, None, r2 -> concat (inter l1 l2) (inter r1 r2)
           | l2, Some v1, r2 -> join (inter l1 l2) v1 (inter r1 r2)))
    in
    inter s1 s2
  ;;

  let diff s1 s2 ~compare_elt =
    let rec diff s1 s2 =
      if phys_equal s1 s2
      then Empty
      else (
        match s1, s2 with
        | Empty, _ -> Empty
        | t1, Empty -> t1
        | Leaf { elt = v1 }, t2 ->
          diff (Node { left = Empty; elt = v1; right = Empty; weight = 2 }) t2
        | Node { left = l1; elt = v1; right = r1; weight = _ }, t2 ->
          (match split t2 v1 ~compare_elt with
           | l2, None, r2 -> join (diff l1 l2) v1 (diff r1 r2)
           | l2, Some _, r2 -> concat (diff l1 l2) (diff r1 r2)))
    in
    diff s1 s2
  ;;

  module Enum : Private.Enum with type ('a, 'cmp) tree := ('a, 'cmp) tree = struct
    type increasing
    type decreasing

    type ('a, 'cmp, 'direction) nonempty =
      | More of 'a * ('a, 'cmp) tree * ('a, 'cmp, 'direction) t

    and ('a, 'cmp, 'direction) t = ('a, 'cmp, 'direction) nonempty or_null

    let rec cons s (e : (_, _, increasing) t) : (_, _, increasing) t =
      match s with
      | Empty -> e
      | Leaf { elt = v } -> This (More (v, Empty, e))
      | Node { left = l; elt = v; right = r; weight = _ } ->
        cons l (This (More (v, r, e)))
    ;;

    let rec cons_right s (e : (_, _, decreasing) t) : (_, _, decreasing) t =
      match s with
      | Empty -> e
      | Leaf { elt = v } -> This (More (v, Empty, e))
      | Node { left = l; elt = v; right = r; weight = _ } ->
        cons_right r (This (More (v, l, e)))
    ;;

    let of_set s : (_, _, increasing) t = cons s Null
    let of_set_right s : (_, _, decreasing) t = cons_right s Null

    let starting_at_increasing t key compare : (_, _, increasing) t =
      let rec loop t e =
        match t with
        | Empty -> e
        | Leaf { elt = v } -> if compare v key < 0 then e else This (More (v, Empty, e))
        | Node { left = l; elt = v; right = r; weight = _ } ->
          if compare v key < 0 then loop r e else loop l (This (More (v, r, e)))
      in
      loop t Null
    ;;

    let starting_at_decreasing t key compare : (_, _, decreasing) t =
      let rec loop t e =
        match t with
        | Empty -> e
        | Leaf { elt = v } -> if compare v key > 0 then e else This (More (v, Empty, e))
        | Node { left = l; elt = v; right = r; weight = _ } ->
          if compare v key > 0 then loop l e else loop r (This (More (v, l, e)))
      in
      loop t Null
    ;;

    let compare compare_elt e1 e2 =
      let rec loop e1 e2 =
        match e1, e2 with
        | Null, Null -> 0
        | Null, _ -> -1
        | _, Null -> 1
        | This (More (v1, r1, e1)), This (More (v2, r2, e2)) ->
          let c = compare_elt v1 v2 in
          if c <> 0
          then c
          else if phys_equal r1 r2
          then loop e1 e2
          else loop (cons r1 e1) (cons r2 e2)
      in
      loop e1 e2
    ;;

    let rec iter ~f = function
      | Null -> ()
      | This (More (a, tree, enum)) ->
        f a;
        iter (cons tree enum) ~f
    ;;

    let iter2 compare_elt t1 t2 ~f =
      let rec loop t1 t2 =
        match t1, t2 with
        | Null, Null -> ()
        | Null, _ -> iter t2 ~f:(fun a -> f (`Right a)) [@nontail]
        | _, Null -> iter t1 ~f:(fun a -> f (`Left a)) [@nontail]
        | This (More (a1, tree1, enum1)), This (More (a2, tree2, enum2)) ->
          let compare_result = compare_elt a1 a2 in
          if compare_result = 0
          then (
            f (`Both (a1, a2));
            loop (cons tree1 enum1) (cons tree2 enum2))
          else if compare_result < 0
          then (
            f (`Left a1);
            loop (cons tree1 enum1) t2)
          else (
            f (`Right a2);
            loop t1 (cons tree2 enum2))
      in
      loop t1 t2 [@nontail]
    ;;

    let symmetric_diff t1 t2 ~compare_elt =
      let step state : ((_, _) Either.t, _) Sequence.Step.t =
        match state with
        | Null, Null -> Done
        | Null, This (More (elt, tree, enum)) ->
          Yield { value = Second elt; state = Null, cons tree enum }
        | This (More (elt, tree, enum)), Null ->
          Yield { value = First elt; state = cons tree enum, Null }
        | ( (This (More (a1, tree1, enum1)) as left)
          , (This (More (a2, tree2, enum2)) as right) ) ->
          let compare_result = compare_elt a1 a2 in
          if compare_result = 0
          then (
            let next_state =
              if phys_equal tree1 tree2
              then enum1, enum2
              else cons tree1 enum1, cons tree2 enum2
            in
            Skip { state = next_state })
          else if compare_result < 0
          then Yield { value = First a1; state = cons tree1 enum1, right }
          else Yield { value = Second a2; state = left, cons tree2 enum2 }
      in
      Sequence.unfold_step ~init:(of_set t1, of_set t2) ~f:step
    ;;
  end

  let to_sequence_increasing comparator ~from_elt t =
    let next enum =
      match enum with
      | Null -> Sequence.Step.Done
      | This (Enum.More (k, t, e)) ->
        Sequence.Step.Yield { value = k; state = Enum.cons t e }
    in
    let init =
      match from_elt with
      | None -> Enum.of_set t
      | Some key -> Enum.starting_at_increasing t key (Comparator.compare comparator)
    in
    Sequence.unfold_step ~init ~f:next
  ;;

  let to_sequence_decreasing comparator ~from_elt t =
    let next enum =
      match enum with
      | Null -> Sequence.Step.Done
      | This (Enum.More (k, t, e)) ->
        Sequence.Step.Yield { value = k; state = Enum.cons_right t e }
    in
    let init =
      match from_elt with
      | None -> Enum.of_set_right t
      | Some key -> Enum.starting_at_decreasing t key (Comparator.compare comparator)
    in
    Sequence.unfold_step ~init ~f:next
  ;;

  let to_sequence
    comparator
    ?(order = `Increasing)
    ?greater_or_equal_to
    ?less_or_equal_to
    t
    =
    let inclusive_bound side t bound =
      let compare_elt = Comparator.compare comparator in
      let l, maybe, r = split t bound ~compare_elt in
      let t = side (l, r) in
      match maybe with
      | None -> t
      | Some elt -> add t elt ~compare_elt
    in
    match order with
    | `Increasing ->
      let t = Option.fold less_or_equal_to ~init:t ~f:(inclusive_bound fst) in
      to_sequence_increasing comparator ~from_elt:greater_or_equal_to t
    | `Decreasing ->
      let t = Option.fold greater_or_equal_to ~init:t ~f:(inclusive_bound snd) in
      to_sequence_decreasing comparator ~from_elt:less_or_equal_to t
  ;;

  let rec find_first_satisfying t ~f =
    match t with
    | Empty -> None
    | Leaf { elt = v } -> if f v then Some v else None
    | Node { left = l; elt = v; right = r; weight = _ } ->
      if f v
      then (
        match find_first_satisfying l ~f with
        | None -> Some v
        | Some _ as x -> x)
      else find_first_satisfying r ~f
  ;;

  let rec find_last_satisfying t ~f =
    match t with
    | Empty -> None
    | Leaf { elt = v } -> if f v then Some v else None
    | Node { left = l; elt = v; right = r; weight = _ } ->
      if f v
      then (
        match find_last_satisfying r ~f with
        | None -> Some v
        | Some _ as x -> x)
      else find_last_satisfying l ~f
  ;;

  let binary_search t ~compare how v =
    match how with
    | `Last_strictly_less_than ->
      find_last_satisfying t ~f:(fun x -> compare x v < 0) [@nontail]
    | `Last_less_than_or_equal_to ->
      find_last_satisfying t ~f:(fun x -> compare x v <= 0) [@nontail]
    | `First_equal_to ->
      (match find_first_satisfying t ~f:(fun x -> compare x v >= 0) with
       | Some x as elt when compare x v = 0 -> elt
       | None | Some _ -> None)
    | `Last_equal_to ->
      (match find_last_satisfying t ~f:(fun x -> compare x v <= 0) with
       | Some x as elt when compare x v = 0 -> elt
       | None | Some _ -> None)
    | `First_greater_than_or_equal_to ->
      find_first_satisfying t ~f:(fun x -> compare x v >= 0) [@nontail]
    | `First_strictly_greater_than ->
      find_first_satisfying t ~f:(fun x -> compare x v > 0) [@nontail]
  ;;

  let binary_search_segmented t ~segment_of how =
    let is_left x =
      match segment_of x with
      | `Left -> true
      | `Right -> false
    in
    let is_right x = not (is_left x) in
    match how with
    | `Last_on_left -> find_last_satisfying t ~f:is_left [@nontail]
    | `First_on_right -> find_first_satisfying t ~f:is_right [@nontail]
  ;;

  let merge_to_sequence
    comparator
    ?(order = `Increasing)
    ?greater_or_equal_to
    ?less_or_equal_to
    t
    t'
    =
    Sequence.merge_with_duplicates
      (to_sequence comparator ~order ?greater_or_equal_to ?less_or_equal_to t)
      (to_sequence comparator ~order ?greater_or_equal_to ?less_or_equal_to t')
      ~compare:
        (match order with
         | `Increasing -> Comparator.compare comparator
         | `Decreasing -> Fn.flip (Comparator.compare comparator))
  ;;

  let compare compare_elt s1 s2 =
    Enum.compare compare_elt (Enum.of_set s1) (Enum.of_set s2)
  ;;

  let iter2 s1 s2 ~compare_elt ~f =
    Enum.iter2 compare_elt (Enum.of_set s1) (Enum.of_set s2) ~f
  ;;

  let equal s1 s2 ~compare_elt = compare compare_elt s1 s2 = 0

  let is_subset s1 ~of_:s2 ~compare_elt =
    let rec is_subset s1 ~of_:s2 =
      match s1, s2 with
      | Empty, _ -> true
      | _, Empty -> false
      | Leaf { elt = v1 }, t2 -> mem t2 v1 ~compare_elt
      | Node { left = l1; elt = v1; right = r1; weight = _ }, Leaf { elt = v2 } ->
        (match l1, r1 with
         | Empty, Empty ->
           (* This case shouldn't occur in practice because we should have constructed a
              Leaf [{elt=rather}] than a Node with two Empty subtrees *)
           compare_elt v1 v2 = 0
         | _, _ -> false)
      | ( Node { left = l1; elt = v1; right = r1; weight = _ }
        , (Node { left = l2; elt = v2; right = r2; weight = _ } as t2) ) ->
        let c = compare_elt v1 v2 in
        if c = 0
        then
          phys_equal s1 s2 || (is_subset l1 ~of_:l2 && is_subset r1 ~of_:r2)
          (* Note that weight doesn't matter here. *)
        else if c < 0
        then
          is_subset (Node { left = l1; elt = v1; right = Empty; weight = 0 }) ~of_:l2
          && is_subset r1 ~of_:t2
        else
          is_subset (Node { left = Empty; elt = v1; right = r1; weight = 0 }) ~of_:r2
          && is_subset l1 ~of_:t2
    in
    is_subset s1 ~of_:s2
  ;;

  let rec are_disjoint s1 s2 ~compare_elt =
    match s1, s2 with
    | Empty, _ | _, Empty -> true
    | Leaf { elt }, other_set | other_set, Leaf { elt } ->
      not (mem other_set elt ~compare_elt)
    | Node { left = l1; elt = v1; right = r1; weight = _ }, t2 ->
      if phys_equal s1 s2
      then false
      else (
        match split t2 v1 ~compare_elt with
        | l2, None, r2 ->
          are_disjoint l1 l2 ~compare_elt && are_disjoint r1 r2 ~compare_elt
        | _, Some _, _ -> false)
  ;;

  let iter t ~f =
    let rec iter = function
      | Empty -> ()
      | Leaf { elt = v } -> f v
      | Node { left = l; elt = v; right = r; weight = _ } ->
        iter l;
        f v;
        iter r
    in
    iter t [@nontail]
  ;;

  let symmetric_diff = Enum.symmetric_diff

  let rec fold s ~init:accu ~f =
    match s with
    | Empty -> accu
    | Leaf { elt = v } -> f accu v
    | Node { left = l; elt = v; right = r; weight = _ } ->
      fold ~f r ~init:(f (fold ~f l ~init:accu) v)
  ;;

  let hash_fold_t_ignoring_structure hash_fold_elem state t =
    fold t ~init:(hash_fold_int state (length t)) ~f:hash_fold_elem
  ;;

  let count t ~f = Container.count ~fold t ~f
  let sum m t ~f = Container.sum ~fold m t ~f

  let%template[@mode m = (global, local)] fold_right
    (s @ m)
    ~init
    ~(f : (_ @ m -> _ @ m -> _ @ m) @ local)
    : _ @ m
    =
    (let rec loop accu t =
       match[@exclave_if_local m ~reasons:[ May_return_local ]] t with
       | Empty -> accu
       | Leaf { elt = v } -> f v accu
       | Node { left = l; elt = v; right = r; weight = _ } ->
         let accu = loop accu r in
         let accu = f v accu in
         loop accu l
     in
     loop init s [@nontail])
    [@exclave_if_local m ~reasons:[ May_return_local ]]
  ;;

  let rec for_all t ~f:p =
    match t with
    | Empty -> true
    | Leaf { elt = v } -> p v
    | Node { left = l; elt = v; right = r; weight = _ } ->
      p v && for_all ~f:p l && for_all ~f:p r
  ;;

  let rec exists t ~f:p =
    match t with
    | Empty -> false
    | Leaf { elt = v } -> p v
    | Node { left = l; elt = v; right = r; weight = _ } ->
      p v || exists ~f:p l || exists ~f:p r
  ;;

  let filter s ~f:p =
    let rec filt = function
      | Empty -> Empty
      | Leaf { elt = v } as t -> if p v then t else Empty
      | Node { left = l; elt = v; right = r; weight = _ } as t ->
        let l' = filt l in
        let keep_v = p v in
        let r' = filt r in
        if keep_v && phys_equal l l' && phys_equal r r'
        then t
        else if keep_v
        then join l' v r'
        else concat l' r'
    in
    filt s [@nontail]
  ;;

  let filter_map s ~f:p ~compare_elt =
    let rec filt accu = function
      | Empty -> accu
      | Leaf { elt = v } ->
        (match p v with
         | None -> accu
         | Some v -> add accu v ~compare_elt)
      | Node { left = l; elt = v; right = r; weight = _ } ->
        filt
          (filt
             (match p v with
              | None -> accu
              | Some v -> add accu v ~compare_elt)
             l)
          r
    in
    filt Empty s [@nontail]
  ;;

  let partition_tf s ~f:p =
    let rec loop = function
      | Empty -> Empty, Empty
      | Leaf { elt = v } as t -> if p v then t, Empty else Empty, t
      | Node { left = l; elt = v; right = r; weight = _ } as t ->
        let l't, l'f = loop l in
        let keep_v_t = p v in
        let r't, r'f = loop r in
        let mk keep_v l' r' =
          if keep_v && phys_equal l l' && phys_equal r r'
          then t
          else if keep_v
          then join l' v r'
          else concat l' r'
        in
        mk keep_v_t l't r't, mk (not keep_v_t) l'f r'f
    in
    loop s [@nontail]
  ;;

  let rec elements_aux accu = function
    | Empty -> accu
    | Leaf { elt = v } -> v :: accu
    | Node { left = l; elt = v; right = r; weight = _ } ->
      elements_aux (v :: elements_aux accu r) l
  ;;

  let elements s = elements_aux [] s

  let call_with_choose ~none ~some = function
    | Empty -> none ()
    | Leaf { elt = v } -> some v
    | Node { left = _; elt = v; right = _; weight = _ } -> some v
  ;;

  let raise_choose_exn () = Error.raise_s (Atom "Set.choose_exn: empty set")
  let choose t = call_with_choose t ~none:return_none ~some:Option.return
  let choose_exn t = call_with_choose t ~none:raise_choose_exn ~some:Fn.id

  let of_list lst ~compare_elt =
    List.fold lst ~init:Empty ~f:(fun t x -> add t x ~compare_elt)
  ;;

  let of_sequence sequence ~compare_elt =
    Sequence.fold sequence ~init:Empty ~f:(fun t x -> add t x ~compare_elt)
  ;;

  let to_list s = elements s

  let of_array a ~compare_elt =
    Array.fold a ~init:Empty ~f:(fun t x -> add t x ~compare_elt)
  ;;

  (* faster but equivalent to [Array.of_list (to_list t)] *)
  let to_array = function
    | Empty -> [||]
    | Leaf { elt = v } -> [| v |]
    | Node { left = l; elt = v; right = r; weight = w } ->
      let res = Array.create ~len:(w - 1) v in
      let pos_ref = ref 0 in
      let rec loop = function
        (* Invariant: on entry and on exit to [loop], !pos_ref is the next available cell
           in the array. *)
        | Empty -> ()
        | Leaf { elt = v } ->
          res.(!pos_ref) <- v;
          incr pos_ref
        | Node { left = l; elt = v; right = r; weight = _ } ->
          loop l;
          res.(!pos_ref) <- v;
          incr pos_ref;
          loop r
      in
      loop l;
      (* res.(!pos_ref) is already initialized (by Array.create ~len:above). *)
      incr pos_ref;
      loop r;
      res
  ;;

  let map t ~f ~compare_elt =
    fold t ~init:Empty ~f:(fun t x -> add t (f x) ~compare_elt) [@nontail]
  ;;

  let group_by set ~equiv =
    let rec loop set equiv_classes =
      if is_empty set
      then equiv_classes
      else (
        let x = choose_exn set in
        let equiv_x, not_equiv_x =
          partition_tf set ~f:(fun elt -> phys_equal x elt || equiv x elt)
        in
        loop not_equiv_x (equiv_x :: equiv_classes))
    in
    loop set [] [@nontail]
  ;;

  let rec find t ~f =
    match t with
    | Empty -> None
    | Leaf { elt = v } -> if f v then Some v else None
    | Node { left = l; elt = v; right = r; weight = _ } ->
      if f v
      then Some v
      else (
        match find l ~f with
        | None -> find r ~f
        | Some _ as r -> r)
  ;;

  let rec find_map t ~f =
    match t with
    | Empty -> None
    | Leaf { elt = v } -> f v
    | Node { left = l; elt = v; right = r; weight = _ } ->
      (match f v with
       | Some _ as r -> r
       | None ->
         (match find_map l ~f with
          | None -> find_map r ~f
          | Some _ as r -> r))
  ;;

  let find_exn t ~f =
    match find t ~f with
    | None -> Error.raise_s (Atom "Set.find_exn: failed to find a matching element")
    | Some e -> e
  ;;

  let rank t elt ~compare_elt =
    let rec local_ loop t acc =
      match t with
      | Empty -> None
      | Leaf { elt = elt' } -> if compare_elt elt' elt = 0 then Some acc else None
      | Node { left = l; elt = elt'; right = r; weight = _ } ->
        let c = compare_elt elt' elt in
        if c = 0
        then Some (acc + length l)
        else if c > 0
        then loop l acc
        else loop r (acc + length l + 1)
    in
    loop t 0 [@nontail]
  ;;

  let rec nth t i =
    match t with
    | Empty -> None
    | Leaf { elt = v } -> if i = 0 then Some v else None
    | Node { left = l; elt = v; right = r; weight = w } ->
      if i >= w - 1
      then None
      else (
        let l_size = length l in
        let c = Poly.compare i l_size in
        if c < 0 then nth l i else if c = 0 then Some v else nth r (i - l_size - 1))
  ;;

  let t_of_sexp_direct a_of_sexp sexp ~compare_elt =
    match sexp with
    | Sexp.List lst ->
      let elt_lst = List.map lst ~f:a_of_sexp in
      let set = of_list elt_lst ~compare_elt in
      if length set = List.length lst
      then set
      else (
        let set = ref Empty in
        List.iter2_exn lst elt_lst ~f:(fun el_sexp el ->
          if mem !set el ~compare_elt
          then of_sexp_error "Set.t_of_sexp: duplicate element in set" el_sexp
          else set := add !set el ~compare_elt);
        assert false)
    | sexp -> of_sexp_error "Set.t_of_sexp: list needed" sexp
  ;;

  let%template[@alloc a @ m = (heap_global, stack_local)] sexp_of_t
    (type a)
    (sexp_of_a : a @ m -> Sexp.t @ m)
    (t @ m)
    : Sexp.t @ m
    =
    Sexp.List
      ((fold_right [@mode m]) t ~init:[] ~f:(fun el acc ->
         sexp_of_a el :: acc [@exclave_if_stack a]))
    [@exclave_if_stack a]
  ;;

  module Named = struct
    let is_subset
      (subset : _ Named.t)
      ~of_:(superset : _ Named.t)
      ~sexp_of_elt
      ~compare_elt
      =
      let invalid_elements = diff subset.set superset.set ~compare_elt in
      if is_empty invalid_elements
      then Ok ()
      else (
        let invalid_elements_sexp = sexp_of_t sexp_of_elt invalid_elements in
        Or_error.error_s
          (Sexp.message
             (subset.name ^ " is not a subset of " ^ superset.name)
             [ "invalid_elements", invalid_elements_sexp ]))
    ;;

    let equal s1 s2 ~sexp_of_elt ~compare_elt =
      Or_error.combine_errors_unit
        [ is_subset s1 ~of_:s2 ~sexp_of_elt ~compare_elt
        ; is_subset s2 ~of_:s1 ~sexp_of_elt ~compare_elt
        ]
    ;;
  end
end

type ('a, 'comparator) t =
  { (* [comparator] is the first field so that polymorphic equality fails on a map due to
       the functional value in the comparator. Note that this does not affect polymorphic
       [compare]: that still produces nonsense. *)
    global_ comparator : ('a, 'comparator) Comparator.t
  ; tree : ('a, 'comparator) Tree0.t
  }

let globalize0 (local_ { comparator; tree }) =
  { comparator; tree = Tree0.globalize0 tree }
;;

let globalize _ _ t = globalize0 t
let like { tree = _; comparator } tree = { tree; comparator }

let like_maybe_no_op ({ tree = old_tree; comparator } as old_t) tree =
  if phys_equal old_tree tree then old_t else { tree; comparator }
;;

let compare_elt t = Comparator.compare t.comparator

module Accessors = struct
  let comparator t = t.comparator
  let comparator_s t = Comparator.to_module t.comparator
  let invariants t = Tree0.invariants t.tree ~compare_elt:(compare_elt t)
  let length t = Tree0.length t.tree
  let is_empty t = Tree0.is_empty t.tree
  let elements t = Tree0.elements t.tree
  let min_elt t = Tree0.min_elt t.tree
  let min_elt_exn t = Tree0.min_elt_exn t.tree
  let max_elt t = Tree0.max_elt t.tree
  let max_elt_exn t = Tree0.max_elt_exn t.tree
  let choose t = Tree0.choose t.tree
  let choose_exn t = Tree0.choose_exn t.tree
  let to_list t = Tree0.to_list t.tree
  let to_array t = Tree0.to_array t.tree
  let fold t ~init ~f = Tree0.fold t.tree ~init ~f
  let fold_until t ~init ~f ~finish = Tree0.fold_until t.tree ~init ~f ~finish
  let fold_right t ~init ~f = Tree0.fold_right t.tree ~init ~f
  let fold_result t ~init ~f = Container.fold_result ~fold_until ~init ~f t
  let iter t ~f = Tree0.iter t.tree ~f
  let iter_until t ~f ~finish = Container.iter_until ~fold_until ~f ~finish t
  let iter2 a b ~f = Tree0.iter2 a.tree b.tree ~f ~compare_elt:(compare_elt a)
  let exists t ~f = Tree0.exists t.tree ~f
  let for_all t ~f = Tree0.for_all t.tree ~f
  let count t ~f = Tree0.count t.tree ~f
  let sum m t ~f = Tree0.sum m t.tree ~f
  let find t ~f = Tree0.find t.tree ~f
  let find_exn t ~f = Tree0.find_exn t.tree ~f
  let find_map t ~f = Tree0.find_map t.tree ~f
  let mem t a = Tree0.mem t.tree a ~compare_elt:(compare_elt t)
  let filter t ~f = like_maybe_no_op t (Tree0.filter t.tree ~f)
  let add t a = like t (Tree0.add t.tree a ~compare_elt:(compare_elt t))
  let remove t a = like t (Tree0.remove t.tree a ~compare_elt:(compare_elt t))
  let union t1 t2 = like t1 (Tree0.union t1.tree t2.tree ~compare_elt:(compare_elt t1))
  let inter t1 t2 = like t1 (Tree0.inter t1.tree t2.tree ~compare_elt:(compare_elt t1))
  let diff t1 t2 = like t1 (Tree0.diff t1.tree t2.tree ~compare_elt:(compare_elt t1))

  let symmetric_diff t1 t2 =
    Tree0.symmetric_diff t1.tree t2.tree ~compare_elt:(compare_elt t1)
  ;;

  let%template[@mode m = (local, global)] compare_direct t1 t2 =
    Tree0.compare (compare_elt t1) t1.tree t2.tree
  ;;

  let%template[@mode m = (local, global)] equal t1 t2 =
    Tree0.equal t1.tree t2.tree ~compare_elt:(compare_elt t1)
  ;;

  let is_subset t ~of_ = Tree0.is_subset t.tree ~of_:of_.tree ~compare_elt:(compare_elt t)

  let are_disjoint t1 t2 =
    Tree0.are_disjoint t1.tree t2.tree ~compare_elt:(compare_elt t1)
  ;;

  module Named = struct
    let to_named_tree (named : (_, _) t Named.t) = { named with set = named.set.tree }

    let is_subset subset ~of_:superset =
      Tree0.Named.is_subset
        (to_named_tree subset)
        ~of_:(to_named_tree superset)
        ~compare_elt:(compare_elt subset.set)
        ~sexp_of_elt:(Comparator.sexp_of_t subset.set.comparator)
    ;;

    let equal t1 t2 =
      Or_error.combine_errors_unit [ is_subset t1 ~of_:t2; is_subset t2 ~of_:t1 ]
    ;;

    include Named
  end

  let partition_tf t ~f =
    let tree_t, tree_f = Tree0.partition_tf t.tree ~f in
    like_maybe_no_op t tree_t, like_maybe_no_op t tree_f
  ;;

  let split t a =
    let tree1, b, tree2 = Tree0.split t.tree a ~compare_elt:(compare_elt t) in
    like t tree1, b, like t tree2
  ;;

  let split_le_gt t a =
    let tree1, tree2 = Tree0.split_le_gt t.tree a ~compare_elt:(compare_elt t) in
    like t tree1, like t tree2
  ;;

  let split_lt_ge t a =
    let tree1, tree2 = Tree0.split_lt_ge t.tree a ~compare_elt:(compare_elt t) in
    like t tree1, like t tree2
  ;;

  let group_by t ~equiv = List.map (Tree0.group_by t.tree ~equiv) ~f:(like t)
  let nth t i = Tree0.nth t.tree i
  let rank t v = Tree0.rank t.tree v ~compare_elt:(compare_elt t)
  let remove_index t i = like t (Tree0.remove_index t.tree i ~compare_elt:(compare_elt t))

  let%template[@alloc a = (heap, stack)] sexp_of_t sexp_of_a _ t =
    (Tree0.sexp_of_t [@alloc a]) sexp_of_a t.tree [@exclave_if_stack a]
  ;;

  let to_sequence ?order ?greater_or_equal_to ?less_or_equal_to t =
    Tree0.to_sequence t.comparator ?order ?greater_or_equal_to ?less_or_equal_to t.tree
  ;;

  let binary_search t ~compare how v = Tree0.binary_search t.tree ~compare how v

  let binary_search_segmented t ~segment_of how =
    Tree0.binary_search_segmented t.tree ~segment_of how
  ;;

  let merge_to_sequence ?order ?greater_or_equal_to ?less_or_equal_to t t' =
    Tree0.merge_to_sequence
      t.comparator
      ?order
      ?greater_or_equal_to
      ?less_or_equal_to
      t.tree
      t'.tree
  ;;

  let hash_fold_direct hash_fold_key state t =
    Tree0.hash_fold_t_ignoring_structure hash_fold_key state t.tree
  ;;
end

include Accessors

let compare _ _ t1 t2 = compare_direct t1 t2

let%template[@mode local] compare _ _ t1 t2 =
  compare_direct (globalize0 t1) (globalize0 t2)
;;

module Tree = struct
  type weight = int

  type ('a, 'cmp) t = ('a, 'cmp) Tree0.t =
    | Empty
    | Leaf of { global_ elt : 'a }
    | Node of
        { global_ left : ('a, 'cmp) t
        ; global_ elt : 'a
        ; global_ right : ('a, 'cmp) t
        ; weight : int
        }

  let globalize0 = Tree0.globalize0
  let globalize _ _ t = globalize0 t
  let ce comparator = Comparator.compare comparator

  let t_of_sexp_direct ~comparator a_of_sexp sexp =
    Tree0.t_of_sexp_direct ~compare_elt:(ce comparator) a_of_sexp sexp
  ;;

  let empty_without_value_restriction = Tree0.Empty
  let empty ~comparator:_ = Tree0.Empty
  let singleton ~comparator:_ e = Tree0.singleton e
  let length t = Tree0.length t
  let invariants ~comparator t = Tree0.invariants t ~compare_elt:(ce comparator)
  let is_empty t = Tree0.is_empty t
  let elements t = Tree0.elements t
  let min_elt t = Tree0.min_elt t
  let min_elt_exn t = Tree0.min_elt_exn t
  let max_elt t = Tree0.max_elt t
  let max_elt_exn t = Tree0.max_elt_exn t
  let choose t = Tree0.choose t
  let choose_exn t = Tree0.choose_exn t
  let to_list t = Tree0.to_list t
  let to_array t = Tree0.to_array t
  let iter t ~f = Tree0.iter t ~f
  let exists t ~f = Tree0.exists t ~f
  let for_all t ~f = Tree0.for_all t ~f
  let count t ~f = Tree0.count t ~f
  let sum m t ~f = Tree0.sum m t ~f
  let find t ~f = Tree0.find t ~f
  let find_exn t ~f = Tree0.find_exn t ~f
  let find_map t ~f = Tree0.find_map t ~f
  let fold t ~init ~f = Tree0.fold t ~init ~f
  let fold_until t ~init ~f ~finish = Tree0.fold_until t ~init ~f ~finish
  let fold_right t ~init ~f = Tree0.fold_right t ~init ~f
  let map ~comparator t ~f = Tree0.map t ~f ~compare_elt:(ce comparator)
  let filter t ~f = Tree0.filter t ~f
  let filter_map ~comparator t ~f = Tree0.filter_map t ~f ~compare_elt:(ce comparator)
  let partition_tf t ~f = Tree0.partition_tf t ~f
  let iter2 ~comparator a b ~f = Tree0.iter2 a b ~f ~compare_elt:(ce comparator)
  let mem ~comparator t a = Tree0.mem t a ~compare_elt:(ce comparator)
  let add ~comparator t a = Tree0.add t a ~compare_elt:(ce comparator)
  let remove ~comparator t a = Tree0.remove t a ~compare_elt:(ce comparator)
  let union ~comparator t1 t2 = Tree0.union t1 t2 ~compare_elt:(ce comparator)
  let inter ~comparator t1 t2 = Tree0.inter t1 t2 ~compare_elt:(ce comparator)
  let diff ~comparator t1 t2 = Tree0.diff t1 t2 ~compare_elt:(ce comparator)

  let symmetric_diff ~comparator t1 t2 =
    Tree0.symmetric_diff t1 t2 ~compare_elt:(ce comparator)
  ;;

  let%template[@mode m = (local, global)] compare_direct ~comparator t1 t2 =
    Tree0.compare (ce comparator) t1 t2
  ;;

  let%template[@mode m = (local, global)] equal ~comparator t1 t2 =
    Tree0.equal t1 t2 ~compare_elt:(ce comparator)
  ;;

  let is_subset ~comparator t ~of_ = Tree0.is_subset t ~of_ ~compare_elt:(ce comparator)

  let are_disjoint ~comparator t1 t2 =
    Tree0.are_disjoint t1 t2 ~compare_elt:(ce comparator)
  ;;

  let of_list ~comparator l = Tree0.of_list l ~compare_elt:(ce comparator)
  let of_sequence ~comparator s = Tree0.of_sequence s ~compare_elt:(ce comparator)
  let of_array ~comparator a = Tree0.of_array a ~compare_elt:(ce comparator)

  let of_sorted_array_unchecked ~comparator a =
    Tree0.of_sorted_array_unchecked a ~compare_elt:(ce comparator)
  ;;

  let of_increasing_iterator_unchecked ~comparator:_ ~len ~f =
    Tree0.of_increasing_iterator_unchecked ~len ~f
  ;;

  let of_sorted_array ~comparator a = Tree0.of_sorted_array a ~compare_elt:(ce comparator)
  let union_list ~comparator l = Tree0.union_list l ~to_tree:Fn.id ~comparator
  let group_by t ~equiv = Tree0.group_by t ~equiv
  let split ~comparator t a = Tree0.split t a ~compare_elt:(ce comparator)
  let split_le_gt ~comparator t a = Tree0.split_le_gt t a ~compare_elt:(ce comparator)
  let split_lt_ge ~comparator t a = Tree0.split_lt_ge t a ~compare_elt:(ce comparator)
  let nth t i = Tree0.nth t i
  let rank ~comparator t v = Tree0.rank t v ~compare_elt:(ce comparator)
  let remove_index ~comparator t i = Tree0.remove_index t i ~compare_elt:(ce comparator)
  let sexp_of_t sexp_of_a _ t = Tree0.sexp_of_t sexp_of_a t
  let to_tree t = t
  let of_tree ~comparator:_ t = t

  let to_sequence ~comparator ?order ?greater_or_equal_to ?less_or_equal_to t =
    Tree0.to_sequence comparator ?order ?greater_or_equal_to ?less_or_equal_to t
  ;;

  let binary_search ~comparator:_ t ~compare how v = Tree0.binary_search t ~compare how v

  let binary_search_segmented ~comparator:_ t ~segment_of how =
    Tree0.binary_search_segmented t ~segment_of how
  ;;

  let merge_to_sequence ~comparator ?order ?greater_or_equal_to ?less_or_equal_to t t' =
    Tree0.merge_to_sequence comparator ?order ?greater_or_equal_to ?less_or_equal_to t t'
  ;;

  let fold_result t ~init ~f = Container.fold_result ~fold_until ~init ~f t
  let iter_until t ~f ~finish = Container.iter_until ~fold_until ~f ~finish t

  module Named = struct
    include Tree0.Named

    let is_subset ~comparator t1 ~of_:t2 =
      Tree0.Named.is_subset
        t1
        ~of_:t2
        ~compare_elt:(ce comparator)
        ~sexp_of_elt:(Comparator.sexp_of_t comparator)
    ;;

    let equal ~comparator t1 t2 =
      Tree0.Named.equal
        t1
        t2
        ~compare_elt:(ce comparator)
        ~sexp_of_elt:(Comparator.sexp_of_t comparator)
    ;;
  end

  module Expert = struct
    type ('a, 'cmp) t = ('a, 'cmp) Tree0.t =
      | Empty
      | Leaf of { global_ elt : 'a }
      | Node of
          { global_ left : ('a, 'cmp) t
          ; global_ elt : 'a
          ; global_ right : ('a, 'cmp) t
          ; weight : int
          }
    [@@deriving sexp_of]

    let balance_invariants t = Tree0.balance_invariants t

    let order_invariants ~comparator t =
      Tree0.order_invariants t ~compare_elt:(Comparator.compare comparator)
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
    let concat_and_rebalance_at_most_once_unchecked = Tree0.merge
    let concat_and_rebalance_unchecked = Tree0.concat
    let singleton = Tree0.singleton
    let empty = Empty
    let length_of_weight = Int.pred
  end
end

module Using_comparator = struct
  type nonrec ('elt, 'cmp) t = ('elt, 'cmp) t

  include Accessors

  let to_tree t = t.tree
  let of_tree ~comparator tree = { comparator; tree }

  let t_of_sexp_direct ~comparator a_of_sexp sexp =
    of_tree
      ~comparator
      (Tree0.t_of_sexp_direct ~compare_elt:(Comparator.compare comparator) a_of_sexp sexp)
  ;;

  let%template empty ~(comparator @ p) = { comparator; tree = Tree0.Empty }
  [@@mode p = (portable, nonportable)]
  ;;

  module%template.portable Empty_without_value_restriction (Elt : Comparator.S1) = struct
    let empty = { comparator = Elt.comparator; tree = Tree0.Empty }
  end

  let singleton ~comparator e = { comparator; tree = Tree0.singleton e }

  let union_list ~comparator l =
    of_tree ~comparator (Tree0.union_list ~comparator ~to_tree l)
  ;;

  let of_sorted_array_unchecked ~comparator array =
    let tree =
      Tree0.of_sorted_array_unchecked array ~compare_elt:(Comparator.compare comparator)
    in
    { comparator; tree }
  ;;

  let of_increasing_iterator_unchecked ~comparator ~len ~f =
    of_tree ~comparator (Tree0.of_increasing_iterator_unchecked ~len ~f)
  ;;

  let of_sorted_array ~comparator array =
    Or_error.Monad_infix.(
      Tree0.of_sorted_array array ~compare_elt:(Comparator.compare comparator)
      >>| fun tree -> { comparator; tree })
  ;;

  let of_list ~comparator l =
    { comparator; tree = Tree0.of_list l ~compare_elt:(Comparator.compare comparator) }
  ;;

  let of_sequence ~comparator s =
    { comparator
    ; tree = Tree0.of_sequence s ~compare_elt:(Comparator.compare comparator)
    }
  ;;

  let of_array ~comparator a =
    { comparator; tree = Tree0.of_array a ~compare_elt:(Comparator.compare comparator) }
  ;;

  let map ~comparator t ~f =
    { comparator
    ; tree = Tree0.map t.tree ~f ~compare_elt:(Comparator.compare comparator)
    }
  ;;

  let filter_map ~comparator t ~f =
    { comparator
    ; tree = Tree0.filter_map t.tree ~f ~compare_elt:(Comparator.compare comparator)
    }
  ;;

  module Tree = Tree
end

let to_comparator = Comparator.of_module

let%template empty (type cw : value mod p) (m : (_, cw) Comparator.Module.t) =
  (Using_comparator.empty [@mode p]) ~comparator:(to_comparator m)
[@@mode p = (nonportable, portable)]
;;

let singleton m a = Using_comparator.singleton ~comparator:(to_comparator m) a
let union_list m a = Using_comparator.union_list ~comparator:(to_comparator m) a

let of_sorted_array_unchecked m a =
  Using_comparator.of_sorted_array_unchecked ~comparator:(to_comparator m) a
;;

let of_increasing_iterator_unchecked m ~len ~f =
  Using_comparator.of_increasing_iterator_unchecked ~comparator:(to_comparator m) ~len ~f
;;

let of_sorted_array m a = Using_comparator.of_sorted_array ~comparator:(to_comparator m) a
let of_list m a = Using_comparator.of_list ~comparator:(to_comparator m) a
let of_sequence m a = Using_comparator.of_sequence ~comparator:(to_comparator m) a
let of_array m a = Using_comparator.of_array ~comparator:(to_comparator m) a
let map m a ~f = Using_comparator.map ~comparator:(to_comparator m) a ~f
let filter_map m a ~f = Using_comparator.filter_map ~comparator:(to_comparator m) a ~f
let to_tree = Using_comparator.to_tree
let of_tree m t = Using_comparator.of_tree ~comparator:(to_comparator m) t

module M (Elt : sig
    type t
    type comparator_witness
  end) =
struct
  type nonrec t = (Elt.t, Elt.comparator_witness) t
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
module type Globalize_m = sig end
module type Hash_fold_m = Hasher.S

let%template[@alloc a @ m = (heap_global, stack_local)] sexp_of_m__t
  (type elt)
  (module Elt : Sexp_of_m with type t = elt[@alloc a])
  t
  =
  (sexp_of_t [@alloc a])
    (Elt.sexp_of_t [@alloc a])
    (fun _ -> Sexp.Atom "_")
    t [@exclave_if_stack a]
;;

let m__t_of_sexp
  (type elt cmp)
  (module Elt : M_of_sexp with type t = elt and type comparator_witness = cmp)
  sexp
  =
  Using_comparator.t_of_sexp_direct ~comparator:Elt.comparator Elt.t_of_sexp sexp
;;

let m__t_sexp_grammar (type elt) (module Elt : M_sexp_grammar with type t = elt)
  : (elt, _) t Sexplib0.Sexp_grammar.t
  =
  Sexplib0.Sexp_grammar.coerce (list_sexp_grammar Elt.t_sexp_grammar)
;;

let compare_m__t (module _ : Compare_m) t1 t2 = compare_direct t1 t2
let equal_m__t (module _ : Equal_m) t1 t2 = equal t1 t2

let%template compare_m__t__local (module _ : Compare_m) t1 t2 =
  (compare_direct [@mode local]) t1 t2
;;

let%template equal_m__t__local (module _ : Equal_m) t1 t2 = (equal [@mode local]) t1 t2
let globalize_m__t (module _ : Globalize_m) (local_ t) = globalize0 t

let hash_fold_m__t (type elt) (module Elt : Hash_fold_m with type t = elt) state =
  hash_fold_direct Elt.hash_fold_t state
;;

let hash_m__t folder t =
  let state = hash_fold_m__t folder (Hash.create ()) t in
  Hash.get_hash_value state
;;

module Poly = struct
  type comparator_witness = Comparator.Poly.comparator_witness
  type nonrec 'elt t = ('elt, comparator_witness) t

  include Accessors

  let comparator = Comparator.Poly.comparator

  include%template
    Using_comparator.Empty_without_value_restriction [@modality portable] (Comparator.Poly)

  let singleton a = Using_comparator.singleton ~comparator a
  let union_list a = Using_comparator.union_list ~comparator a

  let of_sorted_array_unchecked a =
    Using_comparator.of_sorted_array_unchecked ~comparator a
  ;;

  let of_increasing_iterator_unchecked ~len ~f =
    Using_comparator.of_increasing_iterator_unchecked ~comparator ~len ~f
  ;;

  let of_sorted_array a = Using_comparator.of_sorted_array ~comparator a
  let of_list a = Using_comparator.of_list ~comparator a
  let of_sequence a = Using_comparator.of_sequence ~comparator a
  let of_array a = Using_comparator.of_array ~comparator a
  let map a ~f = Using_comparator.map ~comparator a ~f
  let filter_map a ~f = Using_comparator.filter_map ~comparator a ~f
  let of_tree tree = { comparator; tree }
  let to_tree t = t.tree
end

module Private = struct
  module Enum = Tree0.Enum
end
