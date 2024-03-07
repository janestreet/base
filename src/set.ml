(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Apache 2.0 license. See ../THIRD-PARTY.txt  *)
(*  for details.                                                       *)
(*                                                                     *)
(***********************************************************************)

(* Sets over ordered types *)

open! Import
include Set_intf

let with_return = With_return.with_return

module Tree0 = struct
  type 'a t =
    | Empty
    (* Leaf is the same as Node with empty children but uses less space. *)
    | Leaf of { elt : 'a }
    | Node of
        { left : 'a t
        ; elt : 'a
        ; right : 'a t
        ; height : int
        ; size : int
        }

  type 'a tree = 'a t

  (* Sets are represented by balanced binary trees (the heights of the children differ by
     at most 2. *)
  let[@inline always] height = function
    | Empty -> 0
    | Leaf { elt = _ } -> 1
    | Node { left = _; elt = _; right = _; height = h; size = _ } -> h
  ;;

  let[@inline always] length = function
    | Empty -> 0
    | Leaf { elt = _ } -> 1
    | Node { left = _; elt = _; right = _; height = _; size = s } -> s
  ;;

  let invariants =
    let in_range lower upper compare_elt v =
      (match lower with
       | None -> true
       | Some lower -> compare_elt lower v < 0)
      &&
      match upper with
      | None -> true
      | Some upper -> compare_elt v upper < 0
    in
    let rec loop lower upper compare_elt t =
      match t with
      | Empty -> true
      | Leaf { elt = v } -> in_range lower upper compare_elt v
      | Node { left = l; elt = v; right = r; height = h; size = n } ->
        let hl = height l
        and hr = height r in
        abs (hl - hr) <= 2
        && h = max hl hr + 1
        && n = length l + length r + 1
        && in_range lower upper compare_elt v
        && loop lower (Some v) compare_elt l
        && loop (Some v) upper compare_elt r
    in
    fun t ~compare_elt -> loop None None compare_elt t
  ;;

  let is_empty = function
    | Empty -> true
    | Leaf { elt = _ } | Node _ -> false
  ;;

  (* Creates a new node with left son l, value v and right son r.
     We must have all elements of l < v < all elements of r.
     l and r must be balanced and | height l - height r | <= 2. *)

  let[@inline always] create l v r =
    let hl = (height [@inlined]) l in
    let hr = (height [@inlined]) r in
    let h = if hl >= hr then hl + 1 else hr + 1 in
    if h = 1
    then Leaf { elt = v }
    else (
      let sl = (length [@inlined]) l in
      let sr = (length [@inlined]) r in
      Node { left = l; elt = v; right = r; height = h; size = sl + sr + 1 })
  ;;

  (* We must call [f] with increasing indexes, because the bin_prot reader in
     Core.Set needs it. *)
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

  (* Same as create, but performs one step of rebalancing if necessary.
     Assumes l and r balanced and | height l - height r | <= 3. *)

  let bal l v r =
    let hl = (height [@inlined]) l in
    let hr = (height [@inlined]) r in
    if hl > hr + 2
    then (
      match l with
      | Empty -> assert false
      | Leaf { elt = _ } -> assert false (* because h(l)>h(r)+2 and h(leaf)=1 *)
      | Node { left = ll; elt = lv; right = lr; height = _; size = _ } ->
        if height ll >= height lr
        then create ll lv (create lr v r)
        else (
          match lr with
          | Empty -> assert false
          | Leaf { elt = lrv } ->
            assert (is_empty ll);
            create (create ll lv Empty) lrv (create Empty v r)
          | Node { left = lrl; elt = lrv; right = lrr; height = _; size = _ } ->
            create (create ll lv lrl) lrv (create lrr v r)))
    else if hr > hl + 2
    then (
      match r with
      | Empty -> assert false
      | Leaf { elt = _ } -> assert false (* because h(r)>h(l)+2 and h(leaf)=1 *)
      | Node { left = rl; elt = rv; right = rr; height = _; size = _ } ->
        if height rr >= height rl
        then create (create l v rl) rv rr
        else (
          match rl with
          | Empty -> assert false
          | Leaf { elt = rlv } ->
            assert (is_empty rr);
            create (create l v Empty) rlv (create Empty rv rr)
          | Node { left = rll; elt = rlv; right = rlr; height = _; size = _ } ->
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
      | Node { left = l; elt = v; right = r; height = _; size = _ } ->
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
    | Leaf { elt = _ } -> Node { left = Empty; elt = x; right = t; height = 2; size = 2 }
    | Node { left = l; elt = v; right = r; height = _; size = _ } -> bal (add_min x l) v r
  ;;

  (* specialization of [add] that assumes that [x] is greater than all existing elements *)
  let rec add_max t x =
    match t with
    | Empty -> Leaf { elt = x }
    | Leaf { elt = _ } -> Node { left = t; elt = x; right = Empty; height = 2; size = 2 }
    | Node { left = l; elt = v; right = r; height = _; size = _ } -> bal l v (add_max r x)
  ;;

  (* Same as create and bal, but no assumptions are made on the relative heights of l and
     r. *)
  let rec join l v r =
    match l, r with
    | Empty, _ -> add_min v r
    | _, Empty -> add_max l v
    | Leaf { elt = lv }, _ -> add_min lv (add_min v r)
    | _, Leaf { elt = rv } -> add_max (add_max l v) rv
    | ( Node { left = ll; elt = lv; right = lr; height = lh; size = _ }
      , Node { left = rl; elt = rv; right = rr; height = rh; size = _ } ) ->
      if lh > rh + 2
      then bal ll lv (join lr v r)
      else if rh > lh + 2
      then bal (join l v rl) rv rr
      else create l v r
  ;;

  (* Smallest and greatest element of a set *)
  let rec min_elt = function
    | Empty -> None
    | Leaf { elt = v } | Node { left = Empty; elt = v; right = _; height = _; size = _ }
      -> Some v
    | Node { left = l; elt = _; right = _; height = _; size = _ } -> min_elt l
  ;;

  exception Set_min_elt_exn_of_empty_set [@@deriving_inline sexp]

  let () =
    Sexplib0.Sexp_conv.Exn_converter.add
      [%extension_constructor Set_min_elt_exn_of_empty_set]
      (function
      | Set_min_elt_exn_of_empty_set ->
        Sexplib0.Sexp.Atom "set.ml.Tree0.Set_min_elt_exn_of_empty_set"
      | _ -> assert false)
  ;;

  [@@@end]

  exception Set_max_elt_exn_of_empty_set [@@deriving_inline sexp]

  let () =
    Sexplib0.Sexp_conv.Exn_converter.add
      [%extension_constructor Set_max_elt_exn_of_empty_set]
      (function
      | Set_max_elt_exn_of_empty_set ->
        Sexplib0.Sexp.Atom "set.ml.Tree0.Set_max_elt_exn_of_empty_set"
      | _ -> assert false)
  ;;

  [@@@end]

  let min_elt_exn t =
    match min_elt t with
    | None -> raise Set_min_elt_exn_of_empty_set
    | Some v -> v
  ;;

  let fold_until t ~init ~f ~finish =
    let rec fold_until_helper ~f t acc =
      match t with
      | Empty -> Container.Continue_or_stop.Continue acc
      | Leaf { elt = value } -> f acc value [@nontail]
      | Node { left; elt = value; right; height = _; size = _ } ->
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

  let rec max_elt = function
    | Empty -> None
    | Leaf { elt = v } | Node { left = _; elt = v; right = Empty; height = _; size = _ }
      -> Some v
    | Node { left = _; elt = _; right = r; height = _; size = _ } -> max_elt r
  ;;

  let max_elt_exn t =
    match max_elt t with
    | None -> raise Set_max_elt_exn_of_empty_set
    | Some v -> v
  ;;

  (* Remove the smallest element of the given set *)

  let rec remove_min_elt = function
    | Empty -> invalid_arg "Set.remove_min_elt"
    | Leaf { elt = _ } -> Empty
    | Node { left = Empty; elt = _; right = r; height = _; size = _ } -> r
    | Node { left = l; elt = v; right = r; height = _; size = _ } ->
      bal (remove_min_elt l) v r
  ;;

  (* Merge two trees l and r into one.  All elements of l must precede the elements of r.
     Assume | height l - height r | <= 2. *)
  let merge t1 t2 =
    match t1, t2 with
    | Empty, t -> t
    | t, Empty -> t
    | _, _ -> bal t1 (min_elt_exn t2) (remove_min_elt t2)
  ;;

  (* Merge two trees l and r into one.  All elements of l must precede the elements of r.
     No assumption on the heights of l and r. *)
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
      | Node { left = l; elt = v; right = r; height = _; size = _ } ->
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
    | Node { left = l; elt = v; right = r; height = _; size = _ } ->
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
    | Node { left = l; elt = v; right = r; height = _; size = _ } ->
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

  let empty = Empty

  let rec mem t x ~compare_elt =
    match t with
    | Empty -> false
    | Leaf { elt = v } ->
      let c = compare_elt x v in
      c = 0
    | Node { left = l; elt = v; right = r; height = _; size = _ } ->
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
      | Node { left = l; elt = v; right = r; height = _; size = _ } ->
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
      | Node { left = l; elt = v; right = r; height = _; size = _ } ->
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
        | Leaf { elt = v1 }, _ ->
          union (Node { left = Empty; elt = v1; right = Empty; height = 1; size = 1 }) s2
        | _, Leaf { elt = v2 } ->
          union s1 (Node { left = Empty; elt = v2; right = Empty; height = 1; size = 1 })
        | ( Node { left = l1; elt = v1; right = r1; height = h1; size = _ }
          , Node { left = l2; elt = v2; right = r2; height = h2; size = _ } ) ->
          if h1 >= h2
          then
            if h2 = 1
            then add s1 v2 ~compare_elt
            else (
              let l2, _, r2 = split s2 v1 ~compare_elt in
              join (union l1 l2) v1 (union r1 r2))
          else if h1 = 1
          then add s2 v1 ~compare_elt
          else (
            let l1, _, r1 = split s1 v2 ~compare_elt in
            join (union l1 l2) v2 (union r1 r2)))
    in
    union s1 s2
  ;;

  let union_list ~comparator ~to_tree xs =
    let compare_elt = comparator.Comparator.compare in
    List.fold xs ~init:empty ~f:(fun ac x -> union ac (to_tree x) ~compare_elt)
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
        | Node { left = l1; elt = v1; right = r1; height = _; size = _ }, t2 ->
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
          diff (Node { left = Empty; elt = v1; right = Empty; height = 1; size = 1 }) t2
        | Node { left = l1; elt = v1; right = r1; height = _; size = _ }, t2 ->
          (match split t2 v1 ~compare_elt with
           | l2, None, r2 -> join (diff l1 l2) v1 (diff r1 r2)
           | l2, Some _, r2 -> concat (diff l1 l2) (diff r1 r2)))
    in
    diff s1 s2
  ;;

  module Enum = struct
    type increasing
    type decreasing

    type ('a, 'direction) t =
      | End
      | More of 'a * 'a tree * ('a, 'direction) t

    let rec cons s (e : (_, increasing) t) : (_, increasing) t =
      match s with
      | Empty -> e
      | Leaf { elt = v } -> More (v, Empty, e)
      | Node { left = l; elt = v; right = r; height = _; size = _ } ->
        cons l (More (v, r, e))
    ;;

    let rec cons_right s (e : (_, decreasing) t) : (_, decreasing) t =
      match s with
      | Empty -> e
      | Leaf { elt = v } -> More (v, Empty, e)
      | Node { left = l; elt = v; right = r; height = _; size = _ } ->
        cons_right r (More (v, l, e))
    ;;

    let of_set s : (_, increasing) t = cons s End
    let of_set_right s : (_, decreasing) t = cons_right s End

    let starting_at_increasing t key compare : (_, increasing) t =
      let rec loop t e =
        match t with
        | Empty -> e
        | Leaf { elt = v } ->
          loop (Node { left = Empty; elt = v; right = Empty; height = 1; size = 1 }) e
        | Node { left = _; elt = v; right = r; height = _; size = _ }
          when compare v key < 0 -> loop r e
        | Node { left = l; elt = v; right = r; height = _; size = _ } ->
          loop l (More (v, r, e))
      in
      loop t End
    ;;

    let starting_at_decreasing t key compare : (_, decreasing) t =
      let rec loop t e =
        match t with
        | Empty -> e
        | Leaf { elt = v } ->
          loop (Node { left = Empty; elt = v; right = Empty; height = 1; size = 1 }) e
        | Node { left = l; elt = v; right = _; height = _; size = _ }
          when compare v key > 0 -> loop l e
        | Node { left = l; elt = v; right = r; height = _; size = _ } ->
          loop r (More (v, l, e))
      in
      loop t End
    ;;

    let compare compare_elt e1 e2 =
      let rec loop e1 e2 =
        match e1, e2 with
        | End, End -> 0
        | End, _ -> -1
        | _, End -> 1
        | More (v1, r1, e1), More (v2, r2, e2) ->
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
      | End -> ()
      | More (a, tree, enum) ->
        f a;
        iter (cons tree enum) ~f
    ;;

    let iter2 compare_elt t1 t2 ~f =
      let rec loop t1 t2 =
        match t1, t2 with
        | End, End -> ()
        | End, _ -> iter t2 ~f:(fun a -> f (`Right a)) [@nontail]
        | _, End -> iter t1 ~f:(fun a -> f (`Left a)) [@nontail]
        | More (a1, tree1, enum1), More (a2, tree2, enum2) ->
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
        | End, End -> Done
        | End, More (elt, tree, enum) ->
          Yield { value = Second elt; state = End, cons tree enum }
        | More (elt, tree, enum), End ->
          Yield { value = First elt; state = cons tree enum, End }
        | (More (a1, tree1, enum1) as left), (More (a2, tree2, enum2) as right) ->
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
      | Enum.End -> Sequence.Step.Done
      | Enum.More (k, t, e) -> Sequence.Step.Yield { value = k; state = Enum.cons t e }
    in
    let init =
      match from_elt with
      | None -> Enum.of_set t
      | Some key -> Enum.starting_at_increasing t key comparator.Comparator.compare
    in
    Sequence.unfold_step ~init ~f:next
  ;;

  let to_sequence_decreasing comparator ~from_elt t =
    let next enum =
      match enum with
      | Enum.End -> Sequence.Step.Done
      | Enum.More (k, t, e) ->
        Sequence.Step.Yield { value = k; state = Enum.cons_right t e }
    in
    let init =
      match from_elt with
      | None -> Enum.of_set_right t
      | Some key -> Enum.starting_at_decreasing t key comparator.Comparator.compare
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
      let compare_elt = comparator.Comparator.compare in
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
    | Node { left = l; elt = v; right = r; height = _; size = _ } ->
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
    | Node { left = l; elt = v; right = r; height = _; size = _ } ->
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
         | `Increasing -> comparator.compare
         | `Decreasing -> Fn.flip comparator.compare)
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
      | Node { left = l1; elt = v1; right = r1; height = _; size = _ }, Leaf { elt = v2 }
        ->
        (match l1, r1 with
         | Empty, Empty ->
           (* This case shouldn't occur in practice because we should have constructed
              a Leaf {elt=rather} than a Node with two Empty subtrees *)
           compare_elt v1 v2 = 0
         | _, _ -> false)
      | ( Node { left = l1; elt = v1; right = r1; height = _; size = _ }
        , (Node { left = l2; elt = v2; right = r2; height = _; size = _ } as t2) ) ->
        let c = compare_elt v1 v2 in
        if c = 0
        then
          phys_equal s1 s2 || (is_subset l1 ~of_:l2 && is_subset r1 ~of_:r2)
          (* Note that height and size don't matter here. *)
        else if c < 0
        then
          is_subset
            (Node { left = l1; elt = v1; right = Empty; height = 0; size = 0 })
            ~of_:l2
          && is_subset r1 ~of_:t2
        else
          is_subset
            (Node { left = Empty; elt = v1; right = r1; height = 0; size = 0 })
            ~of_:r2
          && is_subset l1 ~of_:t2
    in
    is_subset s1 ~of_:s2
  ;;

  let rec are_disjoint s1 s2 ~compare_elt =
    match s1, s2 with
    | Empty, _ | _, Empty -> true
    | Leaf { elt }, other_set | other_set, Leaf { elt } ->
      not (mem other_set elt ~compare_elt)
    | Node { left = l1; elt = v1; right = r1; height = _; size = _ }, t2 ->
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
      | Node { left = l; elt = v; right = r; height = _; size = _ } ->
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
    | Node { left = l; elt = v; right = r; height = _; size = _ } ->
      fold ~f r ~init:(f (fold ~f l ~init:accu) v)
  ;;

  let hash_fold_t_ignoring_structure hash_fold_elem state t =
    fold t ~init:(hash_fold_int state (length t)) ~f:hash_fold_elem
  ;;

  let count t ~f = Container.count ~fold t ~f
  let sum m t ~f = Container.sum ~fold m t ~f

  let rec fold_right s ~init:accu ~f =
    match s with
    | Empty -> accu
    | Leaf { elt = v } -> f v accu
    | Node { left = l; elt = v; right = r; height = _; size = _ } ->
      fold_right ~f l ~init:(f v (fold_right ~f r ~init:accu))
  ;;

  let rec for_all t ~f:p =
    match t with
    | Empty -> true
    | Leaf { elt = v } -> p v
    | Node { left = l; elt = v; right = r; height = _; size = _ } ->
      p v && for_all ~f:p l && for_all ~f:p r
  ;;

  let rec exists t ~f:p =
    match t with
    | Empty -> false
    | Leaf { elt = v } -> p v
    | Node { left = l; elt = v; right = r; height = _; size = _ } ->
      p v || exists ~f:p l || exists ~f:p r
  ;;

  let filter s ~f:p =
    let rec filt = function
      | Empty -> Empty
      | Leaf { elt = v } as t -> if p v then t else Empty
      | Node { left = l; elt = v; right = r; height = _; size = _ } as t ->
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
      | Node { left = l; elt = v; right = r; height = _; size = _ } ->
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
      | Node { left = l; elt = v; right = r; height = _; size = _ } as t ->
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
    | Node { left = l; elt = v; right = r; height = _; size = _ } ->
      elements_aux (v :: elements_aux accu r) l
  ;;

  let elements s = elements_aux [] s

  let choose t =
    match t with
    | Empty -> None
    | Leaf { elt = v } -> Some v
    | Node { left = _; elt = v; right = _; height = _; size = _ } -> Some v
  ;;

  let choose_exn =
    let not_found = Not_found_s (Atom "Set.choose_exn: empty set") in
    let choose_exn t =
      match choose t with
      | None -> raise not_found
      | Some v -> v
    in
    (* named to preserve symbol in compiled binary *)
    choose_exn
  ;;

  let of_list lst ~compare_elt =
    List.fold lst ~init:empty ~f:(fun t x -> add t x ~compare_elt)
  ;;

  let of_sequence sequence ~compare_elt =
    Sequence.fold sequence ~init:empty ~f:(fun t x -> add t x ~compare_elt)
  ;;

  let to_list s = elements s

  let of_array a ~compare_elt =
    Array.fold a ~init:empty ~f:(fun t x -> add t x ~compare_elt)
  ;;

  (* faster but equivalent to [Array.of_list (to_list t)] *)
  let to_array = function
    | Empty -> [||]
    | Leaf { elt = v } -> [| v |]
    | Node { left = l; elt = v; right = r; height = _; size = s } ->
      let res = Array.create ~len:s v in
      let pos_ref = ref 0 in
      let rec loop = function
        (* Invariant: on entry and on exit to [loop], !pos_ref is the next
           available cell in the array. *)
        | Empty -> ()
        | Leaf { elt = v } ->
          res.(!pos_ref) <- v;
          incr pos_ref
        | Node { left = l; elt = v; right = r; height = _; size = _ } ->
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
    fold t ~init:empty ~f:(fun t x -> add t (f x) ~compare_elt) [@nontail]
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
    | Node { left = l; elt = v; right = r; height = _; size = _ } ->
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
    | Node { left = l; elt = v; right = r; height = _; size = _ } ->
      (match f v with
       | Some _ as r -> r
       | None ->
         (match find_map l ~f with
          | None -> find_map r ~f
          | Some _ as r -> r))
  ;;

  let find_exn t ~f =
    match find t ~f with
    | None -> failwith "Set.find_exn failed to find a matching element"
    | Some e -> e
  ;;

  let rec nth t i =
    match t with
    | Empty -> None
    | Leaf { elt = v } -> if i = 0 then Some v else None
    | Node { left = l; elt = v; right = r; height = _; size = s } ->
      if i >= s
      then None
      else (
        let l_size = length l in
        let c = Poly.compare i l_size in
        if c < 0 then nth l i else if c = 0 then Some v else nth r (i - l_size - 1))
  ;;

  let stable_dedup_list xs ~compare_elt =
    let rec loop xs leftovers already_seen =
      match xs with
      | [] -> List.rev leftovers
      | hd :: tl ->
        if mem already_seen hd ~compare_elt
        then loop tl leftovers already_seen
        else loop tl (hd :: leftovers) (add already_seen hd ~compare_elt)
    in
    loop xs [] empty
  ;;

  let t_of_sexp_direct a_of_sexp sexp ~compare_elt =
    match sexp with
    | Sexp.List lst ->
      let elt_lst = List.map lst ~f:a_of_sexp in
      let set = of_list elt_lst ~compare_elt in
      if length set = List.length lst
      then set
      else (
        let set = ref empty in
        List.iter2_exn lst elt_lst ~f:(fun el_sexp el ->
          if mem !set el ~compare_elt
          then of_sexp_error "Set.t_of_sexp: duplicate element in set" el_sexp
          else set := add !set el ~compare_elt);
        assert false)
    | sexp -> of_sexp_error "Set.t_of_sexp: list needed" sexp
  ;;

  let sexp_of_t sexp_of_a t =
    Sexp.List (fold_right t ~init:[] ~f:(fun el acc -> sexp_of_a el :: acc))
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
  { (* [comparator] is the first field so that polymorphic equality fails on a map due
       to the functional value in the comparator.
       Note that this does not affect polymorphic [compare]: that still produces
       nonsense. *)
    comparator : ('a, 'comparator) Comparator.t
  ; tree : 'a Tree0.t
  }

type ('a, 'comparator) tree = 'a Tree0.t

let like { tree = _; comparator } tree = { tree; comparator }

let like_maybe_no_op ({ tree = old_tree; comparator } as old_t) tree =
  if phys_equal old_tree tree then old_t else { tree; comparator }
;;

let compare_elt t = t.comparator.Comparator.compare

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
  let fold_result t ~init ~f = Container.fold_result ~fold ~init ~f t
  let iter t ~f = Tree0.iter t.tree ~f
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

  let compare_direct t1 t2 = Tree0.compare (compare_elt t1) t1.tree t2.tree
  let equal t1 t2 = Tree0.equal t1.tree t2.tree ~compare_elt:(compare_elt t1)
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
        ~sexp_of_elt:subset.set.comparator.sexp_of_t
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
  let remove_index t i = like t (Tree0.remove_index t.tree i ~compare_elt:(compare_elt t))
  let sexp_of_t sexp_of_a _ t = Tree0.sexp_of_t sexp_of_a t.tree

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

module Tree = struct
  type ('a, 'comparator) t = ('a, 'comparator) tree

  let ce comparator = comparator.Comparator.compare

  let t_of_sexp_direct ~comparator a_of_sexp sexp =
    Tree0.t_of_sexp_direct ~compare_elt:(ce comparator) a_of_sexp sexp
  ;;

  let empty_without_value_restriction = Tree0.empty
  let empty ~comparator:_ = empty_without_value_restriction
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

  let compare_direct ~comparator t1 t2 = Tree0.compare (ce comparator) t1 t2
  let equal ~comparator t1 t2 = Tree0.equal t1 t2 ~compare_elt:(ce comparator)
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

  let stable_dedup_list ~comparator xs =
    Tree0.stable_dedup_list xs ~compare_elt:(ce comparator)
  ;;

  let group_by t ~equiv = Tree0.group_by t ~equiv
  let split ~comparator t a = Tree0.split t a ~compare_elt:(ce comparator)
  let split_le_gt ~comparator t a = Tree0.split_le_gt t a ~compare_elt:(ce comparator)
  let split_lt_ge ~comparator t a = Tree0.split_lt_ge t a ~compare_elt:(ce comparator)
  let nth t i = Tree0.nth t i
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

  let fold_result t ~init ~f = Container.fold_result ~fold ~init ~f t

  module Named = struct
    include Tree0.Named

    let is_subset ~comparator t1 ~of_:t2 =
      Tree0.Named.is_subset
        t1
        ~of_:t2
        ~compare_elt:(ce comparator)
        ~sexp_of_elt:comparator.Comparator.sexp_of_t
    ;;

    let equal ~comparator t1 t2 =
      Tree0.Named.equal
        t1
        t2
        ~compare_elt:(ce comparator)
        ~sexp_of_elt:comparator.Comparator.sexp_of_t
    ;;
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
      (Tree0.t_of_sexp_direct ~compare_elt:comparator.compare a_of_sexp sexp)
  ;;

  let empty ~comparator = { comparator; tree = Tree0.empty }

  module Empty_without_value_restriction (Elt : Comparator.S1) = struct
    let empty = { comparator = Elt.comparator; tree = Tree0.empty }
  end

  let singleton ~comparator e = { comparator; tree = Tree0.singleton e }

  let union_list ~comparator l =
    of_tree ~comparator (Tree0.union_list ~comparator ~to_tree l)
  ;;

  let of_sorted_array_unchecked ~comparator array =
    let tree =
      Tree0.of_sorted_array_unchecked array ~compare_elt:comparator.Comparator.compare
    in
    { comparator; tree }
  ;;

  let of_increasing_iterator_unchecked ~comparator ~len ~f =
    of_tree ~comparator (Tree0.of_increasing_iterator_unchecked ~len ~f)
  ;;

  let of_sorted_array ~comparator array =
    Or_error.Monad_infix.(
      Tree0.of_sorted_array array ~compare_elt:comparator.Comparator.compare
      >>| fun tree -> { comparator; tree })
  ;;

  let of_list ~comparator l =
    { comparator; tree = Tree0.of_list l ~compare_elt:comparator.Comparator.compare }
  ;;

  let of_sequence ~comparator s =
    { comparator; tree = Tree0.of_sequence s ~compare_elt:comparator.Comparator.compare }
  ;;

  let of_array ~comparator a =
    { comparator; tree = Tree0.of_array a ~compare_elt:comparator.Comparator.compare }
  ;;

  let stable_dedup_list ~comparator xs =
    Tree0.stable_dedup_list xs ~compare_elt:comparator.Comparator.compare
  ;;

  let map ~comparator t ~f =
    { comparator; tree = Tree0.map t.tree ~f ~compare_elt:comparator.Comparator.compare }
  ;;

  let filter_map ~comparator t ~f =
    { comparator
    ; tree = Tree0.filter_map t.tree ~f ~compare_elt:comparator.Comparator.compare
    }
  ;;

  module Tree = Tree
end

let to_comparator = Comparator.of_module
let empty m = Using_comparator.empty ~comparator:(to_comparator m)
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

let stable_dedup_list m a =
  Using_comparator.stable_dedup_list ~comparator:(to_comparator m) a
;;

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

module type Sexp_of_m = sig
  type t [@@deriving_inline sexp_of]

  val sexp_of_t : t -> Sexplib0.Sexp.t

  [@@@end]
end

module type M_of_sexp = sig
  type t [@@deriving_inline of_sexp]

  val t_of_sexp : Sexplib0.Sexp.t -> t

  [@@@end]

  include Comparator.S with type t := t
end

module type M_sexp_grammar = sig
  type t [@@deriving_inline sexp_grammar]

  val t_sexp_grammar : t Sexplib0.Sexp_grammar.t

  [@@@end]
end

module type Compare_m = sig end
module type Equal_m = sig end
module type Hash_fold_m = Hasher.S

let sexp_of_m__t (type elt) (module Elt : Sexp_of_m with type t = elt) t =
  sexp_of_t Elt.sexp_of_t (fun _ -> Sexp.Atom "_") t
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

  include Using_comparator.Empty_without_value_restriction (Comparator.Poly)

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
  let stable_dedup_list a = Using_comparator.stable_dedup_list ~comparator a
  let map a ~f = Using_comparator.map ~comparator a ~f
  let filter_map a ~f = Using_comparator.filter_map ~comparator a ~f
  let of_tree tree = { comparator; tree }
  let to_tree t = t.tree
end
