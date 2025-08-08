(* A few small things copied from other parts of Base because they depend on us, so we
   can't use them. *)

open! Import
module Sexp = Sexp0

let raise_s = Error.raise_s

module Int = struct
  type t = int

  let max (x : t) y = if x > y then x else y
end

(* Its important that Empty have no args. It's tempting to make this type a record
   (e.g. to hold the compare function), but a lot of memory is saved by Empty being an
   immediate, since all unused buckets in the hashtbl don't use any memory (besides the
   array cell) *)

[%%template
(* We deliberately leave the [k = value, v = value] for last so that
   [t[@kind value value]] is defined last and the constructors can be inferred to
   correspond to the "regular" version of the type. The inference appears rather delicate,
   so moving the [[@@@kind.default]] to after the type definition and adding a poly
   attribute separately on the type might break code after the end of the templated
   section. *)
[@@@kind.default
  k = (bits64, float64, value_or_null), v = (bits64, float64, value_or_null)]

type ('k, 'v) t =
  | Empty
  | Node of
      { mutable left : (('k, 'v) t[@kind k v])
      ; key : 'k
      ; mutable value : 'v
      ; mutable height : int
      ; mutable right : (('k, 'v) t[@kind k v])
      }
  | Leaf of
      { key : 'k
      ; mutable value : 'v
      }

let empty = Empty
let get_empty () = Empty

let is_empty = function
  | Empty -> true
  | Leaf _ | Node _ -> false
;;

let height = function
  | Empty -> 0
  | Leaf _ -> 1
  | Node { left = _; key = _; value = _; height; right = _ } -> height
;;

let invariant compare =
  let rec inv = function
    | Empty | Leaf _ -> ()
    | Node { left; key = k; value = _; height = h; right } ->
      let hl, hr = (height [@kind k v]) left, (height [@kind k v]) right in
      inv left;
      inv right;
      legal_left_key k left;
      legal_right_key k right;
      assert (h = Int.max hl hr + 1);
      assert (abs (hl - hr) <= 2)
  and legal_left_key key = function
    | Empty -> ()
    | Leaf { key = left_key; value = _ }
    | Node { left = _; key = left_key; value = _; height = _; right = _ } ->
      assert (compare left_key key < 0)
  and legal_right_key key = function
    | Empty -> ()
    | Leaf { key = right_key; value = _ }
    | Node { left = _; key = right_key; value = _; height = _; right = _ } ->
      assert (compare right_key key > 0)
  in
  inv
;;

let invariant t ~compare = (invariant [@kind k v]) compare t

(* In the following comments,
   't is balanced' means that 'invariant t' does not
   raise an exception.  This implies of course that each node's height field is
   correct.
   't is balanceable' means that height of the left and right subtrees of t
   differ by at most 3. *)

(* @pre: left and right subtrees have correct heights
   @post: output has the correct height *)
let update_height = function
  | Node ({ left; key = _; value = _; height = old_height; right } as x) ->
    let new_height =
      Int.max ((height [@kind k v]) left) ((height [@kind k v]) right) + 1
    in
    if new_height <> old_height then x.height <- new_height
  | Empty | Leaf _ -> assert false
;;

(* @pre: left and right subtrees are balanced
   @pre: tree is balanceable
   @post: output is balanced (in particular, height is correct) *)
let balance tree =
  match tree with
  | Empty | Leaf _ -> tree
  | Node ({ left; key = _; value = _; height = _; right } as root_node) ->
    let hl = (height [@kind k v]) left
    and hr = (height [@kind k v]) right in
    (* + 2 is critically important, lowering it to 1 will break the Leaf
       assumptions in the code below, and will force us to promote leaf nodes in
       the balance routine. It's also faster, since it will balance less often.
       Note that the following code is delicate.  The update_height calls must
       occur in the correct order, since update_height assumes its children have
       the correct heights.  *)
    if hl > hr + 2
    then (
      match left with
      (* It cannot be a leaf, because even if right is empty, a leaf
         is only height 1 *)
      | Empty | Leaf _ -> assert false
      | Node
          ({ left = left_node_left
           ; key = _
           ; value = _
           ; height = _
           ; right = left_node_right
           } as left_node) ->
        if (height [@kind k v]) left_node_left >= (height [@kind k v]) left_node_right
        then (
          root_node.left <- left_node_right;
          left_node.right <- tree;
          (update_height [@kind k v]) tree;
          (update_height [@kind k v]) left;
          left)
        else (
          (* if right is a leaf, then left must be empty. That means
             height is 2. Even if hr is empty we still can't get here. *)
          match left_node_right with
          | Empty | Leaf _ -> assert false
          | Node
              ({ left = lr_left; key = _; value = _; height = _; right = lr_right } as
               lr_node) ->
            left_node.right <- lr_left;
            root_node.left <- lr_right;
            lr_node.right <- tree;
            lr_node.left <- left;
            (update_height [@kind k v]) left;
            (update_height [@kind k v]) tree;
            (update_height [@kind k v]) left_node_right;
            left_node_right))
    else if hr > hl + 2
    then (
      (* see above for an explanation of why right cannot be a leaf *)
      match right with
      | Empty | Leaf _ -> assert false
      | Node
          ({ left = right_node_left
           ; key = _
           ; value = _
           ; height = _
           ; right = right_node_right
           } as right_node) ->
        if (height [@kind k v]) right_node_right >= (height [@kind k v]) right_node_left
        then (
          root_node.right <- right_node_left;
          right_node.left <- tree;
          (update_height [@kind k v]) tree;
          (update_height [@kind k v]) right;
          right)
        else (
          (* see above for an explanation of why this cannot be a leaf *)
          match right_node_left with
          | Empty | Leaf _ -> assert false
          | Node
              ({ left = rl_left; key = _; value = _; height = _; right = rl_right } as
               rl_node) ->
            right_node.left <- rl_right;
            root_node.right <- rl_left;
            rl_node.left <- tree;
            rl_node.right <- right;
            (update_height [@kind k v]) right;
            (update_height [@kind k v]) tree;
            (update_height [@kind k v]) right_node_left;
            right_node_left))
    else (
      (update_height [@kind k v]) tree;
      tree)
;;

(* @pre: t is balanced.
   @post: result is balanced, with new node inserted
   @post: !added = true iff the shape of the input tree changed.  *)

let rec add t ~replace ~compare ~added ~key:k ~data:v =
  match t with
  | Empty ->
    added := true;
    Leaf { key = k; value = v }
  | Leaf ({ key = k'; value = _ } as r) ->
    let c = compare k' k in
    (* This compare is reversed on purpose, we are pretending
       that the leaf was just inserted instead of the other way
       round, that way we only allocate one node. *)
    if c = 0
    then (
      added := false;
      if replace then r.value <- v;
      t)
    else (
      added := true;
      if c < 0
      then Node { left = t; key = k; value = v; height = 2; right = Empty }
      else Node { left = Empty; key = k; value = v; height = 2; right = t })
  | Node ({ left; key = k'; value = _; height = _; right } as r) ->
    let c = compare k k' in
    if c = 0
    then (
      added := false;
      if replace then r.value <- v;
      t)
    else (
      if c < 0
      then (
        let left' = (add [@kind k v]) left ~replace ~added ~compare ~key:k ~data:v in
        if not (phys_equal left' left) then r.left <- left')
      else (
        let right' = (add [@kind k v]) right ~replace ~added ~compare ~key:k ~data:v in
        if not (phys_equal right' right) then r.right <- right');
      if !added then (balance [@kind k v]) t else t)
;;

let[@kind
     k = k
     , v = v
     , a = (value_or_null, bits64, float64)
     , b = (value_or_null, bits64, float64)
     , r = (value_or_null, bits64, float64)]
   [@mode c = (uncontended, shared)]
   [@inline always] rec findi_and_call_impl
  : type k v a b r if_ in_.
    ((k, v) t[@kind k v])
    -> compare:(k -> k -> int)
    -> k
    -> a
    -> b
    -> call_if_found:(if_found:if_ -> key:k -> data:v -> a -> b -> r)
    -> call_if_not_found:(if_not_found:in_ -> k -> a -> b -> r)
    -> if_found:if_
    -> if_not_found:in_
    -> r
  =
  fun t ~compare k arg1 arg2 ~call_if_found ~call_if_not_found ~if_found ~if_not_found ->
  match t with
  | Empty -> call_if_not_found ~if_not_found k arg1 arg2
  | Leaf { key = k'; value = v } ->
    if compare k k' = 0
    then call_if_found ~if_found ~key:k' ~data:v arg1 arg2
    else call_if_not_found ~if_not_found k arg1 arg2
  | Node { left; key = k'; value = v; height = _; right } ->
    let c = compare k k' in
    if c = 0
    then call_if_found ~if_found ~key:k' ~data:v arg1 arg2
    else
      (findi_and_call_impl [@kind k v a b r] [@mode c])
        (if c < 0 then left else right)
        ~compare
        k
        arg1
        arg2
        ~call_if_found
        ~call_if_not_found
        ~if_found
        ~if_not_found
;;

[%%template
[@@@mode.default c = (uncontended, shared)]
[@@@kind r = (value_or_null, bits64, float64)]

let[@kind k = k, v = v, r = r] find_and_call =
  let call_if_found ~if_found ~key:_ ~data () () = if_found data in
  let call_if_not_found ~if_not_found key () () = if_not_found key in
  fun t ~compare k ~if_found ~if_not_found ->
    (findi_and_call_impl [@kind k v value_or_null value_or_null r] [@mode c])
      t
      ~compare
      k
      ()
      ()
      ~call_if_found
      ~call_if_not_found
      ~if_found
      ~if_not_found
;;

let[@kind k = k, v = v, r = r] findi_and_call =
  let call_if_found ~if_found ~key ~data () () = if_found ~key ~data in
  let call_if_not_found ~if_not_found key () () = if_not_found key in
  fun t ~compare k ~if_found ~if_not_found ->
    (findi_and_call_impl [@kind k v value_or_null value_or_null r] [@mode c])
      t
      ~compare
      k
      ()
      ()
      ~call_if_found
      ~call_if_not_found
      ~if_found
      ~if_not_found
;;

[@@@kind a = (value_or_null, bits64, float64)]

let[@kind k = k, v = v, a = a, r = r] find_and_call1 =
  let call_if_found ~if_found ~key:_ ~data arg () = if_found data arg in
  let call_if_not_found ~if_not_found key arg () = if_not_found key arg in
  fun t ~compare k ~a ~if_found ~if_not_found ->
    (findi_and_call_impl [@kind k v a value_or_null r] [@mode c])
      t
      ~compare
      k
      a
      ()
      ~call_if_found
      ~call_if_not_found
      ~if_found
      ~if_not_found
;;

let[@kind k = k, v = v, a = a, r = r] findi_and_call1 =
  let call_if_found ~if_found ~key ~data arg () = if_found ~key ~data arg in
  let call_if_not_found ~if_not_found key arg () = if_not_found key arg in
  fun t ~compare k ~a ~if_found ~if_not_found ->
    (findi_and_call_impl [@kind k v a value_or_null r] [@mode c])
      t
      ~compare
      k
      a
      ()
      ~call_if_found
      ~call_if_not_found
      ~if_found
      ~if_not_found
;;

[@@@kind b = (value_or_null, bits64, float64)]

let[@kind k = k, v = v, a = a, b = b, r = r] find_and_call2 =
  let call_if_found ~if_found ~key:_ ~data arg1 arg2 = if_found data arg1 arg2 in
  let call_if_not_found ~if_not_found key arg1 arg2 = if_not_found key arg1 arg2 in
  fun t ~compare k ~a ~b ~if_found ~if_not_found ->
    (findi_and_call_impl [@kind k v a b r] [@mode c])
      t
      ~compare
      k
      a
      b
      ~call_if_found
      ~call_if_not_found
      ~if_found
      ~if_not_found
;;

let[@kind k = k, v = v, a = a, b = b, r = r] findi_and_call2 =
  let call_if_found ~if_found ~key ~data arg1 arg2 = if_found ~key ~data arg1 arg2 in
  let call_if_not_found ~if_not_found key arg1 arg2 = if_not_found key arg1 arg2 in
  fun t ~compare k ~a ~b ~if_found ~if_not_found ->
    (findi_and_call_impl [@kind k v a b r] [@mode c])
      t
      ~compare
      k
      a
      b
      ~call_if_found
      ~call_if_not_found
      ~if_found
      ~if_not_found
;;]

let find =
  let if_found v : ((_ Option.t[@kind v]) Modes.t[@mode c]) = { modal = Some v } in
  let if_not_found _ : ((_ Option.t[@kind v]) Modes.t[@mode c]) = { modal = None } in
  fun t ~compare k ->
    ((find_and_call [@kind k v value_or_null] [@mode c])
       t
       ~compare
       k
       ~if_found
       ~if_not_found)
      .modal
[@@mode c = (uncontended, shared)]
;;

let mem =
  let if_found _ = true in
  let if_not_found _ = false in
  fun t ~compare k ->
    (find_and_call [@kind k v value_or_null] [@mode c])
      t
      ~compare
      k
      ~if_found
      ~if_not_found
[@@mode c = (uncontended, shared)]
;;

let rec remove =
  let rec min_elt tree =
    match tree with
    | Empty -> Empty
    | Leaf _ -> tree
    | Node { left = Empty; key = _; value = _; height = _; right = _ } -> tree
    | Node { left; key = _; value = _; height = _; right = _ } -> min_elt left
  in
  let rec remove_min_elt tree =
    match tree with
    | Empty -> assert false
    | Leaf _ -> Empty
    | Node { left = Empty; key = _; value = _; height = _; right } -> right
    | Node { left = Leaf _; key = k; value = v; height = _; right = Empty } ->
      Leaf { key = k; value = v }
    | Node ({ left; key = _; value = _; height = _; right = _ } as r) ->
      r.left <- remove_min_elt left;
      (balance [@kind k v]) tree
  in
  let merge t1 t2 =
    match t1, t2 with
    | Empty, t -> t
    | t, Empty -> t
    | _, _ ->
      let tree = min_elt t2 in
      (balance [@kind k v])
        (match tree with
         | Empty -> assert false
         | Leaf { key = k; value = v } ->
           let t2 = remove_min_elt t2 in
           Node
             { left = t1
             ; key = k
             ; value = v
             ; height = Int.max ((height [@kind k v]) t1) ((height [@kind k v]) t2) + 1
             ; right = t2
             }
         | Node r ->
           r.right <- remove_min_elt t2;
           r.left <- t1;
           tree)
  in
  fun t ~removed ~compare k ->
    match t with
    | Empty ->
      removed := false;
      Empty
    | Leaf { key = k'; value = _ } ->
      if compare k k' = 0
      then (
        removed := true;
        Empty)
      else (
        removed := false;
        t)
    | Node ({ left; key = k'; value = _; height = _; right } as r) ->
      let c = compare k k' in
      if c = 0
      then (
        removed := true;
        merge left right)
      else (
        if c < 0
        then (
          let left' = (remove [@kind k v]) left ~removed ~compare k in
          if not (phys_equal left' left) then r.left <- left')
        else (
          let right' = (remove [@kind k v]) right ~removed ~compare k in
          if not (phys_equal right' right) then r.right <- right');
        if !removed then (balance [@kind k v]) t else t)
;;

let rec fold t ~init ~f =
  match t with
  | Empty -> init
  | Leaf { key; value = data } -> f ~key ~data init
  | Node
      { left = Leaf { key = lkey; value = ldata }
      ; key
      ; value = data
      ; height = _
      ; right = Leaf { key = rkey; value = rdata }
      } -> f ~key:rkey ~data:rdata (f ~key ~data (f ~key:lkey ~data:ldata init))
  | Node
      { left = Leaf { key = lkey; value = ldata }
      ; key
      ; value = data
      ; height = _
      ; right = Empty
      } -> f ~key ~data (f ~key:lkey ~data:ldata init)
  | Node
      { left = Empty
      ; key
      ; value = data
      ; height = _
      ; right = Leaf { key = rkey; value = rdata }
      } -> f ~key:rkey ~data:rdata (f ~key ~data init)
  | Node
      { left; key; value = data; height = _; right = Leaf { key = rkey; value = rdata } }
    -> f ~key:rkey ~data:rdata (f ~key ~data ((fold [@kind k v]) left ~init ~f))
  | Node
      { left = Leaf { key = lkey; value = ldata }; key; value = data; height = _; right }
    -> (fold [@kind k v]) right ~init:(f ~key ~data (f ~key:lkey ~data:ldata init)) ~f
  | Node { left; key; value = data; height = _; right } ->
    (fold [@kind k v]) right ~init:(f ~key ~data ((fold [@kind k v]) left ~init ~f)) ~f
;;

let rec iter t ~f =
  match t with
  | Empty -> ()
  | Leaf { key; value = data } -> f ~key ~data
  | Node { left; key; value = data; height = _; right } ->
    (iter [@kind k v]) left ~f;
    f ~key ~data;
    (iter [@kind k v]) right ~f
;;

let rec mapi_inplace t ~f =
  match t with
  | Empty -> ()
  | Leaf ({ key; value } as t) -> t.value <- f ~key ~data:value
  | Node ({ left; key; value; height = _; right } as t) ->
    (mapi_inplace [@kind k v]) ~f left;
    t.value <- f ~key ~data:value;
    (mapi_inplace [@kind k v]) ~f right
;;

let choose_exn = function
  | Empty ->
    (match raise_s (Sexp.message "[Avltree.choose_exn] of empty hashtbl" []) with
     | (_ : Nothing.t) -> .)
  | Leaf { key; value; _ } | Node { key; value; _ } -> key, value
;;]

let rec first t =
  match t with
  | Empty -> None
  | Leaf { key = k; value = v }
  | Node { left = Empty; key = k; value = v; height = _; right = _ } -> Some (k, v)
  | Node { left = l; key = _; value = _; height = _; right = _ } -> first l
;;

let rec last t =
  match t with
  | Empty -> None
  | Leaf { key = k; value = v }
  | Node { left = _; key = k; value = v; height = _; right = Empty } -> Some (k, v)
  | Node { left = _; key = _; value = _; height = _; right = r } -> last r
;;
