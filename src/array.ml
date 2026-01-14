open! Import
include Array_intf.Definitions
module Array = Array0
module Result = Result0
module Option = Option0
module List = List0
include Array

[@@@warning "-incompatible-with-upstream"]

type ('a : any mod separable) t = 'a array

[%%template
[@@@kind_set.define base_with_ext = (base, value mod external64)]

type%template ('a : k) t = 'a array [@@kind k = (base_non_value, value mod external64)]

[%%rederive.portable
  type nonrec ('a : value_or_null mod separable) t = 'a array
  [@@deriving globalize, sexp ~stackify, sexp_grammar]]

(* This module implements a new in-place, constant heap sorting algorithm to replace the
   one used by the standard libraries. Its only purpose is to be faster (hopefully
   strictly faster) than the base sort and stable_sort.

   At a high level the algorithm is:
   - pick two pivot points by:
   - pick 5 arbitrary elements from the array
   - sort them within the array
   - take the elements on either side of the middle element of the sort as the pivots
   - sort the array with:
   - all elements less than pivot1 to the left (range 1)
   - all elements >= pivot1 and <= pivot2 in the middle (range 2)
   - all elements > pivot2 to the right (range 3)
   - if pivot1 and pivot2 are equal, then the middle range is sorted, so ignore it
   - recurse into range 1, 2 (if pivot1 and pivot2 are unequal), and 3
   - during recursion there are two inflection points:
   - if the size of the current range is small, use insertion sort to sort it
   - if the stack depth is large, sort the range with heap-sort to avoid n^2 worst-case
     behavior

   See the following for more information:
   - "Dual-Pivot Quicksort" by Vladimir Yaroslavskiy. Available at
     http://www.kriche.com.ar/root/programming/spaceTimeComplexity/DualPivotQuicksort.pdf
   - "Quicksort is Optimal" by Sedgewick and Bentley. Slides at
     http://www.cs.princeton.edu/~rs/talks/QuicksortIsOptimal.pdf
   - http://www.sorting-algorithms.com/quick-sort-3-way *)

module%template.portable
  [@kind k = (value, value mod external64)] [@modality p] Sorter (S : sig
    type ('a : k) t

    val get : local_ 'a t -> int -> 'a
    val set : local_ 'a t -> int -> 'a -> unit
    val length : local_ 'a t -> int
  end) =
struct
  include S

  let swap arr i j =
    let tmp = get arr i in
    set arr i (get arr j);
    set arr j tmp
  ;;

  module type Sort = sig @@ p
    val sort
      :  local_ 'a t
      -> compare:local_ ('a -> 'a -> int)
      -> left:int (* leftmost index of sub-array to sort *)
      -> right:int (* rightmost index of sub-array to sort *)
      -> unit
  end

  (* http://en.wikipedia.org/wiki/Insertion_sort *)
  module Insertion_sort : Sort = struct
    (* loop invariants:
       1. the subarray arr[left .. i-1] is sorted
       2. the subarray arr[i+1 .. pos] is sorted and contains only elements > v
       3. arr[i] may be thought of as containing v
    *)
    let rec insert_loop arr ~left ~compare i v =
      let i_next = i - 1 in
      if i_next >= left && compare (get arr i_next) v > 0
      then (
        set arr i (get arr i_next);
        insert_loop arr ~left ~compare i_next v)
      else i
    ;;

    let sort arr ~compare ~left ~right =
      (* loop invariant: [arr] is sorted from [left] to [pos - 1], inclusive *)
      for pos = left + 1 to right do
        let v = get arr pos in
        let final_pos = insert_loop arr ~left ~compare pos v in
        set arr final_pos v
      done
    ;;
  end

  (* http://en.wikipedia.org/wiki/Heapsort *)
  module Heap_sort : Sort = struct
    (* loop invariant: root's children are both either roots of max-heaps or > right *)
    let rec heapify arr ~compare root ~left ~right =
      let relative_root = root - left in
      let left_child = (2 * relative_root) + left + 1 in
      let right_child = (2 * relative_root) + left + 2 in
      let largest =
        if left_child <= right && compare (get arr left_child) (get arr root) > 0
        then left_child
        else root
      in
      let largest =
        if right_child <= right && compare (get arr right_child) (get arr largest) > 0
        then right_child
        else largest
      in
      if largest <> root
      then (
        swap arr root largest;
        heapify arr ~compare largest ~left ~right)
    ;;

    let build_heap arr ~compare ~left ~right =
      (* Elements in the second half of the array are already heaps of size 1. We move
         through the first half of the array from back to front examining the element at
         hand, and the left and right children, fixing the heap property as we go. *)
      for i = (left + right) / 2 downto left do
        heapify arr ~compare i ~left ~right
      done
    ;;

    let sort arr ~compare ~left ~right =
      build_heap arr ~compare ~left ~right;
      (* loop invariants:
         1. the subarray arr[left ... i] is a max-heap H
         2. the subarray arr[i+1 ... right] is sorted (call it S)
         3. every element of H is less than every element of S *)
      for i = right downto left + 1 do
        swap arr left i;
        heapify arr ~compare left ~left ~right:(i - 1)
      done
    ;;
  end

  (* http://en.wikipedia.org/wiki/Introsort *)
  module Intro_sort : sig @@ p
    include Sort

    val five_element_sort
      :  local_ 'a t
      -> compare:local_ ('a -> 'a -> int)
      -> int
      -> int
      -> int
      -> int
      -> int
      -> unit
  end = struct
    let five_element_sort arr ~(local_ compare : _ -> _ -> _) m1 m2 m3 m4 m5 =
      let compare_and_swap i j =
        if compare (get arr i) (get arr j) > 0 then swap arr i j
      in
      (* Optimal 5-element sorting network:

         {v
            1--o-----o-----o--------------1
               |     |     |
            2--o-----|--o--|-----o--o-----2
                     |  |  |     |  |
            3--------o--o--|--o--|--o-----3
                           |  |  |
            4-----o--------o--o--|-----o--4
                  |              |     |
            5-----o--------------o-----o--5
         v} *)
      compare_and_swap m1 m2;
      compare_and_swap m4 m5;
      compare_and_swap m1 m3;
      compare_and_swap m2 m3;
      compare_and_swap m1 m4;
      compare_and_swap m3 m4;
      compare_and_swap m2 m5;
      compare_and_swap m2 m3;
      compare_and_swap m4 m5 [@nontail]
    ;;

    (* choose pivots for the array by sorting 5 elements and examining the center three
       elements. The goal is to choose two pivots that will either:
       - break the range up into 3 even partitions or
       - eliminate a commonly appearing element by sorting it into the center partition by
         itself To this end we look at the center 3 elements of the 5 and return pairs of
         equal elements or the widest range *)
    let choose_pivots arr ~(local_ compare : _ -> _ -> _) ~left ~right =
      let sixth = (right - left) / 6 in
      let m1 = left + sixth in
      let m2 = m1 + sixth in
      let m3 = m2 + sixth in
      let m4 = m3 + sixth in
      let m5 = m4 + sixth in
      five_element_sort arr ~compare m1 m2 m3 m4 m5;
      let m2_val = get arr m2 in
      let m3_val = get arr m3 in
      let m4_val = get arr m4 in
      if compare m2_val m3_val = 0
      then #(m2_val, m3_val, true)
      else if compare m3_val m4_val = 0
      then #(m3_val, m4_val, true)
      else #(m2_val, m4_val, false)
    ;;

    let dual_pivot_partition arr ~(local_ compare : _ -> _ -> _) ~left ~right =
      let #(pivot1, pivot2, pivots_equal) = choose_pivots arr ~compare ~left ~right in
      (*=loop invariants:
         1.  left <= l < r <= right
         2.  l <= p <= r
         3.  l <= x < p     implies arr[x] >= pivot1
         and arr[x] <= pivot2
         4.  left <= x < l  implies arr[x] < pivot1
         5.  r < x <= right implies arr[x] > pivot2 *)
      let rec loop l p r = exclave_
        if p > r
        then #(l, r)
        else (
          let pv = get arr p in
          if compare pv pivot1 < 0
          then (
            swap arr p l;
            loop (l + 1) (p + 1) r)
          else if compare pv pivot2 > 0
          then (
            (* loop invariants: same as those of the outer loop *)
            let rec scan_backwards r =
              if r > p && compare (get arr r) pivot2 > 0
              then scan_backwards (r - 1)
              else r
            in
            let r = scan_backwards r in
            swap arr r p;
            loop l p (r - 1))
          else loop l (p + 1) r)
      in
      let #(l, r) = loop left left right in
      #(l, r, pivots_equal)
    ;;

    let rec intro_sort arr ~max_depth ~compare ~left ~right =
      let len = right - left + 1 in
      (* This takes care of some edge cases, such as left > right or very short arrays,
         since Insertion_sort.sort handles these cases properly. Thus we don't need to
         make sure that left and right are valid in recursive calls. *)
      if len <= 32
      then Insertion_sort.sort arr ~compare ~left ~right
      else if max_depth < 0
      then Heap_sort.sort arr ~compare ~left ~right
      else (
        let max_depth = max_depth - 1 in
        let #(l, r, middle_sorted) = dual_pivot_partition arr ~compare ~left ~right in
        intro_sort arr ~max_depth ~compare ~left ~right:(l - 1);
        if not middle_sorted then intro_sort arr ~max_depth ~compare ~left:l ~right:r;
        intro_sort arr ~max_depth ~compare ~left:(r + 1) ~right)
    ;;

    let sort arr ~compare ~left ~right =
      let heap_sort_switch_depth =
        (* We bail out to heap sort at a recursion depth of 32. GNU introsort uses 2lg(n).
           The expected recursion depth for perfect 3-way splits is log_3(n).

           Using 32 means a balanced 3-way split would work up to 3^32 elements (roughly
           2^50 or 10^15). GNU reaches a depth of 32 at 65536 elements.

           For small arrays, this makes us less likely to bail out to heap sort, but the
           32*N cost before we do is not that much.

           For large arrays, this means we are more likely to bail out to heap sort at
           some point if we get some bad splits or if the array is huge. But that's only a
           constant factor cost in the final stages of recursion.

           All in all, this seems to be a small tradeoff and avoids paying a cost to
           compute a logarithm at the start. *)
        32
      in
      intro_sort arr ~max_depth:heap_sort_switch_depth ~compare ~left ~right
    ;;
  end

  let sort ?pos ?len arr ~(local_ compare) =
    let pos, len =
      Ordered_collection_common.get_pos_len_exn () ?pos ?len ~total_length:(length arr)
    in
    Intro_sort.sort arr ~compare ~left:pos ~right:(pos + len - 1)
  ;;
end
[@@inline]

module%template [@kind k = (value, value mod external64)] Sort =
Sorter [@kind k] [@modality portable] (struct
    type nonrec ('a : k) t = 'a t

    let get = unsafe_get
    let set = unsafe_set
    let length = length
  end)

let sort = Sort.sort

let%template get_opt arr n : (_ Option.t[@kind k or value_or_null]) =
  if 0 <= n && n < length arr
  then
    Some ((unsafe_get [@mode c]) arr n)
    (* SAFETY: bounds checked above *) [@exclave_if_stack a]
  else None
[@@mode c = (uncontended, shared)] [@@kind k = base] [@@alloc a = (heap, stack)]
;;

let is_sorted t ~compare =
  let i = ref (length t - 1) in
  let result = ref true in
  while !i > 0 && !result do
    let elt_i = unsafe_get t !i in
    let elt_i_minus_1 = unsafe_get t (!i - 1) in
    if compare elt_i_minus_1 elt_i > 0 then result := false;
    decr i
  done;
  !result
;;

let is_sorted_strictly t ~compare =
  let i = ref (length t - 1) in
  let result = ref true in
  while !i > 0 && !result do
    let elt_i = unsafe_get t !i in
    let elt_i_minus_1 = unsafe_get t (!i - 1) in
    if compare elt_i_minus_1 elt_i >= 0 then result := false;
    decr i
  done;
  !result
;;

let folding_map t ~init ~f =
  let acc = ref init in
  map t ~f:(fun x ->
    let new_acc, y = f !acc x in
    acc := new_acc;
    y)
  [@nontail]
;;

let fold_map t ~init ~f =
  let acc = ref init in
  let result =
    map t ~f:(fun x ->
      let new_acc, y = f !acc x in
      acc := new_acc;
      y)
  in
  !acc, result
;;

let raise_length_mismatch name n1 n2 =
  Printf.invalid_argf "length mismatch in %s: %d <> %d" name n1 n2 ()
[@@cold]
;;

[%%template
let length t = length t [@@kind k = (base_non_value, value mod external64)]

[@@@kind.default k1 = base_with_ext]

let to_array t = t
let of_array t = t
let is_empty t = length t = 0

let for_all t ~f =
  let i = ref (length t - 1) in
  let result = ref true in
  while !i >= 0 && !result do
    if not (f (unsafe_get t !i)) then result := false else decr i
  done;
  !result
;;

let for_alli t ~f =
  let length = length t in
  let i = ref (length - 1) in
  let result = ref true in
  while !i >= 0 && !result do
    if not (f !i (unsafe_get t !i)) then result := false else decr i
  done;
  !result
;;

let count t ~(local_ f) =
  let local_ result = ref 0 in
  for i = 0 to Array.length t - 1 do
    result := !result + (f (Array.unsafe_get t i) |> Bool.to_int)
  done;
  !result
;;

let counti t ~(local_ f) =
  let local_ result = ref 0 in
  for i = 0 to Array.length t - 1 do
    result := !result + (f i (Array.unsafe_get t i) |> Bool.to_int)
  done;
  !result
;;

let exists t ~f =
  let i = ref (length t - 1) in
  let result = ref false in
  while !i >= 0 && not !result do
    if f (unsafe_get t !i) then result := true else decr i
  done;
  !result
;;

let existsi t ~f =
  let i = ref (length t - 1) in
  let result = ref false in
  while !i >= 0 && not !result do
    if f !i (unsafe_get t !i) then result := true else decr i
  done;
  !result
;;

let mem t a ~equal = (exists [@kind k1]) t ~f:(equal a) [@nontail]

let[@inline always] extremal_element t ~compare ~keep_left_if
  : (_ Option.t[@kind k1 or value_or_null])
  =
  if (is_empty [@kind k1]) t
  then None
  else (
    let length = length t in
    let rec loop i result =
      if i < length
      then (
        let x = unsafe_get t i in
        loop
          (i + 1)
          ((Bool0.select [@kind k1 or value_or_null])
             ((keep_left_if [@inlined]) ((compare [@inlined hint]) x result))
             x
             result))
      else result
    in
    Some ((loop [@inlined]) 1 (unsafe_get t 0)))
;;

let min_elt t ~compare =
  (extremal_element [@kind k1] [@inlined]) t ~compare ~keep_left_if:(fun compare_result ->
    compare_result < 0)
;;

let max_elt t ~compare =
  (extremal_element [@kind k1] [@inlined]) t ~compare ~keep_left_if:(fun compare_result ->
    compare_result > 0)
;;

let[@inline always] findi_internal t ~(f @ local) ~if_found ~if_not_found =
  let length = length t in
  if length = 0
  then (if_not_found [@inlined]) ()
  else (
    let rec loop i =
      if i < length
      then (
        let x = unsafe_get t i in
        if (f [@inlined hint]) i x then (if_found [@inlined]) ~i ~value:x else loop (i + 1))
      else (if_not_found [@inlined]) ()
    in
    (loop [@inlined]) 0 [@nontail])
[@@kind k1 = k1, k2 = (value, k1, value & k1)]
;;

let find t ~(f @ local) =
  (findi_internal [@inlined] [@kind k1 value])
    t
    ~f:(fun _ v -> f v)
    ~if_found:(fun ~i:_ ~value : (_ Option.t[@kind k1 or value_or_null]) -> Some value)
    ~if_not_found:(fun () -> None) [@nontail]
;;

let find_exn t ~(f @ local) =
  (findi_internal [@inlined] [@kind k1 k1])
    t
    ~f:(fun _i x -> f x)
    ~if_found:(fun ~i:_ ~value -> value)
    ~if_not_found:(fun () ->
      (raise [@kind k1 or value_or_null]) (Not_found_s (Atom "Array.find_exn: not found")))
  [@nontail]
;;

let findi t ~f =
  (findi_internal [@inlined] [@kind k1 value])
    t
    ~f
    ~if_found:(fun ~i ~value : (_ Option.t[@kind value & (k1 or value)]) ->
      Some #(i, value))
    ~if_not_found:(fun () -> None)
;;

let findi_exn t ~f =
  (findi_internal [@inlined] [@kind k1 (value & k1)])
    t
    ~f
    ~if_found:(fun ~i ~value -> #(i, value))
    ~if_not_found:(fun () ->
      (raise [@kind value & (k1 or value)])
        (Not_found_s (Atom "Array.findi_exn: not found")))
;;

(* The [value] version of this implementation initializes the output only once, based on
   the primitive [caml_array_sub]. Other approaches, like [init] or [map], first
   initialize with a fixed value, then blit from the source. *)
let copy t = (sub [@kind k1]) t ~pos:0 ~len:(length t)

let rev_inplace t =
  let i = ref 0 in
  let j = ref (length t - 1) in
  while !i < !j do
    (swap [@kind k1]) t !i !j;
    incr i;
    decr j
  done
;;

let rev t =
  let t = (copy [@kind k1]) t in
  (rev_inplace [@kind k1]) t;
  t
;;

let of_list_rev (l : (_ List.Constructors.t[@kind k1 or value_or_null])) =
  match l with
  | [] -> [||]
  | a :: l ->
    let len = 1 + (List.length [@kind k1 or value_or_null]) l in
    let t = create ~len a in
    let r = ref l in
    (* We start at [len - 2] because we already put [a] at [t.(len - 1)]. *)
    for i = len - 2 downto 0 do
      match !r with
      | [] -> assert false
      | a :: l ->
        t.(i) <- a;
        r := l
    done;
    t
;;

[%%template
[@@@kind.default k1 = k1]
[@@@kind.default k2 = base]

let sum (type sum : k2) (module M : Container.Summable with type t = sum[@kind k2]) t ~f =
  let toplevel_get = Toplevel_value.get [@kind k2] in
  (fold [@kind k1 k2]) t ~init:((toplevel_get [@inlined]) M.zero) ~f:(fun n a ->
    M.( + ) n (f a))
  [@nontail]
;;

let iteri_until t ~f ~finish =
  let length = length t in
  let rec loop i =
    if i < length
    then (
      match
        ((f [@inlined hint]) i (unsafe_get t i)
         : (_ Container.Continue_or_stop.t[@kind value_or_null (k2 or value_or_null)]))
      with
      | Continue () -> loop (i + 1)
      | Stop res -> res)
    else (finish [@inlined hint]) i
  in
  (loop [@inlined]) 0 [@nontail]
;;

let iter_until t ~f ~finish =
  (iteri_until [@kind k1 k2] [@inlined])
    t
    ~f:(fun _i x -> f x)
    ~finish:(fun _i -> finish ()) [@nontail]
;;

let fold_result t ~init ~f =
  let length = length t in
  let rec loop i acc =
    if i < length
    then (
      match ((f [@inlined hint]) acc (unsafe_get t i) : (_ Result.t[@kind k2])) with
      | Error _ as result -> result
      | Ok acc -> loop (i + 1) acc)
    else Ok acc
  in
  (loop [@inlined]) 0 init [@nontail]
;;

let find_map t ~(f @ local) : (_ Option.t[@kind k2]) =
  let length = length t in
  if length = 0
  then None
  else (
    let rec loop i : (_ Option.t[@kind k2]) =
      if i < length
      then (
        let value = unsafe_get t i in
        match ((f [@inlined hint]) value : (_ Option.t[@kind k2])) with
        | None -> loop (i + 1)
        | Some _ as result -> result)
      else None
    in
    (loop [@inlined]) 0 [@nontail])
;;

let find_map_exn =
  let not_found = Not_found_s (Atom "Array.find_map_exn: not found") in
  let find_map_exn t ~f =
    match (find_map [@inlined] [@kind k1 k2]) t ~f with
    | None ->
      (raise [@kind k2])
        (Portability_hacks.magic_uncontended__promise_deeply_immutable not_found)
    | Some x -> x
  in
  (* named to preserve symbol in compiled binary *)
  find_map_exn
;;

let find_mapi t ~f : (_ Option.t[@kind k2]) =
  let length = length t in
  if length = 0
  then None
  else (
    let i = ref 0 in
    let value_found = ref (None : (_ Option.t[@kind k2])) in
    while (Option.is_none [@kind k2]) !value_found && !i < length do
      let value = unsafe_get t !i in
      value_found := f !i value;
      incr i
    done;
    !value_found)
;;

let find_mapi_exn =
  let not_found = Not_found_s (Atom "Array.find_mapi_exn: not found") in
  let find_mapi_exn t ~f =
    match (find_mapi [@kind k1 k2]) t ~f with
    | None ->
      (raise [@kind k2])
        (Portability_hacks.magic_uncontended__promise_deeply_immutable not_found)
    | Some x -> x
  in
  (* named to preserve symbol in compiled binary *)
  find_mapi_exn
;;

let foldi t ~init ~f =
  let length = length t in
  let rec loop i acc =
    if i < length
    then (
      let acc = (f [@inlined hint]) i acc (unsafe_get t i) in
      loop (i + 1) acc)
    else acc
  in
  (loop [@inlined]) 0 init [@nontail]
;;]

[@@@kind.default k2 = base_with_ext]

let filter_mapi t ~f =
  let r = ref [||] in
  let k = ref 0 in
  for i = 0 to length t - 1 do
    match (f i (unsafe_get t i) : (_ Option.t[@kind k2 or value_or_null])) with
    | None -> ()
    | Some a ->
      if !k = 0 then r := create ~len:(length t) a;
      unsafe_set !r !k a;
      incr k
  done;
  if !k = length t then !r else if !k > 0 then (sub [@kind k2]) ~pos:0 ~len:!k !r else [||]
;;

let filter_map t ~f = (filter_mapi [@kind k1 k2]) t ~f:(fun _i a -> f a) [@nontail]
let concat_map t ~f = (concat [@kind k2]) (to_list ((map [@kind k1 value]) ~f t))
let concat_mapi t ~f = (concat [@kind k2]) (to_list ((mapi [@kind k1 value]) ~f t))

let check_length2_exn name t1 t2 =
  let n1 = length t1 in
  let n2 = length t2 in
  if n1 <> n2 then raise_length_mismatch name n1 n2
;;

(* [of_list_map] and [of_list_rev_map] are based on functions from the OCaml distribution. *)

let of_list_map (xs : (_ List.Constructors.t[@kind k1 or value_or_null])) ~f =
  match xs with
  | [] -> [||]
  | hd :: tl ->
    let a =
      create
        ~len:(1 + (List.length [@kind k1 or value_or_null]) tl)
        ((f [@inlined hint]) hd)
    in
    let rec fill i : (_ List.Constructors.t[@kind k1 or value_or_null]) -> _ = function
      | [] -> a
      | hd :: tl ->
        unsafe_set a i ((f [@inlined hint]) hd);
        fill (i + 1) tl
    in
    fill 1 tl [@nontail]
;;

let of_list_mapi (xs : (_ List.Constructors.t[@kind k1 or value_or_null])) ~f =
  match xs with
  | [] -> [||]
  | hd :: tl ->
    let a =
      create
        ~len:(1 + (List.length [@kind k1 or value_or_null]) tl)
        ((f [@inlined hint]) 0 hd)
    in
    let rec fill a i : (_ List.Constructors.t[@kind k1 or value_or_null]) -> _ = function
      | [] -> a
      | hd :: tl ->
        unsafe_set a i ((f [@inlined hint]) i hd);
        fill a (i + 1) tl
    in
    fill a 1 tl [@nontail]
;;

[%%template
[@@@kind.default k1 k2]
[@@@mode.default m = (global, local)]

let exists2_exn t1 t2 ~f =
  (check_length2_exn [@kind k1 k2]) "Array.exists2_exn" t1 t2;
  let rec loop i =
    if i >= 0 then f (unsafe_get t1 i) (unsafe_get t2 i) || loop (i - 1) else false
  in
  (loop [@inlined]) (length t1 - 1) [@nontail]
;;

let for_all2_exn t1 t2 ~f =
  (check_length2_exn [@kind k1 k2]) "Array.for_all2_exn" t1 t2;
  let rec loop i =
    if i >= 0 then f (unsafe_get t1 i) (unsafe_get t2 i) && loop (i - 1) else true
  in
  (loop [@inlined]) (length t1 - 1) [@nontail]
;;]]

let globalize = (globalize_array [@kind k]) [@@kind k = base]

[%%template
[@@@kind.default k = base]
[@@@mode.default m = (global, local)]

let equal = (equal_array [@kind k] [@mode m])
let compare = (compare_array [@kind k] [@mode m])]

[%%template
[@@@kind.default k1 = base_with_ext]

let filter t ~f =
  (filter_map [@kind k1 k1]) t ~f:(fun x -> if f x then Some x else None) [@nontail]
;;

let filteri t ~f =
  (filter_mapi [@kind k1 k1]) t ~f:(fun i x -> if f i x then Some x else None) [@nontail]
;;

[%%template
[@@@kind.default k1 = k1, k2 = base, k3 = base]

let foldi_until t ~init ~f ~finish =
  let length = length t in
  let rec loop i acc =
    if i < length
    then (
      match
        ((f [@inlined hint]) i acc (unsafe_get t i)
         : (_ Container.Continue_or_stop.t
           [@kind (k2 or value_or_null) (k3 or value_or_null)]))
      with
      | Continue acc -> loop (i + 1) acc
      | Stop res -> res)
    else finish i acc
  in
  (loop [@inlined]) 0 init [@nontail]
;;

let fold_until t ~init ~f ~finish =
  (foldi_until [@kind k1 k2 k3] [@inlined])
    t
    ~init
    ~f:(fun _i acc x -> f acc x)
    ~finish:(fun _i acc -> finish acc) [@nontail]
;;]

[@@@kind.default k2 = base_with_ext]

let of_list_rev_map xs ~f =
  let t = (of_list_map [@kind k1 k2]) xs ~f in
  (rev_inplace [@kind k2]) t;
  t
;;

let of_list_rev_mapi xs ~f =
  let t = (of_list_mapi [@kind k1 k2]) xs ~f in
  (rev_inplace [@kind k2]) t;
  t
;;

[@@@kind.default k3 = base_with_ext]

let partition_mapi t ~f =
  let (both : (_ Either0.t[@kind (k2 or value_or_null) (k3 or value_or_null)]) t) =
    (mapi [@kind k1 value]) t ~f
  in
  let firsts =
    (filter_map [@kind value k2]) both ~f:(function
      | First x -> (Some x : (_ Option.t[@kind k2 or value_or_null]))
      | Second _ -> None)
  in
  let seconds =
    (filter_map [@kind value k3]) both ~f:(function
      | First _ -> (None : (_ Option.t[@kind k3 or value_or_null]))
      | Second x -> Some x)
  in
  firsts, seconds
;;

let partition_map t ~f =
  (partition_mapi [@kind k1 k2 k3]) t ~f:(fun _ x -> f x) [@nontail]
;;]

[%%template
[@@@kind.default k = base_with_ext]

let partitioni_tf t ~f =
  (partition_mapi [@kind k k k]) t ~f:(fun i x -> if f i x then First x else Second x)
  [@nontail]
;;

let partition_tf t ~f = (partitioni_tf [@kind k]) t ~f:(fun _ x -> f x) [@nontail]]

(* These are copies of sexplib0 functions because we don't have access to ppx-template
   there.

   These are tested in core/test/test_array.ml because we want access to quicheck.
*)

[%%template
[@@@alloc.default a @ m = (heap_global, stack_local)]

let sexp_of_t (sexp_of_elt : _ @ m -> Sexp0.t @ m) (t @ m) : Sexp0.t =
  (let rec loop i res =
     if i < 0
     then res
     else loop (i - 1) (sexp_of_elt (unsafe_get t i) :: res) [@exclave_if_stack a]
   in
   List ((loop [@inlined]) (length t - 1) []))
  [@exclave_if_stack a]
[@@kind k = base_non_value]
;;]

[%%template
  let t_of_sexp elt_of_sexp (sexp : Sexp0.t) =
    match sexp with
    | List [] -> [||]
    | List (_ :: _ as l) -> (of_list_map [@kind value k]) l ~f:elt_of_sexp
    | Atom _ -> of_sexp_error "array_of_sexp: list needed" sexp
  [@@kind k = base_non_value]
  ;;]

(* We generated [findi]s that return [value & value]s, but for backwards compatibility we
   want to return the boxed product instead when dealing only with values. *)

let findi t ~f =
  match (findi [@inlined]) t ~f with
  | Some #(i, value) -> Some (i, value)
  | None -> None
;;

let findi_exn t ~f =
  let #(i, value) = findi_exn t ~f in
  i, value
;;

let merge a1 a2 ~compare =
  let l1 = Array.length a1 in
  let l2 = Array.length a2 in
  if l1 = 0
  then copy a2
  else if l2 = 0
  then copy a1
  else if compare (unsafe_get a2 0) (unsafe_get a1 (l1 - 1)) >= 0
  then append a1 a2
  else if compare (unsafe_get a1 0) (unsafe_get a2 (l2 - 1)) > 0
  then append a2 a1
  else (
    let len = l1 + l2 in
    let merged = create ~len (unsafe_get a1 0) in
    let a1_index = ref 0 in
    let a2_index = ref 0 in
    for i = 0 to len - 1 do
      let use_a1 =
        if l1 = !a1_index
        then false
        else if l2 = !a2_index
        then true
        else compare (unsafe_get a1 !a1_index) (unsafe_get a2 !a2_index) <= 0
      in
      if use_a1
      then (
        unsafe_set merged i (unsafe_get a1 !a1_index);
        a1_index := !a1_index + 1)
      else (
        unsafe_set merged i (unsafe_get a2 !a2_index);
        a2_index := !a2_index + 1)
    done;
    merged)
;;

let copy_matrix tt = map ~f:copy tt

let%template foldi_right (t @ local) ~(init @ m) ~(f @ local) =
  (let rec (aux @ local) (t @ local) ~idx ~(acc @ m) ~(f @ local) =
     (if idx < 0
      then acc
      else (
        (* [unsafe_get] is safe, since [idx >= 0 && idx < Array.length t] *)
        let acc = f idx (unsafe_get t idx) acc in
        aux t ~idx:(idx - 1) ~acc ~f))
     [@exclave_if_stack a]
   in
   aux t ~idx:(length t - 1) ~acc:init ~f [@nontail])
  [@exclave_if_stack a]
[@@alloc a @ m = (stack_local, heap_global)]
;;

let folding_mapi t ~init ~f =
  let acc = ref init in
  mapi t ~f:(fun i x ->
    let new_acc, y = f i !acc x in
    acc := new_acc;
    y)
  [@nontail]
;;

let fold_mapi t ~init ~f =
  let acc = ref init in
  let result =
    mapi t ~f:(fun i x ->
      let new_acc, y = f i !acc x in
      acc := new_acc;
      y)
  in
  !acc, result
;;

let filter_opt t = filter_map t ~f:Fn.id

let iter2_exn t1 t2 ~f =
  check_length2_exn "Array.iter2_exn" t1 t2;
  iteri t1 ~f:(fun i x1 -> f x1 (unsafe_get t2 i)) [@nontail]
;;

let map2_exn t1 t2 ~f =
  check_length2_exn "Array.map2_exn" t1 t2;
  init (length t1) ~f:(fun i -> f (unsafe_get t1 i) (unsafe_get t2 i)) [@nontail]
;;

let fold2_exn t1 t2 ~init ~f =
  check_length2_exn "Array.fold2_exn" t1 t2;
  foldi t1 ~init ~f:(fun i ac x -> f ac x (unsafe_get t2 i)) [@nontail]
;;

let map_inplace t ~f =
  for i = 0 to length t - 1 do
    unsafe_set t i (f (unsafe_get t i))
  done
;;

let find_consecutive_duplicate t ~equal =
  let n = length t in
  if n <= 1
  then None
  else (
    let result = ref None in
    let i = ref 1 in
    let prev = ref (unsafe_get t 0) in
    while !i < n do
      let cur = unsafe_get t !i in
      if equal cur !prev
      then (
        result := Some (!prev, cur);
        i := n)
      else (
        prev := cur;
        incr i)
    done;
    !result)
;;

let reduce t ~f =
  if length t = 0
  then None
  else (
    let r = ref (unsafe_get t 0) in
    for i = 1 to length t - 1 do
      r := f !r (unsafe_get t i)
    done;
    Some !r)
;;

let reduce_exn t ~f =
  match reduce t ~f with
  | None -> invalid_arg "Array.reduce_exn"
  | Some v -> v
;;

let permute = Array_permute.permute

let random_element_exn ?(random_state = Random.State.get_default ()) t =
  if is_empty t
  then failwith "Array.random_element_exn: empty array"
  else t.(Random.State.int random_state (length t))
;;

let random_element ?(random_state = Random.State.get_default ()) t =
  try Some (random_element_exn ~random_state t) with
  | _ -> None
;;

let zip t1 t2 =
  if length t1 <> length t2 then None else Some (map2_exn t1 t2 ~f:(fun x1 x2 -> x1, x2))
;;

let zip_exn t1 t2 =
  if length t1 <> length t2
  then failwith "Array.zip_exn"
  else map2_exn t1 t2 ~f:(fun x1 x2 -> x1, x2)
;;

let unzip t =
  let n = length t in
  if n = 0
  then [||], [||]
  else (
    let x, y = t.(0) in
    let res1 = create ~len:n x in
    let res2 = create ~len:n y in
    for i = 1 to n - 1 do
      let x, y = t.(i) in
      res1.(i) <- x;
      res2.(i) <- y
    done;
    res1, res2)
;;

let sorted_copy t ~compare =
  let t1 = copy t in
  sort t1 ~compare;
  t1
;;

let last_exn t = t.(length t - 1)
let last = last_exn

(* Convert to a sequence but does not attempt to protect against modification in the
   array. *)
let to_sequence_mutable t =
  Sequence.unfold_step ~init:0 ~f:(fun i ->
    if i >= length t
    then Sequence.Step.Done
    else Sequence.Step.Yield { value = t.(i); state = i + 1 })
;;

let to_sequence t = to_sequence_mutable (copy t)

let cartesian_product t1 t2 =
  if is_empty t1 || is_empty t2
  then [||]
  else (
    let n1 = length t1 in
    let n2 = length t2 in
    let t = create ~len:(n1 * n2) (t1.(0), t2.(0)) in
    let r = ref 0 in
    for i1 = 0 to n1 - 1 do
      for i2 = 0 to n2 - 1 do
        t.(!r) <- t1.(i1), t2.(i2);
        incr r
      done
    done;
    t)
;;

let transpose tt =
  if length tt = 0
  then Some [||]
  else (
    let width = length tt in
    let depth = length tt.(0) in
    if exists tt ~f:(fun t -> length t <> depth)
    then None
    else Some (init depth ~f:(fun d -> init width ~f:(fun w -> tt.(w).(d)))))
;;

let transpose_exn tt =
  match transpose tt with
  | None -> invalid_arg "Array.transpose_exn"
  | Some tt' -> tt'
;;

[@@@warning "-incompatible-with-upstream"]

let%template[@kind k1 = base_with_ext, k2 = base_with_ext] map t ~f =
  (map [@kind k1 k2]) t ~f
;;

include%template Binary_searchable.Make1 [@modality portable] (struct
    type nonrec 'a t = 'a t

    let get = get
    let length = length
  end)

let blito ~src ?(src_pos = 0) ?(src_len = length src - src_pos) ~dst ?(dst_pos = 0) () =
  blit ~src ~src_pos ~len:src_len ~dst ~dst_pos
;;

let split_n t_orig n =
  if n <= 0
  then [||], t_orig
  else (
    let length = length t_orig in
    if n >= length
    then t_orig, [||]
    else (
      let first = sub t_orig ~pos:0 ~len:n in
      let second = sub t_orig ~pos:n ~len:(length - n) in
      first, second))
;;

let chunks_of t ~length:chunk_length =
  if chunk_length <= 0
  then Printf.invalid_argf "Array.chunks_of: Expected length > 0, got %d" chunk_length ();
  let length = length t in
  if length = 0
  then [||]
  else (
    let num_chunks = (length + chunk_length - 1) / chunk_length in
    init num_chunks ~f:(fun i ->
      let start = i * chunk_length in
      let current_chunk_length = min chunk_length (length - start) in
      sub t ~pos:start ~len:current_chunk_length))
;;

let subo ?(pos = 0) ?len src =
  sub
    src
    ~pos
    ~len:
      (match len with
       | Some i -> i
       | None -> length src - pos)
;;

let sub t ~pos ~len = sub t ~pos ~len
let invariant invariant_a t = iter t ~f:invariant_a

module Private = struct
  module%template [@kind k = (value, value mod external64)] Sort = Sort [@kind k]

  module%template.portable [@kind k = (value, value mod external64)] [@modality p] Sorter =
    Sorter
    [@kind k]
    [@modality p]
end]

let array_should_be_polymorphic_over_value_or_null = ()
