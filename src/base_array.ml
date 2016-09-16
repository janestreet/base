open! Import

module Array = StdLabels.Array
module Base_sequence = Sequence

module List = Base_list

let invalid_argf = Base_printf.invalid_argf

let raise_s = Error.raise_s

type 'a t = 'a array [@@deriving compare, sexp]

(* This module implements a new in-place, constant heap sorting algorithm to replace the
   one used by the standard libraries.  Its only purpose is to be faster (hopefully
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
   - "Dual-Pivot Quicksort" by Vladimir Yaroslavskiy.
   Available at http://iaroslavski.narod.ru/quicksort/DualPivotQuicksort.pdf
   - "Quicksort is Optimal" by Sedgewick and Bentley.
   Slides at http://www.cs.princeton.edu/~rs/talks/QuicksortIsOptimal.pdf
   - http://www.sorting-algorithms.com/quick-sort-3-way

*)

module Sort = struct
  (* For the sake of speed we could use unsafe get/set throughout, but speed tests don't
     show a significant improvement. *)
  let get = Array.get
  let set = Array.set

  let swap arr i j =
    let tmp = get arr i in
    set arr i (get arr j);
    set arr j tmp
  ;;

  module type Sort = sig
    val sort
      :  'a t
      -> cmp:('a -> 'a -> int)
      -> left:int (* leftmost index of sub-array to sort *)
      -> right:int (* rightmost index of sub-array to sort *)
      -> unit
  end

  (* http://en.wikipedia.org/wiki/Insertion_sort *)
  module Insertion_sort : Sort = struct
    let sort arr ~cmp ~left ~right =
      let insert pos v =
        (* loop invariants:
           1.  the subarray arr[left .. i-1] is sorted
           2.  the subarray arr[i+1 .. pos] is sorted and contains only elements > v
           3.  arr[i] may be thought of as containing v
        *)
        let rec loop i =
          let i_next = i - 1 in
          if i_next >= left && cmp (get arr i_next) v > 0 then begin
            set arr i (get arr i_next);
            loop i_next
          end else
            i
        in
        let final_pos = loop pos in
        set arr final_pos v
      in
      (* loop invariant:
         arr is sorted from left to i-1, inclusive
      *)
      for i = left + 1 to right do
        insert i (get arr i)
      done
    ;;
  end

  (* http://en.wikipedia.org/wiki/Heapsort *)
  module Heap_sort : Sort = struct
    (* loop invariant:
       root's children are both either roots of max-heaps or > right
    *)
    let rec heapify arr ~cmp root ~left ~right =
      let relative_root = root - left in
      let left_child    = (2 * relative_root) + left + 1 in
      let right_child   = (2 * relative_root) + left + 2 in
      let largest =
        if left_child <= right && cmp (get arr left_child) (get arr root) > 0
        then left_child
        else root
      in
      let largest =
        if right_child <= right && cmp (get arr right_child) (get arr largest) > 0
        then right_child
        else largest
      in
      if largest <> root then begin
        swap arr root largest;
        heapify arr ~cmp largest ~left ~right
      end;
    ;;

    let build_heap arr ~cmp ~left ~right =
      (* Elements in the second half of the array are already heaps of size 1.  We move
         through the first half of the array from back to front examining the element at
         hand, and the left and right children, fixing the heap property as we go. *)
      for i = (left + right) / 2 downto left do
        heapify arr ~cmp i ~left ~right;
      done;
    ;;

    let sort arr ~cmp ~left ~right =
      build_heap arr ~cmp ~left ~right;
      (* loop invariants:
         1.  the subarray arr[left ... i] is a max-heap H
         2.  the subarray arr[i+1 ... right] is sorted (call it S)
         3.  every element of H is less than every element of S
      *)
      for i = right downto left + 1 do
        swap arr left i;
        heapify arr ~cmp left ~left ~right:(i - 1);
      done;
    ;;
  end

  (* http://en.wikipedia.org/wiki/Introsort *)
  module Intro_sort : Sort = struct

    let five_element_sort arr ~cmp m1 m2 m3 m4 m5 =
      let compare_and_swap i j =
        if cmp (get arr i) (get arr j) > 0 then swap arr i j
      in
      (* optimal 5-element sorting network *)
      compare_and_swap m1 m2;  (* 1--o-----o-----o--------------1 *)
      compare_and_swap m4 m5;  (*    |     |     |                *)
      compare_and_swap m1 m3;  (* 2--o-----|--o--|-----o--o-----2 *)
      compare_and_swap m2 m3;  (*          |  |  |     |  |       *)
      compare_and_swap m1 m4;  (* 3--------o--o--|--o--|--o-----3 *)
      compare_and_swap m3 m4;  (*                |  |  |          *)
      compare_and_swap m2 m5;  (* 4-----o--------o--o--|-----o--4 *)
      compare_and_swap m2 m3;  (*       |              |     |    *)
      compare_and_swap m4 m5;  (* 5-----o--------------o-----o--5 *)
    ;;

    let%test_module _ = (module struct
                          (* run [five_element_sort] on all permutations of an array of five elements *)

                          let rec sprinkle x xs =
                            (x :: xs) :: begin
                              match xs with
                              | [] -> []
                              | x' :: xs' ->
                                List.map (sprinkle x xs') ~f:(fun sprinkled -> x' :: sprinkled)
                            end

                          let rec permutations = function
                            | [] -> [[]]
                            | x :: xs ->
                              List.concat_map (permutations xs) ~f:(fun perms -> sprinkle x perms)

                          let all_perms = permutations [1;2;3;4;5]
                          let%test _ = List.length all_perms = 120
                          let%test _ = not (List.contains_dup ~compare:[%compare: int list] all_perms)

                          let%test _ =
                            List.for_all all_perms ~f:(fun l ->
                              let arr = Array.of_list l in
                              five_element_sort arr ~cmp:[%compare: int] 0 1 2 3 4;
                              arr = [|1;2;3;4;5|])
                        end)

    (* choose pivots for the array by sorting 5 elements and examining the center three
       elements.  The goal is to choose two pivots that will either:
       - break the range up into 3 even partitions
       or
       - eliminate a commonly appearing element by sorting it into the center partition
       by itself
       To this end we look at the center 3 elements of the 5 and return pairs of equal
       elements or the widest range *)
    let choose_pivots arr ~cmp ~left ~right =
      let sixth = (right - left) / 6 in
      let m1 = left + sixth in
      let m2 = m1 + sixth in
      let m3 = m2 + sixth in
      let m4 = m3 + sixth in
      let m5 = m4 + sixth in
      five_element_sort arr ~cmp m1 m2 m3 m4 m5;
      let m2_val = get arr m2 in
      let m3_val = get arr m3 in
      let m4_val = get arr m4 in
      if cmp m2_val m3_val = 0      then (m2_val, m3_val, true)
      else if cmp m3_val m4_val = 0 then (m3_val, m4_val, true)
      else                               (m2_val, m4_val, false)
    ;;

    let dual_pivot_partition arr ~cmp ~left ~right =
      let pivot1, pivot2, pivots_equal = choose_pivots arr ~cmp ~left ~right in
      (* loop invariants:
         1.  left <= l < r <= right
         2.  l <= p <= r
         3.  l <= x < p     implies arr[x] >= pivot1
         and arr[x] <= pivot2
         4.  left <= x < l  implies arr[x] < pivot1
         5.  r < x <= right implies arr[x] > pivot2
      *)
      let rec loop l p r =
        let pv = get arr p in
        if cmp pv pivot1 < 0 then begin
          swap arr p l;
          cont (l + 1) (p + 1) r
        end else if cmp pv pivot2 > 0 then begin
          (* loop invariants:  same as those of the outer loop *)
          let rec scan_backwards r =
            if r > p && cmp (get arr r) pivot2 > 0
            then scan_backwards (r - 1)
            else r
          in
          let r = scan_backwards r in
          swap arr r p;
          cont l p (r - 1)
        end else
          cont l (p + 1) r
      and cont l p r =
        if p > r then (l, r) else loop l p r
      in
      let (l, r) = cont left left right in
      (l, r, pivots_equal)
    ;;

    let rec intro_sort arr ~max_depth ~cmp ~left ~right =
      let len = right - left + 1 in
      (* This takes care of some edge cases, such as left > right or very short arrays,
         since Insertion_sort.sort handles these cases properly.  Thus we don't need to
         make sure that left and right are valid in recursive calls. *)
      if len <= 32 then begin
        Insertion_sort.sort arr ~cmp ~left ~right
      end else if max_depth < 0 then begin
        Heap_sort.sort arr ~cmp ~left ~right;
      end else begin
        let max_depth = max_depth - 1 in
        let (l, r, middle_sorted) = dual_pivot_partition arr ~cmp ~left ~right in
        intro_sort arr ~max_depth ~cmp ~left ~right:(l - 1);
        if not middle_sorted then intro_sort arr ~max_depth ~cmp ~left:l ~right:r;
        intro_sort arr ~max_depth ~cmp ~left:(r + 1) ~right;
      end
    ;;

    let log10_of_3 = log10 3.

    let log3 x = log10 x /. log10_of_3

    let sort arr ~cmp ~left ~right =
      let len = right - left + 1 in
      let heap_sort_switch_depth =
        (* with perfect 3-way partitioning, this is the recursion depth *)
        int_of_float (log3 (float_of_int len))
      in
      intro_sort arr ~max_depth:heap_sort_switch_depth ~cmp ~left ~right;
    ;;
  end

  module Test (M : Sort) = struct

    let%test_module _ = (module struct
                          let random_data ~length ~range =
                            let arr = Array.make length 0 in
                            for i = 0 to length - 1 do
                              arr.(i) <- Random.int range;
                            done;
                            arr
                          ;;

                          let assert_sorted arr =
                            M.sort arr ~left:0 ~right:(Array.length arr - 1) ~cmp:[%compare: int];
                            let len = Array.length arr in
                            let rec loop i prev =
                              if i = len then true
                              else if arr.(i) < prev then false
                              else loop (i + 1) arr.(i)
                            in
                            loop 0 (-1)
                          ;;

                          let%test _ = assert_sorted (random_data ~length:0 ~range:100)
                          let%test _ = assert_sorted (random_data ~length:1 ~range:100)
                          let%test _ = assert_sorted (random_data ~length:100 ~range:1_000)
                          let%test _ = assert_sorted (random_data ~length:1_000 ~range:1)
                          let%test _ = assert_sorted (random_data ~length:1_000 ~range:10)
                          let%test _ = assert_sorted (random_data ~length:1_000 ~range:1_000_000)
                        end)
  end

  module Insertion_test = Test (Insertion_sort)
  module Heap_test = Test (Heap_sort)
  module Intro_test = Test (Intro_sort)
end

let sort ?pos ?len arr ~cmp =
  let pos, len =
    Ordered_collection_common.get_pos_len_exn ?pos ?len ~length:(Array.length arr)
  in
  Sort.Intro_sort.sort arr ~cmp ~left:pos ~right:(pos + len - 1)

(* Standard functions *)
let append                = Array.append
let concat                = Array.concat
let copy                  = Array.copy
let fill                  = Array.fill
let fold_right t ~f ~init = Array.fold_right ~f t ~init (* permute params in signature *)
let init                  = Array.init
let iteri                 = Array.iteri
let make_matrix           = Array.make_matrix
let map                   = Array.map
let mapi                  = Array.mapi
let of_list               = Array.of_list
let stable_sort t ~cmp    = Array.stable_sort t ~cmp
let sub                   = Array.sub
let to_list               = Array.to_list

external create : int -> 'a -> 'a array = "caml_make_vect"

let create ~len x =
  try create len x
  with Invalid_argument _ ->
    invalid_argf "Array.create ~len:%d: invalid length" len ()
;;

external get : 'a array -> int -> 'a = "%array_safe_get"
external set : 'a array -> int -> 'a -> unit = "%array_safe_set"
external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"
external length : 'a array -> int = "%array_length"

let to_array t = t

let is_empty t = length t = 0

let is_sorted t ~cmp =
  let rec loop i =
    if i < 1 then
      true
    else
      cmp t.(i - 1) t.(i) <= 0 && loop (i - 1)
  in
  loop (length t - 1)

let%test _ = is_sorted [||] ~cmp:[%compare: int]
let%test _ = is_sorted [|0|] ~cmp:[%compare: int]
let%test _ = is_sorted [|0;1;2;2;4|] ~cmp:[%compare: int]
let%test _ = not (is_sorted [|0;1;2;3;2|] ~cmp:[%compare: int])

let is_sorted_strictly t ~cmp =
  let rec loop i =
    if i < 1 then
      true
    else
      cmp t.(i - 1) t.(i) < 0 && loop (i - 1)
  in
  loop (length t - 1)
;;

let%test_unit _ =
  List.iter
    ~f:(fun (t, expect) ->
      assert (expect = is_sorted_strictly (of_list t) ~cmp:[%compare: int]))
    [ []         , true;
      [ 1 ]      , true;
      [ 1; 2 ]   , true;
      [ 1; 1 ]   , false;
      [ 2; 1 ]   , false;
      [ 1; 2; 3 ], true;
      [ 1; 1; 3 ], false;
      [ 1; 2; 2 ], false;
    ]
;;

let fold t ~init ~f = Array.fold_left t ~init ~f

let fold_result t ~init ~f = Container.fold_result ~fold ~init ~f t
let fold_until  t ~init ~f = Container.fold_until  ~fold ~init ~f t
let count t ~f = Container.count ~fold t ~f
let sum m t ~f = Container.sum ~fold m t ~f
let min_elt t ~cmp = Container.min_elt ~fold t ~cmp
let max_elt t ~cmp = Container.max_elt ~fold t ~cmp

let foldi t ~init ~f =
  let rec loop i ac =
    if i = length t then
      ac
    else loop (i + 1) (f i ac t.(i))
  in
  loop 0 init
;;

let%test _ = foldi [||] ~init:13 ~f:(fun _ _ _ -> failwith "bad") = 13
let%test _ = foldi [| 13 |] ~init:17 ~f:(fun i ac x -> ac + i + x) = 30
let%test _ = foldi [| 13; 17 |] ~init:19 ~f:(fun i ac x -> ac + i + x) = 50

let counti t ~f = foldi t ~init:0 ~f:(fun idx count a -> if f idx a then count + 1 else count)

let%test _ = counti [|0;1;2;3;4|] ~f:(fun idx x -> idx = x) = 5
let%test _ = counti [|0;1;2;3;4|] ~f:(fun idx x -> idx = 4-x) = 1

let iter t ~f = Array.iter t ~f

let concat_map  t ~f = concat (to_list (map  ~f t))
let concat_mapi t ~f = concat (to_list (mapi ~f t))

(** [normalize array index] returns a new index into the array such that if index is less
    than zero, the returned index will "wrap around" -- i.e. array.(normalize array (-1))
    returns the last element of the array. *)
let normalize t i =
  Ordered_collection_common.normalize ~length_fun:length t i

(** [slice array start stop] returns a fresh array including elements [array.(start)]
    through [array.(stop-1)] with the small tweak that the start and stop positions are
    normalized and a stop index of 0 means the same thing a stop index of
    [Array.length array].  In summary, it's like the slicing in Python or Matlab. *)
let slice t start stop =
  Ordered_collection_common.slice ~length_fun:length ~sub_fun:sub
    t start stop

(** [nget array index] "normalizes" the index to {!Array.get} -- see normalize *)
let nget t i =
  t.(normalize t i)

(** [nset array index value] "normalizes" the index to {!Array.set} -- see normalize *)
let nset t i v =
  t.(normalize t i) <- v

let swap = Array_permute.swap;;

(** reverses an array in place. *)
let rev_inplace t =
  let i = ref 0 in
  let j = ref (length t - 1) in
  while !i < !j; do
    swap t !i !j;
    incr i;
    decr j;
  done
;;

let of_list_rev l =
  match l with
  | [] -> [||]
  | a :: l ->
    let len = 1 + List.length l in
    let t = create ~len a in
    let r = ref l in
    (* We start at [len - 2] because we already put [a] at [t.(len - 1)]. *)
    for i = len - 2 downto 0 do
      match !r with
      | [] -> assert false
      | a :: l -> t.(i) <- a; r := l
    done;
    t
;;

let%test_unit _ =
  for i = 0 to 5 do
    let l1 = List.init i ~f:Fn.id in
    let l2 = List.rev (to_list (of_list_rev l1)) in
    assert (l1 = l2);
  done
;;

(* [list_length] and [of_list_rev_map] are based on functions from the
   OCaml distribution. *)

(* Cannot use List.length here because the List module depends on Array. *)
let rec list_length accu = function
  | [] -> accu
  | _h::t -> list_length (succ accu) t

let of_list_map xs ~f =
  match xs with
  | [] -> [||]
  | hd::tl ->
    let a = create ~len:(list_length 1 tl) (f hd) in
    let rec fill i = function
      | [] -> a
      | hd::tl -> unsafe_set a i (f hd); fill (i+1) tl in
    fill 1 tl

let of_list_rev_map xs ~f =
  let t = of_list_map xs ~f in
  rev_inplace t;
  t

(* [Obj.truncate] reduces the size of a block on the ocaml heap.  For arrays, the block
   size is the array length. This holds even for float arrays. *)
let unsafe_truncate t ~len =
  if len <= 0 || len > length t then
    raise_s [%message "Array.unsafe_truncate got invalid len" (len : int)];
  if len < length t then Obj.truncate (Obj.repr t) len;
;;

let%test_unit _ =
  List.iter
    ~f:(fun (t, len) ->
      assert (Exn.does_raise (fun () -> unsafe_truncate t ~len)))
    [ [| |]  , -1
    ; [| |]  , 0
    ; [| |]  , 1
    ; [| 1 |], -1
    ; [| 1 |], 0
    ; [| 1 |], 2
    ]
;;

let%test_unit _ =
  for orig_len = 1 to 5 do
    for new_len = 1 to orig_len do
      let t = init orig_len ~f:Fn.id in
      unsafe_truncate t ~len:new_len;
      assert (length t = new_len);
      for i = 0 to new_len - 1 do
        assert (t.(i) = i);
      done;
    done;
  done
;;

let filter_mapi t ~f =
  let r = ref [||] in
  let k = ref 0 in
  for i = 0 to length t - 1 do
    match f i (unsafe_get t i) with
    | None -> ()
    | Some a ->
      if !k = 0 then begin
        r := create ~len:(length t) a
      end;
      unsafe_set !r !k a;
      incr k;
  done;
  if !k > 0 then begin
    unsafe_truncate !r ~len:!k;
    !r
  end else
    [||]

let filter_map t ~f =
  filter_mapi t ~f:(fun _i a -> f a)

let filter_opt t =
  filter_map t ~f:Fn.id

let%test _ = filter_opt [|Some 1; None; Some 2; None; Some 3|] = [|1; 2; 3|]
let%test _ = filter_opt [|Some 1; None; Some 2|] = [|1; 2|]
let%test _ = filter_opt [|Some 1|] = [|1|]
let%test _ = filter_opt [|None|] = [||]
let%test _ = filter_opt [||] = [||]

let iter2_exn t1 t2 ~f =
  if length t1 <> length t2 then invalid_arg "Array.iter2_exn";
  iteri t1 ~f:(fun i x1 -> f x1 t2.(i))

let map2_exn t1 t2 ~f =
  let len = length t1 in
  if length t2 <> len then invalid_arg "Array.map2_exn";
  init len ~f:(fun i -> f t1.(i) t2.(i))

let fold2_exn t1 t2 ~init ~f =
  if length t1 <> length t2 then invalid_arg "Array.fold2_exn";
  foldi t1 ~init ~f:(fun i ac x -> f ac x t2.(i))
;;

let%test _ = fold2_exn [||] [||] ~init:13 ~f:(fun _ -> failwith "fail") = 13
let%test _ = fold2_exn [| 1 |] [| "1" |] ~init:[] ~f:(fun ac a b -> (a, b) :: ac) = [ 1, "1" ]

let filter ~f = filter_map ~f:(fun x -> if f x then Some x else None)

let filteri ~f = filter_mapi ~f:(fun i x -> if f i x then Some x else None)

let%test _ = filter [| 0; 1 |] ~f:(fun n -> n < 2) = [| 0; 1 |]
let%test _ = filter [| 0; 1 |] ~f:(fun n -> n < 1) = [| 0 |]
let%test _ = filter [| 0; 1 |] ~f:(fun n -> n < 0) = [||]

let exists t ~f =
  let rec loop i =
    if i < 0
    then false
    else f t.(i) || loop (i - 1)
  in
  loop (length t - 1)

let existsi t ~f =
  let rec loop i =
    if i < 0
    then false
    else f i t.(i) || loop (i - 1)
  in
  loop (length t - 1)

let%test _ = existsi [||] ~f:(fun _ _ -> true) = false
let%test _ = existsi [|0;1;2;3|] ~f:(fun i x -> i <> x) = false
let%test _ = existsi [|0;1;3;3|] ~f:(fun i x -> i <> x) = true

let mem ?(equal = (=)) t a = exists t ~f:(equal a)

let for_all t ~f =
  let rec loop i =
    if i < 0
    then true
    else f t.(i) && loop (i - 1)
  in
  loop (length t - 1)

let for_alli t ~f =
  let rec loop i =
    if i < 0
    then true
    else f i t.(i) && loop (i - 1)
  in
  loop (length t - 1)

let%test _ = for_alli [||] ~f:(fun _ _ -> false) = true
let%test _ = for_alli [|0;1;2;3|] ~f:(fun i x -> i = x) = true
let%test _ = for_alli [|0;1;3;3|] ~f:(fun i x -> i = x) = false

let exists2_exn t1 t2 ~f =
  let len = length t1 in
  if length t2 <> len then invalid_arg "Array.exists2_exn";
  let rec loop i =
    if i < 0
    then false
    else f t1.(i) t2.(i) || loop (i - 1)
  in
  loop (len - 1)

let for_all2_exn t1 t2 ~f =
  let len = length t1 in
  if length t2 <> len then invalid_arg "Array.for_all2_exn";
  let rec loop i =
    if i < 0
    then true
    else f t1.(i) t2.(i) && loop (i - 1)
  in
  loop (len - 1)

let%test _ = exists2_exn [||] [||] ~f:(fun _ _ -> true) = false
let%test _ = exists2_exn [|0;2;4;6|] [|0;2;4;6|] ~f:(fun x y -> x <> y) = false
let%test _ = exists2_exn [|0;2;4;8|] [|0;2;4;6|] ~f:(fun x y -> x <> y) = true
let%test _ = exists2_exn [|2;2;4;6|] [|0;2;4;6|] ~f:(fun x y -> x <> y) = true
let%test _ = for_all2_exn [||] [||] ~f:(fun _ _ -> false) = true
let%test _ = for_all2_exn [|0;2;4;6|] [|0;2;4;6|] ~f:(fun x y -> x = y) = true
let%test _ = for_all2_exn [|0;2;4;8|] [|0;2;4;6|] ~f:(fun x y -> x = y) = false
let%test _ = for_all2_exn [|2;2;4;6|] [|0;2;4;6|] ~f:(fun x y -> x = y) = false

let equal t1 t2 ~equal = length t1 = length t2 && for_all2_exn t1 t2 ~f:equal

let%test _ = equal [||] [||] ~equal:(=)
let%test _ = equal [| 1 |] [| 1 |] ~equal:(=)
let%test _ = equal [| 1; 2 |] [| 1; 2 |] ~equal:(=)
let%test _ = not (equal [||] [| 1 |] ~equal:(=))
let%test _ = not (equal [| 1 |] [||] ~equal:(=))
let%test _ = not (equal [| 1 |] [| 1; 2 |] ~equal:(=))
let%test _ = not (equal [| 1; 2 |] [| 1; 3 |] ~equal:(=))

let replace t i ~f = t.(i) <- f t.(i)

(** modifies an array in place -- [t.(i)] will be set to [f(t.(i))] *)
let replace_all t ~f =
  for i = 0 to length t - 1 do
    t.(i) <- f t.(i)
  done

let findi t ~f =
  let length = length t in
  let rec loop i =
    if i >= length then None
    else if f i t.(i) then Some (i, t.(i))
    else loop (i + 1)
  in
  loop 0
;;

let findi_exn t ~f =
  match findi t ~f with
  | None -> raise Not_found
  | Some x -> x
;;

let find_exn t ~f =
  match findi t ~f:(fun _i x -> f x) with
  | None -> raise Not_found
  | Some (_i, x) -> x
;;

let find t ~f = Option.map (findi t ~f:(fun _i x -> f x)) ~f:(fun (_i, x) -> x)

let find_map t ~f =
  let length = length t in
  let rec loop i =
    if i >= length then None
    else
      match f t.(i) with
      | None -> loop (i + 1)
      | Some _ as res -> res
  in
  loop 0
;;

let find_map_exn t ~f =
  match find_map t ~f with
  | None -> raise Not_found
  | Some x -> x

let find_mapi t ~f =
  let length = length t in
  let rec loop i =
    if i >= length then None
    else
      match f i t.(i) with
      | None -> loop (i + 1)
      | Some _ as res -> res
  in
  loop 0
;;

let%test _ = find_mapi [|0;5;2;1;4|] ~f:(fun i x -> if i = x then Some (i+x) else None) = Some 0
let%test _ = find_mapi [|3;5;2;1;4|] ~f:(fun i x -> if i = x then Some (i+x) else None) = Some 4
let%test _ = find_mapi [|3;5;1;1;4|] ~f:(fun i x -> if i = x then Some (i+x) else None) = Some 8
let%test _ = find_mapi [|3;5;1;1;2|] ~f:(fun i x -> if i = x then Some (i+x) else None) = None

let find_mapi_exn t ~f =
  match find_mapi t ~f with
  | None -> raise Not_found
  | Some x -> x

let find_consecutive_duplicate t ~equal =
  let n = length t in
  if n <= 1
  then None
  else begin
    let result = ref None in
    let i = ref 1 in
    let prev = ref t.(0) in
    while !i < n do
      let cur = t.(!i) in
      if equal cur !prev
      then (result := Some (!prev, cur); i := n)
      else (prev := cur; incr i)
    done;
    !result
  end
;;

let%test_unit _ =
  List.iter
    ~f:(fun (l, expect) ->
      let t = of_list l in
      assert (Poly.equal expect (find_consecutive_duplicate t ~equal:Poly.equal)))
    [ []            , None
    ; [ 1 ]         , None
    ; [ 1; 1 ]      , Some (1, 1)
    ; [ 1; 2 ]      , None
    ; [ 1; 2; 1 ]   , None
    ; [ 1; 2; 2 ]   , Some (2, 2)
    ; [ 1; 1; 2; 2 ], Some (1, 1)
    ]
;;

let reduce t ~f =
  if length t = 0 then None
  else begin
    let r = ref t.(0) in
    for i = 1 to length t - 1 do
      r := f !r t.(i)
    done;
    Some !r
  end

let reduce_exn t ~f =
  match reduce t ~f with
  | None -> invalid_arg "Array.reduce_exn"
  | Some v -> v

let permute = Array_permute.permute

let random_element_exn ?(random_state = Base_random.State.default) t =
  if is_empty t
  then failwith "Array.random_element_exn: empty array"
  else t.(Base_random.State.int random_state (length t))

let random_element ?(random_state = Base_random.State.default) t =
  try Some (random_element_exn ~random_state t)
  with _ -> None

let%test _ = random_element [| |] = None
let%test _ = random_element [| 0 |] = Some 0

let zip t1 t2 =
  if length t1 <> length t2 then None
  else Some (map2_exn t1 t2 ~f:(fun x1 x2 -> x1, x2))

let zip_exn t1 t2 =
  if length t1 <> length t2 then failwith "Array.zip_exn"
  else map2_exn t1 t2 ~f:(fun x1 x2 -> x1, x2)

let unzip t =
  let n = length t in
  if n = 0 then [||], [||]
  else
    let x, y = t.(0) in
    let res1 = create ~len:n x in
    let res2 = create ~len:n y in
    for i = 1 to n - 1 do
      let x, y = t.(i) in
      res1.(i) <- x;
      res2.(i) <- y;
    done;
    res1, res2

let sorted_copy t ~cmp =
  let t1 = copy t in
  sort t1 ~cmp;
  t1

let partitioni_tf t ~f =
  let both = mapi t ~f:(fun i x -> if f i x then Either.First x else Either.Second x) in
  let trues = filter_map both ~f:(function First x -> Some x | Second _ -> None) in
  let falses = filter_map both ~f:(function First _ -> None | Second x -> Some x) in
  (trues, falses)

let partition_tf t ~f =
  partitioni_tf t ~f:(fun _i x -> f x)

let last t = t.(length t - 1)

(* Convert to a sequence but does not attempt to protect against modification
   in the array. *)
let to_sequence_mutable t =
  Sequence.unfold_step ~init:0 ~f:(fun i ->
    if i >= Array.length t
    then Sequence.Step.Done
    else Sequence.Step.Yield (t.(i), i+1))

let to_sequence t = to_sequence_mutable (copy t)

let%test_unit _ =
  List.iter
    [ [||]
    ; [| 1 |]
    ; [| 1; 2; 3; 4; 5 |]
    ]
    ~f:(fun t ->
      assert (Sequence.to_array (to_sequence t) = t))
;;

(* As far as I can tell, all empty arrays are physically equal in recent versions of the
   OCaml compiler, whether created with [ [||] ] or with [init 0]. *)
let empty () = [||]

let cartesian_product t1 t2 =
  if is_empty t1 || is_empty t2 then
    [||]
  else
    let n1 = length t1 in
    let n2 = length t2 in
    let t = create ~len:(n1 * n2) (t1.(0), t2.(0)) in
    let r = ref 0 in
    for i1 = 0 to n1 - 1 do
      for i2 = 0 to n2 - 1 do
        t.(!r) <- (t1.(i1), t2.(i2));
        incr r;
      done
    done;
    t
;;

let transpose tt =
  if length tt = 0
  then Some [||]
  else
    let width = length tt in
    let depth = length tt.(0) in
    if exists tt ~f:(fun t -> length t <> depth)
    then None
    else
      Some
        (Array.init depth ~f:(fun d -> Array.init width ~f:(fun w -> tt.(w).(d))))

let transpose_exn tt =
  match transpose tt with
  | None -> invalid_arg "Array.transpose_exn";
  | Some tt' -> tt'

include Binary_searchable.Make1 (struct
    type nonrec 'a t = 'a t

    let get = get
    let length = length

    module For_test = struct
      let of_array a = a
    end
  end)

include
  Blit.Make1
    (struct
      type nonrec 'a t = 'a t [@@deriving sexp_of]
      type 'a z = 'a
      let length = length
      let get    = get
      let set    = set
      let create_like ~len t =
        if len = 0
        then [||]
        else (assert (length t > 0); create ~len t.(0))
      ;;
      let unsafe_blit = Array.blit
      let create_bool ~len = create ~len false
    end)
;;

let invariant invariant_a t = iter t ~f:invariant_a

let max_length = Sys.max_array_length
