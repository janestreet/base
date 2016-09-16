open! Import

module List   = StdLabels.List
module String = StdLabels.String

module Random = Base_random

let invalid_argf = Base_printf.invalid_argf

module T = struct
  type 'a t = 'a list [@@deriving sexp]
end

module Or_unequal_lengths = struct
  type 'a t =
    | Ok of 'a
    | Unequal_lengths
  [@@deriving sexp_of]
end

include T

let of_list t = t

let range' ~compare ~stride ?(start=`inclusive) ?(stop=`exclusive) start_i stop_i =
  let next_i = stride start_i in
  let order x y = Ordering.of_int (compare x y) in
  let raise_stride_cannot_return_same_value () =
    invalid_arg "Base_list.range': stride function cannot return the same value"
  in
  let initial_stride_order =
    match order start_i next_i with
    | Equal -> raise_stride_cannot_return_same_value ()
    | Less -> `Less
    | Greater -> `Greater
  in
  let rec loop i accum =
    let i_to_stop_order = order i stop_i in
    match i_to_stop_order, initial_stride_order with
    | (Less, `Less)
    | (Greater, `Greater) -> begin
      (* haven't yet reached [stop_i]. Continue. *)
      let next_i = stride i in
      match order i next_i, initial_stride_order with
      | Equal, _ -> raise_stride_cannot_return_same_value ()
      | Less, `Greater
      | Greater, `Less ->
        invalid_arg "Base_list.range': stride function cannot change direction"
      | Less, `Less
      | Greater, `Greater -> loop next_i (i :: accum)
      end
    | (Less, `Greater)
    | (Greater, `Less) ->
      (* stepped past [stop_i].  Finished. *)
      accum
    | (Equal, _) ->
      (* reached [stop_i].  Finished. *)
      match stop with
      | `inclusive -> i :: accum
      | `exclusive -> accum
  in
  let start_i =
    match start with
    | `inclusive -> start_i
    | `exclusive -> next_i
  in
  List.rev (loop start_i [])
;;

let range ?(stride=1) ?(start=`inclusive) ?(stop=`exclusive) start_i stop_i =
  if stride = 0 then invalid_arg "Base_list.range: stride must be non-zero";
  range' ~compare ~stride:(fun x -> x + stride) ~start ~stop start_i stop_i
;;

let%test_module "range symmetries" = (module struct

  let basic ~stride ~start ~stop ~start_n ~stop_n ~result =
    range ~stride ~start ~stop start_n stop_n = result

  let test stride (start_n, start) (stop_n, stop) result =
    basic ~stride ~start ~stop ~start_n ~stop_n ~result
    && (* works for negative [start] and [stop] *)
      basic ~stride:(-stride)
        ~start_n:(-start_n)
        ~stop_n:(-stop_n)
        ~start
        ~stop
        ~result:(List.map result ~f:(fun x -> -x))

  let%test _ = test 1    ( 3, `inclusive) ( 1, `exclusive) []
  let%test _ = test 1    ( 3, `inclusive) ( 3, `exclusive) []
  let%test _ = test 1    ( 3, `inclusive) ( 4, `exclusive) [3]
  let%test _ = test 1    ( 3, `inclusive) ( 8, `exclusive) [3;4;5;6;7]
  let%test _ = test 3    ( 4, `inclusive) (10, `exclusive) [4;7]
  let%test _ = test 3    ( 4, `inclusive) (11, `exclusive) [4;7;10]
  let%test _ = test 3    ( 4, `inclusive) (12, `exclusive) [4;7;10]
  let%test _ = test 3    ( 4, `inclusive) (13, `exclusive) [4;7;10]
  let%test _ = test 3    ( 4, `inclusive) (14, `exclusive) [4;7;10;13]

  let%test _ = test (-1) ( 1, `inclusive) ( 3, `exclusive) []
  let%test _ = test (-1) ( 3, `inclusive) ( 3, `exclusive) []
  let%test _ = test (-1) ( 4, `inclusive) ( 3, `exclusive) [4]
  let%test _ = test (-1) ( 8, `inclusive) ( 3, `exclusive) [8;7;6;5;4]
  let%test _ = test (-3) (10, `inclusive) ( 4, `exclusive) [10;7]
  let%test _ = test (-3) (10, `inclusive) ( 3, `exclusive) [10;7;4]
  let%test _ = test (-3) (10, `inclusive) ( 2, `exclusive) [10;7;4]
  let%test _ = test (-3) (10, `inclusive) ( 1, `exclusive) [10;7;4]
  let%test _ = test (-3) (10, `inclusive) ( 0, `exclusive) [10;7;4;1]

  let%test _ = test 1    ( 3, `exclusive) ( 1, `exclusive) []
  let%test _ = test 1    ( 3, `exclusive) ( 3, `exclusive) []
  let%test _ = test 1    ( 3, `exclusive) ( 4, `exclusive) []
  let%test _ = test 1    ( 3, `exclusive) ( 8, `exclusive) [4;5;6;7]
  let%test _ = test 3    ( 4, `exclusive) (10, `exclusive) [7]
  let%test _ = test 3    ( 4, `exclusive) (11, `exclusive) [7;10]
  let%test _ = test 3    ( 4, `exclusive) (12, `exclusive) [7;10]
  let%test _ = test 3    ( 4, `exclusive) (13, `exclusive) [7;10]
  let%test _ = test 3    ( 4, `exclusive) (14, `exclusive) [7;10;13]

  let%test _ = test (-1) ( 1, `exclusive) ( 3, `exclusive) []
  let%test _ = test (-1) ( 3, `exclusive) ( 3, `exclusive) []
  let%test _ = test (-1) ( 4, `exclusive) ( 3, `exclusive) []
  let%test _ = test (-1) ( 8, `exclusive) ( 3, `exclusive) [7;6;5;4]
  let%test _ = test (-3) (10, `exclusive) ( 4, `exclusive) [7]
  let%test _ = test (-3) (10, `exclusive) ( 3, `exclusive) [7;4]
  let%test _ = test (-3) (10, `exclusive) ( 2, `exclusive) [7;4]
  let%test _ = test (-3) (10, `exclusive) ( 1, `exclusive) [7;4]
  let%test _ = test (-3) (10, `exclusive) ( 0, `exclusive) [7;4;1]

  let%test _ = test 1    ( 3, `inclusive) ( 1, `inclusive) []
  let%test _ = test 1    ( 3, `inclusive) ( 3, `inclusive) [3]
  let%test _ = test 1    ( 3, `inclusive) ( 4, `inclusive) [3;4]
  let%test _ = test 1    ( 3, `inclusive) ( 8, `inclusive) [3;4;5;6;7;8]
  let%test _ = test 3    ( 4, `inclusive) (10, `inclusive) [4;7;10]
  let%test _ = test 3    ( 4, `inclusive) (11, `inclusive) [4;7;10]
  let%test _ = test 3    ( 4, `inclusive) (12, `inclusive) [4;7;10]
  let%test _ = test 3    ( 4, `inclusive) (13, `inclusive) [4;7;10;13]
  let%test _ = test 3    ( 4, `inclusive) (14, `inclusive) [4;7;10;13]

  let%test _ = test (-1) ( 1, `inclusive) ( 3, `inclusive) []
  let%test _ = test (-1) ( 3, `inclusive) ( 3, `inclusive) [3]
  let%test _ = test (-1) ( 4, `inclusive) ( 3, `inclusive) [4;3]
  let%test _ = test (-1) ( 8, `inclusive) ( 3, `inclusive) [8;7;6;5;4;3]
  let%test _ = test (-3) (10, `inclusive) ( 4, `inclusive) [10;7;4]
  let%test _ = test (-3) (10, `inclusive) ( 3, `inclusive) [10;7;4]
  let%test _ = test (-3) (10, `inclusive) ( 2, `inclusive) [10;7;4]
  let%test _ = test (-3) (10, `inclusive) ( 1, `inclusive) [10;7;4;1]
  let%test _ = test (-3) (10, `inclusive) ( 0, `inclusive) [10;7;4;1]

  let%test _ = test 1    ( 3, `exclusive) ( 1, `inclusive) []
  let%test _ = test 1    ( 3, `exclusive) ( 3, `inclusive) []
  let%test _ = test 1    ( 3, `exclusive) ( 4, `inclusive) [4]
  let%test _ = test 1    ( 3, `exclusive) ( 8, `inclusive) [4;5;6;7;8]
  let%test _ = test 3    ( 4, `exclusive) (10, `inclusive) [7;10]
  let%test _ = test 3    ( 4, `exclusive) (11, `inclusive) [7;10]
  let%test _ = test 3    ( 4, `exclusive) (12, `inclusive) [7;10]
  let%test _ = test 3    ( 4, `exclusive) (13, `inclusive) [7;10;13]
  let%test _ = test 3    ( 4, `exclusive) (14, `inclusive) [7;10;13]

  let%test _ = test (-1) ( 1, `exclusive) ( 3, `inclusive) []
  let%test _ = test (-1) ( 3, `exclusive) ( 3, `inclusive) []
  let%test _ = test (-1) ( 4, `exclusive) ( 3, `inclusive) [3]
  let%test _ = test (-1) ( 8, `exclusive) ( 3, `inclusive) [7;6;5;4;3]
  let%test _ = test (-3) (10, `exclusive) ( 4, `inclusive) [7;4]
  let%test _ = test (-3) (10, `exclusive) ( 3, `inclusive) [7;4]
  let%test _ = test (-3) (10, `exclusive) ( 2, `inclusive) [7;4]
  let%test _ = test (-3) (10, `exclusive) ( 1, `inclusive) [7;4;1]
  let%test _ = test (-3) (10, `exclusive) ( 0, `inclusive) [7;4;1]

  let test_start_inc_exc stride start (stop, stop_inc_exc) result =
    test stride (start, `inclusive) (stop, stop_inc_exc) result
    && begin
        match result with
        | [] -> true
        | head :: tail ->
          head = start && test stride (start, `exclusive) (stop, stop_inc_exc) tail
      end

  let test_inc_exc stride start stop result =
    test_start_inc_exc stride start (stop, `inclusive) result
    && begin
        match List.rev result with
        | [] -> true
        | last :: all_but_last ->
          let all_but_last = List.rev all_but_last in
          if last = stop then
            test_start_inc_exc stride start (stop, `exclusive) all_but_last
          else
            true
      end

  let%test _ = test_inc_exc 1 4 10 [4;5;6;7;8;9;10]
  let%test _ = test_inc_exc 3 4 10 [4;7;10]
  let%test _ = test_inc_exc 3 4 11 [4;7;10]
  let%test _ = test_inc_exc 3 4 12 [4;7;10]
  let%test _ = test_inc_exc 3 4 13 [4;7;10;13]
  let%test _ = test_inc_exc 3 4 14 [4;7;10;13]

end)

module Test_values = struct
  let long1 =
    let v = lazy (range 1 100_000) in
    fun () -> Lazy.force v

  let l1 = [1;2;3;4;5;6;7;8;9;10]
end

(* Standard functions *)
let length = List.length
let hd_exn = List.hd
let tl_exn = List.tl

let hd t =
  match t with
  | [] -> None
  | x :: _ -> Some x
;;

let tl t =
  match t with
  | [] -> None
  | _ :: t' -> Some t'
;;

let nth t n =
  if n < 0 then None else
  let rec nth_aux t n =
    match t with
    | [] -> None
    | a :: t -> if n = 0 then Some a else nth_aux t (n-1)
  in nth_aux t n
;;

let nth_exn t n =
  match nth t n with
  | None ->
      invalid_argf "List.nth_exn %d called on list of length %d"
        n (length t) ()
  | Some a -> a
;;

let rev_append = List.rev_append

let%test _ = rev_append [1;2;3] [4;5;6] = [3;2;1;4;5;6]
let%test _ = rev_append [] [4;5;6] = [4;5;6]
let%test _ = rev_append [1;2;3] [] = [3;2;1]
let%test _ = rev_append [1] [2;3] = [1;2;3]
let%test _ = rev_append [1;2] [3] = [2;1;3]
let%test _ =
  let long = Test_values.long1 () in
  ignore (rev_append long long:int list);
  true

let rev = function
  | [] | [_] as res -> res
  | x :: y :: rest -> rev_append rest [y; x]

let unordered_append l1 l2 =
  match l1, l2 with
  | [], l | l, [] -> l
  | _             -> List.rev_append l1 l2

let rev_map t ~f = List.rev_map t ~f

let check_length2_exn name l1 l2 =
  let n1 = length l1 in
  let n2 = length l2 in
  if n1 <> n2
  then raise (invalid_argf "length mismatch in %s: %d <> %d " name n1 n2 ())
;;

let check_length3_exn name l1 l2 l3 =
  let n1 = length l1 in
  let n2 = length l2 in
  let n3 = length l3 in
  if n1 <> n2 || n2 <> n3
  then raise (invalid_argf "length mismatch in %s: %d <> %d || %d <> %d"
                name n1 n2 n2 n3 ())
;;

let check_length2 l1 l2 ~f =
  if length l1 <> length l2
  then Or_unequal_lengths.Unequal_lengths
  else Ok (f l1 l2)
;;

let check_length3 l1 l2 l3 ~f =
  let n1 = length l1 in
  let n2 = length l2 in
  let n3 = length l3 in
  if n1 <> n2 || n2 <> n3
  then Or_unequal_lengths.Unequal_lengths
  else Ok (f l1 l2 l3)
;;

let iter2_ok l1 l2 ~f = List.iter2 l1 l2 ~f

let iter2 l1 l2 ~f = check_length2 l1 l2 ~f:(iter2_ok ~f)

let iter2_exn l1 l2 ~f =
  check_length2_exn "iter2_exn" l1 l2;
  iter2_ok l1 l2 ~f;
;;

let rev_map2_ok l1 l2 ~f = List.rev_map2 l1 l2 ~f

let rev_map2 l1 l2 ~f = check_length2 l1 l2 ~f:(rev_map2_ok ~f)

let rev_map2_exn l1 l2 ~f  =
  check_length2_exn "rev_map2_exn" l1 l2;
  rev_map2_ok l1 l2 ~f;
;;

let fold2_ok l1 l2 ~init ~f = List.fold_left2 l1 l2 ~init ~f

let fold2 l1 l2 ~init ~f = check_length2 l1 l2 ~f:(fold2_ok ~init ~f)

let fold2_exn l1 l2 ~init ~f =
  check_length2_exn "fold2_exn" l1 l2;
  fold2_ok l1 l2 ~init ~f;
;;

let for_all2_ok l1 l2 ~f = List.for_all2 l1 l2 ~f

let for_all2 l1 l2 ~f = check_length2 l1 l2 ~f:(for_all2_ok ~f)

let for_all2_exn l1 l2 ~f =
  check_length2_exn "for_all2_exn" l1 l2;
  for_all2_ok l1 l2 ~f;
;;

let%test _ = for_all2_exn [] [] ~f:(fun _ _ -> assert false)

let exists2_ok l1 l2 ~f = List.exists2 l1 l2 ~f

let exists2 l1 l2 ~f = check_length2 l1 l2 ~f:(exists2_ok ~f)

let exists2_exn l1 l2 ~f =
  check_length2_exn "exists2_exn" l1 l2;
  exists2_ok l1 l2 ~f;
;;

let mem ?(equal = (=)) t a = List.exists t ~f:(equal a)

(* This is a copy of the code from the standard library, with an extra eta-expansion to
   avoid creating partial closures (showed up for List.filter in profiling). *)
let rev_filter t ~f =
  let rec find ~f accu = function
    | [] -> accu
    | x :: l -> if f x then find ~f (x :: accu) l else find ~f accu l
  in
  find ~f [] t
;;

let filter t ~f = rev (rev_filter t ~f)

let sort = List.sort
let stable_sort = List.stable_sort

let find_map t ~f =
  let rec loop = function
    | [] -> None
    | x :: l ->
        match f x with
        | None -> loop l
        | Some _ as r -> r
  in
  loop t
;;

let find_map_exn t ~f =
  match find_map t ~f with
  | None -> raise Not_found
  | Some x -> x

let find t ~f =
  let rec loop = function
    | [] -> None
    | x :: l -> if f x then Some x else loop l
  in
  loop t
;;

let find_exn t ~f = List.find t ~f

let findi t ~f =
  let rec loop i t =
    match t with
    | [] -> None
    | x :: l -> if f i x then Some (i, x) else loop (i + 1) l
  in
  loop 0 t
;;

let find_mapi t ~f =
  let rec loop i t =
    match t with
    | [] -> None
    | x :: l ->
      match f i x with
      | Some _ as result -> result
      | None -> loop (i + 1) l
  in
  loop 0 t
;;

let%test _ = find_mapi [0;5;2;1;4] ~f:(fun i x -> if i = x then Some (i+x) else None) = Some 0
let%test _ = find_mapi [3;5;2;1;4] ~f:(fun i x -> if i = x then Some (i+x) else None) = Some 4
let%test _ = find_mapi [3;5;1;1;4] ~f:(fun i x -> if i = x then Some (i+x) else None) = Some 8
let%test _ = find_mapi [3;5;1;1;2] ~f:(fun i x -> if i = x then Some (i+x) else None) = None

let find_mapi_exn t ~f =
  match find_mapi t ~f with
  | None -> raise Not_found
  | Some x -> x

let for_alli t ~f =
  let rec loop i t =
    match t with
    | [] -> true
    | hd :: tl -> f i hd && loop (i+1) tl
  in
  loop 0 t

let existsi t ~f =
  let rec loop i t =
    match t with
    | [] -> false
    | hd :: tl -> f i hd || loop (i+1) tl
  in
  loop 0 t

let%test _ = for_alli [] ~f:(fun _ _ -> false) = true
let%test _ = for_alli [0;1;2;3] ~f:(fun i x -> i = x) = true
let%test _ = for_alli [0;1;3;3] ~f:(fun i x -> i = x) = false
let%test _ = existsi [] ~f:(fun _ _ -> true) = false
let%test _ = existsi [0;1;2;3] ~f:(fun i x -> i <> x) = false
let%test _ = existsi [0;1;3;3] ~f:(fun i x -> i <> x) = true

(** changing the order of arguments on some standard [List] functions. *)
let exists t ~f = List.exists t ~f
let for_all t ~f = List.for_all t ~f
let iter t ~f = List.iter t ~f

(** For the container interface. *)
let fold t ~init ~f = List.fold_left t ~f ~init
let fold_left = fold
let to_array = Caml.Array.of_list
let to_list t = t

(** Tail recursive versions of standard [List] module *)

let slow_append l1 l2 = List.rev_append (List.rev l1) l2

(* There are a few optimized list operations here, including append and map.  There are
   basically two optimizations in play: loop unrolling, and dynamic switching between
   stack and heap allocation.

   The loop-unrolling is straightforward, we just unroll 5 levels of the loop.  This makes
   each iteration faster, and also reduces the number of stack frames consumed per list
   element.

   The dynamic switching is done by counting the number of stack frames, and then
   switching to the "slow" implementation when we exceed a given limit.  This means that
   short lists use the fast stack-allocation method, and long lists use a slower one that
   doesn't require stack space.
*)
let rec count_append l1 l2 count =
  match l2 with
  | [] -> l1
  | _ ->
    match l1 with
    | []               ->                         l2
    | [x1]             -> x1                   :: l2
    | [x1; x2]         -> x1 :: x2             :: l2
    | [x1; x2; x3]     -> x1 :: x2 :: x3       :: l2
    | [x1; x2; x3; x4] -> x1 :: x2 :: x3 :: x4 :: l2
    | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
      x1 :: x2 :: x3 :: x4 :: x5 ::
        (if count > 1000
         then slow_append tl l2
         else count_append tl l2 (count + 1))

let append l1 l2 = count_append l1 l2 0

let%test _ = append [1;2;3] [4;5;6] = [1;2;3;4;5;6]
let%test _ = append [] [4;5;6] = [4;5;6]
let%test _ = append [1;2;3] [] = [1;2;3]
let%test _ = append [1] [2;3] = [1;2;3]
let%test _ = append [1;2] [3] = [1;2;3]
let%test_unit _ =
  let long = Test_values.long1 () in
  ignore (append long long:int list)

let map_slow l ~f = List.rev (List.rev_map ~f l)

let rec count_map ~f l ctr =
  match l with
  | [] -> []
  | [x1] ->
    let f1 = f x1 in
    [f1]
  | [x1; x2] ->
    let f1 = f x1 in
    let f2 = f x2 in
    [f1; f2]
  | [x1; x2; x3] ->
    let f1 = f x1 in
    let f2 = f x2 in
    let f3 = f x3 in
    [f1; f2; f3]
  | [x1; x2; x3; x4] ->
    let f1 = f x1 in
    let f2 = f x2 in
    let f3 = f x3 in
    let f4 = f x4 in
    [f1; f2; f3; f4]
  | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
    let f1 = f x1 in
    let f2 = f x2 in
    let f3 = f x3 in
    let f4 = f x4 in
    let f5 = f x5 in
    f1 :: f2 :: f3 :: f4 :: f5 ::
      (if ctr > 1000
        then map_slow ~f tl
        else count_map ~f tl (ctr + 1))

let map l ~f = count_map ~f l 0

let%test _ = map ~f:(fun x -> x) Test_values.l1 = Test_values.l1
let%test _ = map ~f:(fun x -> x) [] = []
let%test _ = map ~f:(fun x -> x +. 5.) [1.;2.;3.] = [6.;7.;8.]
let%test_unit _ =
  ignore (map ~f:(fun x -> x) (Test_values.long1 ()):int list)

let (>>|) l f = map l ~f

let map2_ok l1 l2 ~f = List.rev (rev_map2_ok l1 l2 ~f)

let map2 l1 l2 ~f = check_length2 l1 l2 ~f:(map2_ok ~f)

let map2_exn l1 l2 ~f =
  check_length2_exn "map2_exn" l1 l2;
  map2_ok l1 l2 ~f
;;

let%test _ = map2_exn ~f:(fun a b -> a, b) [1;2;3] ['a';'b';'c']
    = [(1,'a'); (2,'b'); (3,'c')]
let%test _ = map2_exn ~f:(fun _ _ -> ()) [] [] = []
let%test_unit _ =
  let long = Test_values.long1 () in
  ignore (map2_exn ~f:(fun _ _ -> ()) long long:unit list)

let rev_map3_ok l1 l2 l3 ~f =
  let rec loop l1 l2 l3 ac =
    match (l1, l2, l3) with
    | ([], [], []) -> ac
    | (x1 :: l1, x2 :: l2, x3 :: l3) -> loop l1 l2 l3 (f x1 x2 x3 :: ac)
    | _ -> assert false
  in
  loop l1 l2 l3 [];
;;

let rev_map3 l1 l2 l3 ~f =
  check_length3 l1 l2 l3 ~f:(rev_map3_ok ~f)
;;

let rev_map3_exn l1 l2 l3 ~f =
  check_length3_exn "rev_map3_exn" l1 l2 l3;
  rev_map3_ok l1 l2 l3 ~f
;;

let map3_ok l1 l2 l3 ~f = List.rev (rev_map3_ok l1 l2 l3 ~f)

let map3 l1 l2 l3 ~f = check_length3 l1 l2 l3 ~f:(map3_ok ~f)

let map3_exn l1 l2 l3 ~f =
  check_length3_exn "map3_exn" l1 l2 l3;
  map3_ok l1 l2 l3 ~f;
;;

let rec rev_map_append l1 l2 ~f =
  match l1 with
  | [] -> l2
  | h :: t -> rev_map_append ~f t (f h :: l2)

let%test _ = rev_map_append [1;2;3;4;5] [6] ~f:(fun x -> x) = [5;4;3;2;1;6]
let%test _ = rev_map_append [1;2;3;4;5] [6] ~f:(fun x -> 2 * x) = [10;8;6;4;2;6]
let%test _ = rev_map_append [] [6] ~f:(fun _ -> failwith "bug!") = [6]

let fold_right l ~f ~init =
  match l with
  | [] -> init (* avoid the allocation of [~f] below *)
  | _ -> fold ~f:(fun a b -> f b a) ~init (List.rev l)

let%test _ = fold_right ~f:(fun e acc -> e :: acc) Test_values.l1 ~init:[] =
  Test_values.l1
let%test _ = fold_right ~f:(fun e acc -> e ^ acc) ["1";"2"] ~init:"3" = "123"
let%test _ = fold_right ~f:(fun _ _ -> ()) [] ~init:() = ()
let%test_unit _ =
  let long = Test_values.long1 () in
  ignore (fold_right ~f:(fun e acc -> e :: acc) long ~init:[])

let unzip list =
  let rec loop list l1 l2 =
    match list with
    | [] -> (List.rev l1, List.rev l2)
    | (x, y) :: tl -> loop tl (x :: l1) (y :: l2)
  in
  loop list [] []

let zip_exn l1 l2 = map2_exn ~f:(fun a b -> (a, b)) l1 l2

let%test  _ =
  let l1 = Test_values.l1 in
  unzip (zip_exn l1 (List.rev l1)) = (l1, List.rev l1)
;;

let%test_unit _ =
  let long = Test_values.long1 () in
  ignore (unzip (zip_exn long long))
;;

let zip l1 l2 = try Some (zip_exn l1 l2) with _ -> None
let%test _ = zip [1;2;3] [4;5;6] = Some [1,4;2,5;3,6]
let%test _ = zip [1] [4;5;6]     = None

(** Additional list operations *)

let rev_mapi l ~f =
  let rec loop i acc = function
    | [] -> acc
    | h :: t -> loop (i + 1) (f i h :: acc) t
  in
  loop 0 [] l

let mapi l ~f = List.rev (rev_mapi l ~f)


let%test _ = mapi ~f:(fun i x -> (i,x))
  ["one";"two";"three";"four"] = [0,"one";1,"two";2,"three";3,"four"]
let%test _ = mapi ~f:(fun i x -> (i,x)) [] = []

let iteri l ~f =
  ignore (fold l ~init:0 ~f:(fun i x -> f i x; i + 1));
;;

let foldi t ~f ~init =
  snd (fold t ~init:(0, init) ~f:(fun (i, acc) v -> (i + 1, f i acc v)))
;;

let filteri l ~f =
  List.rev (foldi l
               ~f:(fun pos acc x ->
                 if f pos x then x :: acc else acc)
               ~init:[])

let reduce l ~f = match l with
  | [] -> None
  | hd :: tl -> Some (fold ~init:hd ~f tl)

let reduce_exn l ~f =
  match reduce l ~f with
  | None -> raise (Invalid_argument "List.reduce_exn")
  | Some v -> v

let reduce_balanced l ~f =
  (* Call the "size" of a value the number of list elements that have been combined into
     it via calls to [f].  We proceed by using [f] to combine elements in the accumulator
     of the same size until we can't combine any more, then getting a new element from the
     input list and repeating.

     With this strategy, in the accumulator:
     - we only ever have elements of sizes a power of two
     - we never have more than one element of each size
     - the sum of all the element sizes is equal to the number of elements consumed

     These conditions enforce that list of elements of each size is precisely the binary
     expansion of the number of elements consumed: if you've consumed 13 = 0b1101
     elements, you have one element of size 8, one of size 4, and one of size 1.  Hence
     when a new element comes along, the number of combinings you need to do is the number
     of trailing 1s in the binary expansion of [num], the number of elements that have
     already gone into the accumulator.  The accumulator is in ascending order of size, so
     the next element to combine with is always the head of the list. *)
  let rec step_accum num acc x =
    if num land 1 = 0
    then x :: acc
    else
      match acc with
      | [] -> assert false
      (* New elements from later in the input list go on the front of the accumulator, so
         the accumulator is in reverse order wrt the original list order, hence [f y x]
         instead of [f x y]. *)
      | y :: ys -> step_accum (num asr 1) ys (f y x)
  in
  (* Experimentally, inlining [foldi] and unrolling this loop a few times can reduce
     runtime down to a third and allocation to 1/16th or so in the microbenchmarks below.
     However, in most use cases [f] is likely to be expensive (otherwise why do you care
     about the order of reduction?) so the overhead of this function itself doesn't really
     matter. If you come up with a use-case where it does, then that's something you might
     want to try: see hg log -pr 49ef065f429d. *)
  match foldi l ~init:[] ~f:step_accum with
  | [] -> None
  | x :: xs -> Some (fold xs ~init:x ~f:(fun x y -> f y x))

let reduce_balanced_exn l ~f =
  match reduce_balanced l ~f with
  | None -> raise (Invalid_argument "List.reduce_balanced_exn")
  | Some v -> v

let groupi l ~break =
  let groups =
    foldi l ~init:[] ~f:(fun i acc x ->
      match acc with
      | [] -> [[x]]
      | current_group :: tl ->
        if break i (hd_exn current_group) x then
          [x] :: current_group :: tl  (* start new group *)
        else
          (x :: current_group) :: tl) (* extend current group *)
  in
  match groups with
  | [] -> []
  | l -> rev_map l ~f:rev

let group l ~break = groupi l ~break:(fun _ x y -> break x y)

let%test_module "group" = (module struct
  let%test _ = (group [1;2;3;4] ~break:(fun _ x -> x = 3) = [[1;2];[3;4]])

  let%test _ = (group [] ~break:(fun _ -> assert false)) = []

  let mis = ['M';'i';'s';'s';'i';'s';'s';'i';'p';'p';'i']
  let equal_letters =
    [['M'];['i'];['s';'s'];['i'];['s';'s'];['i'];['p';'p'];['i']]
  let single_letters =
    [['M';'i';'s';'s';'i';'s';'s';'i';'p';'p';'i']]
  let every_three =
    [['M'; 'i'; 's']; ['s'; 'i'; 's']; ['s'; 'i'; 'p']; ['p'; 'i' ]]

  let%test _ = (group ~break:(<>) mis) = equal_letters
  let%test _ = (group ~break:(fun _ _ -> false) mis) = single_letters
  let%test _ = (groupi ~break:(fun i _ _ -> i mod 3 = 0) mis) = every_three
end)

let concat_map l ~f =
  let rec aux acc = function
    | [] -> List.rev acc
    | hd :: tl -> aux (rev_append (f hd) acc) tl
  in
  aux [] l

let concat_mapi l ~f =
  let rec aux cont acc = function
    | [] -> List.rev acc
    | hd :: tl -> aux (cont + 1) (rev_append (f cont hd) acc) tl
  in
  aux 0 [] l

let merge l1 l2 ~cmp =
  let rec loop acc l1 l2 =
    match l1,l2 with
    | [], l2 -> rev_append acc l2
    | l1, [] -> rev_append acc l1
    | h1 :: t1, h2 :: t2 ->
        if cmp h1 h2 <= 0
        then loop (h1 :: acc) t1 l2
        else loop (h2 :: acc) l1 t2
  in
  loop [] l1 l2
;;


include struct
  (* We are explicit about what we import from the general Monad functor so that
   * we don't accidentally rebind more efficient list-specific functions.
   *)
  module Monad = Monad.Make (struct
    type 'a t = 'a list
    let bind x ~f = concat_map x ~f
    let map = `Custom map
    let return x = [x]
  end)
  open Monad
  module Monad_infix = Monad_infix
  module Let_syntax = Let_syntax
  let ignore_m = ignore_m
  let join = join
  let bind = bind
  let (>>=) t f = bind t ~f
  let return = return
  let all = all
  let all_ignore = all_ignore
end

(** returns final element of list *)
let rec last_exn list = match list with
  | [x] -> x
  | _ :: tl -> last_exn tl
  | [] -> raise (Invalid_argument "Base_list.last")

let%test _ = last_exn [1;2;3] = 3
let%test _ = last_exn [1] = 1
let%test _ = last_exn (Test_values.long1 ()) = 99_999

(** optionally returns final element of list *)
let rec last list = match list with
  | [x] -> Some x
  | _ :: tl -> last tl
  | [] -> None
;;

let rec is_prefix list ~prefix ~equal =
  match prefix with
  | [] -> true
  | hd::tl ->
    match list with
    | [] -> false
    | hd'::tl' -> equal hd hd' && is_prefix tl' ~prefix:tl ~equal
;;

let%test _ = is_prefix []  ~prefix:[]  ~equal:(=)
let%test _ = is_prefix [1] ~prefix:[]  ~equal:(=)
let%test _ = is_prefix [1] ~prefix:[1] ~equal:(=)
let%test _ = not (is_prefix [1] ~prefix:[1;2] ~equal:(=))
let%test _ = not (is_prefix [1;3] ~prefix:[1;2] ~equal:(=))
let%test _ = is_prefix [1;2;3] ~prefix:[1;2] ~equal:(=)

let find_consecutive_duplicate t ~equal =
  match t with
  | [] -> None
  | a1 :: t ->
    let rec loop a1 t =
      match t with
      | [] -> None
      | a2 :: t -> if equal a1 a2 then Some (a1, a2) else loop a2 t
    in
    loop a1 t
;;

let%test_unit _ =
  List.iter ~f:(fun (t, expect) ->
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

let%test _ = find_consecutive_duplicate [(0,'a');(1,'b');(2,'b')]
         ~equal:(fun (_, a) (_, b) -> Pervasives.(=) a b) = Some ((1, 'b'), (2, 'b'))
;;

(* returns list without adjacent duplicates *)
let remove_consecutive_duplicates list ~equal =
  let rec loop list accum = match list with
    | [] -> accum
    | hd :: [] -> hd :: accum
    | hd1 :: hd2 :: tl ->
        if equal hd1 hd2
        then loop (hd2 :: tl) accum
        else loop (hd2 :: tl) (hd1 :: accum)
  in
  rev (loop list [])

let%test _ = remove_consecutive_duplicates ~equal:Pervasives.(=) [] = []
let%test _ = remove_consecutive_duplicates ~equal:Pervasives.(=) [5;5;5;5;5] = [5]
let%test _ = remove_consecutive_duplicates ~equal:Pervasives.(=) [5;6;5;6;5;6] = [5;6;5;6;5;6]
let%test _ = remove_consecutive_duplicates ~equal:Pervasives.(=) [5;5;6;6;5;5;8;8] = [5;6;5;8]
let%test _ = remove_consecutive_duplicates         [(0,1);(0,2);(2,2);(4,1)]
  ~equal:(fun (a,_) (b,_) -> Pervasives.(=) a b) = [      (0,2);(2,2);(4,1)]
let%test _ = remove_consecutive_duplicates         [(0,1);(2,2);(0,2);(4,1)]
  ~equal:(fun (a,_) (b,_) -> Pervasives.(=) a b) = [(0,1);(2,2);(0,2);(4,1)]
let%test _ = remove_consecutive_duplicates         [(0,1);(2,1);(0,2);(4,2)]
  ~equal:(fun (_,a) (_,b) -> Pervasives.(=) a b) = [      (2,1);      (4,2)]
let%test _ = remove_consecutive_duplicates         [(0,1);(2,2);(0,2);(4,1)]
  ~equal:(fun (_,a) (_,b) -> Pervasives.(=) a b) = [(0,1);      (0,2);(4,1)]

(** returns sorted version of list with duplicates removed *)
let dedup ?(compare=Pervasives.compare) list =
  match list with
  | [] -> []                            (* performance hack *)
  | _ ->
    let equal x x' = compare x x' = 0 in
    let sorted = List.sort ~cmp:compare list in
    remove_consecutive_duplicates ~equal sorted

let%test _ = dedup [] = []
let%test _ = dedup [5;5;5;5;5] = [5]
let%test _ = length (dedup [2;1;5;3;4]) = 5
let%test _ = length (dedup [2;3;5;3;4]) = 4
let%test _ = length (dedup [(0,1);(2,2);(0,2);(4,1)] ~compare:(fun (a,_) (b,_) ->
  Pervasives.compare a b)) = 3
let%test _ = length (dedup [(0,1);(2,2);(0,2);(4,1)] ~compare:(fun (_,a) (_,b) ->
  Pervasives.compare a b)) = 2

let contains_dup ?compare lst = length (dedup ?compare lst) <> length lst

let find_a_dup ?(compare=Pervasives.compare) l =
  let sorted = List.sort ~cmp:compare l in
  let rec loop l = match l with
      [] | [_] -> None
    | hd1 :: hd2 :: tl ->
      if compare hd1 hd2 = 0 then Some (hd1) else loop (hd2 :: tl)
  in
  loop sorted

let%test _ = find_a_dup [] = None
let%test _ = find_a_dup [3] = None
let%test _ = find_a_dup [3;4] = None
let%test _ = find_a_dup [3;3] = Some 3
let%test _ = find_a_dup [3;5;4;6;12] = None
let%test _ = find_a_dup [3;5;4;5;12] = Some 5
let%test _ = find_a_dup [3;5;12;5;12] = Some 5
let%test _ = find_a_dup [(0,1);(2,2);(0,2);(4,1)] = None
let%test _ = (find_a_dup [(0,1);(2,2);(0,2);(4,1)]
          ~compare:(fun (_,a) (_,b) -> Pervasives.compare a b)) <> None
let%test _ = let dup = find_a_dup [(0,1);(2,2);(0,2);(4,1)]
         ~compare:(fun (a,_) (b,_) -> Pervasives.compare a b)
       in
       match dup with
       | Some (0, _) -> true
       | _ -> false

let find_all_dups ?(compare=Pervasives.compare) l =
  (* We add this reversal, so we can skip a [List.rev] at the end. We could skip
     [List.rev] anyway since we don not give any ordering guarantees, but it is
     nice to get results in natural order. *)
  let compare a b = (-1) * compare a b in
  let sorted = List.sort ~cmp:compare l in
  (* Walk the list and record the first of each consecutive run of identical elements *)
  let rec loop sorted prev ~already_recorded acc =
    match sorted with
    | [] -> acc
    | hd :: tl ->
      if compare prev hd <> 0
      then loop tl hd ~already_recorded:false acc
      else if already_recorded
      then loop tl hd ~already_recorded:true acc
      else loop tl hd ~already_recorded:true (hd :: acc)
  in
  match sorted with
  | [] -> []
  | hd :: tl -> loop tl hd ~already_recorded:false []

let%test _ = find_all_dups [] = []
let%test _ = find_all_dups [3] = []
let%test _ = find_all_dups [3;4] = []
let%test _ = find_all_dups [3;3] = [3]
let%test _ = find_all_dups [3;5;4;6;12] = []
let%test _ = find_all_dups [3;5;4;5;12] = [5]
let%test _ = find_all_dups [3;5;12;5;12] = [5;12]
let%test _ = find_all_dups [(0,1);(2,2);(0,2);(4,1)] = []
let%test _ = length (find_all_dups [(0,1);(2,2);(0,2);(4,1)]
                       ~compare:(fun (_,a) (_,b) -> Pervasives.compare a b)) = 2
let%test _ = length (find_all_dups [(0,1);(2,2);(0,2);(4,1)]
                       ~compare:(fun (a,_) (b,_) -> Pervasives.compare a b)) = 1

type sexp_thunk = unit -> Sexp.t
let sexp_of_sexp_thunk x = x ()
exception Duplicate_found of sexp_thunk * string [@@deriving sexp]

let exn_if_dup ?compare ?(context="exn_if_dup") t ~to_sexp =
  match find_a_dup ?compare t with
  | None -> ()
  | Some dup ->
    raise (Duplicate_found ((fun () -> to_sexp dup),context))

let count t ~f = Container.count ~fold t ~f
let sum m t ~f = Container.sum ~fold m t ~f
let min_elt t ~cmp = Container.min_elt ~fold t ~cmp
let max_elt t ~cmp = Container.max_elt ~fold t ~cmp

let counti t ~f = foldi t ~init:0 ~f:(fun idx count a -> if f idx a then count + 1 else count)

let%test _ = counti [0;1;2;3;4] ~f:(fun idx x -> idx = x) = 5
let%test _ = counti [0;1;2;3;4] ~f:(fun idx x -> idx = 4-x) = 1

let init n ~f =
  if n < 0 then invalid_argf "List.init %d" n ();
  let rec loop i accum =
    assert (i >= 0);
    if i = 0 then accum
    else loop (i-1) (f (i-1) :: accum)
  in
  loop n []
;;

let rev_filter_map l ~f =
  let rec loop l accum =
    match l with
    | [] -> accum
    | hd :: tl ->
      match f hd with
      | Some x -> loop tl (x :: accum)
      | None   -> loop tl accum
  in
  loop l []
;;

let filter_map l ~f = List.rev (rev_filter_map l ~f)

let%test _ = filter_map ~f:(fun x -> Some x) Test_values.l1 = Test_values.l1
let%test _ = filter_map ~f:(fun x -> Some x) [] = []
let%test _ = filter_map ~f:(fun _x -> None) [1.;2.;3.] = []
let%test _ = filter_map
    ~f:(fun x -> if (x > 0) then Some x else None) [1;-1;3] = [1;3]

let rev_filter_mapi l ~f =
  let rec loop i l accum =
    match l with
    | [] -> accum
    | hd :: tl ->
      match f i hd with
      | Some x -> loop (i + 1) tl (x :: accum)
      | None   -> loop (i + 1) tl accum
  in
  loop 0 l []
;;

let filter_mapi l ~f = List.rev (rev_filter_mapi l ~f)
let%test _ = filter_mapi ~f:(fun _i x -> Some x) Test_values.l1 = Test_values.l1
let%test _ = filter_mapi ~f:(fun _i x -> Some x) [] = []
let%test _ = filter_mapi ~f:(fun _i _x -> None) [1.;2.;3.] = []
let%test _ = filter_mapi ~f:(fun _i x -> if (x > 0) then Some x else None) [1;-1;3]
       = [1;3]
let%test _ = filter_mapi ~f:(fun i x -> if (i mod 2=0) then Some x else None)
  [1;-1;3] = [1;3]


let filter_opt l = filter_map l ~f:(fun x -> x)

let partition_map t ~f =
  let rec loop t fst snd =
    match t with
    | [] -> (rev fst, rev snd)
    | x :: t ->
      match f x with
      | `Fst y -> loop t (y :: fst) snd
      | `Snd y -> loop t fst (y :: snd)
  in
  loop t [] []
;;

let partition3_map t ~f =
  let rec loop t fst snd trd =
    match t with
    | [] -> (rev fst, rev snd, rev trd)
    | x :: t ->
      match f x with
      | `Fst y -> loop t (y :: fst) snd trd
      | `Snd y -> loop t fst (y :: snd) trd
      | `Trd y -> loop t fst snd (y :: trd)
  in
  loop t [] [] []
;;

let partition_tf t ~f =
  let f x = if f x then `Fst x else `Snd x in
  partition_map t ~f
;;

module Assoc = struct

  type ('a, 'b) t = ('a * 'b) list [@@deriving sexp]

  let compare (type a) (type b) compare_a compare_b = [%compare: (a * b) list]
    [@@deprecated "[since 2016-06] This does not respect the equivalence class promised by List.Assoc. Use List.compare directly if that's what you want."]

  let find t ?(equal=Poly.equal) key =
    match find t ~f:(fun (key', _) -> equal key key') with
    | None -> None
    | Some x -> Some (snd x)

  let find_exn t ?(equal=Poly.equal) key =
    match find t key ~equal with
    | None -> raise Not_found
    | Some value -> value

  let mem t ?(equal=Poly.equal) key = (find t ~equal key) <> None

  let remove t ?(equal=Poly.equal) key =
    filter t ~f:(fun (key', _) -> not (equal key key'))

  let add t ?(equal=Poly.equal) key value =
    (* the remove doesn't change the map semantics, but keeps the list small *)
    (key, value) :: remove t ~equal key

  let inverse t = map t ~f:(fun (x, y) -> (y, x))

  let map t ~f = List.map t ~f:(fun (key, value) -> (key, f value))

end

let sub l ~pos ~len =
  (* We use [pos > length l - len] rather than [pos + len > length l] to avoid the
     possibility of overflow. *)
  if pos < 0 || len < 0 || pos > length l - len then invalid_arg "List.sub";
  List.rev
    (foldi l ~init:[]
       ~f:(fun i acc el ->
             if i >= pos && i < (pos + len)
             then el :: acc
             else acc
          )
    )
;;

let slice a start stop =
  Ordered_collection_common.slice ~length_fun:length ~sub_fun:sub
    a start stop

let split_n t_orig n =
  if n <= 0 then
    ([], t_orig)
  else
    let rec loop n t accum =
      if n = 0 then
        (List.rev accum, t)
      else
        match t with
        | [] -> (t_orig, []) (* in this case, t_orig = List.rev accum *)
        | hd :: tl -> loop (n - 1) tl (hd :: accum)
    in
    loop n t_orig []

let%test _ = split_n [1;2;3;4;5;6] 3 = ([1;2;3],[4;5;6])
let%test _ = split_n [1;2;3;4;5;6] 100 = ([1;2;3;4;5;6],[])
let%test _ = split_n [1;2;3;4;5;6] 0 = ([],[1;2;3;4;5;6])
let%test _ = split_n [1;2;3;4;5;6] (-5) = ([],[1;2;3;4;5;6])

let take t n = fst (split_n t n)
let drop t n = snd (split_n t n)

let split_while xs ~f =
  let rec loop acc = function
    | hd :: tl when f hd -> loop (hd :: acc) tl
    | t -> (rev acc, t)
  in
  loop [] xs
;;

let take_while t ~f = fst (split_while t ~f)
let drop_while t ~f = snd (split_while t ~f)

let%test_module "{take,drop,split}_while" = (module struct

  let pred = function
    | '0' .. '9' -> true
    | _ -> false

  let test xs prefix suffix =
    let (prefix1, suffix1) = split_while ~f:pred xs in
    let prefix2 = take_while xs ~f:pred in
    let suffix2 = drop_while xs ~f:pred in
    xs = prefix @ suffix
    && prefix = prefix1 && prefix = prefix2
    && suffix = suffix1 && suffix = suffix2

  let%test _ = test ['1';'2';'3';'a';'b';'c'] ['1';'2';'3'] ['a';'b';'c']
  let%test _ = test ['1';'2';    'a';'b';'c'] ['1';'2'    ] ['a';'b';'c']
  let%test _ = test ['1';        'a';'b';'c'] ['1'        ] ['a';'b';'c']
  let%test _ = test [            'a';'b';'c'] [           ] ['a';'b';'c']
  let%test _ = test ['1';'2';'3'            ] ['1';'2';'3'] [           ]
  let%test _ = test [                       ] [           ] [           ]

end)

let cartesian_product list1 list2 =
  if list2 = [] then [] else
    let rec loop l1 l2 accum = match l1 with
      | [] -> accum
      | (hd :: tl) ->
          loop tl l2
            (List.rev_append
               (map ~f:(fun x -> (hd,x)) l2)
               accum)
    in
    List.rev (loop list1 list2 [])

let concat l = fold_right l ~init:[] ~f:append

let%test _ = concat [] = []
let%test _ = concat [[]] = []
let%test _ = concat [[3]] = [3]
let%test _ = concat [[1;2;3;4]] = [1;2;3;4]
let%test _ = concat
  [[1;2;3;4];[5;6;7];[8;9;10];[];[11;12]]
  = [1;2;3;4;5;6;7;8;9;10;11;12]

let concat_no_order l = fold l ~init:[] ~f:(fun acc l -> rev_append l acc)

let cons x l = x :: l

let is_empty l = match l with [] -> true | _ -> false

let is_sorted l ~compare =
  let rec loop l =
    match l with
    | [] | [_] -> true
    | x1 :: ((x2 :: _) as rest) ->
        compare x1 x2 <= 0 && loop rest
  in loop l

let%test _ = is_sorted [] ~compare
let%test _ = is_sorted [1] ~compare
let%test _ = is_sorted [1; 2; 3; 4] ~compare
let%test _ = not (is_sorted [2; 1] ~compare)
let%test _ = not (is_sorted [1; 3; 2] ~compare)

let is_sorted_strictly l ~compare =
  let rec loop l =
    match l with
    | [] | [_] -> true
    | x1 :: ((x2 :: _) as rest) ->
        compare x1 x2 < 0 && loop rest
  in loop l
;;

let%test_unit _ =
  List.iter
    ~f:(fun (t, expect) -> assert (expect = is_sorted_strictly t ~compare))
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

module Infix = struct
  let ( @ ) = append
end

let permute ?(random_state = Random.State.default) list =
  match list with
  (* special cases to speed things up in trivial cases *)
  | [] | [_] -> list
  | [ x; y ] -> if Random.State.bool random_state then [ y; x ] else list
  | _ ->
    let arr = Array.of_list list in
    Array_permute.permute arr ~random_state;
    Array.to_list arr;
;;

let random_element_exn ?(random_state = Random.State.default) list =
  if is_empty list
  then failwith "List.random_element_exn: empty list"
  else nth_exn list (Random.State.int random_state (length list))
;;

let random_element ?(random_state = Random.State.default) list =
  try Some (random_element_exn ~random_state list)
  with _ -> None
;;

let%test _ = random_element [] = None
let%test _ = random_element [0] = Some 0

let compare cmp a b =
  let rec loop a b =
    match a, b with
    | [], [] -> 0
    | [], _  -> -1
    | _ , [] -> 1
    | x :: xs, y :: ys ->
      let n = cmp x y in
      if n = 0 then loop xs ys
      else n
  in
  loop a b
;;

let hash_fold_t = hash_fold_list

let equal t1 t2 ~equal =
  let rec loop t1 t2 =
    match t1, t2 with
    | [], [] -> true
    | x1 :: t1, x2 :: t2 -> equal x1 x2 && loop t1 t2
    | _ -> false
  in
  loop t1 t2
;;

let transpose =
  let rec transpose_aux t rev_columns =
    match partition_map t ~f:(function [] -> `Snd () | x :: xs -> `Fst (x, xs)) with
    | (_ :: _, _ :: _) -> None
    | ([], _) -> Some (rev_append rev_columns [])
    | (heads_and_tails, []) ->
      let (column, trimmed_rows) = unzip heads_and_tails in
      transpose_aux trimmed_rows (column :: rev_columns)
  in
  fun t ->
    transpose_aux t []

exception Transpose_got_lists_of_different_lengths of int list [@@deriving sexp]

let transpose_exn l =
  match transpose l with
  | Some l -> l
  | None ->
    raise (Transpose_got_lists_of_different_lengths (List.map l ~f:List.length))

let%test_module "transpose" = (module struct

  let round_trip a b = transpose a = Some b && transpose b = Some a

  let%test _ = round_trip [] []

  let%test _ = transpose [[]] = Some []
  let%test _ = transpose [[]; []] = Some []
  let%test _ = transpose [[]; []; []] = Some []

  let%test _ = round_trip [[1]] [[1]]

  let%test _ = round_trip [[1];
                     [2]] [[1; 2]]

  let%test _ = round_trip [[1];
                     [2];
                     [3]] [[1; 2; 3]]

  let%test _ = round_trip [[1; 2];
                     [3; 4]] [[1; 3];
                              [2; 4]]

  let%test _ = round_trip [[1; 2; 3];
                     [4; 5; 6]] [[1; 4];
                                 [2; 5];
                                 [3; 6]]

  let%test _ = transpose [[]; [1]] = None

  let%test _ = transpose [[1;2];[3]] = None

end)

let intersperse t ~sep =
  match t with
  | [] -> []
  | x :: xs -> x :: fold_right xs ~init:[] ~f:(fun y acc -> sep :: y :: acc)

let%test _ = intersperse [1;2;3] ~sep:0 = [1;0;2;0;3]
let%test _ = intersperse [1;2]   ~sep:0 = [1;0;2]
let%test _ = intersperse [1]     ~sep:0 = [1]
let%test _ = intersperse []      ~sep:0 = []

let fold_result t ~init ~f = Container.fold_result ~fold ~init ~f t
let fold_until  t ~init ~f = Container.fold_until  ~fold ~init ~f t
