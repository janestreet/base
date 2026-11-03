open! Import
module Array = Array0
module Either = Either0
module Nothing = Nothing0
module Option = Option0
include List0
include Constructors

module Derived = struct
  include Container.Derived
  include Indexed_container.Derived
end

(* This itself includes [List0]. *)

let invalid_argf = Printf.invalid_argf

[%%rederive.portable type 'a t = 'a list [@@deriving globalize, sexp, sexp_grammar]]

module Or_unequal_lengths = struct
  type 'a t =
    | Ok of 'a
    | Unequal_lengths
  [@@deriving compare, sexp_of]
end

let invariant f t = iter t ~f
let of_list t = t
let singleton x = [ x ]

let range' ~compare ~stride ?(start = `inclusive) ?(stop = `exclusive) start_i stop_i =
  let next_i = stride start_i in
  let order x y = Ordering.of_int (compare x y) in
  let raise_stride_cannot_return_same_value () =
    invalid_arg "List.range': stride function cannot return the same value"
  in
  let initial_stride_order =
    match order start_i next_i with
    | Equal -> raise_stride_cannot_return_same_value ()
    | Less -> `Less
    | Greater -> `Greater
  in
  let[@tail_mod_cons] rec range_loop i =
    let i_to_stop_order = order i stop_i in
    match i_to_stop_order, initial_stride_order with
    | Less, `Less | Greater, `Greater ->
      (* haven't yet reached [stop_i]. Continue. *)
      let next_i = stride i in
      (match order i next_i, initial_stride_order with
       | Equal, _ -> (raise_stride_cannot_return_same_value [@tailcall false]) ()
       | Less, `Greater | Greater, `Less ->
         invalid_arg "List.range': stride function cannot change direction"
       | Less, `Less | Greater, `Greater -> i :: range_loop next_i)
    | Less, `Greater | Greater, `Less ->
      (* stepped past [stop_i]. Finished. *)
      []
    | Equal, _ ->
      (* reached [stop_i]. Finished. *)
      (match stop with
       | `inclusive -> [ i ]
       | `exclusive -> [])
  in
  let start_i =
    match start with
    | `inclusive -> start_i
    | `exclusive -> next_i
  in
  range_loop start_i [@nontail]
;;

let compare_int = Replace_polymorphic_compare.Int_replace_polymorphic_compare.compare

let range ?(stride = 1) ?(start = `inclusive) ?(stop = `exclusive) start_i stop_i =
  if stride = 0 then invalid_arg "List.range: stride must be non-zero";
  range' ~compare:compare_int ~stride:(fun x -> x + stride) ~start ~stop start_i stop_i
;;

let hd t =
  match t with
  | [] -> None
  | x :: _ -> Some x
;;

let[@zero_alloc] hd_or_null t =
  match t with
  | [] -> Null
  | x :: _ -> This x
;;

let tl t =
  match t with
  | [] -> None
  | _ :: t' -> Some t'
;;

let[@zero_alloc] tl_or_null t =
  match t with
  | [] -> Null
  | _ :: t' -> This t'
;;

open struct
  type nonrec 'a t = 'a t =
    | []
    | ( :: ) of 'a * 'a t
end

let nth t n : _ Option.t =
  if n < 0
  then None
  else (
    let rec nth_aux t n : _ Option.t =
      match t with
      | [] -> None
      | a :: t ->
        if n = 0
        then Some a
        else (
          let n = n - 1 in
          nth_aux t n)
    in
    nth_aux t n)
;;

let nth_exn t n =
  match nth t n with
  | None ->
    (match
       (invalid_argf "List.nth_exn %d called on list of length %d" n (length t) ()
        : Nothing.t)
     with
     | _ -> .)
  | Some a -> a
;;

let init_internal n ~f ~name =
  if n < 0 then invalid_argf "%s %d" name n ();
  let rec init_loop i accum =
    assert (i >= 0);
    if i = 0 then accum else init_loop (i - 1) (f (i - 1) :: accum)
  in
  init_loop n [] [@nontail]
;;

let init n ~f = init_internal n ~f ~name:"List.init"

let init_i64 n ~f =
  let open struct
    type t = int64 [@@deriving specialize_polymorphic_compare]

    external ( - ) : t -> t -> t = "%int64_sub"
  end in
  let rec init_i64_loop n acc =
    let n = n - 1L in
    if n < 0L then acc else (init_i64_loop [@tailcall]) n (f n :: acc)
  in
  init_i64_loop n [] [@nontail]
;;

let iteri l ~f =
  ignore
    (fold l ~init:0 ~f:(fun i x ->
       f i x;
       i + 1)
     : int)
;;

let mem t a ~equal =
  let rec mem_loop equal a = function
    | [] -> false
    | b :: bs -> equal a b || mem_loop equal a bs
  in
  mem_loop equal a t
;;

let[@zero_alloc] nth_or_null t n =
  if n < 0
  then Null
  else (
    let rec nth_aux t n : 'a or_null =
      match t with
      | [] -> Null
      | a :: t ->
        if n = 0
        then This a
        else (
          let n = n - 1 in
          nth_aux t n)
    in
    nth_aux t n)
;;

let unordered_append l1 l2 =
  match l1, l2 with
  | [], l | l, [] -> l
  | _ -> rev_append l1 l2
;;

module Check_length2 = struct
  type ('a, 'b) t =
    | Same_length of int
    | Unequal_lengths of
        { shared_length : int
        ; tail_of_a : 'a list
        ; tail_of_b : 'b list
        }

  (* In the [Unequal_lengths] case, at least one of the tails will be non-empty. *)
  let of_lists l1 l2 =
    let rec of_lists2_loop a b shared_length =
      match a, b with
      | [], [] -> Same_length shared_length
      | _ :: a, _ :: b -> of_lists2_loop a b (shared_length + 1)
      | [], _ | _, [] -> Unequal_lengths { shared_length; tail_of_a = a; tail_of_b = b }
    in
    of_lists2_loop l1 l2 0
  ;;
end

let check_length2_exn name l1 l2 =
  match Check_length2.of_lists l1 l2 with
  | Same_length _ -> ()
  | Unequal_lengths { shared_length; tail_of_a; tail_of_b } ->
    invalid_argf
      "length mismatch in %s: %d <> %d"
      name
      (shared_length + length tail_of_a)
      (shared_length + length tail_of_b)
      ()
;;

let check_length2 l1 l2 ~f =
  match Check_length2.of_lists l1 l2 with
  | Same_length _ -> Or_unequal_lengths.Ok (f l1 l2)
  | Unequal_lengths _ -> Unequal_lengths
;;

module Check_length3 = struct
  type ('a, 'b, 'c) t =
    | Same_length of int
    | Unequal_lengths of
        { shared_length : int
        ; tail_of_a : 'a list
        ; tail_of_b : 'b list
        ; tail_of_c : 'c list
        }

  (* In the [Unequal_lengths] case, at least one of the tails will be non-empty. *)
  let of_lists l1 l2 l3 =
    let rec of_lists3_loop a b c shared_length =
      match a, b, c with
      | [], [], [] -> Same_length shared_length
      | _ :: a, _ :: b, _ :: c -> of_lists3_loop a b c (shared_length + 1)
      | [], _, _ | _, [], _ | _, _, [] ->
        Unequal_lengths { shared_length; tail_of_a = a; tail_of_b = b; tail_of_c = c }
    in
    of_lists3_loop l1 l2 l3 0
  ;;
end

let check_length3_exn name l1 l2 l3 =
  match Check_length3.of_lists l1 l2 l3 with
  | Same_length _ -> ()
  | Unequal_lengths { shared_length; tail_of_a; tail_of_b; tail_of_c } ->
    let n1 = shared_length + length tail_of_a in
    let n2 = shared_length + length tail_of_b in
    let n3 = shared_length + length tail_of_c in
    invalid_argf "length mismatch in %s: %d <> %d || %d <> %d" name n1 n2 n2 n3 ()
;;

let check_length3 l1 l2 l3 ~f =
  match Check_length3.of_lists l1 l2 l3 with
  | Same_length _ -> Or_unequal_lengths.Ok (f l1 l2 l3)
  | Unequal_lengths _ -> Unequal_lengths
;;

let iter2 l1 l2 ~f = check_length2 l1 l2 ~f:(iter2_ok ~f) [@nontail]

let iter2_exn l1 l2 ~f =
  check_length2_exn "iter2_exn" l1 l2;
  iter2_ok l1 l2 ~f
;;

let rev_map2 l1 l2 ~f = check_length2 l1 l2 ~f:(rev_map2_ok ~f) [@nontail]

let rev_map2_exn l1 l2 ~f =
  check_length2_exn "rev_map2_exn" l1 l2;
  rev_map2_ok l1 l2 ~f
;;

let fold2 l1 l2 ~init ~f = check_length2 l1 l2 ~f:(fold2_ok ~init ~f) [@nontail]

let fold2_exn l1 l2 ~init ~f =
  check_length2_exn "fold2_exn" l1 l2;
  fold2_ok l1 l2 ~init ~f
;;

let fold_right2 l1 l2 ~f ~init =
  check_length2 l1 l2 ~f:(fold_right2_ok ~f ~init) [@nontail]
;;

let fold_right2_exn l1 l2 ~f ~init =
  check_length2_exn "fold_right2_exn" l1 l2;
  fold_right2_ok l1 l2 ~f ~init
;;

let for_all2 l1 l2 ~f = check_length2 l1 l2 ~f:(for_all2_ok ~f) [@nontail]

let for_all2_exn l1 l2 ~f =
  check_length2_exn "for_all2_exn" l1 l2;
  for_all2_ok l1 l2 ~f
;;

let exists2 l1 l2 ~f = check_length2 l1 l2 ~f:(exists2_ok ~f) [@nontail]

let exists2_exn l1 l2 ~f =
  check_length2_exn "exists2_exn" l1 l2;
  exists2_ok l1 l2 ~f
;;

(* This is a copy of the code from the standard library, with an extra eta-expansion to
   avoid creating partial closures (showed up for [filter]) in profiling). *)
let rev_filter t ~f =
  let rec find ~f accu = function
    | [] -> accu
    | x :: l -> if f x then find ~f (x :: accu) l else find ~f accu l
  in
  find ~f [] t
;;

let filteri l ~f =
  let[@tail_mod_cons] rec filteri_loop pos l =
    match l with
    | [] -> []
    | hd :: tl ->
      if f pos hd then hd :: filteri_loop (pos + 1) tl else filteri_loop (pos + 1) tl
  in
  filteri_loop 0 l [@nontail]
;;

let filter =
  let[@tail_mod_cons] rec filter l ~f =
    match l with
    | [] -> []
    | hd :: tl -> if f hd then hd :: filter tl ~f else filter tl ~f
  in
  filter
;;

let find_map t ~f =
  let rec find_map_loop = function
    | [] -> None
    | x :: l ->
      (match f x with
       | None -> find_map_loop l
       | Some _ as r -> r)
  in
  find_map_loop t [@nontail]
;;

let find_map_or_null t ~f =
  let rec find_map_or_null_loop = function
    | [] -> Null
    | x :: l ->
      (match f x with
       | Null -> find_map_or_null_loop l
       | This _ as r -> r)
  in
  find_map_or_null_loop t [@nontail]
;;

let find_map_or_null_exn =
  let not_found = Not_found_s (Atom "List.find_map_or_null_exn: not found") in
  let find_map_or_null_exn t ~f =
    match find_map_or_null t ~f with
    | Null ->
      raise (Portability_hacks.magic_uncontended__promise_deeply_immutable not_found)
    | This x -> x
  in
  (* named to preserve symbol in compiled binary *)
  find_map_or_null_exn
;;

let find_map_exn =
  let not_found = Not_found_s (Atom "List.find_map_exn: not found") in
  let find_map_exn t ~f =
    match find_map t ~f with
    | None ->
      raise (Portability_hacks.magic_uncontended__promise_deeply_immutable not_found)
    | Some x -> x
  in
  (* named to preserve symbol in compiled binary *)
  find_map_exn
;;

let find t ~f =
  let rec find_loop = function
    | [] -> None
    | x :: l -> if f x then Some x else find_loop l
  in
  find_loop t [@nontail]
;;

let find_or_null t ~f =
  let rec find_or_null_loop = function
    | [] -> Null
    | x :: l -> if f x then This x else find_or_null_loop l
  in
  find_or_null_loop t [@nontail]
;;

let find_exn =
  let not_found = Not_found_s (Atom "List.find_exn: not found") in
  let rec find_exn t ~f =
    match t with
    | [] ->
      raise (Portability_hacks.magic_uncontended__promise_deeply_immutable not_found)
    | x :: t -> if f x then x else find_exn t ~f
  in
  (* named to preserve symbol in compiled binary *)
  find_exn
;;

let findi t ~f =
  let rec findi_loop i t =
    match t with
    | [] -> None
    | x :: l -> if f i x then Some (i, x) else findi_loop (i + 1) l
  in
  findi_loop 0 t [@nontail]
;;

let findi_exn =
  let not_found = Not_found_s (Atom "List.findi_exn: not found") in
  let findi_exn t ~f =
    match findi t ~f with
    | None ->
      raise (Portability_hacks.magic_uncontended__promise_deeply_immutable not_found)
    | Some x -> x
  in
  findi_exn
;;

let find_mapi t ~f =
  let rec find_mapi_loop i t =
    match t with
    | [] -> None
    | x :: l ->
      (match f i x with
       | Some _ as result -> result
       | None ->
         let next = i + 1 in
         find_mapi_loop next l)
  in
  find_mapi_loop 0 t [@nontail]
;;

let find_mapi_or_null t ~f =
  let rec find_mapi_or_null_loop i t =
    match t with
    | [] -> Null
    | x :: l ->
      (match f i x with
       | This _ as result -> result
       | Null ->
         let next = i + 1 in
         find_mapi_or_null_loop next l)
  in
  find_mapi_or_null_loop 0 t [@nontail]
;;

let find_mapi_or_null_exn =
  let not_found = Not_found_s (Atom "List.find_mapi_or_null_exn: not found") in
  let find_mapi_or_null_exn t ~f =
    match find_mapi_or_null t ~f with
    | Null ->
      raise (Portability_hacks.magic_uncontended__promise_deeply_immutable not_found)
    | This x -> x
  in
  (* named to preserve symbol in compiled binary *)
  find_mapi_or_null_exn
;;

let find_mapi_exn =
  let not_found = Not_found_s (Atom "List.find_mapi_exn: not found") in
  let find_mapi_exn t ~f =
    match find_mapi t ~f with
    | None ->
      raise (Portability_hacks.magic_uncontended__promise_deeply_immutable not_found)
    | Some x -> x
  in
  (* named to preserve symbol in compiled binary *)
  find_mapi_exn
;;

let for_alli t ~f =
  let rec for_alli_loop i t =
    match t with
    | [] -> true
    | hd :: tl -> f i hd && for_alli_loop (i + 1) tl
  in
  for_alli_loop 0 t [@nontail]
;;

let existsi t ~f =
  let rec existsi_loop i t =
    match t with
    | [] -> false
    | hd :: tl -> f i hd || existsi_loop (i + 1) tl
  in
  existsi_loop 0 t [@nontail]
;;

let fold_left = fold
let of_array = Array.to_list
let to_array = Array.of_list
let to_list t = t

(** Tail recursive versions of standard [List] module *)

let[@tail_mod_cons] rec append_loop l1 l2 =
  match l1 with
  | [] -> l2
  | [ x1 ] -> x1 :: l2
  | [ x1; x2 ] -> x1 :: x2 :: l2
  | [ x1; x2; x3 ] -> x1 :: x2 :: x3 :: l2
  | [ x1; x2; x3; x4 ] -> x1 :: x2 :: x3 :: x4 :: l2
  | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
    x1 :: x2 :: x3 :: x4 :: x5 :: (append_loop [@tailcall]) tl l2
;;

let append l1 l2 =
  match l2 with
  | [] -> l1
  | _ :: _ -> append_loop l1 l2
;;

(* call-stack size <= first input data-stack size *)

(* The [_internal] version of [filteri] (and several other non-tail-recursive functions
   that are higher-order over a two-argument callback) takes an [f] that accepts a single
   unboxed product argument instead of two regular arguments. [filteri] below wraps the
   typical two-argument [f] provided by clients into an unboxed-product-accepting [f].

   We do this because non-tail-recursive functions are not inlined, so the call to [f] is
   always indirect, and that indirect call incurs a [caml_apply2] for 2-argument
   functions. In the current state, the call to [f] inside [filteri_internal] remains
   indirect but does not incur a [caml_apply2], while the thin wrapper [filteri] is
   typically inlined enough for the two-argument application of [f] to be direct or
   inlined. *)

(* call-stack size <= output data-stack size *)

(* call-stack size <= output data-stack size *)

let rev_mapi l ~f =
  let rec rev_mapi_loop i acc : _ t -> _ t = function
    | [] -> acc
    | h :: t -> rev_mapi_loop (i + 1) (f i h :: acc) t
  in
  rev_mapi_loop 0 [] l [@nontail]
;;

(* call-stack size <= input and output data-stack size *)

(* call-stack size <= output data-stack size *)

(* call-stack size <= output data-stack size *)

(* call-stack size <= input data stack size + max inner list data stack size

   unlike other functions, the maximum call-stack depth is [2 * max_non_tailcall] *)

let foldi t ~init ~f =
  let _, r = fold t ~init:(0, init) ~f:(fun (i, acc) v -> i + 1, f i acc v) in
  r
;;

(* call-stack size <= output data-stack size *)

(* call-stack size <= output data-stack size *)

open struct
  type nonrec 'a t = 'a t =
    | []
    | ( :: ) of 'a * 'a t
end

let mapi l ~f =
  let[@tail_mod_cons] rec mapi_loop i = function
    | [] -> ([] : _ t)
    | h :: t -> f i h :: mapi_loop (i + 1) t
  in
  mapi_loop 0 l [@nontail]
;;

let map l ~f =
  let[@tail_mod_cons] rec map_loop l : _ t =
    match l with
    | [] -> []
    | x :: tl -> f x :: (map_loop [@tailcall]) tl
  in
  map_loop l [@nontail]
;;

let filter_mapi l ~f =
  let[@tail_mod_cons] rec filter_mapi_loop pos l : _ t =
    match l with
    | [] -> []
    | hd :: tl ->
      (match (f pos hd : _ Option.t) with
       | None -> filter_mapi_loop (pos + 1) tl
       | Some x -> x :: filter_mapi_loop (pos + 1) tl)
  in
  filter_mapi_loop 0 l [@nontail]
;;

let filter_map l ~f =
  let[@tail_mod_cons] rec filter_map_loop l : _ t =
    match l with
    | [] -> []
    | hd :: tl ->
      (match (f hd : _ Option.t) with
       | None -> filter_map_loop tl
       | Some x -> x :: filter_map_loop tl)
  in
  filter_map_loop l [@nontail]
;;

let concat_mapi l ~f =
  let[@tail_mod_cons] rec concat_mapi_outer_loop pos = function
    | [] -> ([] : _ t)
    | [ hd ] -> (f [@tailcall false]) pos hd
    | hd :: (_ :: _ as tl) -> concat_mapi_inner_loop (pos + 1) (f pos hd) tl
  and[@tail_mod_cons] concat_mapi_inner_loop pos l1 l2 =
    match l1 with
    | [] -> concat_mapi_outer_loop pos l2
    | [ x1 ] -> x1 :: concat_mapi_outer_loop pos l2
    | [ x1; x2 ] -> x1 :: x2 :: concat_mapi_outer_loop pos l2
    | [ x1; x2; x3 ] -> x1 :: x2 :: x3 :: concat_mapi_outer_loop pos l2
    | [ x1; x2; x3; x4 ] -> x1 :: x2 :: x3 :: x4 :: concat_mapi_outer_loop pos l2
    | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
      x1 :: x2 :: x3 :: x4 :: x5 :: concat_mapi_inner_loop pos tl l2
  in
  concat_mapi_outer_loop 0 l [@nontail]
;;

let concat_map l ~f = concat_mapi l ~f:(fun _ x -> f x) [@nontail]

let filter_mapi_or_null l ~f =
  let[@tail_mod_cons] rec filter_mapi_or_null_loop pos (l : _ t) : _ t =
    match l with
    | [] -> []
    | hd :: tl ->
      (match (f pos hd : _ or_null) with
       | Null -> filter_mapi_or_null_loop (pos + 1) tl
       | This x -> x :: filter_mapi_or_null_loop (pos + 1) tl)
  in
  filter_mapi_or_null_loop 0 l [@nontail]
;;

let filter_map_or_null l ~f =
  let[@tail_mod_cons] rec filter_map_or_null_loop (l : _ t) ~f : _ t =
    match l with
    | [] -> []
    | hd :: tl ->
      (match (f hd : _ or_null) with
       | Null -> filter_map_or_null_loop tl ~f
       | This x -> x :: filter_map_or_null_loop tl ~f)
  in
  filter_map_or_null_loop l ~f
;;

(* Copied from [Sexplib0] for templating *)

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

(* We cannot store a [local] accumulator inside of a [ref], so we need a separate
   stack-allocating version *)

let ( >>| ) l f = map l ~f

let[@tail_mod_cons] rec map2_ok l1 l2 ~f =
  match l1, l2 with
  | [], [] -> []
  | x1 :: l1, x2 :: l2 -> f x1 x2 :: map2_ok l1 l2 ~f
  | _, _ -> invalid_arg "List.map2"
;;

(* call-stack size <= input and output data-stack size *)

let map2 l1 l2 ~f = check_length2 l1 l2 ~f:(map2_ok ~f) [@nontail]

let map2_exn l1 l2 ~f =
  check_length2_exn "map2_exn" l1 l2;
  map2_ok l1 l2 ~f
;;

let rev_map3_ok l1 l2 l3 ~f =
  let rec rev_map3_ok_loop l1 l2 l3 ac =
    match l1, l2, l3 with
    | [], [], [] -> ac
    | x1 :: l1, x2 :: l2, x3 :: l3 -> rev_map3_ok_loop l1 l2 l3 (f x1 x2 x3 :: ac)
    | _ -> assert false
  in
  rev_map3_ok_loop l1 l2 l3 [] [@nontail]
;;

let rev_map3 l1 l2 l3 ~f = check_length3 l1 l2 l3 ~f:(rev_map3_ok ~f) [@nontail]

let rev_map3_exn l1 l2 l3 ~f =
  check_length3_exn "rev_map3_exn" l1 l2 l3;
  rev_map3_ok l1 l2 l3 ~f
;;

let[@tail_mod_cons] rec map3_ok l1 l2 l3 ~f =
  match l1, l2, l3 with
  | [], [], [] -> []
  | x1 :: l1, x2 :: l2, x3 :: l3 -> f x1 x2 x3 :: map3_ok l1 l2 l3 ~f
  | _, _, _ -> invalid_arg "List.map3"
;;

let map3 l1 l2 l3 ~f = check_length3 l1 l2 l3 ~f:(map3_ok ~f) [@nontail]

let map3_exn l1 l2 l3 ~f =
  check_length3_exn "map3_exn" l1 l2 l3;
  map3_ok l1 l2 l3 ~f
;;

let rec rev_map_append l1 l2 ~f =
  match l1 with
  | [] -> l2
  | h :: t -> rev_map_append ~f t (f h :: l2)
;;

let unzip list =
  let rec unzip_loop list l1 l2 =
    match list with
    | [] -> l1, l2
    | (x, y) :: tl -> unzip_loop tl (x :: l1) (y :: l2)
  in
  unzip_loop (rev list) [] []
;;

(* call-stack size = input data-stack size *)

let unzip3 list =
  let rec unzip3_loop list l1 l2 l3 =
    match list with
    | [] -> l1, l2, l3
    | (x, y, z) :: tl -> unzip3_loop tl (x :: l1) (y :: l2) (z :: l3)
  in
  unzip3_loop (rev list) [] [] []
;;

let zip_exn l1 l2 =
  try map2_ok ~f:(fun a b -> a, b) l1 l2 with
  | _ -> invalid_argf "length mismatch in zip_exn: %d <> %d" (length l1) (length l2) ()
;;

let zip l1 l2 = map2 ~f:(fun a b -> a, b) l1 l2

let zip3_exn l1 l2 l3 =
  check_length3_exn "zip3_exn" l1 l2 l3;
  map3_ok l1 l2 l3 ~f:(fun a b c -> a, b, c)
;;

let zip3 l1 l2 l3 = map3 l1 l2 l3 ~f:(fun a b c -> a, b, c)

(** Additional list operations *)

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

(* We cannot store a [local] accumulator inside of a [ref] *)

let reduce l ~f =
  match l with
  | [] -> None
  | hd :: tl -> Some (fold ~init:hd ~f tl)
;;

(* Match on [l] instead of [reduce] to avoid allocating in the caller's region for in the
   stack-allocated version *)
let reduce_exn l ~f =
  match l with
  | [] -> invalid_arg "List.reduce_exn"
  | hd :: tl -> fold ~init:hd ~f tl
;;

let reduce_balanced l ~f =
  (* Call the "size" of a value the number of list elements that have been combined into
     it via calls to [f]. We proceed by using [f] to combine elements in the accumulator
     of the same size until we can't combine any more, then getting a new element from the
     input list and repeating.

     With this strategy, in the accumulator:
     - we only ever have elements of sizes a power of two
     - we never have more than one element of each size
     - the sum of all the element sizes is equal to the number of elements consumed

     These conditions enforce that list of elements of each size is precisely the binary
     expansion of the number of elements consumed: if you've consumed 13 = 0b1101
     elements, you have one element of size 8, one of size 4, and one of size 1. Hence
     when a new element comes along, the number of combinings you need to do is the number
     of trailing 1s in the binary expansion of [num], the number of elements that have
     already gone into the accumulator. The accumulator is in ascending order of size, so
     the next element to combine with is always the head of the list. *)
  let rec step_accum num acc x =
    if num land 1 = 0
    then x :: acc
    else (
      match acc with
      | [] -> assert false
      (* New elements from later in the input list go on the front of the accumulator, so
         the accumulator is in reverse order wrt the original list order, hence [f y x]
         instead of [f x y]. *)
      | y :: ys -> step_accum (num asr 1) ys (f y x))
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
;;

let reduce_balanced_exn l ~f =
  match reduce_balanced l ~f with
  | None -> invalid_arg "List.reduce_balanced_exn"
  | Some v -> v
;;

let groupi l ~break =
  (* We allocate shared position and list references so we can make the inner loop use
     [[@tail_mod_cons]], and still return back information about position and where in the
     list we left off. *)
  let pos = ref 0 in
  let l = ref l in
  (* As a result of using local references, our inner loop does not need arguments. *)
  let[@tail_mod_cons] rec take_group () =
    match !l with
    | ([] | [ _ ]) as group ->
      l := [];
      group
    | x :: (y :: _ as tl) ->
      pos := !pos + 1;
      l := tl;
      if break !pos x y then [ x ] else x :: take_group ()
  in
  (* Our outer loop does not need arguments, either. *)
  let[@tail_mod_cons] rec groups () =
    if is_empty !l
    then []
    else (
      let group = take_group () in
      group :: groups ())
  in
  groups () [@nontail]
;;

(* We cannot store a [local] accumulator inside of a [ref], so we need a separate
   stack-allocating version *)

let group l ~break = groupi l ~break:(fun _ x y -> break x y) [@nontail]

let[@tail_mod_cons] rec merge l1 l2 ~compare =
  match l1, l2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | h1 :: t1, h2 :: t2 ->
    if compare h1 h2 <= 0 then h1 :: merge t1 l2 ~compare else h2 :: merge l1 t2 ~compare
;;

let merge_all lists ~compare =
  (* Merge pairs of lists until all are merged *)
  let[@tail_mod_cons] rec merge_pairs ~compare = function
    | [] -> []
    | [ l ] -> [ l ]
    | l1 :: l2 :: rest ->
      (merge [@tailcall false]) l1 l2 ~compare :: (merge_pairs [@tailcall]) ~compare rest
  in
  let rec loop ~compare = function
    | [] -> []
    | [ l ] -> l
    | lists -> loop ~compare (merge_pairs ~compare lists)
  in
  loop ~compare lists
;;

let stable_sort l ~compare:cmp =
  let rec rev_merge cmp l1 l2 accu =
    match l1, l2 with
    | [], l2 -> rev_append l2 accu
    | l1, [] -> rev_append l1 accu
    | h1 :: t1, h2 :: t2 ->
      if (cmp [@zero_alloc assume]) h1 h2 <= 0
      then rev_merge cmp t1 l2 (h1 :: accu)
      else rev_merge cmp l1 t2 (h2 :: accu)
  in
  let rec rev_merge_rev cmp l1 l2 accu =
    match l1, l2 with
    | [], l2 -> rev_append l2 accu
    | l1, [] -> rev_append l1 accu
    | h1 :: t1, h2 :: t2 ->
      if (cmp [@zero_alloc assume]) h1 h2 > 0
      then rev_merge_rev cmp t1 l2 (h1 :: accu)
      else rev_merge_rev cmp l1 t2 (h2 :: accu)
  in
  let rec sort cmp n l =
    match n, l with
    | 2, x1 :: x2 :: tl ->
      let s = if (cmp [@zero_alloc assume]) x1 x2 <= 0 then [ x1; x2 ] else [ x2; x1 ] in
      s, tl
    | 3, x1 :: x2 :: x3 :: tl ->
      let s =
        if (cmp [@zero_alloc assume]) x1 x2 <= 0
        then
          if (cmp [@zero_alloc assume]) x2 x3 <= 0
          then [ x1; x2; x3 ]
          else if (cmp [@zero_alloc assume]) x1 x3 <= 0
          then [ x1; x3; x2 ]
          else [ x3; x1; x2 ]
        else if (cmp [@zero_alloc assume]) x1 x3 <= 0
        then [ x2; x1; x3 ]
        else if (cmp [@zero_alloc assume]) x2 x3 <= 0
        then [ x2; x3; x1 ]
        else [ x3; x2; x1 ]
      in
      s, tl
    | n, l ->
      let n1 = n asr 1 in
      let n2 = n - n1 in
      let s1, l2 = rev_sort cmp n1 l in
      let s2, tl = rev_sort cmp n2 l2 in
      rev_merge_rev cmp s1 s2 [], tl
  and rev_sort cmp n l =
    match n, l with
    | 2, x1 :: x2 :: tl ->
      let s = if (cmp [@zero_alloc assume]) x1 x2 > 0 then [ x1; x2 ] else [ x2; x1 ] in
      s, tl
    | 3, x1 :: x2 :: x3 :: tl ->
      let s =
        if (cmp [@zero_alloc assume]) x1 x2 > 0
        then
          if (cmp [@zero_alloc assume]) x2 x3 > 0
          then [ x1; x2; x3 ]
          else if (cmp [@zero_alloc assume]) x1 x3 > 0
          then [ x1; x3; x2 ]
          else [ x3; x1; x2 ]
        else if (cmp [@zero_alloc assume]) x1 x3 > 0
        then [ x2; x1; x3 ]
        else if (cmp [@zero_alloc assume]) x2 x3 > 0
        then [ x2; x3; x1 ]
        else [ x3; x2; x1 ]
      in
      s, tl
    | n, l ->
      let n1 = n asr 1 in
      let n2 = n - n1 in
      let s1, l2 = sort cmp n1 l in
      let s2, tl = sort cmp n2 l2 in
      rev_merge cmp s1 s2 [], tl
  in
  let len = length l in
  if len < 2 then l else fst (sort cmp len l)
;;

let sort = stable_sort

let sort_and_group l ~compare =
  (l |> stable_sort ~compare |> group ~break:(fun x y -> compare x y <> 0)) [@nontail]
;;

let dedup_and_sort l ~compare:cmp =
  let rec rev_merge cmp l1 l2 accu =
    match l1, l2 with
    | [], l2 -> rev_append l2 accu
    | l1, [] -> rev_append l1 accu
    | h1 :: t1, h2 :: t2 ->
      (match (cmp [@zero_alloc assume]) h1 h2 with
       | c when c < 0 -> rev_merge cmp t1 l2 (h1 :: accu)
       | c when c > 0 -> rev_merge cmp l1 t2 (h2 :: accu)
       | _ -> rev_merge cmp t1 l2 accu)
  in
  let rec rev_merge_rev cmp l1 l2 accu =
    match l1, l2 with
    | [], l2 -> rev_append l2 accu
    | l1, [] -> rev_append l1 accu
    | h1 :: t1, h2 :: t2 ->
      (match (cmp [@zero_alloc assume]) h1 h2 with
       | c when c > 0 -> rev_merge_rev cmp t1 l2 (h1 :: accu)
       | c when c < 0 -> rev_merge_rev cmp l1 t2 (h2 :: accu)
       | _ -> rev_merge_rev cmp t1 l2 accu)
  in
  let rec sort cmp n l =
    match n, l with
    | 2, x1 :: x2 :: tl ->
      let s =
        match (cmp [@zero_alloc assume]) x1 x2 with
        | c when c < 0 -> [ x1; x2 ]
        | c when c > 0 -> [ x2; x1 ]
        | _ -> [ x2 ]
      in
      s, tl
    | 3, x1 :: x2 :: x3 :: tl ->
      let s =
        match (cmp [@zero_alloc assume]) x1 x2 with
        | c when c < 0 ->
          (match (cmp [@zero_alloc assume]) x2 x3 with
           | c when c < 0 -> [ x1; x2; x3 ]
           | c when c > 0 ->
             (match (cmp [@zero_alloc assume]) x1 x3 with
              | c when c < 0 -> [ x1; x3; x2 ]
              | c when c > 0 -> [ x3; x1; x2 ]
              | _ -> [ x3; x2 ])
           | _ -> [ x1; x3 ])
        | c when c > 0 ->
          (match (cmp [@zero_alloc assume]) x1 x3 with
           | c when c < 0 -> [ x2; x1; x3 ]
           | c when c > 0 ->
             (match (cmp [@zero_alloc assume]) x2 x3 with
              | c when c < 0 -> [ x2; x3; x1 ]
              | c when c > 0 -> [ x3; x2; x1 ]
              | _ -> [ x3; x1 ])
           | _ -> [ x2; x3 ])
        | _ ->
          (match (cmp [@zero_alloc assume]) x2 x3 with
           | c when c < 0 -> [ x2; x3 ]
           | c when c > 0 -> [ x3; x2 ]
           | _ -> [ x3 ])
      in
      s, tl
    | n, l ->
      let n1 = n asr 1 in
      let n2 = n - n1 in
      let s1, l2 = rev_sort cmp n1 l in
      let s2, tl = rev_sort cmp n2 l2 in
      rev_merge_rev cmp s1 s2 [], tl
  and rev_sort cmp n l =
    match n, l with
    | 2, x1 :: x2 :: tl ->
      let s =
        match (cmp [@zero_alloc assume]) x1 x2 with
        | c when c > 0 -> [ x1; x2 ]
        | c when c < 0 -> [ x2; x1 ]
        | _ -> [ x2 ]
      in
      s, tl
    | 3, x1 :: x2 :: x3 :: tl ->
      let s =
        match (cmp [@zero_alloc assume]) x1 x2 with
        | c when c > 0 ->
          (match (cmp [@zero_alloc assume]) x2 x3 with
           | c when c > 0 -> [ x1; x2; x3 ]
           | c when c < 0 ->
             (match (cmp [@zero_alloc assume]) x1 x3 with
              | c when c > 0 -> [ x1; x3; x2 ]
              | c when c < 0 -> [ x3; x1; x2 ]
              | _ -> [ x3; x2 ])
           | _ -> [ x1; x3 ])
        | c when c < 0 ->
          (match (cmp [@zero_alloc assume]) x1 x3 with
           | c when c > 0 -> [ x2; x1; x3 ]
           | c when c < 0 ->
             (match (cmp [@zero_alloc assume]) x2 x3 with
              | c when c > 0 -> [ x2; x3; x1 ]
              | c when c < 0 -> [ x3; x2; x1 ]
              | _ -> [ x3; x1 ])
           | _ -> [ x2; x3 ])
        | _ ->
          (match (cmp [@zero_alloc assume]) x2 x3 with
           | c when c > 0 -> [ x2; x3 ]
           | c when c < 0 -> [ x3; x2 ]
           | _ -> [ x3 ])
      in
      s, tl
    | n, l ->
      let n1 = n asr 1 in
      let n2 = n - n1 in
      let s1, l2 = sort cmp n1 l in
      let s2, tl = sort cmp n2 l2 in
      rev_merge cmp s1 s2 [], tl
  in
  let len = length l in
  if len < 2 then l else fst (sort cmp len l)
;;

let stable_dedup list ~compare =
  match list with
  | [] | [ _ ] -> list (* special case for performance *)
  | _ :: _ :: _ ->
    let open struct
      type 'a dedup =
        { elt : 'a
        ; mutable dup : bool
        }
    end in
    (* [stable_dedup] keeps the first of each set of duplicates. [dedup_and_sort] keeps
       the last. We define one in terms of the other by passing the values in reverse
       order, hence the [rev_map] in the definition of [dedups]. We restore the order in
       the final [fold]. *)
    let dedups = rev_map list ~f:(fun elt -> { elt; dup = true }) in
    let unique = dedup_and_sort dedups ~compare:(fun x y -> compare x.elt y.elt) in
    iter unique ~f:(fun dedup -> dedup.dup <- false);
    fold dedups ~init:[] ~f:(fun acc dedup -> if dedup.dup then acc else dedup.elt :: acc)
;;

module Cartesian_product = struct
  (* We are explicit about what we export from functors so that we don't accidentally
     rebind more efficient list-specific functions. *)

  let bind = concat_map
  let map = map

  let map2 a b ~f =
    concat_map a ~f:(fun x -> map b ~f:(fun y -> f x y) [@nontail]) [@nontail]
  ;;

  let return = singleton
  let ( >>| ) = ( >>| )
  let ( >>= ) t f = bind t ~f

  open struct
    module Applicative = Applicative.Make_using_map2 (struct
        type 'a t = 'a list

        let return = return
        let map = `Custom map
        let map2 = map2
      end)

    module Monad = Monad.Make (struct
        type 'a t = 'a list

        let return = return
        let map = `Custom map
        let bind = bind
      end)
  end

  let all = Monad.all
  let all_unit = Monad.all_unit
  let ignore_m = Monad.ignore_m
  let join = Monad.join

  module Monad_infix = struct
    let ( >>| ) = ( >>| )
    let ( >>= ) = ( >>= )
  end

  let apply = Applicative.apply
  let both = Applicative.both
  let map3 = Applicative.map3
  let ( <*> ) = Applicative.( <*> )
  let ( *> ) = Applicative.( *> )
  let ( <* ) = Applicative.( <* )

  module Applicative_infix = struct
    let ( >>| ) = ( >>| )
    let ( <*> ) = Applicative.( <*> )
    let ( *> ) = Applicative.( *> )
    let ( <* ) = Applicative.( <* )
  end

  module Let_syntax = struct
    let return = return
    let ( >>| ) = ( >>| )
    let ( >>= ) = ( >>= )

    module Let_syntax = struct
      let return = return
      let bind = bind
      let map = map
      let both = both

      module Open_on_rhs = struct end
    end
  end
end

include (
  Cartesian_product :
  sig
    include Monad.S with type 'a t := 'a t
  end)

(** returns final element of list *)
let rec last_exn list =
  match list with
  | [ x ] -> x
  | _ :: tl -> last_exn tl
  | [] -> invalid_arg "List.last"
;;

(** optionally returns final element of list *)
let rec last list =
  match list with
  | [ x ] -> Some x
  | _ :: tl -> last tl
  | [] -> None
;;

let rec is_prefix list ~prefix ~equal =
  match prefix with
  | [] -> true
  | hd :: tl ->
    (match list with
     | [] -> false
     | hd' :: tl' -> equal hd hd' && is_prefix tl' ~prefix:tl ~equal)
;;

let find_consecutive_duplicate t ~equal =
  match t with
  | [] -> None
  | a1 :: t ->
    let rec find_consecutive_duplicate_loop a1 t =
      match t with
      | [] -> None
      | a2 :: t ->
        if equal a1 a2 then Some (a1, a2) else find_consecutive_duplicate_loop a2 t
    in
    find_consecutive_duplicate_loop a1 t [@nontail]
;;

(* returns list without adjacent duplicates *)
let remove_consecutive_duplicates ?(which_to_keep = `Last) list ~equal =
  let[@tail_mod_cons] rec remove_consecutive_duplicates_loop to_keep = function
    | [] -> [ to_keep ]
    | hd :: tl ->
      if equal hd to_keep
      then (
        let to_keep =
          match which_to_keep with
          | `First -> to_keep
          | `Last -> hd
        in
        remove_consecutive_duplicates_loop to_keep tl)
      else to_keep :: remove_consecutive_duplicates_loop hd tl
  in
  match list with
  | [] -> []
  | hd :: tl -> remove_consecutive_duplicates_loop hd tl [@nontail]
;;

let find_a_dup l ~compare =
  let sorted = sort l ~compare in
  let rec find_a_dup_loop l =
    match l with
    | [] | [ _ ] -> None
    | hd1 :: (hd2 :: _ as tl) ->
      if compare hd1 hd2 = 0 then Some hd1 else find_a_dup_loop tl
  in
  find_a_dup_loop sorted [@nontail]
;;

let contains_dup lst ~compare =
  match find_a_dup lst ~compare with
  | Some _ -> true
  | None -> false
;;

let find_all_dups l ~compare =
  let sorted = sort ~compare l in
  (* Walk the list and record the first of each consecutive run of identical elements *)
  let[@tail_mod_cons] rec find_all_dups_loop sorted prev ~already_recorded =
    match sorted with
    | [] -> []
    | hd :: tl ->
      if compare prev hd <> 0
      then find_all_dups_loop tl hd ~already_recorded:false
      else if already_recorded
      then find_all_dups_loop tl hd ~already_recorded:true
      else hd :: find_all_dups_loop tl hd ~already_recorded:true
  in
  match sorted with
  | [] -> []
  | hd :: tl -> find_all_dups_loop tl hd ~already_recorded:false [@nontail]
;;

let rec all_equal_to t v ~equal =
  match t with
  | [] -> true
  | x :: xs -> equal x v && all_equal_to xs v ~equal
;;

let all_equal t ~equal =
  match t with
  | [] -> None
  | x :: xs -> if all_equal_to xs x ~equal then Some x else None
;;

let sum m t ~f = Derived.sum ~fold m t ~f

let min_elt t ~compare =
  match t with
  | [] -> None
  | elt :: t ->
    Some (fold t ~init:elt ~f:(fun acc elt -> if compare acc elt > 0 then elt else acc))
;;

let max_elt t ~compare =
  match t with
  | [] -> None
  | elt :: t ->
    Some (fold t ~init:elt ~f:(fun acc elt -> if compare acc elt < 0 then elt else acc))
;;

let count t ~f = Derived.count ~fold t ~f

let counti t ~f =
  foldi t ~init:0 ~f:(fun idx count a -> if f idx a then count + 1 else count) [@nontail]
;;

let create ~len x = init_internal len ~f:(fun _ -> x) ~name:"List.create" [@nontail]
let filter_opt l = filter_map l ~f:Fn.id
let filter_or_null l = filter_map_or_null l ~f:Fn.id

let rev_filter_map l ~f =
  let rec rev_filter_map_loop l accum =
    match l with
    | [] -> accum
    | hd :: tl ->
      (match f hd with
       | Some x -> rev_filter_map_loop tl (x :: accum)
       | None -> rev_filter_map_loop tl accum)
  in
  rev_filter_map_loop l [] [@nontail]
;;

let rev_filter_mapi l ~f =
  let rec rev_filter_mapi_loop i l accum =
    match l with
    | [] -> accum
    | hd :: tl ->
      (match f i hd with
       | Some x -> rev_filter_mapi_loop (i + 1) tl (x :: accum)
       | None -> rev_filter_mapi_loop (i + 1) tl accum)
  in
  rev_filter_mapi_loop 0 l [] [@nontail]
;;

let rev_filter_map_or_null l ~f =
  let rec rev_filter_map_or_null_loop l accum =
    match l with
    | [] -> accum
    | hd :: tl ->
      (match f hd with
       | This x -> rev_filter_map_or_null_loop tl (x :: accum)
       | Null -> rev_filter_map_or_null_loop tl accum)
  in
  rev_filter_map_or_null_loop l [] [@nontail]
;;

let rev_filter_mapi_or_null l ~f =
  let rec rev_filter_mapi_or_null_loop i l accum =
    match l with
    | [] -> accum
    | hd :: tl ->
      (match f i hd with
       | This x -> rev_filter_mapi_or_null_loop (i + 1) tl (x :: accum)
       | Null -> rev_filter_mapi_or_null_loop (i + 1) tl accum)
  in
  rev_filter_mapi_or_null_loop 0 l [] [@nontail]
;;

let partition3_map t ~f =
  let rec partition3_map_loop t fst snd trd =
    match t with
    | [] -> rev fst, rev snd, rev trd
    | x :: t ->
      (match f x with
       | `Fst y -> partition3_map_loop t (y :: fst) snd trd
       | `Snd y -> partition3_map_loop t fst (y :: snd) trd
       | `Trd y -> partition3_map_loop t fst snd (y :: trd))
  in
  partition3_map_loop t [] [] [] [@nontail]
;;

let partition_tf t ~f =
  let f x : _ Either.t = if f x then First x else Second x in
  partition_map t ~f [@nontail]
;;

let partition_result t = partition_map t ~f:Result.to_either

let partition_mapi t ~f =
  let rec partition_mapi_loop i t fst snd =
    match t with
    | [] -> rev fst, rev snd
    | x :: t ->
      (match (f i x : _ Either.t) with
       | First y -> partition_mapi_loop (i + 1) t (y :: fst) snd
       | Second y -> partition_mapi_loop (i + 1) t fst (y :: snd))
  in
  partition_mapi_loop 0 t [] [] [@nontail]
;;

let partitioni_tf t ~f =
  let f i x : _ Either.t = if f i x then First x else Second x in
  partition_mapi t ~f [@nontail]
;;

let of_iter ~iter =
  let acc = ref [] in
  iter ~f:(fun x -> acc := x :: !acc);
  rev !acc
;;

module Assoc = struct
  type 'a key = ('a[@tag Sexplib0.Sexp_grammar.assoc_key_tag = List []])
  [@@deriving sexp, sexp_grammar]

  type 'a value = ('a[@tag Sexplib0.Sexp_grammar.assoc_value_tag = List []])
  [@@deriving sexp, sexp_grammar]

  type ('a, 'b) t =
    (('a key * 'b value) list[@tag Sexplib0.Sexp_grammar.assoc_tag = List []])
  [@@deriving sexp, sexp_grammar]

  let pair_of_group = function
    | [] -> assert false
    | (k, _) :: _ as list -> k, map list ~f:snd
  ;;

  let group alist ~equal =
    group alist ~break:(fun (x, _) (y, _) -> not (equal x y)) |> map ~f:pair_of_group
  ;;

  let sort_and_group alist ~compare =
    sort_and_group alist ~compare:(fun (x, _) (y, _) -> compare x y)
    |> map ~f:pair_of_group
  ;;

  let find t ~equal key =
    match find t ~f:(fun (key', _) -> equal key key') with
    | None -> None
    | Some x -> Some (snd x)
  ;;

  let find_or_null t ~equal key =
    match find_or_null t ~f:(fun (key', _) -> equal key key') with
    | Null -> Null
    | This x -> This (snd x)
  ;;

  let find_exn =
    let not_found = Not_found_s (Atom "List.Assoc.find_exn: not found") in
    let rec find_exn t ~equal key =
      match t with
      | [] ->
        raise (Portability_hacks.magic_uncontended__promise_deeply_immutable not_found)
      | (key', value) :: t -> if equal key key' then value else find_exn t ~equal key
    in
    (* named to preserve symbol in compiled binary *)
    find_exn
  ;;

  let mem t ~equal key =
    match find t ~equal key with
    | None -> false
    | Some _ -> true
  ;;

  let remove t ~equal key = filter t ~f:(fun (key', _) -> not (equal key key')) [@nontail]

  let add t ~equal key value =
    (* the remove doesn't change the map semantics, but keeps the list small *)
    (key, value) :: remove t ~equal key
  ;;

  let inverse t = map t ~f:(fun (x, y) -> y, x)
  let map t ~f = map t ~f:(fun (key, value) -> key, f value) [@nontail]
end

let sub l ~pos ~len =
  if pos < 0 || len < 0 then invalid_arg "List.sub";
  let rec drop_for_sub l n =
    if n = 0
    then l
    else (
      match l with
      | [] -> invalid_arg "List.sub"
      | _ :: tl -> drop_for_sub tl (n - 1))
  in
  (* We don't try to share [take_for_sub n l] with [l]. Unlike [take] and [split_n] where
     that applies whenever [n >= length l], here it only applies when [n = length l]. It's
     significantly less likely, and so currently (2026-09) we think it's less worth
     complicating the implementation. *)
  let[@tail_mod_cons] rec take_for_sub l n =
    if n = 0
    then []
    else (
      match l with
      | [] -> invalid_arg "List.sub"
      | hd :: tl -> hd :: take_for_sub tl (n - 1))
  in
  take_for_sub (drop_for_sub l pos) len
;;

let split_n t_orig n =
  if n <= 0
  then [], t_orig
  else (
    let rec split_n_loop n t accum =
      match t with
      | [] -> t_orig, [] (* in this case, t_orig = rev accum *)
      | hd :: tl -> if n = 0 then rev accum, t else split_n_loop (n - 1) tl (hd :: accum)
    in
    split_n_loop n t_orig [])
;;

(* call-stack size <= min(first input data-stack size, n) *)

(* does not use [split_n] to avoid passing around the dropped part *)
let take t_orig n =
  if n <= 0
  then []
  else (
    let rec take_loop n t accum =
      match t with
      | [] -> t_orig
      | hd :: tl -> if n = 0 then rev accum else take_loop (n - 1) tl (hd :: accum)
    in
    take_loop n t_orig [])
;;

(* call-stack size <= output data-stack size *)

let rec drop t n =
  match t with
  | _ :: tl when n > 0 -> drop tl (n - 1)
  | t -> t
;;

let[@cold] chunks_of_invalid_length ~length =
  invalid_argf "List.chunks_of: Expected length > 0, got %d" length ()
;;

let chunks_of l ~length =
  if length <= 0 then chunks_of_invalid_length ~length;
  let rec aux length acc l =
    match l with
    | [] -> rev acc
    | _ :: _ ->
      let sublist, l = split_n l length in
      aux length (sublist :: acc) l
  in
  aux length [] l
;;

(* call stack size is O(input list length)

   unlike other functions, the maximum call-stack depth is [2 * max_non_tailcall]
*)

let chunk_evenly t ~into =
  if into <= 0 then invalid_argf "List.chunk_evenly: Expected [into] > 0, got %d" into ();
  let num_items = length t in
  let smaller_bucket_size = num_items / into in
  let num_smaller_into = into - (num_items mod into) in
  let[@tail_mod_cons] rec chunk_evenly_loop t ~into =
    match into with
    | 0 -> []
    | _ ->
      let bucket_size =
        if into <= num_smaller_into then smaller_bucket_size else smaller_bucket_size + 1
      in
      let this_bucket, remaining = split_n t bucket_size in
      this_bucket :: chunk_evenly_loop remaining ~into:(into - 1)
  in
  chunk_evenly_loop t ~into [@nontail]
;;

let split_while =
  let[@tail_mod_cons] rec split_while_loop ~f ~rest t =
    match t with
    | hd :: tl when f hd -> hd :: (split_while_loop [@tailcall]) ~f ~rest tl
    | _ ->
      rest := t;
      []
  in
  fun xs ~f ->
    let rest = ref [] in
    let first = split_while_loop ~f ~rest xs in
    first, !rest
;;

(* Implemented without [split_while] to avoid allocating a tuple *)
let[@tail_mod_cons] rec take_while xs ~f =
  match xs with
  | hd :: tl when f hd -> hd :: (take_while [@tailcall]) tl ~f
  | _ -> []
;;

let rec drop_while t ~f =
  match t with
  | hd :: tl when f hd -> drop_while tl ~f
  | t -> t
;;

let drop_last =
  let[@tail_mod_cons] rec loop ~hd:hd1 ~tl =
    match tl with
    | [] -> []
    | hd2 :: tl -> hd1 :: (loop [@tailcall]) ~hd:hd2 ~tl
  in
  function
  | [] -> None
  | hd :: tl -> Some (loop ~hd ~tl)
;;

let drop_last_exn t =
  match drop_last t with
  | None -> failwith "List.drop_last_exn: empty list"
  | Some lst -> lst
;;

let cartesian_product list1 list2 =
  if is_empty list2
  then []
  else (
    let[@tail_mod_cons] rec cartesian_product_outer_loop l1 =
      match l1 with
      | [] -> []
      | x1 :: l1 -> cartesian_product_inner_loop x1 l1 list2
    and[@tail_mod_cons] cartesian_product_inner_loop x1 l1 l2 =
      match l2 with
      | [] -> cartesian_product_outer_loop l1
      | x2 :: l2 -> (x1, x2) :: cartesian_product_inner_loop x1 l1 l2
    in
    cartesian_product_outer_loop list1 [@nontail])
;;

(* call-stack size <= output data-stack size *)

let concat l = fold_right l ~init:[] ~f:append
let concat_no_order l = fold l ~init:[] ~f:(fun acc l -> rev_append l acc)
let cons x l = x :: l

let is_sorted l ~compare =
  let rec is_sorted_loop l =
    match l with
    | [] | [ _ ] -> true
    | x1 :: (x2 :: _ as rest) -> compare x1 x2 <= 0 && is_sorted_loop rest
  in
  is_sorted_loop l [@nontail]
;;

let is_sorted_strictly l ~compare =
  let rec is_sorted_strictly_loop l =
    match l with
    | [] | [ _ ] -> true
    | x1 :: (x2 :: _ as rest) -> compare x1 x2 < 0 && is_sorted_strictly_loop rest
  in
  is_sorted_strictly_loop l [@nontail]
;;

module Infix = struct
  let ( @ ) = append
end

let permute ?(random_state = Random.State.get_default ()) list =
  match list with
  (* special cases to speed things up in trivial cases *)
  | [] | [ _ ] -> list
  | [ x; y ] -> if Random.State.bool random_state then [ y; x ] else list
  | _ ->
    let arr = Array.of_list list in
    Array_permute.permute arr ~random_state;
    Array.to_list arr
;;

let random_element_exn ?(random_state = Random.State.get_default ()) list =
  if is_empty list
  then failwith "List.random_element_exn: empty list"
  else nth_exn list (Random.State.int random_state (length list))
;;

let random_element ?random_state list =
  try Some (random_element_exn ?random_state list) with
  | _ -> None
;;

let rec compare cmp a b =
  match a, b with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x :: xs, y :: ys ->
    let n = cmp x y in
    if n = 0 then compare cmp xs ys else n
;;

let hash_fold_t = hash_fold_list

let equal_with_local_closure (equal : _ -> _ -> _) t1 t2 =
  let rec equal_with_local_closure_loop t1 t2 =
    match t1, t2 with
    | [], [] -> true
    | x1 :: t1, x2 :: t2 -> equal x1 x2 && equal_with_local_closure_loop t1 t2
    | _ -> false
  in
  equal_with_local_closure_loop t1 t2 [@nontail]
;;

let equal : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool =
  fun f x y -> equal_with_local_closure f x y
;;

let transpose =
  let rec split_off_first_column t column_acc trimmed found_empty =
    match t with
    | [] -> column_acc, trimmed, found_empty
    | [] :: tl -> split_off_first_column tl column_acc trimmed true
    | (x :: xs) :: tl ->
      split_off_first_column tl (x :: column_acc) (xs :: trimmed) found_empty
  in
  let split_off_first_column rows = split_off_first_column rows [] [] false in
  let rec transpose_loop rows columns do_rev =
    match split_off_first_column rows with
    | [], [], _ -> Some (rev columns)
    | column, trimmed_rows, found_empty ->
      if found_empty
      then None
      else (
        let column = if do_rev then rev column else column in
        transpose_loop trimmed_rows (column :: columns) (not do_rev))
  in
  fun t -> transpose_loop t [] true
;;

exception Transpose_got_lists_of_different_lengths of int list [@@deriving sexp]

let transpose_exn l =
  match transpose l with
  | Some l -> l
  | None -> raise (Transpose_got_lists_of_different_lengths (map l ~f:length))
;;

let intersperse t ~sep =
  match t with
  | [] -> []
  | x :: xs -> x :: fold_right xs ~init:[] ~f:(fun y acc -> sep :: y :: acc)
;;

let fold_until t ~init ~f ~finish =
  let rec fold_until_loop t ~init =
    match t with
    | [] -> finish init
    | a :: xs ->
      (match f init a with
       | Container.Continue_or_stop.Continue x -> (fold_until_loop [@tailcall]) xs ~init:x
       | Stop x -> x)
  in
  fold_until_loop t ~init [@nontail]
;;

let fold_result t ~init ~f = Derived.fold_result ~fold_until ~init ~f t
let foldi_until t ~init ~f ~finish = Derived.foldi_until ~fold_until ~init ~f t ~finish
let iter_until t ~f ~finish = Derived.iter_until ~fold_until t ~f ~finish
let iteri_until t ~f ~finish = Derived.iteri_until ~foldi_until t ~f ~finish

let is_suffix list ~suffix ~equal:(equal_elt : _ -> _ -> _) =
  let list_len = length list in
  let suffix_len = length suffix in
  list_len >= suffix_len
  && equal_with_local_closure equal_elt (drop list (list_len - suffix_len)) suffix
;;

module Local = struct
  module Let_syntax = struct
    let return = singleton
    let ( >>| ) x f = map x ~f
    let ( >>= ) x f = concat_map x ~f

    module Let_syntax = struct
      let return = singleton
      let map = map
      let bind = concat_map
      let both = cartesian_product

      module Open_on_rhs = struct end
    end
  end
end

module Private = struct
  let max_non_tailcall = max_non_tailcall
end
