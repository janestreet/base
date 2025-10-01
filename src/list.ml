open! Import
module Array = Array0
module Either = Either0
include List1
include Constructors

(* This itself includes [List0]. *)

let invalid_argf = Printf.invalid_argf

[%%rederive.portable
  type ('a : value_or_null) t = 'a list
  [@@deriving globalize, sexp ~stackify, sexp_grammar]]

module Or_unequal_lengths = struct
  type ('a : value_or_null) t =
    | Ok of 'a
    | Unequal_lengths
  [@@deriving compare ~localize, sexp_of ~stackify]
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
  let[@tail_mod_cons] rec local_ loop i =
    let i_to_stop_order = order i stop_i in
    match i_to_stop_order, initial_stride_order with
    | Less, `Less | Greater, `Greater ->
      (* haven't yet reached [stop_i]. Continue. *)
      let next_i = stride i in
      (match order i next_i, initial_stride_order with
       | Equal, _ -> (raise_stride_cannot_return_same_value [@tailcall false]) ()
       | Less, `Greater | Greater, `Less ->
         invalid_arg "List.range': stride function cannot change direction"
       | Less, `Less | Greater, `Greater -> i :: loop next_i)
    | Less, `Greater | Greater, `Less ->
      (* stepped past [stop_i].  Finished. *)
      []
    | Equal, _ ->
      (* reached [stop_i].  Finished. *)
      (match stop with
       | `inclusive -> [ i ]
       | `exclusive -> [])
  in
  let start_i =
    match start with
    | `inclusive -> start_i
    | `exclusive -> next_i
  in
  loop start_i [@nontail]
;;

let range ?(stride = 1) ?(start = `inclusive) ?(stop = `exclusive) start_i stop_i =
  if stride = 0 then invalid_arg "List.range: stride must be non-zero";
  range' ~compare ~stride:(fun x -> x + stride) ~start ~stop start_i stop_i
;;

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

[@@@warning "-incompatible-with-upstream"]

[%%template
[@@@kind k = (float64, bits32, bits64, word, immediate, immediate64, value_or_null)]

open struct
  type nonrec ('a : k) t = ('a t[@kind k]) =
    | []
    | ( :: ) of 'a * ('a t[@kind k])
end

[@@@kind.default k]

let nth t n : (_ Option0.t[@kind k]) =
  if n < 0
  then None
  else (
    let rec nth_aux t n : (_ Option0.t[@kind k]) =
      match t with
      | [] -> None
      | a :: t -> if n = 0 then Some a else nth_aux t (n - 1)
    in
    nth_aux t n)
;;

let nth_exn t n =
  match (nth [@kind k]) t n with
  | None ->
    (match
       (invalid_argf
          "List.nth_exn %d called on list of length %d"
          n
          ((length [@kind k]) t)
          ()
        : Nothing0.t)
     with
     | _ -> .)
  | Some a -> a
;;

let init_internal n ~f ~name =
  if n < 0 then invalid_argf "%s %d" name n ();
  let rec loop i accum =
    assert (i >= 0);
    if i = 0 then accum else loop (i - 1) (f (i - 1) :: accum)
  in
  loop n [] [@nontail]
;;

let init n ~f = (init_internal [@kind k]) n ~f ~name:"List.init"

let iteri l ~f =
  ignore
    ((fold [@kind k value_or_null]) l ~init:0 ~f:(fun i x ->
       f i x;
       i + 1)
     : int)
;;

let mem t a ~equal =
  let rec loop equal a = function
    | [] -> false
    | b :: bs -> equal a b || loop equal a bs
  in
  loop equal a t
;;]

let unordered_append l1 l2 =
  match l1, l2 with
  | [], l | l, [] -> l
  | _ -> rev_append l1 l2
;;

module Check_length2 = struct
  type ('a : value_or_null, 'b : value_or_null) t =
    | Same_length of int
    | Unequal_lengths of
        { shared_length : int
        ; tail_of_a : 'a list
        ; tail_of_b : 'b list
        }

  (* In the [Unequal_lengths] case, at least one of the tails will be non-empty. *)
  let of_lists l1 l2 =
    let rec loop a b shared_length =
      match a, b with
      | [], [] -> Same_length shared_length
      | _ :: a, _ :: b -> loop a b (shared_length + 1)
      | [], _ | _, [] -> Unequal_lengths { shared_length; tail_of_a = a; tail_of_b = b }
    in
    loop l1 l2 0
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
  type ('a : value_or_null, 'b : value_or_null, 'c : value_or_null) t =
    | Same_length of int
    | Unequal_lengths of
        { shared_length : int
        ; tail_of_a : 'a list
        ; tail_of_b : 'b list
        ; tail_of_c : 'c list
        }

  (* In the [Unequal_lengths] case, at least one of the tails will be non-empty. *)
  let of_lists l1 l2 l3 =
    let rec loop a b c shared_length =
      match a, b, c with
      | [], [], [] -> Same_length shared_length
      | _ :: a, _ :: b, _ :: c -> loop a b c (shared_length + 1)
      | [], _, _ | _, [], _ | _, _, [] ->
        Unequal_lengths { shared_length; tail_of_a = a; tail_of_b = b; tail_of_c = c }
    in
    loop l1 l2 l3 0
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

let[@tail_mod_cons] rec filter l ~f =
  match l with
  | [] -> []
  | hd :: tl -> if f hd then hd :: filter tl ~f else filter tl ~f
;;

let find_map t ~f =
  let rec loop = function
    | [] -> None
    | x :: l ->
      (match f x with
       | None -> loop l
       | Some _ as r -> r)
  in
  loop t [@nontail]
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
  let rec loop = function
    | [] -> None
    | x :: l -> if f x then Some x else loop l
  in
  loop t [@nontail]
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
  let rec loop i t =
    match t with
    | [] -> None
    | x :: l -> if f i x then Some (i, x) else loop (i + 1) l
  in
  loop 0 t [@nontail]
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
  let rec loop i t =
    match t with
    | [] -> None
    | x :: l ->
      (match f i x with
       | Some _ as result -> result
       | None -> loop (i + 1) l)
  in
  loop 0 t [@nontail]
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
  let rec loop i t =
    match t with
    | [] -> true
    | hd :: tl -> f i hd && loop (i + 1) tl
  in
  loop 0 t [@nontail]
;;

let existsi t ~f =
  let rec loop i t =
    match t with
    | [] -> false
    | hd :: tl -> f i hd || loop (i + 1) tl
  in
  loop 0 t [@nontail]
;;

(** For the container interface. *)
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

[%%template
[@@@kind.default
  ka = (float64, bits32, bits64, word, immediate, immediate64, value_or_null)
  , kb = (float64, bits32, bits64, word, immediate, immediate64, value_or_null)]

let rev_mapi l ~f =
  let rec loop i acc : (_ t[@kind ka]) -> (_ t[@kind kb]) = function
    | [] -> acc
    | h :: t -> loop (i + 1) (f i h :: acc) t
  in
  loop 0 [] l [@nontail]
;;

let foldi t ~init ~f =
  let #(_, r) =
    (fold [@kind ka (value_or_null & kb)]) t ~init:#(0, init) ~f:(fun #(i, acc) v ->
      #(i + 1, f i acc v))
  in
  r
;;]

[%%template
[@@@kind.default
  ka = (float64, bits32, bits64, word, immediate, immediate64, value_or_null)
  , kb = (value_or_null, immediate, immediate64)]

open struct
  type nonrec ('a : ka) t = ('a t[@kind ka]) =
    | []
    | ( :: ) of 'a * ('a t[@kind ka])
  [@@kind ka]
end

let[@tail_mod_cons] rec map l ~f : (_ t[@kind kb]) =
  match l with
  | [] -> []
  | x :: tl -> f x :: (map [@kind ka kb] [@tailcall]) tl ~f
;;

let mapi l ~f =
  let[@tail_mod_cons] rec local_ loop i = function
    | [] -> ([] : (_ t[@kind kb]))
    | h :: t -> f i h :: loop (i + 1) t
  in
  loop 0 l [@nontail]
;;

let[@tail_mod_cons] rec filter_map l ~f : (_ t[@kind kb]) =
  match l with
  | [] -> []
  | hd :: tl ->
    (match (f hd : (_ Option0.t[@kind kb])) with
     | None -> (filter_map [@kind ka kb]) tl ~f
     | Some x -> x :: (filter_map [@kind ka kb]) tl ~f)
;;

let filter_mapi l ~f =
  let[@tail_mod_cons] rec local_ loop pos l : (_ t[@kind kb]) =
    match l with
    | [] -> []
    | hd :: tl ->
      (match (f pos hd : (_ Option0.t[@kind kb])) with
       | None -> loop (pos + 1) tl
       | Some x -> x :: loop (pos + 1) tl)
  in
  loop 0 l [@nontail]
;;

let concat_mapi l ~(local_ f) =
  let[@tail_mod_cons] rec local_ outer_loop pos = function
    | [] -> ([] : (_ t[@kind kb]))
    | [ hd ] -> (f [@tailcall false]) pos hd
    | hd :: (_ :: _ as tl) -> inner_loop (pos + 1) (f pos hd) tl
  and[@tail_mod_cons] local_ inner_loop pos l1 l2 =
    match l1 with
    | [] -> outer_loop pos l2
    | [ x1 ] -> x1 :: outer_loop pos l2
    | [ x1; x2 ] -> x1 :: x2 :: outer_loop pos l2
    | [ x1; x2; x3 ] -> x1 :: x2 :: x3 :: outer_loop pos l2
    | [ x1; x2; x3; x4 ] -> x1 :: x2 :: x3 :: x4 :: outer_loop pos l2
    | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
      x1 :: x2 :: x3 :: x4 :: x5 :: inner_loop pos tl l2
  in
  outer_loop 0 l [@nontail]
;;

let concat_map l ~(local_ f) =
  (concat_mapi [@kind ka kb]) l ~f:(fun _ x -> f x) [@nontail]
;;]

[%%template
[@@@kind.default k = (float64, bits32, bits64, word, immediate, immediate64)]

(* Copied from [Sexplib0] for templating *)

let sexp_of_t sexp_of__a t : Sexplib0.Sexp.t =
  List ((map [@kind k value_or_null]) ~f:sexp_of__a t)
;;

let sexp_of_t : _ -> (_ t[@kind k]) @ local -> Sexplib0.Sexp.t @ local =
  fun sexp_of__a t ->
  let map ~f lst = exclave_
    let rec rev lst acc = exclave_
      match lst with
      | [] -> acc
      | hd :: tl -> rev tl (hd :: acc)
    in
    let rec rev_map (lst : (_ t[@kind k])) acc = exclave_
      match lst with
      | [] -> acc
      | hd :: tl -> rev_map tl (f hd :: acc)
    in
    rev (rev_map lst []) []
  in
  exclave_ List (map ~f:sexp_of__a t)
[@@mode __ = stack]
;;]

[%%template
[@@@kind.default k = (float64, bits32, bits64, word, immediate, immediate64)]

open struct
  type nonrec 'a t = ('a t[@kind k]) =
    | []
    | ( :: ) of 'a * ('a t[@kind k])
  [@@kind k]
end

let filter l ~f =
  let rec local_ loop ~acc l =
    match l with
    | [] -> acc
    | hd :: tl ->
      let acc = if f hd then hd :: acc else acc in
      (loop [@tailcall]) ~acc tl
  in
  loop ~acc:[] l |> (rev [@kind k])
;;

let filteri l ~f =
  let rec local_ loop ~acc pos l =
    match l with
    | [] -> acc
    | hd :: tl ->
      let acc = if f pos hd then hd :: acc else acc in
      (loop [@tailcall]) ~acc (pos + 1) tl
  in
  (loop ~acc:[] 0 l |> (rev [@kind k])) [@nontail]
;;

let append l1 l2 =
  match l2 with
  | [] -> l1
  | _ :: _ -> (rev [@kind k]) ((rev_append [@kind k]) l2 ((rev [@kind k]) l1))
;;]

[%%template
[@@@kind.default
  ka = (float64, bits32, bits64, word, immediate, immediate64, value_or_null)
  , kb = (float64, bits32, bits64, word)]

open struct
  type nonrec ('a : ka) t = ('a t[@kind ka]) =
    | []
    | ( :: ) of 'a * ('a t[@kind ka])
  [@@kind ka]
end

let map (l : (_ t[@kind ka])) ~f : (_ t[@kind kb]) =
  match l with
  | [] -> []
  | [ x ] -> [ f x ]
  | _ :: _ -> ((rev_map [@kind ka kb]) l ~f |> (rev [@kind kb])) [@nontail]
;;

let mapi (l : (_ t[@kind ka])) ~(local_ f) : (_ t[@kind kb]) =
  match l with
  | [] -> []
  | [ x ] -> [ f 0 x ]
  | _ :: _ -> ((rev_mapi [@kind ka kb]) l ~f |> (rev [@kind kb])) [@nontail]
;;

let filter_map l ~f =
  let rec local_ loop ~acc : (_ t[@kind ka]) -> (_ t[@kind kb]) = function
    | [] -> acc
    | hd :: tl ->
      (match (f hd : (_ Option0.t[@kind kb])) with
       | Some hd -> (loop [@tailcall]) ~acc:(hd :: acc) tl
       | None -> (loop [@tailcall]) ~acc tl)
  in
  (loop ~acc:[] l |> (rev [@kind kb])) [@nontail]
;;

let filter_mapi l ~f =
  let rec local_ loop ~acc pos : (_ t[@kind ka]) -> (_ t[@kind kb]) = function
    | [] -> acc
    | hd :: tl ->
      (match (f pos hd : (_ Option0.t[@kind kb])) with
       | Some hd -> (loop [@tailcall]) ~acc:(hd :: acc) (pos + 1) tl
       | None -> (loop [@tailcall]) ~acc (pos + 1) tl)
  in
  (loop ~acc:[] 0 l |> (rev [@kind kb])) [@nontail]
;;

let concat_mapi l ~(local_ f) =
  let rec local_ loop ~acc pos : (_ t[@kind ka]) -> (_ t[@kind kb]) = function
    | [] -> []
    | [ hd ] -> (rev_append [@kind kb]) acc (f pos hd)
    | hd :: (_ :: _ as tl) ->
      let acc = (rev_append [@kind kb]) (f pos hd) acc in
      (loop [@tailcall]) ~acc (pos + 1) tl
  in
  loop ~acc:[] 0 l [@nontail]
;;

let concat_map l ~(local_ f) =
  let rec local_ loop ~acc : (_ t[@kind ka]) -> (_ t[@kind kb]) = function
    | [] -> []
    | [ hd ] -> (rev_append [@kind kb]) acc (f hd)
    | hd :: (_ :: _ as tl) ->
      let acc = (rev_append [@kind kb]) (f hd) acc in
      (loop [@tailcall]) ~acc tl
  in
  loop ~acc:[] l [@nontail]
;;]

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

let ( >>| ) l f = map l ~f

let[@tail_mod_cons] rec map2_ok l1 l2 ~f =
  match l1, l2 with
  | [], [] -> []
  | x1 :: l1, x2 :: l2 -> f x1 x2 :: map2_ok l1 l2 ~f
  | _, _ -> invalid_arg "List.map2"
;;

let map2 l1 l2 ~f = check_length2 l1 l2 ~f:(map2_ok ~f) [@nontail]

let map2_exn l1 l2 ~f =
  check_length2_exn "map2_exn" l1 l2;
  map2_ok l1 l2 ~f
;;

let rev_map3_ok l1 l2 l3 ~f =
  let rec loop l1 l2 l3 ac =
    match l1, l2, l3 with
    | [], [], [] -> ac
    | x1 :: l1, x2 :: l2, x3 :: l3 -> loop l1 l2 l3 (f x1 x2 x3 :: ac)
    | _ -> assert false
  in
  loop l1 l2 l3 [] [@nontail]
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
  let rec loop list l1 l2 =
    match list with
    | [] -> l1, l2
    | (x, y) :: tl -> loop tl (x :: l1) (y :: l2)
  in
  loop (rev list) [] []
;;

let unzip3 list =
  let rec loop list l1 l2 l3 =
    match list with
    | [] -> l1, l2, l3
    | (x, y, z) :: tl -> loop tl (x :: l1) (y :: l2) (z :: l3)
  in
  loop (rev list) [] [] []
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

let filteri l ~f =
  let[@tail_mod_cons] rec local_ loop pos l =
    match l with
    | [] -> []
    | hd :: tl -> if f pos hd then hd :: loop (pos + 1) tl else loop (pos + 1) tl
  in
  loop 0 l [@nontail]
;;

let reduce l ~f =
  match l with
  | [] -> None
  | hd :: tl -> Some (fold ~init:hd ~f tl)
;;

let reduce_exn l ~f =
  match reduce l ~f with
  | None -> invalid_arg "List.reduce_exn"
  | Some v -> v
;;

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
  let local_ pos = ref 0 in
  let local_ l = ref l in
  (* As a result of using local references, our inner loop does not need arguments. *)
  let[@tail_mod_cons] rec local_ take_group () =
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
  let[@tail_mod_cons] rec local_ groups () =
    if is_empty !l
    then []
    else (
      let group = take_group () in
      group :: groups ())
  in
  groups () [@nontail]
;;

let group l ~break = groupi l ~break:(fun _ x y -> break x y) [@nontail]

let[@tail_mod_cons] rec merge l1 l2 ~compare =
  match l1, l2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | h1 :: t1, h2 :: t2 ->
    if compare h1 h2 <= 0 then h1 :: merge t1 l2 ~compare else h2 :: merge l1 t2 ~compare
;;

let stable_sort l ~compare:cmp =
  let rec rev_merge cmp l1 l2 accu =
    match l1, l2 with
    | [], l2 -> rev_append l2 accu
    | l1, [] -> rev_append l1 accu
    | h1 :: t1, h2 :: t2 ->
      if cmp h1 h2 <= 0
      then rev_merge cmp t1 l2 (h1 :: accu)
      else rev_merge cmp l1 t2 (h2 :: accu)
  in
  let rec rev_merge_rev cmp l1 l2 accu =
    match l1, l2 with
    | [], l2 -> rev_append l2 accu
    | l1, [] -> rev_append l1 accu
    | h1 :: t1, h2 :: t2 ->
      if cmp h1 h2 > 0
      then rev_merge_rev cmp t1 l2 (h1 :: accu)
      else rev_merge_rev cmp l1 t2 (h2 :: accu)
  in
  let rec sort n l =
    match n, l with
    | 2, x1 :: x2 :: tl ->
      let s = if cmp x1 x2 <= 0 then [ x1; x2 ] else [ x2; x1 ] in
      s, tl
    | 3, x1 :: x2 :: x3 :: tl ->
      let s =
        if cmp x1 x2 <= 0
        then
          if cmp x2 x3 <= 0
          then [ x1; x2; x3 ]
          else if cmp x1 x3 <= 0
          then [ x1; x3; x2 ]
          else [ x3; x1; x2 ]
        else if cmp x1 x3 <= 0
        then [ x2; x1; x3 ]
        else if cmp x2 x3 <= 0
        then [ x2; x3; x1 ]
        else [ x3; x2; x1 ]
      in
      s, tl
    | n, l ->
      let n1 = n asr 1 in
      let n2 = n - n1 in
      let s1, l2 = rev_sort n1 l in
      let s2, tl = rev_sort n2 l2 in
      rev_merge_rev cmp s1 s2 [], tl
  and rev_sort n l =
    match n, l with
    | 2, x1 :: x2 :: tl ->
      let s = if cmp x1 x2 > 0 then [ x1; x2 ] else [ x2; x1 ] in
      s, tl
    | 3, x1 :: x2 :: x3 :: tl ->
      let s =
        if cmp x1 x2 > 0
        then
          if cmp x2 x3 > 0
          then [ x1; x2; x3 ]
          else if cmp x1 x3 > 0
          then [ x1; x3; x2 ]
          else [ x3; x1; x2 ]
        else if cmp x1 x3 > 0
        then [ x2; x1; x3 ]
        else if cmp x2 x3 > 0
        then [ x2; x3; x1 ]
        else [ x3; x2; x1 ]
      in
      s, tl
    | n, l ->
      let n1 = n asr 1 in
      let n2 = n - n1 in
      let s1, l2 = sort n1 l in
      let s2, tl = sort n2 l2 in
      rev_merge cmp s1 s2 [], tl
  in
  let len = length l in
  if len < 2 then l else fst (sort len l)
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
      (match cmp h1 h2 with
       | c when c < 0 -> rev_merge cmp t1 l2 (h1 :: accu)
       | c when c > 0 -> rev_merge cmp l1 t2 (h2 :: accu)
       | _ -> rev_merge cmp t1 l2 accu)
  in
  let rec rev_merge_rev cmp l1 l2 accu =
    match l1, l2 with
    | [], l2 -> rev_append l2 accu
    | l1, [] -> rev_append l1 accu
    | h1 :: t1, h2 :: t2 ->
      (match cmp h1 h2 with
       | c when c > 0 -> rev_merge_rev cmp t1 l2 (h1 :: accu)
       | c when c < 0 -> rev_merge_rev cmp l1 t2 (h2 :: accu)
       | _ -> rev_merge_rev cmp t1 l2 accu)
  in
  let rec sort n l =
    match n, l with
    | 2, x1 :: x2 :: tl ->
      let s =
        match cmp x1 x2 with
        | c when c < 0 -> [ x1; x2 ]
        | c when c > 0 -> [ x2; x1 ]
        | _ -> [ x2 ]
      in
      s, tl
    | 3, x1 :: x2 :: x3 :: tl ->
      let s =
        match cmp x1 x2 with
        | c when c < 0 ->
          (match cmp x2 x3 with
           | c when c < 0 -> [ x1; x2; x3 ]
           | c when c > 0 ->
             (match cmp x1 x3 with
              | c when c < 0 -> [ x1; x3; x2 ]
              | c when c > 0 -> [ x3; x1; x2 ]
              | _ -> [ x3; x2 ])
           | _ -> [ x1; x3 ])
        | c when c > 0 ->
          (match cmp x1 x3 with
           | c when c < 0 -> [ x2; x1; x3 ]
           | c when c > 0 ->
             (match cmp x2 x3 with
              | c when c < 0 -> [ x2; x3; x1 ]
              | c when c > 0 -> [ x3; x2; x1 ]
              | _ -> [ x3; x1 ])
           | _ -> [ x2; x3 ])
        | _ ->
          (match cmp x2 x3 with
           | c when c < 0 -> [ x2; x3 ]
           | c when c > 0 -> [ x3; x2 ]
           | _ -> [ x3 ])
      in
      s, tl
    | n, l ->
      let n1 = n asr 1 in
      let n2 = n - n1 in
      let s1, l2 = rev_sort n1 l in
      let s2, tl = rev_sort n2 l2 in
      rev_merge_rev cmp s1 s2 [], tl
  and rev_sort n l =
    match n, l with
    | 2, x1 :: x2 :: tl ->
      let s =
        match cmp x1 x2 with
        | c when c > 0 -> [ x1; x2 ]
        | c when c < 0 -> [ x2; x1 ]
        | _ -> [ x2 ]
      in
      s, tl
    | 3, x1 :: x2 :: x3 :: tl ->
      let s =
        match cmp x1 x2 with
        | c when c > 0 ->
          (match cmp x2 x3 with
           | c when c > 0 -> [ x1; x2; x3 ]
           | c when c < 0 ->
             (match cmp x1 x3 with
              | c when c > 0 -> [ x1; x3; x2 ]
              | c when c < 0 -> [ x3; x1; x2 ]
              | _ -> [ x3; x2 ])
           | _ -> [ x1; x3 ])
        | c when c < 0 ->
          (match cmp x1 x3 with
           | c when c > 0 -> [ x2; x1; x3 ]
           | c when c < 0 ->
             (match cmp x2 x3 with
              | c when c > 0 -> [ x2; x3; x1 ]
              | c when c < 0 -> [ x3; x2; x1 ]
              | _ -> [ x3; x1 ])
           | _ -> [ x2; x3 ])
        | _ ->
          (match cmp x2 x3 with
           | c when c > 0 -> [ x2; x3 ]
           | c when c < 0 -> [ x3; x2 ]
           | _ -> [ x3 ])
      in
      s, tl
    | n, l ->
      let n1 = n asr 1 in
      let n2 = n - n1 in
      let s1, l2 = sort n1 l in
      let s2, tl = sort n2 l2 in
      rev_merge cmp s1 s2 [], tl
  in
  let len = length l in
  if len < 2 then l else fst (sort len l)
;;

let stable_dedup list ~compare =
  match list with
  | [] | [ _ ] -> list (* special case for performance *)
  | _ :: _ :: _ ->
    let open struct
      type ('a : value_or_null) dedup =
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
  let ( >>= ) t (local_ f) = bind t ~f

  open struct
    module%template Applicative =
    Applicative.Make_using_map2
      [@kind value_or_null mod maybe_null]
      [@mode local]
      [@modality portable]
      (struct
        type ('a : value_or_null) t = 'a list

        let return = return
        let map = `Custom map
        let map2 = map2
      end)

    module%template Monad =
    Monad.Make [@kind value_or_null mod maybe_null] [@mode local] [@modality portable] (struct
        type ('a : value_or_null) t = 'a list

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
  @@ portable
    include%template
      Monad.S
      [@kind value_or_null mod maybe_null] [@mode local]
      with type ('a : value_or_null) t := 'a t
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
    let rec loop a1 t =
      match t with
      | [] -> None
      | a2 :: t -> if equal a1 a2 then Some (a1, a2) else loop a2 t
    in
    loop a1 t [@nontail]
;;

(* returns list without adjacent duplicates *)
let remove_consecutive_duplicates ?(which_to_keep = `Last) list ~equal =
  let rec loop to_keep accum = function
    | [] -> to_keep :: accum
    | hd :: tl ->
      if equal hd to_keep
      then (
        let to_keep =
          match which_to_keep with
          | `First -> to_keep
          | `Last -> hd
        in
        loop to_keep accum tl)
      else loop hd (to_keep :: accum) tl
  in
  match list with
  | [] -> []
  | hd :: tl -> rev (loop hd [] tl)
;;

let find_a_dup l ~compare =
  let sorted = sort l ~compare in
  let rec loop l =
    match l with
    | [] | [ _ ] -> None
    | hd1 :: (hd2 :: _ as tl) -> if compare hd1 hd2 = 0 then Some hd1 else loop tl
  in
  loop sorted [@nontail]
;;

let contains_dup lst ~compare =
  match find_a_dup lst ~compare with
  | Some _ -> true
  | None -> false
;;

let find_all_dups l ~compare =
  let sorted = sort ~compare l in
  (* Walk the list and record the first of each consecutive run of identical elements *)
  let[@tail_mod_cons] rec local_ loop sorted prev ~already_recorded =
    match sorted with
    | [] -> []
    | hd :: tl ->
      if compare prev hd <> 0
      then loop tl hd ~already_recorded:false
      else if already_recorded
      then loop tl hd ~already_recorded:true
      else hd :: loop tl hd ~already_recorded:true
  in
  match sorted with
  | [] -> []
  | hd :: tl -> loop tl hd ~already_recorded:false [@nontail]
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

let count t ~f = Container.count ~fold t ~f
let sum m t ~f = Container.sum ~fold m t ~f

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

let counti t ~f =
  foldi t ~init:0 ~f:(fun idx count a -> if f idx a then count + 1 else count) [@nontail]
;;

let create ~len x =
  init_internal len ~f:(local_ fun _ -> x) ~name:"List.create" [@nontail]
;;

let rev_filter_map l ~f =
  let rec loop l accum =
    match l with
    | [] -> accum
    | hd :: tl ->
      (match f hd with
       | Some x -> loop tl (x :: accum)
       | None -> loop tl accum)
  in
  loop l [] [@nontail]
;;

let rev_filter_mapi l ~f =
  let rec loop i l accum =
    match l with
    | [] -> accum
    | hd :: tl ->
      (match f i hd with
       | Some x -> loop (i + 1) tl (x :: accum)
       | None -> loop (i + 1) tl accum)
  in
  loop 0 l [] [@nontail]
;;

let filter_opt l = filter_map l ~f:Fn.id

let partition3_map t ~f =
  let rec loop t fst snd trd =
    match t with
    | [] -> rev fst, rev snd, rev trd
    | x :: t ->
      (match f x with
       | `Fst y -> loop t (y :: fst) snd trd
       | `Snd y -> loop t fst (y :: snd) trd
       | `Trd y -> loop t fst snd (y :: trd))
  in
  loop t [] [] [] [@nontail]
;;

let partition_tf t ~f =
  let f x : _ Either.t = if f x then First x else Second x in
  partition_map t ~f [@nontail]
;;

let partition_result t = partition_map t ~f:Result.to_either

let partition_mapi t ~f =
  let rec loop i t fst snd =
    match t with
    | [] -> rev fst, rev snd
    | x :: t ->
      (match (f i x : _ Either0.t) with
       | First y -> loop (i + 1) t (y :: fst) snd
       | Second y -> loop (i + 1) t fst (y :: snd))
  in
  loop 0 t [] [] [@nontail]
;;

let partitioni_tf t ~f =
  let f i x : _ Either.t = if f i x then First x else Second x in
  partition_mapi t ~f [@nontail]
;;

let of_iter ~iter =
  let local_ acc = ref [] in
  iter ~f:(fun x -> acc := x :: !acc);
  rev !acc
;;

module Assoc = struct
  type ('a : value_or_null) key = ('a[@tag Sexplib0.Sexp_grammar.assoc_key_tag = List []])
  [@@deriving sexp ~stackify, sexp_grammar]

  type ('a : value_or_null) value =
    ('a[@tag Sexplib0.Sexp_grammar.assoc_value_tag = List []])
  [@@deriving sexp ~stackify, sexp_grammar]

  type ('a : value_or_null, 'b : value_or_null) t =
    (('a key * 'b value) list[@tag Sexplib0.Sexp_grammar.assoc_tag = List []])
  [@@deriving sexp ~stackify, sexp_grammar]

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
  (* We use [pos > length l - len] rather than [pos + len > length l] to avoid the
     possibility of overflow. *)
  if pos < 0 || len < 0 || pos > length l - len then invalid_arg "List.sub";
  let stop = pos + len in
  let[@tail_mod_cons] rec local_ loop i l =
    match l with
    | [] -> []
    | hd :: tl ->
      if i < pos then loop (i + 1) tl else if i < stop then hd :: loop (i + 1) tl else []
  in
  loop 0 l [@nontail]
;;

let split_n t_orig n =
  if n <= 0
  then [], t_orig
  else (
    let rec loop n t accum =
      match t with
      | [] -> t_orig, [] (* in this case, t_orig = rev accum *)
      | hd :: tl -> if n = 0 then rev accum, t else loop (n - 1) tl (hd :: accum)
    in
    loop n t_orig [])
;;

(* copied from [split_n] to avoid allocating a tuple *)
let take t_orig n =
  if n <= 0
  then []
  else (
    let rec loop n t accum =
      match t with
      | [] -> t_orig
      | hd :: tl -> if n = 0 then rev accum else loop (n - 1) tl (hd :: accum)
    in
    loop n t_orig [])
;;

let rec drop t n =
  match t with
  | _ :: tl when n > 0 -> drop tl (n - 1)
  | t -> t
;;

let chunks_of l ~length =
  if length <= 0 then invalid_argf "List.chunks_of: Expected length > 0, got %d" length ();
  let rec aux length acc l =
    match l with
    | [] -> rev acc
    | _ :: _ ->
      let sublist, l = split_n l length in
      aux length (sublist :: acc) l
  in
  aux length [] l
;;

let split_while xs ~f =
  let rec loop acc = function
    | hd :: tl when f hd -> loop (hd :: acc) tl
    | t -> rev acc, t
  in
  loop [] xs [@nontail]
;;

(* copied from [split_while] to avoid allocating a tuple *)
let take_while xs ~f =
  let rec loop acc = function
    | hd :: tl when f hd -> loop (hd :: acc) tl
    | _ -> rev acc
  in
  loop [] xs [@nontail]
;;

let rec drop_while t ~f =
  match t with
  | hd :: tl when f hd -> drop_while tl ~f
  | t -> t
;;

let drop_last t =
  match rev t with
  | [] -> None
  | _ :: lst -> Some (rev lst)
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
    let[@tail_mod_cons] rec local_ outer_loop l1 =
      match l1 with
      | [] -> []
      | x1 :: l1 -> inner_loop x1 l1 list2
    and[@tail_mod_cons] local_ inner_loop x1 l1 l2 =
      match l2 with
      | [] -> outer_loop l1
      | x2 :: l2 -> (x1, x2) :: inner_loop x1 l1 l2
    in
    outer_loop list1 [@nontail])
;;

let concat l = fold_right l ~init:[] ~f:append
let concat_no_order l = fold l ~init:[] ~f:(fun acc l -> rev_append l acc)
let cons x l = x :: l

let is_sorted l ~compare =
  let rec loop l =
    match l with
    | [] | [ _ ] -> true
    | x1 :: (x2 :: _ as rest) -> compare x1 x2 <= 0 && loop rest
  in
  loop l [@nontail]
;;

let is_sorted_strictly l ~compare =
  let rec loop l =
    match l with
    | [] | [ _ ] -> true
    | x1 :: (x2 :: _ as rest) -> compare x1 x2 < 0 && loop rest
  in
  loop l [@nontail]
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

let rec compare__local cmp a b =
  match a, b with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x :: xs, y :: ys ->
    let n = cmp x y in
    if n = 0 then compare__local cmp xs ys else n
;;

let hash_fold_t = hash_fold_list

let equal_with_local_closure (local_ (equal : _ -> _ -> _)) t1 t2 =
  let rec loop ~equal t1 t2 =
    match t1, t2 with
    | [], [] -> true
    | x1 :: t1, x2 :: t2 -> equal x1 x2 && loop ~equal t1 t2
    | _ -> false
  in
  loop ~equal t1 t2
;;

let equal : ('a : value_or_null). ('a -> 'a -> bool) -> 'a t -> 'a t -> bool =
  fun f x y -> equal_with_local_closure f x y
;;

let equal__local equal_a__local t1 t2 =
  let rec loop ~equal_a__local t1 t2 =
    match t1, t2 with
    | [], [] -> true
    | x1 :: t1, x2 :: t2 -> equal_a__local x1 x2 && loop ~equal_a__local t1 t2
    | _ -> false
  in
  loop ~equal_a__local t1 t2 [@nontail]
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
  let rec loop rows columns do_rev =
    match split_off_first_column rows with
    | [], [], _ -> Some (rev columns)
    | column, trimmed_rows, found_empty ->
      if found_empty
      then None
      else (
        let column = if do_rev then rev column else column in
        loop trimmed_rows (column :: columns) (not do_rev))
  in
  fun t -> loop t [] true
;;

exception Transpose_got_lists_of_different_lengths of int list [@@deriving sexp]

let transpose_exn l =
  match transpose l with
  | Some l -> l
  | None -> raise (Transpose_got_lists_of_different_lengths (map l ~f:(length :> _ -> _)))
;;

let intersperse t ~sep =
  match t with
  | [] -> []
  | x :: xs -> x :: fold_right xs ~init:[] ~f:(fun y acc -> sep :: y :: acc)
;;

let fold_until t ~init ~f ~finish = Container.fold_until ~fold ~init ~f t ~finish
let fold_result t ~init ~f = Container.fold_result ~fold_until ~init ~f t

let foldi_until t ~init ~f ~finish =
  Indexed_container.foldi_until ~fold_until ~init ~f t ~finish
;;

let iter_until t ~f ~finish = Container.iter_until ~fold_until t ~f ~finish
let iteri_until t ~f ~finish = Indexed_container.iteri_until ~foldi_until t ~f ~finish

let is_suffix list ~suffix ~equal:(local_ (equal_elt : _ -> _ -> _)) =
  let list_len = length list in
  let suffix_len = length suffix in
  list_len >= suffix_len
  && equal_with_local_closure equal_elt (drop list (list_len - suffix_len)) suffix
;;
