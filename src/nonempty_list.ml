[@@@ocaml.flambda_o3]

open Import
open Ppx_compare_lib.Builtin
open Globalize

[%%template
[@@@kind_set.define all_ks_non_value = base_non_value]
[@@@kind_set.define all_ks = (all_ks_non_value, value_or_null)]

module%template T = struct
  type ('a : k) t = ( :: ) of 'a * ('a List.t[@kind k])
  [@@kind.explicit k = all_ks_non_value]

  type ('a : value_or_null) t = 'a Nonempty_list_type.Nonempty_list.t =
    | ( :: ) of 'a * 'a list

  type ('a : value_or_null) t = 'a t = ( :: ) of 'a * 'a list
  [@@kind.explicit __ = value_or_null]

  [@@@kind k = all_ks]

  open struct
    type nonrec ('a : k) t = ('a t[@kind.explicit k]) =
      | ( :: ) of 'a * ('a List.t[@kind k])
  end

  [@@@kind.default k]
  [@@@mode.default l = (global, local)]

  let to_list (hd :: tl @ l) : (_ List.t[@kind k]) = hd :: tl [@exclave_if_local l]

  let of_list_exn : (_ List.t[@kind k]) @ l -> _ t @ l = function
    | [] -> Error.raise_s (Atom "Nonempty_list.of_list_exn: empty list")
    | hd :: tl -> hd :: tl [@exclave_if_local l]
  ;;
end

include T

module For_deriving = struct
  [%%template
  [@@@kind.default k = all_ks_non_value]

  module Format = struct
    type ('a : k) t = ('a List.t[@kind k]) [@@deriving sexp_of ~stackify]
  end

  module To = struct
    include T

    let[@alloc a @ l = (heap_global, stack_local)] to_sexpable =
      (to_list [@mode l] [@kind k])
    ;;
  end

  type nonrec ('a : k) t = ('a t[@kind k]) = ( :: ) of 'a * ('a List.t[@kind k])
  [@@deriving compare ~localize, equal ~localize]

  (* Copied from [Sexpable.Of_sexpable1] since there is no version of the functor that
     only produces [sexp_of_t] and this is a temporary state while we wait for
     [t_of_sexp__stack]. *)
  let[@alloc a = (heap, stack)] sexp_of_t sexp_of_a t =
    let module Format = Format [@kind k] in
    let module To = To [@kind k] in
    (Format.sexp_of_t [@alloc a])
      sexp_of_a
      ((To.to_sexpable [@alloc a]) t) [@exclave_if_stack a]
  ;;]
end

include For_deriving

module Format = struct
  type 'a t = 'a List.t [@@deriving sexp ~stackify]
end

module To = struct
  include T

  let%template[@alloc a @ l = (heap_global, stack_local)] to_sexpable =
    (to_list [@mode l])
  ;;

  let of_sexpable = of_list_exn
end

include%template Sexpable.Of_sexpable1 [@modality portable] [@alloc stack] (Format) (To)

let t_sexp_grammar (type a) ({ untyped = element } : [%sexp_grammar: a])
  : [%sexp_grammar: a t]
  =
  { untyped = List (Cons (element, Many element)) }
;;

type nonrec ('a : value_or_null) t = 'a t = ( :: ) of 'a * 'a list
[@@deriving compare ~localize, equal ~localize, hash, globalize]

include%template Comparator.Derived [@modality portable] [@mode local] (struct
    type nonrec 'a t = 'a t [@@deriving compare ~localize, sexp_of]
  end)

[%%template
[@@@kind k = all_ks]

open struct
  type nonrec ('a : k) t = ('a t[@kind.explicit k]) =
    | ( :: ) of 'a * ('a List.t[@kind k])
end

let length (_ :: tl) = 1 + (List.length [@kind k]) tl [@@kind k]

include struct
  [@@@kind.default k]
  [@@@mode.default l = (global, local)]

  let of_list : (_ List.t[@kind k]) @ l -> _ t option @ l = function
    | [] -> None
    | hd :: tl -> Some (hd :: tl) [@exclave_if_local l]
  ;;

  let of_list_or_null : (_ List.t[@kind k]) @ l -> _ t or_null @ l = function
    | [] -> Null
    | hd :: tl -> This (hd :: tl) [@exclave_if_local l]
  ;;

  let of_list_error : (_ List.t[@kind k]) @ l -> _ t Or_error.t @ l = function
    | [] -> Or_error.error_s (Atom "empty list")
    | hd :: tl -> Ok (hd :: tl) [@exclave_if_local l]
  ;;

  let of_list_exn l = (of_list_exn [@kind k] [@mode l]) l [@exclave_if_local l]
  let to_list t = (to_list [@kind k] [@mode l]) t [@exclave_if_local l]
  let hd (hd :: _ @ l) = hd [@exclave_if_local l]
  let tl (_ :: tl @ l) : (_ List.t[@kind k]) = tl [@exclave_if_local l]

  let nth (hd :: tl @ l) n : (_ Option.t[@kind k]) =
    match n with
    | 0 -> Some hd [@exclave_if_local l]
    | n ->
      (List.nth [@mode l] [@kind k])
        tl
        (n - 1) [@exclave_if_local l ~reasons:[ May_return_local ]]
  ;;

  let nth_exn (t @ l) n =
    match[@exclave_if_local l ~reasons:[ May_return_local ]]
      (nth [@kind k] [@mode l]) t n
    with
    | Some a -> a
    | None ->
      (match
         Printf.invalid_argf
           "Nonempty_list.nth_exn %d called on list of length %d"
           n
           ((length [@kind k]) t)
           ()
       with
       | (_ : Nothing.t) -> .)
  ;;

  let reduce (hd :: tl) ~(f @ local) =
    (List.fold [@kind k k] [@mode l l]) ~init:hd tl ~f [@exclave_if_local l]
  ;;

  let last (hd :: tl) =
    (List.fold [@kind k k] [@mode l l]) tl ~init:hd ~f:(fun _ elt ->
      elt [@exclave_if_local l])
    [@exclave_if_local l ~reasons:[ May_return_local ]]
  ;;

  let iter (type a : k) (hd :: tl : (a t[@kind k])) ~f =
    f hd;
    (List.iter [@kind k] [@mode l]) ~f tl [@nontail]
  ;;

  let iteri (type a : k) (hd :: tl : (a t[@kind k])) ~f =
    f 0 hd;
    (List.iteri [@kind k] [@mode l]) ~f:(fun i x -> f (i + 1) x) tl [@nontail]
  ;;

  let min_elt' (type a : k) (hd :: tl : (a t[@kind k]) @ l) ~(compare @ local) =
    (List.fold [@kind k k] [@mode l l]) tl ~init:hd ~f:(fun min elt ->
      if compare min elt > 0 then elt [@exclave_if_local l] else min [@exclave_if_local l])
    [@exclave_if_local l ~reasons:[ May_return_local ]] [@nontail]
  ;;

  let max_elt' (type a : k) (t : (a t[@kind k]) @ l) ~compare =
    (min_elt' [@kind k] [@mode l]) t ~compare:(fun x y -> compare y x)
    [@exclave_if_local l ~reasons:[ May_return_local ]] [@nontail]
  ;;
end

include struct
  [@@@kind.default k]
  [@@@alloc.default a @ l = (heap_global, stack_local)]

  let create hd tl = hd :: tl [@exclave_if_stack a]
  let singleton hd = [ hd ] [@exclave_if_stack a]

  let cons (type a : k) (x : a @ l) (hd :: tl : (a t[@kind k]) @ l) : (a t[@kind k]) @ l =
    x :: hd :: tl [@exclave_if_stack a]
  ;;

  let filter (hd :: tl) ~f : (_ List.t[@kind k]) =
    match[@exclave_if_stack a] f hd with
    | false -> (List.filter [@kind k] [@alloc a]) tl ~f
    | true -> hd :: (List.filter [@kind k] [@alloc a]) tl ~f
  ;;

  let filteri (hd :: tl) ~f : (_ List.t[@kind k]) =
    (let include_hd = f 0 hd in
     let[@inline always] f i x = f (i + 1) x in
     match include_hd with
     | false -> (List.filteri [@kind k] [@alloc a]) tl ~f [@nontail]
     | true -> hd :: (List.filteri [@kind k] [@alloc a]) tl ~f)
    [@exclave_if_stack a]
  ;;

  let reverse (hd :: tl) =
    let rec loop (acc @ l u) (x @ l u) (xs : (_ List.t[@kind k]) @ l u) =
      match xs with
      | [] -> x :: acc [@exclave_if_stack a]
      | y :: ys -> loop (x :: acc) y ys [@exclave_if_stack a]
    in
    loop [] hd tl [@exclave_if_stack a]
  [@@mode u = (aliased, unique)]
  ;;

  let init n ~f =
    if n < 1 then Printf.invalid_argf "Nonempty_list.init %d" n ();
    (* [List.init] calls [f] on the highest index first and works its way down. We do the
       same here. *)
    (let tl =
       (List.init [@kind k] [@alloc a]) (n - 1) ~f:(fun i ->
         f (i + 1) [@exclave_if_stack a])
     in
     let hd = f 0 in
     hd :: tl)
    [@exclave_if_stack a]
  ;;

  let append (type a : k) (hd :: tl : (a t[@kind k])) l : (a t[@kind k]) =
    hd :: (List.append [@alloc a] [@kind k]) tl l [@exclave_if_stack a]
  ;;
end]

let ( @ ) t1 t2 = append t1 (to_list t2)

[%%template
[@@@kind.default ka = all_ks, kb = all_ks]

open struct
  type nonrec ('a : ka) t = ('a t[@kind ka]) = ( :: ) of 'a * ('a List.t[@kind ka])
  [@@kind ka]
end

[@@@mode.default li = (global, local)]
[@@@alloc.default a @ lo = (heap_global, stack_local)]

let mapi (hd :: tl) ~f : (_ t[@kind kb]) =
  (* Being overly cautious about evaluation order *)
  (let hd = f 0 hd in
   hd
   :: (List.mapi [@kind ka kb] [@mode li] [@alloc a]) tl ~f:(fun i x ->
     f (i + 1) x [@exclave_if_stack a]))
  [@exclave_if_stack a]
;;

let rev_mapi (hd :: tl : (_ t[@kind ka]) @ li) ~(f @ local) =
  (let rec rev_mapi_loop i (acc @ lo) (last @ lo) (rest : (_ List.t[@kind ka]) @ li)
     : (_ t[@kind kb])
     =
     match rest with
     | [] -> last :: acc [@exclave_if_stack a]
     | hd :: tl -> rev_mapi_loop (i + 1) (last :: acc) (f i hd) tl [@exclave_if_stack a]
   in
   rev_mapi_loop 1 [] (f 0 hd) tl [@nontail])
  [@exclave_if_stack a]
;;

let filter_map (hd :: tl) ~f : (_ List.t[@kind kb]) @ lo =
  match[@exclave_if_stack a] (f hd : (_ Option.t[@kind kb])) with
  | None -> (List.filter_map [@kind ka kb] [@mode li] [@alloc a]) tl ~f
  | Some hd -> hd :: (List.filter_map [@kind ka kb] [@mode li] [@alloc a]) tl ~f
;;

let filter_map_or_null (hd :: tl) ~f : _ List.t @ lo =
  match[@exclave_if_stack a] (f hd : _ or_null) with
  | Null -> (List.filter_map_or_null [@kind ka] [@mode li] [@alloc a]) tl ~f
  | This hd -> hd :: (List.filter_map_or_null [@kind ka] [@mode li] [@alloc a]) tl ~f
;;

let filter_mapi (hd :: tl) ~f : (_ List.t[@kind kb]) =
  (let hd = f 0 hd in
   let[@inline always] f i x = f (i + 1) x [@exclave_if_stack a] in
   match (hd : (_ Option.t[@kind kb])) with
   | None -> (List.filter_mapi [@kind ka kb] [@mode li] [@alloc a]) tl ~f [@nontail]
   | Some hd -> hd :: (List.filter_mapi [@kind ka kb] [@mode li] [@alloc a]) tl ~f)
  [@exclave_if_stack a]
;;

let filter_mapi_or_null (hd :: tl) ~f : _ List.t =
  (let hd = f 0 hd in
   let[@inline always] f i x = f (i + 1) x [@exclave_if_stack a] in
   match (hd : _ or_null) with
   | Null -> (List.filter_mapi_or_null [@kind ka] [@mode li] [@alloc a]) tl ~f [@nontail]
   | This hd -> hd :: (List.filter_mapi_or_null [@kind ka] [@mode li] [@alloc a]) tl ~f)
  [@exclave_if_stack a]
;;

let map t ~f =
  (mapi [@kind ka kb] [@mode li] [@alloc a]) t ~f:(fun (_ : int) x ->
    f x [@exclave_if_stack a])
  [@nontail] [@exclave_if_stack a]
;;

let rev_map t ~f =
  (rev_mapi [@kind ka kb] [@mode li] [@alloc a] [@inlined]) t ~f:(fun (_ : int) x ->
    f x [@exclave_if_stack a])
  [@nontail] [@exclave_if_stack a]
;;

let concat_map
  (type (a : ka) (b : kb))
  (hd :: tl : (a t[@kind ka]) @ li)
  ~(f : a @ li -> (b t[@kind kb]) @ lo)
  =
  (let f_hd = f hd in
   (append [@kind kb] [@alloc a])
     f_hd
     ((List.concat_map [@kind ka kb] [@mode li] [@alloc a]) tl ~f:(fun x ->
        (let x = f x in
         (to_list [@kind kb] [@mode lo]) x)
        [@exclave_if_stack a])))
  [@exclave_if_stack a]
;;

let bind = (concat_map [@kind ka kb] [@mode li] [@alloc a])]

let map2 t1 t2 ~f : _ List.Or_unequal_lengths.t =
  match List.map2 (to_list t1) (to_list t2) ~f with
  | Ok x -> Ok (of_list_exn x)
  | Unequal_lengths -> Unequal_lengths
;;

let map2_exn t1 t2 ~f = List.map2_exn (to_list t1) (to_list t2) ~f |> of_list_exn
let filter_opt t = List.filter_opt (to_list t)

let append' l t =
  match l with
  | [] -> t
  | x :: xs -> x :: List.append xs (to_list t)
;;

include%template
  Monad.Make [@kind value_or_null mod maybe_null] [@mode local] [@modality portable] (struct
    type nonrec ('a : value_or_null) t = 'a t

    let return hd = [ hd ]
    let map = `Custom map
    let bind = bind
  end)

let unzip ((hd1, hd2) :: tl) =
  let tl1, tl2 = List.unzip tl in
  hd1 :: tl1, hd2 :: tl2
;;

let unzip3 ((hd1, hd2, hd3) :: tl) =
  let tl1, tl2, tl3 = List.unzip3 tl in
  hd1 :: tl1, hd2 :: tl2, hd3 :: tl3
;;

let concat t = bind t ~f:Fn.id

let zip t1 t2 : _ List.Or_unequal_lengths.t =
  match List.zip (to_list t1) (to_list t2) with
  | Ok x -> Ok (of_list_exn x)
  | Unequal_lengths -> Unequal_lengths
;;

let zip_exn t1 t2 = List.zip_exn (to_list t1) (to_list t2) |> of_list_exn

let zip3 t1 t2 t3 : _ List.Or_unequal_lengths.t =
  match List.zip3 (to_list t1) (to_list t2) (to_list t3) with
  | Ok x -> Ok (of_list_exn x)
  | Unequal_lengths -> Unequal_lengths
;;

let zip3_exn t1 t2 t3 =
  List.zip3_exn (to_list t1) (to_list t2) (to_list t3) |> of_list_exn
;;

let drop_last (hd :: tl) =
  match List.drop_last tl with
  | None -> []
  | Some l -> hd :: l
;;

let to_sequence t =
  (* [to_list] just performs one [::], so this sequence is created with only constant
     up-front work *)
  Sequence.of_list (to_list t)
;;

let sort t ~compare = List.sort (to_list t) ~compare |> of_list_exn

let sort_and_group t ~compare =
  List.sort_and_group (to_list t) ~compare
  |> of_list_exn
  |> (* an empty group is not created unless the input list is empty *)
  map ~f:of_list_exn
;;

let group t ~break =
  List.group (to_list t) ~break
  |> of_list_exn
  |> (* an empty group is not created unless the input list is empty *)
  map ~f:of_list_exn
;;

let stable_sort t ~compare = List.stable_sort (to_list t) ~compare |> of_list_exn
let stable_dedup t ~compare = List.stable_dedup (to_list t) ~compare |> of_list_exn
let dedup_and_sort t ~compare = List.dedup_and_sort ~compare (to_list t) |> of_list_exn
let permute ?random_state t = List.permute ?random_state (to_list t) |> of_list_exn
let random_element ?random_state t = to_list t |> List.random_element_exn ?random_state

[%%template
[@@@mode.default l = (global, local)]

let all_equal t ~equal =
  ((to_list [@mode l]) t |> (List.all_equal [@mode l]) ~equal)
  [@exclave_if_local l ~reasons:[ May_return_regional; Will_return_unboxed ]]
;;]

let is_sorted t = to_list t |> List.is_sorted
let is_sorted_strictly t = to_list t |> List.is_sorted_strictly

let map_add_multi map ~key ~data =
  Map.update map key ~f:(function
    | None -> singleton data
    | Some t -> cons data t)
;;

let hashtbl_add_multi map ~key ~data =
  Hashtbl.update map key ~f:(function
    | None -> singleton data
    | Some t -> cons data t)
;;

let map_of_container_multi_rev fold container ~comparator =
  fold container ~init:(Map.empty comparator) ~f:(fun acc (key, data) ->
    map_add_multi acc ~key ~data)
;;

let map_of_container_multi fold container ~comparator =
  (* [map_of_container_multi_rev] will reverse the elements of the container that we pass
     into it, so instead of passing in [container] directly, fold over it to construct a
     list that contains its elements in reverse order *)
  let reversed = fold container ~init:[] ~f:(fun l x -> List.cons x l) in
  map_of_container_multi_rev List.fold reversed ~comparator
;;

let map_of_alist_multi alist = map_of_container_multi List.fold alist
let map_of_alist_multi_rev alist = map_of_container_multi_rev List.fold alist
let map_of_sequence_multi sequence = map_of_container_multi Sequence.fold sequence
let map_of_sequence_multi_rev sequence = map_of_container_multi_rev Sequence.fold sequence

let%template fold_nonempty (type a : ka) (hd :: tl : (a t[@kind ka])) ~init ~f =
  (List.fold [@kind ka kb] [@mode ma mb])
    tl
    ~init:(init hd)
    ~f [@exclave_if_local mb ~reasons:[ May_return_local ]]
[@@mode ma = (local, global), mb = (local, global)] [@@kind ka = all_ks, kb = all_ks]
;;

let map_of_list_with_key_multi_rev list ~comparator ~get_key =
  List.fold list ~init:(Map.empty comparator) ~f:(fun acc data ->
    let key = get_key data in
    map_add_multi acc ~key ~data)
;;

let map_of_list_with_key_multi list ~comparator ~get_key =
  map_of_list_with_key_multi_rev (List.rev list) ~comparator ~get_key
;;

let fold_right (hd :: tl) ~init:acc ~f =
  let acc = List.fold_right tl ~init:acc ~f in
  f hd acc
;;

let folding_map (hd :: tl) ~init ~f =
  let acc, hd = f init hd in
  hd :: List.folding_map tl ~init:acc ~f
;;

let fold_map (hd :: tl) ~init:acc ~f =
  let acc, hd = f acc hd in
  let acc, tl = List.fold_map tl ~init:acc ~f in
  acc, hd :: tl
;;

let map2_opt t1 t2 ~f : _ option =
  match map2 t1 t2 ~f with
  | Unequal_lengths -> None
  | Ok x -> Some x
;;

let rec transpose (hd :: tl) =
  match tl with
  | [] -> Some (map hd ~f:return)
  | hd' :: tl ->
    let rest = hd' :: tl in
    Option.bind (transpose rest) ~f:(fun transposed -> map2_opt ~f:cons hd transposed)
;;

let transpose_exn list =
  match transpose list with
  | Some list -> list
  | None ->
    Error.raise_s
      (Sexp.message
         "transpose got lists of different lengths"
         [ "lengths", map list ~f:length |> sexp_of_t sexp_of_int ])
;;

let combine_errors t =
  match Result.combine_errors (to_list t) with
  | Ok oks -> Ok (of_list_exn oks)
  | Error errors -> Error (of_list_exn errors)
;;

let combine_errors_unit t =
  match Result.combine_errors_unit (to_list t) with
  | Ok _ as ok -> ok
  | Error errors -> Error (of_list_exn errors)
;;

let combine_or_errors t =
  match Or_error.combine_errors (to_list t) with
  | Ok oks -> Ok (of_list_exn oks)
  | Error _ as e -> e
;;

let combine_or_errors_unit t = to_list t |> Or_error.combine_errors_unit

let filter_ok_at_least_one t =
  match Or_error.filter_ok_at_least_one (to_list t) with
  | Ok oks -> Ok (of_list_exn oks)
  | Error _ as e -> e
;;

let option_all t =
  let (hd :: tl) = t in
  Option.map2 hd (Option.all tl) ~f:create
;;

let remove_consecutive_duplicates ?(which_to_keep = `Last) (hd :: tl) ~equal =
  let tl = List.remove_consecutive_duplicates tl ~which_to_keep ~equal in
  match tl with
  | [] -> [ hd ]
  | snd :: tl ->
    if equal snd hd
    then (
      match which_to_keep with
      | `Last -> snd :: tl
      | `First -> hd :: tl)
    else hd :: snd :: tl
;;

type 'a nonempty_list = 'a t [@@deriving sexp_of]

module Partition = struct
  type ('fst, 'snd) t =
    | Fst of 'fst nonempty_list
    | Snd of 'snd nonempty_list
    | Both of ('fst nonempty_list * 'snd nonempty_list)
  [@@deriving sexp_of]

  let%template[@mode l = (local, global)] of_lists_exn
    (((xs : _ list), (ys : _ list)) @ l)
    =
    match xs, ys with
    | x :: xs, [] -> Fst (x :: xs) [@exclave_if_local l]
    | [], y :: ys -> Snd (y :: ys) [@exclave_if_local l]
    | x :: xs, y :: ys -> Both (x :: xs, y :: ys) [@exclave_if_local l]
    | [], [] ->
      failwith "Partition of [Nonempty_list.t] unexpectedly resulted in two empty lists!"
  ;;

  let fst = function
    | Both (xs, _) | Fst xs -> Some xs
    | Snd _ -> None
  ;;

  let snd = function
    | Both (_, ys) | Snd ys -> Some ys
    | Fst _ -> None
  ;;
end

let partition_tf t ~f = to_list t |> List.partition_tf ~f |> Partition.of_lists_exn
let partition_tf' t ~f = to_list t |> List.partition_tf ~f
let partition_map t ~f = to_list t |> List.partition_map ~f |> Partition.of_lists_exn
let partition_map' t ~f = to_list t |> List.partition_map ~f

let%template partition_result (t @ l) =
  ((to_list [@mode l]) t
   |> (List.partition_result [@alloc a])
   |> (Partition.of_lists_exn [@mode l]))
  [@exclave_if_stack a]
[@@alloc a @ l = (heap_global, stack_local)]
;;

let partition_result' t = to_list t |> List.partition_result

module Partition3 = struct
  type ('fst, 'snd, 'trd) t =
    | Fst of 'fst nonempty_list
    | Snd of 'snd nonempty_list
    | Trd of 'trd nonempty_list
    | Fst_snd of 'fst nonempty_list * 'snd nonempty_list
    | Fst_trd of 'fst nonempty_list * 'trd nonempty_list
    | Snd_trd of 'snd nonempty_list * 'trd nonempty_list
    | Fst_snd_trd of 'fst nonempty_list * 'snd nonempty_list * 'trd nonempty_list
  [@@deriving sexp_of]
end

let partition3_map t ~f : _ Partition3.t =
  let open Either.Export in
  match
    partition_map t ~f:(fun elem ->
      match f elem with
      | `Fst elem -> First (First elem)
      | `Snd elem -> First (Second elem)
      | `Trd elem -> Second elem)
  with
  | Snd trds -> Trd trds
  | Fst fsts_and_snds ->
    (match partition_map fsts_and_snds ~f:Fn.id with
     | Fst fsts -> Fst fsts
     | Snd snds -> Snd snds
     | Both (fsts, snds) -> Fst_snd (fsts, snds))
  | Both (fsts_and_snds, trds) ->
    (match partition_map fsts_and_snds ~f:Fn.id with
     | Fst fsts -> Fst_trd (fsts, trds)
     | Snd snds -> Snd_trd (snds, trds)
     | Both (fsts, snds) -> Fst_snd_trd (fsts, snds, trds))
;;

let cartesian_product t t' =
  List.cartesian_product (to_list t) (to_list t') |> of_list_exn
;;

let invariant f t = iter t ~f

module%template From_indexed_container_make =
Indexed_container.Make [@modality portable] [@alloc stack] (struct
    type nonrec 'a t = 'a t

    [%%template
    [@@@mode.default li = (local, global)]

    let iter = `Custom (iter [@mode li])
    let iteri = `Custom (iteri [@mode li])

    [@@@mode.default lo = (local, global)]

    let fold (hd :: tl) ~init ~f =
      (List.fold [@mode li lo])
        tl
        ~init:(f init hd)
        ~f [@exclave_if_local lo ~reasons:[ May_return_local ]]
    ;;

    let fold_until (hd :: tl) ~init ~f ~finish =
      match[@exclave_if_local lo ~reasons:[ May_return_local ]]
        (f init hd : _ Container.Continue_or_stop.t @ lo)
      with
      | Continue init -> (List.fold_until [@mode li lo]) tl ~init ~f ~finish
      | Stop x -> x
    ;;

    let fold = `Custom (fold [@mode li lo])
    let iter_until = `Define_using_fold_until
    let foldi = `Define_using_fold
    let foldi_until = `Define_using_fold_until]

    let length = `Custom length
  end)

let is_empty (_ :: _) = false
let to_list = (to_list [@mode l]) [@@alloc a @ l = stack @ local]
let to_array = From_indexed_container_make.to_array

[%%template
[@@@mode.default li = (local, global)]

let count = (From_indexed_container_make.count [@mode li])
let counti = (From_indexed_container_make.counti [@mode li])
let exists = (From_indexed_container_make.exists [@mode li])
let existsi = (From_indexed_container_make.existsi [@mode li])
let find = (From_indexed_container_make.find [@mode li])
let findi = (From_indexed_container_make.findi [@mode li])
let for_all = (From_indexed_container_make.for_all [@mode li])
let for_alli = (From_indexed_container_make.for_alli [@mode li])
let max_elt = (From_indexed_container_make.max_elt [@mode li])
let mem = (From_indexed_container_make.mem [@mode li])
let min_elt = (From_indexed_container_make.min_elt [@mode li])

[@@@mode.default lo = (local, global)]

let find_map = (From_indexed_container_make.find_map [@mode li lo])
let find_mapi = (From_indexed_container_make.find_mapi [@mode li lo])
let fold = (From_indexed_container_make.fold [@mode li lo])
let fold_result = (From_indexed_container_make.fold_result [@mode li lo])
let fold_until = (From_indexed_container_make.fold_until [@mode li lo])
let foldi = (From_indexed_container_make.foldi [@mode li lo])
let foldi_until = (From_indexed_container_make.foldi_until [@mode li lo])
let iter_until = (From_indexed_container_make.iter_until [@mode li lo])
let iteri_until = (From_indexed_container_make.iteri_until [@mode li lo])
let sum = (From_indexed_container_make.sum [@mode li lo])]

let findi_exn =
  let not_found () = Not_found_s (Atom "Nonempty_list.findi_exn: not found") in
  let findi_exn t ~f =
    match findi t ~f with
    | None -> raise (not_found ())
    | Some x -> x
  in
  findi_exn
;;]
