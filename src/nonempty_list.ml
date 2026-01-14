[@@@ocaml.flambda_o3]

open Import
open Ppx_compare_lib.Builtin
open Globalize

module T = struct
  type%template ('a : k) t = ( :: ) of 'a * ('a List.t[@kind k])
  [@@kind k = base_non_value]

  type nonrec ('a : value_or_null) t = ( :: ) of 'a * 'a list

  [%%template
  [@@@kind k = (base_non_value, value_or_null)]

  open struct
    type nonrec ('a : k) t = ('a t[@kind k]) = ( :: ) of 'a * ('a List.t[@kind k])
  end

  [@@@kind.default k]
  [@@@mode.default m = (global, local)]

  let to_list (hd :: tl @ m) : (_ List.t[@kind k]) = hd :: tl [@exclave_if_local m]

  let of_list_exn : (_ List.t[@kind k]) @ m -> _ t @ m = function
    | [] -> Error.raise_s (Atom "Nonempty_list.of_list_exn: empty list")
    | hd :: tl -> hd :: tl [@exclave_if_local m]
  ;;]
end

include T

module Format = struct
  type 'a t = 'a list [@@deriving sexp]
end

include%template
  Sexpable.Of_sexpable1 [@modality portable]
    (Format)
    (struct
      include T

      let to_sexpable = to_list
      let of_sexpable = of_list_exn
    end)

let t_sexp_grammar (type a) ({ untyped = element } : [%sexp_grammar: a])
  : [%sexp_grammar: a t]
  =
  { untyped = List (Cons (element, Many element)) }
;;

type%template nonrec ('a : k) t = ('a t[@kind k]) = ( :: ) of 'a * ('a List.t[@kind k])
[@@kind k = base_non_value] [@@deriving compare ~localize, equal ~localize]

type nonrec ('a : value_or_null) t = 'a t = ( :: ) of 'a * 'a list
[@@deriving compare ~localize, equal ~localize, hash, globalize]

include%template Comparator.Derived [@modality portable] (struct
    type nonrec 'a t = 'a t [@@deriving compare, sexp_of]
  end)

[%%template
[@@@kind k = base_or_null]

open struct
  type nonrec ('a : k) t = ('a t[@kind k]) = ( :: ) of 'a * ('a List.t[@kind k])
end

[@@@kind.default k]

let[@mode m = (global, local)] to_list t =
  (to_list [@kind k] [@mode m]) t [@exclave_if_local m]
;;

let[@mode m = (global, local)] of_list_exn l =
  (of_list_exn [@kind k] [@mode m]) l [@exclave_if_local m]
;;

let hd (hd :: _) = hd
let tl (_ :: tl) = tl

let of_list : (_ List.t[@kind k]) @ m -> _ t option @ m = function
  | [] -> None
  | hd :: tl -> Some (hd :: tl) [@exclave_if_local m]
[@@mode m = (global, local)]
;;

let of_list_or_null : (_ List.t[@kind k]) @ m -> _ t or_null @ m = function
  | [] -> Null
  | hd :: tl -> This (hd :: tl) [@exclave_if_local m]
[@@mode m = (global, local)]
;;

let of_list_error : (_ List.t[@kind k]) @ m -> _ t Or_error.t @ m = function
  | [] -> Or_error.error_s (Atom "empty list")
  | hd :: tl -> Ok (hd :: tl) [@exclave_if_local m]
[@@mode m = (global, local)]
;;

let length (_ :: tl) = 1 + (List.length [@kind k]) tl
let create hd tl = hd :: tl
let singleton hd = [ hd ]
let cons x (hd :: tl) = x :: hd :: tl

let nth (hd :: tl) n : (_ Option.t[@kind k]) =
  match n with
  | 0 -> Some hd
  | n -> (List.nth [@kind k]) tl (n - 1)
;;

let nth_exn (t : _ t) n =
  match (nth [@kind k]) t n with
  | None ->
    (match
       Printf.invalid_argf
         "Nonempty_list.nth_exn %d called on list of length %d"
         n
         ((length [@kind k]) t)
         ()
     with
     | (_ : Nothing.t) -> .)
  | Some a -> a
;;

let filter (hd :: tl) ~f : (_ List.t[@kind k]) =
  match f hd with
  | false -> (List.filter [@kind k]) tl ~f
  | true -> hd :: (List.filter [@kind k]) tl ~f
;;

let filteri (hd :: tl) ~f : (_ List.t[@kind k]) =
  let include_hd = f 0 hd in
  let[@inline always] f i x = f (i + 1) x in
  match include_hd with
  | false -> (List.filteri [@kind k]) tl ~f [@nontail]
  | true -> hd :: (List.filteri [@kind k]) tl ~f
;;

let reduce (hd :: tl) ~f = (List.fold [@kind k k]) ~init:hd tl ~f

let[@alloc a @ m = (stack_local, heap_global)] reverse (hd :: tl @ m) =
  let rec loop (acc @ m) (x @ m) (xs : (_ List.t[@kind k]) @ m) =
    match xs with
    | [] -> x :: acc [@exclave_if_local m]
    | y :: ys -> loop (x :: acc) y ys [@exclave_if_stack a]
  in
  loop [] hd tl [@exclave_if_local m]
;;

let append (hd :: tl) l = hd :: (List.append [@kind k]) tl l

let init n ~f =
  if n < 1 then Printf.invalid_argf "Nonempty_list.init %d" n ();
  (* [List.init] calls [f] on the highest index first and works its way down. We do the
     same here. *)
  let tl = (List.init [@kind k]) (n - 1) ~f:(fun i -> f (i + 1)) in
  let hd = f 0 in
  hd :: tl
;;

let last (hd :: tl) = (List.fold [@kind k k]) tl ~init:hd ~f:(fun _ elt -> elt)

let iter (hd :: tl) ~f =
  f hd;
  (List.iter [@kind k]) ~f tl
;;

(* Temporarily copied from base while we wait for list.ml to be localized *)
let rec iter_local_list (t : (_ List.t[@kind k]) @ local) ~(f @ local) =
  match t with
  | [] -> ()
  | a :: xs ->
    f a;
    (iter_local_list [@kind k] [@tailcall]) xs ~f
;;

let[@mode local] iter (hd :: tl : (_ t[@kind k]) @ local) ~(f @ local) =
  f hd;
  (iter_local_list [@kind k]) tl ~f
;;

let iteri (hd :: tl) ~f =
  f 0 hd;
  (List.iteri [@kind k]) ~f:(fun i x -> f (i + 1) x) tl [@nontail]
;;

let ( @ ) t1 t2 = (append [@kind k]) t1 ((to_list [@kind k]) t2)]

[%%template
[@@@kind.default ka = base_or_null, kb = base_or_null]

open struct
  type nonrec ('a : ka) t = ('a t[@kind ka]) = ( :: ) of 'a * ('a List.t[@kind ka])
  [@@kind ka]
end

let mapi (hd :: tl) ~f : (_ t[@kind kb]) =
  (* Being overly cautious about evaluation order *)
  let hd = f 0 hd in
  hd :: (List.mapi [@kind ka kb]) tl ~f:(fun i x -> f (i + 1) x)
;;

let filter_map (hd :: tl) ~f : (_ List.t[@kind kb]) =
  match (f hd : (_ Option.t[@kind kb])) with
  | None -> (List.filter_map [@kind ka kb]) tl ~f
  | Some hd -> hd :: (List.filter_map [@kind ka kb]) tl ~f
;;

let filter_mapi (hd :: tl) ~f : (_ List.t[@kind kb]) =
  let hd = f 0 hd in
  let[@inline always] f i x = f (i + 1) x in
  match (hd : (_ Option.t[@kind kb])) with
  | None -> (List.filter_mapi [@kind ka kb]) tl ~f [@nontail]
  | Some hd -> hd :: (List.filter_mapi [@kind ka kb]) tl ~f
;;

let map t ~f = (mapi [@kind ka kb]) t ~f:(fun (_ : int) x -> f x) [@nontail]

let bind (type (a : ka) (b : kb)) (hd :: tl : (a t[@kind ka])) ~(f : a -> (b t[@kind kb]))
  =
  let f_hd = f hd in
  (append [@kind kb])
    f_hd
    ((List.concat_map [@kind ka kb]) tl ~f:(fun x ->
       let x = f x in
       (to_list [@kind kb]) x))
;;

let concat_map = (bind [@kind ka kb])]

let map2 t1 t2 ~f : _ List.Or_unequal_lengths.t =
  match List.map2 (to_list t1) (to_list t2) ~f with
  | Ok x -> Ok (of_list_exn x)
  | Unequal_lengths -> Unequal_lengths
;;

let map2_exn t1 t2 ~f = List.map2_exn (to_list t1) (to_list t2) ~f |> of_list_exn
let filter_opt t = filter_map t ~f:Fn.id

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
let all_equal t ~equal = to_list t |> List.all_equal ~equal
let is_sorted t = to_list t |> List.is_sorted
let is_sorted_strictly t = to_list t |> List.is_sorted_strictly

let min_elt' (hd :: tl) ~compare =
  List.fold tl ~init:hd ~f:(fun min elt -> if compare min elt > 0 then elt else min)
  [@nontail]
;;

let max_elt' t ~compare = min_elt' t ~compare:(fun x y -> compare y x) [@nontail]

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

let%template fold_nonempty (hd :: tl) ~init ~f =
  (List.fold [@mode li lo])
    tl
    ~init:(init hd)
    ~f [@exclave_if_local lo ~reasons:[ May_return_local ]]
[@@mode li = (local, global), lo = (local, global)]
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

  let%template[@mode m = (local, global)] of_lists_exn
    (((xs : _ list), (ys : _ list)) @ m)
    =
    match xs, ys with
    | x :: xs, [] -> Fst (x :: xs) [@exclave_if_local m]
    | [], y :: ys -> Snd (y :: ys) [@exclave_if_local m]
    | x :: xs, y :: ys -> Both (x :: xs, y :: ys) [@exclave_if_local m]
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

let%template partition_result (t @ m) =
  ((to_list [@mode m]) t
   |> (List.partition_result [@alloc a])
   |> (Partition.of_lists_exn [@mode m]))
  [@exclave_if_stack a]
[@@alloc a @ m = (heap_global, stack_local)]
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
Indexed_container.Make [@modality portable] (struct
    type nonrec 'a t = 'a t

    let fold (hd :: tl) ~init ~f = List.fold tl ~init:(f init hd) ~f
    let fold_until t ~init ~f ~finish = Container.fold_until ~fold ~init ~f t ~finish
    let fold = `Custom fold
    let iter_until = `Define_using_fold_until
    let iter = `Custom iter
    let length = `Custom length
    let iteri = `Custom iteri
    let foldi = `Define_using_fold
    let foldi_until = `Define_using_fold_until
  end)

let is_empty (_ :: _) = false
let mem = From_indexed_container_make.mem
let iter_until = From_indexed_container_make.iter_until
let fold = From_indexed_container_make.fold
let fold_result = From_indexed_container_make.fold_result
let fold_until = From_indexed_container_make.fold_until
let exists = From_indexed_container_make.exists
let for_all = From_indexed_container_make.for_all
let count = From_indexed_container_make.count
let sum = From_indexed_container_make.sum
let find = From_indexed_container_make.find
let find_map = From_indexed_container_make.find_map
let to_array = From_indexed_container_make.to_array
let min_elt = From_indexed_container_make.min_elt
let max_elt = From_indexed_container_make.max_elt
let find_mapi = From_indexed_container_make.find_mapi
let findi = From_indexed_container_make.findi
let counti = From_indexed_container_make.counti
let for_alli = From_indexed_container_make.for_alli
let existsi = From_indexed_container_make.existsi
let foldi = From_indexed_container_make.foldi
let foldi_until = From_indexed_container_make.foldi_until
let iteri_until = From_indexed_container_make.iteri_until

let findi_exn =
  let not_found () = Not_found_s (Atom "Nonempty_list.findi_exn: not found") in
  let findi_exn t ~f =
    match findi t ~f with
    | None -> raise (not_found ())
    | Some x -> x
  in
  findi_exn
;;
