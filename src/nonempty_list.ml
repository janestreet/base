[@@@ocaml.flambda_o3]

open Import
open Ppx_compare_lib.Builtin
open Globalize
module Error = Error0
module Nothing = Nothing0
module Sexp = Sexp0

module T = struct
  type 'a t = 'a Nonempty_list_type.Nonempty_list.t = ( :: ) of 'a * 'a list

  let to_list (hd :: tl) : _ List.t = hd :: tl

  let of_list_exn : _ List.t -> _ t = function
    | [] -> Error.raise_s (Atom "Nonempty_list.of_list_exn: empty list")
    | hd :: tl -> hd :: tl
  ;;
end

include T
module For_deriving = struct end
include For_deriving

module Format = struct
  type 'a t = 'a list [@@deriving sexp]
end

module To = struct
  include T

  let to_sexpable = to_list
  let of_sexpable = of_list_exn
end

include Sexpable.Of_sexpable1 (Format) (To)

let t_sexp_grammar (type a) ({ untyped = element } : [%sexp_grammar: a])
  : [%sexp_grammar: a t]
  =
  { untyped = List (Cons (element, Many element)) }
;;

type nonrec 'a t = 'a t = ( :: ) of 'a * 'a list
[@@deriving compare, equal, hash, globalize]

include Comparator.Derived (struct
    type nonrec 'a t = 'a t [@@deriving compare, sexp_of]
  end)

open struct
  type nonrec 'a t = 'a t = ( :: ) of 'a * 'a List.t
end

let length (_ :: tl) = 1 + List.length tl

include struct
  let of_list : _ List.t -> _ t option = function
    | [] -> None
    | hd :: tl -> Some (hd :: tl)
  ;;

  let of_list_or_null : _ List.t -> _ t or_null = function
    | [] -> Null
    | hd :: tl -> This (hd :: tl)
  ;;

  let of_list_error : _ List.t -> _ t Or_error.t = function
    | [] -> Or_error.error_s (Atom "empty list")
    | hd :: tl -> Ok (hd :: tl)
  ;;

  let of_list_exn l = of_list_exn l
  let to_list t = to_list t
  let hd (hd :: _) = hd
  let tl (_ :: tl) : _ List.t = tl

  let nth (hd :: tl) n : _ Option.t =
    match n with
    | 0 -> Some hd
    | n -> List.nth tl (n - 1)
  ;;

  let nth_exn t n =
    match nth t n with
    | Some a -> a
    | None ->
      (match
         Printf.invalid_argf
           "Nonempty_list.nth_exn %d called on list of length %d"
           n
           (length t)
           ()
       with
       | (_ : Nothing.t) -> .)
  ;;

  let reduce (hd :: tl) ~f = List.fold ~init:hd tl ~f
  let last (hd :: tl) = List.fold tl ~init:hd ~f:(fun _ elt -> elt)

  let iter (type a) (hd :: tl : a t) ~f =
    f hd;
    List.iter ~f tl [@nontail]
  ;;

  let iteri (type a) (hd :: tl : a t) ~f =
    f 0 hd;
    List.iteri ~f:(fun i x -> f (i + 1) x) tl [@nontail]
  ;;

  let min_elt' (type a) (hd :: tl : a t) ~compare =
    List.fold tl ~init:hd ~f:(fun min elt -> if compare min elt > 0 then elt else min)
    [@nontail]
  ;;

  let max_elt' (type a) (t : a t) ~compare =
    min_elt' t ~compare:(fun x y -> compare y x) [@nontail]
  ;;
end

include struct
  let create hd tl = hd :: tl
  let singleton hd = [ hd ]
  let cons (type a) (x : a) (hd :: tl : a t) : a t = x :: hd :: tl

  let filter (hd :: tl) ~f : _ List.t =
    match f hd with
    | false -> List.filter tl ~f
    | true -> hd :: List.filter tl ~f
  ;;

  let filteri (hd :: tl) ~f : _ List.t =
    let include_hd = f 0 hd in
    let[@inline always] f i x = f (i + 1) x in
    match include_hd with
    | false -> List.filteri tl ~f [@nontail]
    | true -> hd :: List.filteri tl ~f
  ;;

  let reverse (hd :: tl) =
    let rec loop acc x (xs : _ List.t) =
      match xs with
      | [] -> x :: acc
      | y :: ys -> loop (x :: acc) y ys
    in
    loop [] hd tl
  ;;

  let init n ~f =
    if n < 1 then Printf.invalid_argf "Nonempty_list.init %d" n ();
    (* [List.init] calls [f] on the highest index first and works its way down. We do the
       same here. *)
    let tl = List.init (n - 1) ~f:(fun i -> f (i + 1)) in
    let hd = f 0 in
    hd :: tl
  ;;

  let append (type a) (hd :: tl : a t) l : a t = hd :: List.append tl l
end

let ( @ ) t1 t2 = append t1 (to_list t2)

open struct
  type nonrec 'a t = 'a t = ( :: ) of 'a * 'a List.t
end

let mapi (hd :: tl) ~f : _ t =
  (* Being overly cautious about evaluation order *)
  let hd = f 0 hd in
  hd :: List.mapi tl ~f:(fun i x -> f (i + 1) x)
;;

let rev_mapi (hd :: tl : _ t) ~f =
  let rec rev_mapi_loop i acc last (rest : _ List.t) : _ t =
    match rest with
    | [] -> last :: acc
    | hd :: tl -> rev_mapi_loop (i + 1) (last :: acc) (f i hd) tl
  in
  rev_mapi_loop 1 [] (f 0 hd) tl [@nontail]
;;

let filter_map (hd :: tl) ~f : _ List.t =
  match (f hd : _ Option.t) with
  | None -> List.filter_map tl ~f
  | Some hd -> hd :: List.filter_map tl ~f
;;

let filter_map_or_null (hd :: tl) ~f : _ list =
  match (f hd : _ or_null) with
  | Null -> List.filter_map_or_null tl ~f
  | This hd -> hd :: List.filter_map_or_null tl ~f
;;

let filter_mapi (hd :: tl) ~f : _ List.t =
  let hd = f 0 hd in
  let[@inline always] f i x = f (i + 1) x in
  match (hd : _ Option.t) with
  | None -> List.filter_mapi tl ~f [@nontail]
  | Some hd -> hd :: List.filter_mapi tl ~f
;;

let filter_mapi_or_null (hd :: tl) ~f : _ list =
  let hd = f 0 hd in
  let[@inline always] f i x = f (i + 1) x in
  match (hd : _ or_null) with
  | Null -> List.filter_mapi_or_null tl ~f [@nontail]
  | This hd -> hd :: List.filter_mapi_or_null tl ~f
;;

let map t ~f = mapi t ~f:(fun (_ : int) x -> f x) [@nontail]
let rev_map t ~f = (rev_mapi [@inlined]) t ~f:(fun (_ : int) x -> f x) [@nontail]

let concat_map (type a b) (hd :: tl : a t) ~(f : a -> b t) =
  let f_hd = f hd in
  append
    f_hd
    (List.concat_map tl ~f:(fun x ->
       let x = f x in
       to_list x))
;;

let bind = concat_map

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

include Monad.Make (struct
    type nonrec 'a t = 'a t

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

let of_iarray t = of_list (Iarray.to_list t)
let to_iarray t = Iarray.of_list (to_list t)

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
let map_add_multi map ~key ~data = Map.add_multi_nonempty map ~key ~data

let hashtbl_add_multi map ~key ~data =
  Hashtbl.update map key ~f:(function
    | None -> singleton data
    | Some t -> cons data t)
;;

let map_of_container_multi_rev fold container ~comparator =
  fold container ~init:(Map.empty comparator) ~f:(fun acc (key, data) ->
    map_add_multi acc ~key ~data)
;;

let map_of_alist_multi alist ~comparator = Map.of_alist_multi_nonempty comparator alist
let map_of_alist_multi_rev alist = map_of_container_multi_rev List.fold alist

let map_of_sequence_multi sequence ~comparator =
  Map.of_sequence_multi_nonempty comparator sequence
;;

let map_of_sequence_multi_rev sequence = map_of_container_multi_rev Sequence.fold sequence
let fold_nonempty (type a) (hd :: tl : a t) ~init ~f = List.fold tl ~init:(init hd) ~f

let map_of_list_with_key_multi_rev list ~comparator ~get_key =
  List.fold list ~init:(Map.empty comparator) ~f:(fun acc data ->
    let key = get_key data in
    map_add_multi acc ~key ~data)
;;

let map_of_list_with_key_multi list ~comparator ~get_key =
  Map.of_list_with_key_multi_nonempty comparator list ~get_key
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

  let of_lists_exn ((xs : _ list), (ys : _ list)) =
    match xs, ys with
    | x :: xs, [] -> Fst (x :: xs)
    | [], y :: ys -> Snd (y :: ys)
    | x :: xs, y :: ys -> Both (x :: xs, y :: ys)
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
let partition_result t = to_list t |> List.partition_result |> Partition.of_lists_exn
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

module From_indexed_container_make = Indexed_container.Make (struct
    type nonrec 'a t = 'a t

    let iter = `Custom iter
    let iteri = `Custom iteri
    let fold (hd :: tl) ~init ~f = List.fold tl ~init:(f init hd) ~f

    let fold_until (hd :: tl) ~init ~f ~finish =
      match (f init hd : _ Container.Continue_or_stop.t) with
      | Continue init -> List.fold_until tl ~init ~f ~finish
      | Stop x -> x
    ;;

    let fold = `Custom fold
    let iter_until = `Define_using_fold_until
    let foldi = `Define_using_fold
    let foldi_until = `Define_using_fold_until
    let length = `Custom length
  end)

let is_empty (_ :: _) = false
let to_array = From_indexed_container_make.to_array
let count = From_indexed_container_make.count
let counti = From_indexed_container_make.counti
let exists = From_indexed_container_make.exists
let existsi = From_indexed_container_make.existsi
let find = From_indexed_container_make.find
let findi = From_indexed_container_make.findi
let for_all = From_indexed_container_make.for_all
let for_alli = From_indexed_container_make.for_alli
let max_elt = From_indexed_container_make.max_elt
let mem = From_indexed_container_make.mem
let min_elt = From_indexed_container_make.min_elt
let find_map = From_indexed_container_make.find_map
let find_mapi = From_indexed_container_make.find_mapi
let fold = From_indexed_container_make.fold
let fold_result = From_indexed_container_make.fold_result
let fold_until = From_indexed_container_make.fold_until
let foldi = From_indexed_container_make.foldi
let foldi_until = From_indexed_container_make.foldi_until
let iter_until = From_indexed_container_make.iter_until
let iteri_until = From_indexed_container_make.iteri_until
let sum = From_indexed_container_make.sum

let findi_exn =
  let not_found () = Not_found_s (Atom "Nonempty_list.findi_exn: not found") in
  let findi_exn t ~f =
    match findi t ~f with
    | None -> raise (not_found ())
    | Some x -> x
  in
  findi_exn
;;
