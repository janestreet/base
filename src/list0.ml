(* [List0] defines list functions that are primitives or can be simply defined in terms of
   [Stdlib.List]. [List0] is intended to completely express the part of [Stdlib.List] that
   [Base] uses -- no other file in Base other than list0.ml should use [Stdlib.List].
   [List0] has few dependencies, and so is available early in Base's build order. All Base
   files that need to use lists and come before [Base.List] in build order should do
   [module List = List0]. Defining [module List = List0] is also necessary because it
   prevents ocamldep from mistakenly causing a file to depend on [Base.List]. *)

open! Import0
module Either = Either0
module Sys = Sys0

(* For [[@@deriving compare]] below *)
open Ppx_compare_lib.Builtin

(* Before OCaml got the [@tail_mod_cons] transformation, these are the limits we used for
   various [List] operations in [Base] and [Core]. *)
let max_non_tailcall =
  match Sys.backend_type with
  | Sys.Native | Sys.Bytecode -> 1_000
  (* We don't know the size of the stack, better be safe and assume it's small. *)
  | Sys.Other _ -> 50
;;

let unzip = Stdlib.List.split

module Constructors = struct
  type 'a t = 'a list =
    | []
    | ( :: ) of 'a * 'a t

  (* Avoid re-deriving [compare] and [equal] *)
  [%%rederive type 'a t = 'a list [@@deriving compare, equal]]
end

open Constructors

(* Some of these are eta expanded in order to permute parameter order to follow Base
   conventions. *)

let hd_exn = function
  | [] -> failwith "hd"
  | a :: _ -> a
;;

let tl_exn = function
  | [] -> failwith "tl"
  | _ :: l -> l
;;

let is_empty = function
  | [] -> true
  | _ -> false
;;

open struct
  type nonrec 'a t = 'a t =
    | []
    | ( :: ) of 'a * 'a t
end

let length =
  let rec length_aux len = function
    | [] -> len
    | _ :: l -> length_aux (len + 1) l
  in
  fun l -> length_aux 0 l
;;

let exists t ~f =
  let rec exists_loop t =
    match t with
    | [] -> false
    | x :: xs -> if f x then true else exists_loop xs
  in
  exists_loop t [@nontail]
;;

let iter t ~f =
  let rec iter_loop t =
    match t with
    | [] -> ()
    | a :: l ->
      f a;
      iter_loop l
  in
  iter_loop t [@nontail]
;;

(* Copied from [Stdlib] for templating *)
let rev_append l1 l2 =
  let rec rev_append_loop l1 l2 =
    match l1 with
    | [] -> l2
    | a :: l -> rev_append_loop l (a :: l2)
  in
  rev_append_loop l1 l2
;;

let rev l =
  match l with
  | ([] | [ _ ]) as res -> res
  | x :: y :: rest -> rev_append rest [ y; x ]
;;

let for_all t ~f = not (exists t ~f:(fun x -> not (f x)))

let fold (type a b) (t : a t) ~(init : b) ~(f : b -> a -> b) : b =
  let rec fold_loop (acc : b) (l : a t) =
    match l with
    | [] -> acc
    | a :: l -> fold_loop (f acc a) l
  in
  fold_loop init t [@nontail]
;;

(* list.ml's [foldi] implementation uses an unboxed record in the accumulator to track
   both [accum] and [i] so we add [value_or_null] & all_ks]. *)

let rev_map (type a b) (l : a t) ~f : b t =
  let rec rmap_f accu : _ t -> _ t = function
    | [] -> accu
    | a :: l -> rmap_f (f a :: accu) l
  in
  rmap_f [] l [@nontail]
;;

let rec fold2_ok l1 l2 ~init ~(f : _ -> _ -> _ -> _) =
  match l1, l2 with
  | [], [] -> init
  | a1 :: l1, a2 :: l2 -> fold2_ok l1 l2 ~f ~init:(f init a1 a2)
  | _, _ -> invalid_arg "List.fold_left2"
;;

let rec exists2_ok l1 l2 ~(f : _ -> _ -> _) =
  match l1, l2 with
  | [], [] -> false
  | a1 :: l1, a2 :: l2 -> f a1 a2 || exists2_ok l1 l2 ~f
  | _, _ -> invalid_arg "List.exists2"
;;

let iter2_ok l1 l2 ~(f : _ -> _ -> unit) =
  let rec iter2_ok_loop l1 l2 =
    match l1, l2 with
    | [], [] -> ()
    | a1 :: l1, a2 :: l2 ->
      f a1 a2;
      iter2_ok_loop l1 l2
    | _, _ -> invalid_arg "List.iter2"
  in
  iter2_ok_loop l1 l2 [@nontail]
;;

let rec for_all2_ok l1 l2 ~(f : _ -> _ -> _) =
  match l1, l2 with
  | [], [] -> true
  | a1 :: l1, a2 :: l2 -> f a1 a2 && for_all2_ok l1 l2 ~f
  | _, _ -> invalid_arg "List.for_all2"
;;

let[@tail_mod_cons] rec map t ~f =
  match t with
  | [] -> []
  | x :: xs ->
    let y = f x in
    y :: map xs ~f
;;

let rev_map2_ok =
  let rec rmap2_f f accu l1 l2 =
    match l1, l2 with
    | [], [] -> accu
    | a1 :: l1, a2 :: l2 -> rmap2_f f (f a1 a2 :: accu) l1 l2
    | _, _ -> invalid_arg "List.rev_map2"
  in
  fun l1 l2 ~(f : _ -> _ -> _) -> rmap2_f f [] l1 l2
;;

let mapi t ~f = Stdlib.List.mapi t ~f
let partition t ~f = Stdlib.List.partition t ~f

(* call-stack size <= input data-stack size *)
let partition_map_unboxed ~depth ~f xs =
  let rec partition_map_unboxed_tail ~fst ~snd = function
    | [] -> rev fst, rev snd
    | x :: xs ->
      let fst, snd =
        match (f x : _ Either.t) with
        | First y -> y :: fst, snd
        | Second y -> fst, y :: snd
      in
      partition_map_unboxed_tail ~fst ~snd xs
  in
  let rec partition_map_unboxed_loop ~depth = function
    | [] -> [], []
    | x :: xs ->
      let y = f x in
      let fst, snd =
        if depth <= max_non_tailcall
        then partition_map_unboxed_loop ~depth:(depth + 1) xs
        else (partition_map_unboxed_tail [@inlined never]) ~fst:[] ~snd:[] xs
      in
      (match (y : _ Either.t) with
       | First y -> y :: fst, snd
       | Second y -> fst, y :: snd)
  in
  partition_map_unboxed_loop ~depth xs [@nontail]
;;

let partition_map t ~f =
  let fst, snd = partition_map_unboxed ~depth:0 ~f t in
  fst, snd
;;

module For_fold_right = struct
  module Wrapper = struct
    (* Avoid dependency on [Modes], plus generalizes [wrap_list] to [value_or_null]. *)
    external wrap_list : 'a. 'a list -> 'a Basement.Modes.global list = "%identity"

    let[@inline] unwrap { Basement.Modes.global } = global
  end
end

let fold_right l ~(f : _ -> _ -> _) ~init =
  let open For_fold_right.Wrapper in
  match l with
  | [] -> init (* avoid the allocation of [~f] below *)
  | _ ->
    fold ~f:(fun a b -> f (unwrap b) a [@nontail]) ~init (rev (wrap_list l)) [@nontail]
;;

let fold_right2_ok l1 l2 ~(f : _ -> _ -> _ -> _) ~init =
  match l1, l2 with
  | [], [] -> init (* avoid the allocation of [~f] below *)
  | _, _ -> fold2_ok ~f:(fun a b c -> f b c a) ~init (rev l1) (rev l2) [@nontail]
;;
