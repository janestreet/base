(* [List0] defines list functions that are primitives or can be simply defined in terms of
   [Stdlib.List].  [List0] is intended to completely express the part of [Stdlib.List] that
   [Base] uses -- no other file in Base other than list0.ml should use [Stdlib.List].
   [List0] has few dependencies, and so is available early in Base's build order.  All
   Base files that need to use lists and come before [Base.List] in build order should do
   [module List = List0].  Defining [module List = List0] is also necessary because it
   prevents ocamldep from mistakenly causing a file to depend on [Base.List]. *)


open! Import0

let hd_exn = Stdlib.List.hd
let length = Stdlib.List.length
let rev_append = Stdlib.List.rev_append
let tl_exn = Stdlib.List.tl
let unzip = Stdlib.List.split

(* Some of these are eta expanded in order to permute parameter order to follow Base
   conventions. *)

let rec exists t ~f:(f [@local]) =
  match t with
  | [] -> false
  | x :: xs -> if f x then true else exists xs ~f
;;

let rec exists2_ok l1 l2 ~f:((f : _ -> _ -> _) [@local]) =
  match l1, l2 with
  | [], [] -> false
  | a1 :: l1, a2 :: l2 -> f a1 a2 || exists2_ok l1 l2 ~f
  | _, _ -> invalid_arg "List.exists2"
;;

let rec fold t ~init ~f:((f : _ -> _ -> _) [@local]) =
  match t with
  | [] -> init
  | a :: l -> fold l ~init:(f init a) ~f
;;

let rec fold2_ok l1 l2 ~init ~f:((f : _ -> _ -> _ -> _) [@local]) =
  match l1, l2 with
  | [], [] -> init
  | a1 :: l1, a2 :: l2 -> fold2_ok l1 l2 ~f ~init:(f init a1 a2)
  | _, _ -> invalid_arg "List.fold_left2"
;;

let for_all t ~f:(f [@local]) = not (exists t ~f:(fun x -> not (f x)))

let rec for_all2_ok l1 l2 ~f:((f : _ -> _ -> _) [@local]) =
  match l1, l2 with
  | [], [] -> true
  | a1 :: l1, a2 :: l2 -> f a1 a2 && for_all2_ok l1 l2 ~f
  | _, _ -> invalid_arg "List.for_all2"
;;

let rec iter t ~f:((f : _ -> _) [@local]) =
  match t with
  | [] -> ()
  | a :: l ->
    f a;
    iter l ~f
;;

let rec iter2_ok l1 l2 ~f:((f : _ -> _ -> unit) [@local]) =
  match l1, l2 with
  | [], [] -> ()
  | a1 :: l1, a2 :: l2 ->
    f a1 a2;
    iter2_ok l1 l2 ~f
  | _, _ -> invalid_arg "List.iter2"
;;

let rec nontail_map t ~f:(f [@local]) =
  match t with
  | [] -> []
  | x :: xs ->
    let y = f x in
    y :: nontail_map xs ~f
;;

let nontail_mapi t ~f = Stdlib.List.mapi t ~f
let partition t ~f = Stdlib.List.partition t ~f

let rev_map =
  let rec rmap_f f accu = function
    | [] -> accu
    | a :: l -> rmap_f f (f a :: accu) l
  in
  fun l ~f:(f [@local]) -> rmap_f f [] l
;;

let rev_map2_ok =
  let rec rmap2_f f accu l1 l2 =
    match l1, l2 with
    | [], [] -> accu
    | a1 :: l1, a2 :: l2 -> rmap2_f f (f a1 a2 :: accu) l1 l2
    | _, _ -> invalid_arg "List.rev_map2"
  in
  fun l1 l2 ~f:((f : _ -> _ -> _) [@local]) -> rmap2_f f [] l1 l2
;;

let sort l ~compare = Stdlib.List.sort l ~cmp:compare
let stable_sort l ~compare = Stdlib.List.stable_sort l ~cmp:compare

let rev = function
  | ([] | [ _ ]) as res -> res
  | x :: y :: rest -> rev_append rest [ y; x ]
;;

let fold_right l ~f:((f : _ -> _ -> _) [@local]) ~init =
  match l with
  | [] -> init (* avoid the allocation of [~f] below *)
  | _ -> fold ~f:(fun a b -> f b a) ~init (rev l) [@nontail]
;;

let fold_right2_ok l1 l2 ~f:((f : _ -> _ -> _ -> _) [@local]) ~init =
  match l1, l2 with
  | [], [] -> init (* avoid the allocation of [~f] below *)
  | _, _ -> fold2_ok ~f:(fun a b c -> f b c a) ~init (rev l1) (rev l2) [@nontail]
;;
