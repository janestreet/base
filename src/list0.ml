(* [List0] defines list functions that are primitives or can be simply defined in terms of
   [Stdlib.List]. [List0] is intended to completely express the part of [Stdlib.List] that
   [Base] uses -- no other file in Base other than list0.ml should use [Stdlib.List].
   [List0] has few dependencies, and so is available early in Base's build order. All Base
   files that need to use lists and come before [Base.List] in build order should do
   [module List = List0]. Defining [module List = List0] is also necessary because it
   prevents ocamldep from mistakenly causing a file to depend on [Base.List]. *)

open! Import0

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

[%%template
[@@@kind_set.define
  all_ks_non_value
  = ( base_non_value
    , value_or_null & value_or_null
    , value_or_null & value_or_null & value_or_null
    , value_or_null & value_or_null & value_or_null & value_or_null )]

[@@@kind_set.define all_ks = (all_ks_non_value, value_or_null)]

module%template Constructors = struct
  type ('a : k) t =
    | []
    | ( :: ) of 'a * ('a t[@kind.explicit k])
  [@@kind.explicit k = all_ks_non_value] [@@deriving compare ~localize, equal ~localize]

  type ('a : value_or_null) t = 'a list =
    | []
    | ( :: ) of 'a * 'a t

  type ('a : value_or_null) t = 'a list =
    | []
    | ( :: ) of 'a * 'a t
  [@@kind.explicit value_or_null]

  (* Avoid re-deriving [compare] and [equal] *)
  [%%rederive
    type ('a : value_or_null) t = 'a list [@@deriving compare ~localize, equal ~localize]]
end

open Constructors

(* Some of these are eta expanded in order to permute parameter order to follow Base
   conventions. *)

let%template hd_exn = function
  | [] -> failwith "hd"
  | a :: _ -> a
[@@mode l = (global, local)]
;;

let%template tl_exn = function
  | [] -> failwith "tl"
  | _ :: l -> l
[@@mode l = (global, local)]
;;

let is_empty = function
  | [] -> true
  | _ -> false
;;

[%%template
[@@@kind.default k = all_ks]

open struct
  type nonrec ('a : any) t = ('a t[@kind k]) =
    | []
    | ( :: ) of 'a * ('a t[@kind k])
  [@@kind k]
end

let length =
  let rec length_aux len = function
    | [] -> len
    | _ :: l -> length_aux (len + 1) l
  in
  fun (local_ l) -> length_aux 0 l
;;

let exists t ~f =
  let rec exists_loop t =
    match t with
    | [] -> false
    | x :: xs -> if f x then true else exists_loop xs
  in
  exists_loop t [@nontail]
[@@mode l = (local, global)]
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
[@@mode l = (local, global)]
;;

(* Copied from [Stdlib] for templating *)
let rev_append (l1 @ l u) (l2 @ l u) =
  let rec rev_append_loop l1 l2 =
    match l1 with
    | [] -> l2
    | a :: l -> rev_append_loop l (a :: l2) [@exclave_if_stack a]
  in
  rev_append_loop l1 l2 [@exclave_if_stack a]
[@@mode u = (aliased, unique)] [@@alloc a @ l = (stack_local, heap_global)]
;;

let rev (l @ l u) =
  match l with
  | ([] | [ _ ]) as res -> res
  | x :: y :: rest ->
    (rev_append [@mode u] [@alloc a] [@kind k]) rest [ y; x ] [@exclave_if_stack a]
[@@mode u = (aliased, unique)] [@@alloc a @ l = (stack_local, heap_global)]
;;

let for_all t ~(f @ local) = not ((exists [@kind k] [@mode l]) t ~f:(fun x -> not (f x)))
[@@mode l = (local, global)]
;;]

let fold
  (type (a : ka) (b : kb))
  (t : (a t[@kind ka]) @ ma)
  ~(init : b @ mb)
  ~(f : (b @ mb -> a @ ma -> b @ mb) @ local)
  : b @ mb
  =
  (let rec fold_loop (acc : b @ mb) (l : (a t[@kind ka]) @ ma) =
     match l with
     | [] -> acc
     | a :: l ->
       fold_loop (f acc a) l [@exclave_if_local mb ~reasons:[ May_return_local ]]
   in
   fold_loop init t [@nontail])
  [@exclave_if_local mb ~reasons:[ May_return_local ]]
[@@mode ma = (local, global), mb = (local, global)]
(* list.ml's [foldi] implementation uses an unboxed record in the accumulator to track
   both [accum] and [i] so we add [value_or_null] & all_ks]. *)
[@@kind ka = all_ks, kb = (all_ks, value_or_null & all_ks)]
;;

let rev_map (type (a : ka) (b : kb)) (l : (a t[@kind ka]) @ ma) ~(local_ f)
  : (b t[@kind kb])
  =
  (let rec rmap_f accu : (_ t[@kind ka]) @ ma -> (_ t[@kind kb]) @ mb = function
     | [] -> accu
     | a :: l -> rmap_f (f a :: accu) l [@exclave_if_stack ab]
   in
   rmap_f [] l [@nontail])
  [@exclave_if_stack ab]
[@@mode ma = (local, global)]
[@@alloc ab @ mb = (stack_local, heap_global)]
[@@kind ka = all_ks, kb = all_ks]
;;

let rec fold2_ok l1 l2 ~init ~(local_ f : _ -> _ -> _ -> _) =
  match l1, l2 with
  | [], [] -> init
  | a1 :: l1, a2 :: l2 -> fold2_ok l1 l2 ~f ~init:(f init a1 a2)
  | _, _ -> invalid_arg "List.fold_left2"
;;

let rec exists2_ok l1 l2 ~(local_ f : _ -> _ -> _) =
  match l1, l2 with
  | [], [] -> false
  | a1 :: l1, a2 :: l2 -> f a1 a2 || exists2_ok l1 l2 ~f
  | _, _ -> invalid_arg "List.exists2"
;;

let%template iter2_ok l1 l2 ~(local_ f : _ @ l -> _ @ l -> unit) =
  let rec iter2_ok_loop l1 l2 =
    match l1, l2 with
    | [], [] -> ()
    | a1 :: l1, a2 :: l2 ->
      f a1 a2;
      iter2_ok_loop l1 l2
    | _, _ -> invalid_arg "List.iter2"
  in
  iter2_ok_loop l1 l2 [@nontail]
[@@mode l = (global, local)]
;;

let rec for_all2_ok l1 l2 ~(local_ f : _ -> _ -> _) =
  match l1, l2 with
  | [], [] -> true
  | a1 :: l1, a2 :: l2 -> f a1 a2 && for_all2_ok l1 l2 ~f
  | _, _ -> invalid_arg "List.for_all2"
;;

let rec nontail_map t ~(local_ f) =
  match t with
  | [] -> []
  | x :: xs ->
    let y = f x in
    y :: nontail_map xs ~f
;;

let rev_map2_ok =
  let rec rmap2_f f accu l1 l2 =
    match l1, l2 with
    | [], [] -> accu
    | a1 :: l1, a2 :: l2 -> rmap2_f f (f a1 a2 :: accu) l1 l2
    | _, _ -> invalid_arg "List.rev_map2"
  in
  fun l1 l2 ~(local_ f : _ -> _ -> _) -> rmap2_f f [] l1 l2
;;

let nontail_mapi t ~f = Stdlib.List.mapi t ~f
let partition t ~f = Stdlib.List.partition t ~f

[%%template
[@@@mode.default li = (global, local)]
[@@@alloc.default a @ lo = (heap_global, stack_local)]

(* call-stack size <= input data-stack size *)
let partition_map_unboxed ~depth ~f xs =
  (let rec partition_map_unboxed_tail ~fst ~snd = function
     | [] -> #((rev [@alloc a]) fst, (rev [@alloc a]) snd) [@exclave_if_stack a]
     | x :: xs ->
       (let #(fst, snd) =
          match (f x : _ Either0.t) with
          | First y -> #(y :: fst, snd)
          | Second y -> #(fst, y :: snd)
        in
        partition_map_unboxed_tail ~fst ~snd xs)
       [@exclave_if_stack a]
   in
   let rec partition_map_unboxed_loop ~depth = function
     | [] -> #([], [])
     | x :: xs ->
       (let y = f x in
        let #(fst, snd) =
          if depth <= max_non_tailcall
          then partition_map_unboxed_loop ~depth:(depth + 1) xs
          else (partition_map_unboxed_tail [@inlined never]) ~fst:[] ~snd:[] xs
        in
        (match (y : _ Either0.t) with
         | First y -> #(y :: fst, snd)
         | Second y -> #(fst, y :: snd)))
       [@exclave_if_stack a]
   in
   partition_map_unboxed_loop ~depth xs [@nontail])
  [@exclave_if_stack a]
;;

let partition_map t ~f =
  (let #(fst, snd) = (partition_map_unboxed [@mode li] [@alloc a]) ~depth:0 ~f t in
   fst, snd)
  [@exclave_if_stack a]
;;]

module For_fold_right = struct
  module%template [@mode global] Wrapper = struct
    external wrap_list
      : ('a : value_or_null).
      'a list -> 'a Modes.Global.t list
      @@ portable
      = "%identity"

    let[@inline] unwrap { Modes.Global.global } = global
  end

  module%template [@mode local] Wrapper = struct
    let wrap_list = Fn.id
    let unwrap = Fn.id
  end
end

let%template fold_right (l @ ma) ~(local_ f : _ @ ma -> _ @ mb -> _ @ mb) ~init =
  let open For_fold_right.Wrapper [@mode ma] in
  match l with
  | [] -> init (* avoid the allocation of [~f] below *)
  | _ ->
    (fold [@mode local mb])
      ~f:(fun a b ->
        f (unwrap b) a [@nontail] [@exclave_if_local mb ~reasons:[ May_return_local ]])
      ~init
      ((rev [@alloc stack]) (wrap_list l))
    [@nontail] [@exclave_if_local mb ~reasons:[ May_return_local ]]
[@@mode ma = (local, global), mb = (local, global)]
;;

let fold_right2_ok l1 l2 ~(local_ f : _ -> _ -> _ -> _) ~init =
  match l1, l2 with
  | [], [] -> init (* avoid the allocation of [~f] below *)
  | _, _ -> fold2_ok ~f:(fun a b c -> f b c a) ~init (rev l1) (rev l2) [@nontail]
;;]
