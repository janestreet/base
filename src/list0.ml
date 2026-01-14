(* [List0] defines list functions that are primitives or can be simply defined in terms of
   [Stdlib.List]. [List0] is intended to completely express the part of [Stdlib.List] that
   [Base] uses -- no other file in Base other than list0.ml should use [Stdlib.List].
   [List0] has few dependencies, and so is available early in Base's build order. All Base
   files that need to use lists and come before [Base.List] in build order should do
   [module List = List0]. Defining [module List = List0] is also necessary because it
   prevents ocamldep from mistakenly causing a file to depend on [Base.List]. *)

[@@@warning "-incompatible-with-upstream"]

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

let hd_exn = Stdlib.List.hd
let tl_exn = Stdlib.List.tl
let unzip = Stdlib.List.split

module%template Constructors = struct
  type ('a : k) t =
    | []
    | ( :: ) of 'a * ('a t[@kind k])
  [@@kind
    k
    = ( base_non_value
      , value_or_null & value_or_null
      , value_or_null & value_or_null & value_or_null
      , value_or_null & value_or_null & value_or_null & value_or_null )]
  [@@deriving compare ~localize, equal ~localize]

  type ('a : value_or_null) t = 'a list =
    | []
    | ( :: ) of 'a * 'a t

  [%%rederive
    type ('a : value_or_null) t = 'a list [@@deriving compare ~localize, equal ~localize]]
end

open Constructors

(* Some of these are eta expanded in order to permute parameter order to follow Base
   conventions. *)

let is_empty = function
  | [] -> true
  | _ -> false
;;

[%%template
[@@@kind.default
  k
  = ( base_or_null
    , value_or_null & value_or_null
    , value_or_null & value_or_null & value_or_null
    , value_or_null & value_or_null & value_or_null & value_or_null )]

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
  let rec loop t ~f =
    match t with
    | [] -> false
    | x :: xs -> if f x then true else loop xs ~f
  in
  loop t ~f
[@@mode m = (local, global)]
;;

let iter t ~f =
  let rec loop t ~f =
    match t with
    | [] -> ()
    | a :: l ->
      f a;
      loop l ~f
  in
  loop t ~f
[@@mode m = (local, global)]
;;

(* Copied from [Stdlib] for templating *)
let rev_append (l1 @ m) (l2 @ m) =
  let rec loop l1 l2 =
    match l1 with
    | [] -> l2
    | a :: l -> loop l (a :: l2) [@exclave_if_stack a]
  in
  loop l1 l2 [@exclave_if_stack a]
[@@alloc a @ m = (stack_local, heap_global)]
;;

let rev (l @ m) =
  match l with
  | ([] | [ _ ]) as res -> res
  | x :: y :: rest ->
    (rev_append [@alloc a] [@kind k]) rest [ y; x ] [@exclave_if_stack a]
[@@alloc a @ m = (stack_local, heap_global)]
;;

let for_all t ~(f @ local) = not ((exists [@kind k] [@mode m]) t ~f:(fun x -> not (f x)))
[@@mode m = (local, global)]
;;

[@@@kind ka = k]

[%%template
[@@@kind.default
  ka = ka
  , kb
    = ( base_or_null
      , value_or_null & base_or_null
      , value_or_null & value_or_null & value_or_null
      , value_or_null & value_or_null & value_or_null & value_or_null )]

let fold_alloc
  (t @ ma)
  ~(init @ mb)
  ~(f : ((_ : kb) @ mb -> _ @ ma -> (_ : kb) @ mb) @ local)
  =
  (let rec loop acc = function
     | [] -> acc
     | a :: l -> loop (f acc a) l [@exclave_if_stack ab]
   in
   loop init t [@nontail])
  [@exclave_if_stack ab]
[@@mode ma = (local, global)] [@@alloc ab @ mb = (stack_local, heap_global)]
;;

let fold = (fold_alloc [@mode ma] [@alloc stack] [@kind ka kb])
[@@mode ma = (local, global), mb = local]
;;

let fold = (fold_alloc [@mode ma] [@alloc heap] [@kind ka kb])
[@@mode ma = (local, global), mb = global]
;;]

[@@@kind.default kb = base_or_null]

let rev_map =
  let rec rmap_f f accu : (_ t[@kind ka]) @ ma -> (_ t[@kind kb]) @ mb = function
    | [] -> accu
    | a :: l -> rmap_f f (f a :: accu) l [@exclave_if_stack ab]
  in
  fun l ~(local_ f) -> rmap_f f [] l [@exclave_if_stack ab]
[@@mode ma = (local, global)] [@@alloc ab @ mb = (stack_local, heap_global)]
;;]

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

let rec iter2_ok l1 l2 ~(local_ f : _ -> _ -> unit) =
  match l1, l2 with
  | [], [] -> ()
  | a1 :: l1, a2 :: l2 ->
    f a1 a2;
    iter2_ok l1 l2 ~f
  | _, _ -> invalid_arg "List.iter2"
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
[@@@mode.default mi = (global, local)]
[@@@alloc.default a @ mo = (heap_global, stack_local)]

let partition_map_unboxed_tail ~fst ~snd ~f xs =
  let rec loop ~fst ~snd ~(f @ local) = function
    | [] -> #((rev [@alloc a]) fst, (rev [@alloc a]) snd) [@exclave_if_stack a]
    | x :: xs ->
      (let #(fst, snd) =
         match (f x : _ Either0.t) with
         | First y -> #(y :: fst, snd)
         | Second y -> #(fst, y :: snd)
       in
       loop ~fst ~snd ~f xs)
      [@exclave_if_stack a]
  in
  loop ~fst ~snd ~f xs [@exclave_if_stack a]
;;

(* call-stack size <= input data-stack size *)
let partition_map_unboxed ~depth ~f xs =
  let rec loop ~depth ~f = function
    | [] -> #([], [])
    | x :: xs ->
      (let y = f x in
       let #(fst, snd) =
         if depth <= max_non_tailcall
         then loop ~depth:(depth + 1) ~f xs
         else
           (partition_map_unboxed_tail [@mode mi] [@alloc a] [@inlined never])
             ~fst:[]
             ~snd:[]
             ~f
             xs
       in
       (match (y : _ Either0.t) with
        | First y -> #(y :: fst, snd)
        | Second y -> #(fst, y :: snd)))
      [@exclave_if_stack a]
  in
  loop ~depth ~f xs [@exclave_if_stack a]
;;

let partition_map t ~f =
  (let #(fst, snd) = (partition_map_unboxed [@mode mi] [@alloc a]) ~depth:0 ~f t in
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
      ~f:(fun a b -> f (unwrap b) a [@nontail] [@exclave_if_stack ab])
      ~init
      ((rev [@alloc stack]) (wrap_list l)) [@nontail] [@exclave_if_stack ab]
[@@mode ma = (local, global)] [@@alloc ab @ mb = (stack_local, heap_global)]
;;

let%template fold_right = (fold_right [@mode ma] [@alloc stack])
[@@mode ma = (local, global), __ = local]
;;

let%template fold_right = (fold_right [@mode ma] [@alloc heap])
[@@mode ma = (local, global), __ = global]
;;

let fold_right2_ok l1 l2 ~(local_ f : _ -> _ -> _ -> _) ~init =
  match l1, l2 with
  | [], [] -> init (* avoid the allocation of [~f] below *)
  | _, _ -> fold2_ok ~f:(fun a b c -> f b c a) ~init (rev l1) (rev l2) [@nontail]
;;
