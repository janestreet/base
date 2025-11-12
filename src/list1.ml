open! Import
include List0

let is_empty = function
  | [] -> true
  | _ -> false
;;

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
