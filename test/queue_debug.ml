open! Core_kernel

module Debug (Queue : module type of Queue) = struct

  module Debug = Debug.Make ()

  include Debug

  open Queue

  module type S = S

  type nonrec 'a t = 'a t [@@deriving bin_io, sexp]

  let invariant = invariant

  let debug x = debug (invariant ignore) ~module_name:"Queue" x

  let equal equal_elt t1 t2 =
    debug "equal" [ t1; t2 ] (t1, t2) [%sexp_of: _ t * _ t] [%sexp_of: bool]
      (fun () -> equal equal_elt t1 t2)
  ;;

  let compare compare_elt t1 t2 =
    debug "compare" [ t1; t2 ] (t1, t2) [%sexp_of: _ t * _ t] [%sexp_of: int]
      (fun () -> compare compare_elt t1 t2)
  ;;

  let mem t elt ~equal =
    debug "mem" [ t ] t [%sexp_of: _ t] [%sexp_of: bool]
      (fun () -> mem t elt ~equal)
  ;;

  let length t =
    debug "length" [ t ] t [%sexp_of: _ t] [%sexp_of: int]
      (fun () -> length t)
  ;;

  let is_empty t =
    debug "is_empty" [ t ] t [%sexp_of: _ t] [%sexp_of: bool]
      (fun () -> is_empty t)
  ;;

  let iter t ~f =
    debug "iter" [ t ] t [%sexp_of: _ t] [%sexp_of: unit]
      (fun () -> iter t ~f)
  ;;

  let iteri t ~f =
    debug "iteri" [ t ] t [%sexp_of: _ t] [%sexp_of: unit]
      (fun () -> iteri t ~f)
  ;;

  let fold t ~init ~f =
    debug "fold" [ t ] t [%sexp_of: _ t] [%sexp_of: _]
      (fun () -> fold t ~init ~f)
  ;;

  let foldi t ~init ~f =
    debug "foldi" [ t ] t [%sexp_of: _ t] [%sexp_of: _]
      (fun () -> foldi t ~init ~f)
  ;;

  let fold_result t ~init ~f =
    debug "fold_result" [ t ] t [%sexp_of: _ t] [%sexp_of: _]
      (fun () -> fold_result t ~init ~f)
  ;;

  let fold_until t ~init ~f =
    debug "fold_until" [ t ] t [%sexp_of: _ t] [%sexp_of: _]
      (fun () -> fold_until t ~init ~f)
  ;;

  let exists t ~f =
    debug "exists" [ t ] t [%sexp_of: _ t] [%sexp_of: bool]
      (fun () -> exists t ~f)
  ;;

  let existsi t ~f =
    debug "existsi" [ t ] t [%sexp_of: _ t] [%sexp_of: bool]
      (fun () -> existsi t ~f)
  ;;

  let for_all t ~f =
    debug "for_all" [ t ] t [%sexp_of: _ t] [%sexp_of: bool]
      (fun () -> for_all t ~f)
  ;;

  let for_alli t ~f =
    debug "for_alli" [ t ] t [%sexp_of: _ t] [%sexp_of: bool]
      (fun () -> for_alli t ~f)
  ;;

  let count t ~f =
    debug "count" [ t ] t [%sexp_of: _ t] [%sexp_of: int]
      (fun () -> count t ~f)
  ;;

  let counti t ~f =
    debug "counti" [ t ] t [%sexp_of: _ t] [%sexp_of: int]
      (fun () -> counti t ~f)
  ;;

  let sum (type a) (module M : Commutative_group.S with type t = a) t ~f =
    debug "sum" [ t ] t [%sexp_of: _ t] [%sexp_of: M.t]
      (fun () -> sum (module M) t ~f)
  ;;

  let find t ~f =
    debug "find" [ t ] t [%sexp_of: _ t] [%sexp_of: _ option]
      (fun () -> find t ~f)
  ;;

  let findi t ~f =
    debug "findi" [ t ] t [%sexp_of: _ t] [%sexp_of: _ option]
      (fun () -> findi t ~f)
  ;;

  let find_map t ~f =
    debug "find_map" [ t ] t [%sexp_of: _ t] [%sexp_of: _ option]
      (fun () -> find_map t ~f)
  ;;

  let find_mapi t ~f =
    debug "find_mapi" [ t ] t [%sexp_of: _ t] [%sexp_of: _ option]
      (fun () -> find_mapi t ~f)
  ;;

  let min_elt t ~compare =
    debug "min_elt" [ t ] t [%sexp_of: _ t] [%sexp_of: _ option]
      (fun () -> min_elt t ~compare)
  ;;

  let max_elt t ~compare =
    debug "max_elt" [ t ] t [%sexp_of: _ t] [%sexp_of: _ option]
      (fun () -> max_elt t ~compare)
  ;;

  let to_list t =
    debug "to_list" [ t ] t [%sexp_of: _ t] [%sexp_of: _ list]
      (fun () -> to_list t)
  ;;

  let to_array t =
    debug "to_array" [ t ] t [%sexp_of: _ t] [%sexp_of: _ array]
      (fun () -> to_array t)
  ;;

  let create ?capacity () =
    debug "create" [ ] capacity [%sexp_of: int option] [%sexp_of: _ t]
      (fun () -> create ?capacity ())
  ;;

  let singleton a =
    debug "singleton" [ ] () [%sexp_of: unit] [%sexp_of: _ t]
      (fun () -> singleton a)
  ;;

  let init n ~f =
    debug "init" [ ] n [%sexp_of: int] [%sexp_of: _ t]
      (fun () -> init n ~f)
  ;;

  let enqueue t a =
    debug "enqueue" [ t ] t [%sexp_of: _ t] [%sexp_of: unit]
      (fun () -> enqueue t a)
  ;;

  let enqueue_all t l =
    debug "enqueue_all" [ t ] t [%sexp_of: _ t] [%sexp_of: unit]
      (fun () -> enqueue_all t l)
  ;;

  let dequeue t =
    debug "dequeue" [ t ] t [%sexp_of: _ t] [%sexp_of: _ option]
      (fun () -> dequeue t)
  ;;

  let dequeue_exn t =
    debug "dequeue_exn" [ t ] t [%sexp_of: _ t] [%sexp_of: _]
      (fun () -> dequeue_exn t)
  ;;

  let peek t =
    debug "peek" [ t ] t [%sexp_of: _ t] [%sexp_of: _ option]
      (fun () -> peek t)
  ;;

  let peek_exn t =
    debug "peek_exn" [ t ] t [%sexp_of: _ t] [%sexp_of: _]
      (fun () -> peek_exn t)
  ;;

  let last t =
    debug "last" [ t ] t [%sexp_of: _ t] [%sexp_of: _ option]
      (fun () -> last t)
  ;;

  let last_exn t =
    debug "last_exn" [ t ] t [%sexp_of: _ t] [%sexp_of: _]
      (fun () -> last_exn t)
  ;;

  let clear t =
    debug "clear" [ t ] t [%sexp_of: _ t] [%sexp_of: unit]
      (fun () -> clear t)
  ;;

  let copy t =
    debug "copy" [ t ] t [%sexp_of: _ t] [%sexp_of: _ t]
      (fun () -> copy t)
  ;;

  let blit_transfer ~src ~dst ?len () =
    debug "blit_transfer" [ src; dst ] (src, dst, len)
      [%sexp_of: _ t * _ t * int option] [%sexp_of: unit]
      (fun () -> blit_transfer ~src ~dst ?len ())
  ;;

  let of_list l =
    debug "of_list" [ ] l [%sexp_of: _ list] [%sexp_of: _ t]
      (fun () -> of_list l)
  ;;

  let of_array a =
    debug "of_array" [ ] a [%sexp_of: _ array] [%sexp_of: _ t]
      (fun () -> of_array a)
  ;;

  let map t ~f =
    debug "map" [ t ] t [%sexp_of: _ t] [%sexp_of: _ t]
      (fun () -> map t ~f)
  ;;

  let mapi t ~f =
    debug "mapi" [ t ] t [%sexp_of: _ t] [%sexp_of: _ t]
      (fun () -> mapi t ~f)
  ;;

  let concat_map t ~f =
    debug "concat_map" [ t ] t [%sexp_of: _ t] [%sexp_of: _ t]
      (fun () -> concat_map t ~f)
  ;;

  let concat_mapi t ~f =
    debug "concat_mapi" [ t ] t [%sexp_of: _ t] [%sexp_of: _ t]
      (fun () -> concat_mapi t ~f)
  ;;

  let filter_map t ~f =
    debug "filter_map" [ t ] t [%sexp_of: _ t] [%sexp_of: _ t]
      (fun () -> filter_map t ~f)
  ;;

  let filter_mapi t ~f =
    debug "filter_mapi" [ t ] t [%sexp_of: _ t] [%sexp_of: _ t]
      (fun () -> filter_mapi t ~f)
  ;;

  let filter t ~f =
    debug "filter" [ t ] t [%sexp_of: _ t] [%sexp_of: _ t]
      (fun () -> filter t ~f)
  ;;

  let filteri t ~f =
    debug "filteri" [ t ] t [%sexp_of: _ t] [%sexp_of: _ t]
      (fun () -> filteri t ~f)
  ;;

  let filter_inplace t ~f =
    debug "filter_inplace" [ t ] t [%sexp_of: _ t] [%sexp_of: unit]
      (fun () -> filter_inplace t ~f)
  ;;

  let filteri_inplace t ~f =
    debug "filteri_inplace" [ t ] t [%sexp_of: _ t] [%sexp_of: unit]
      (fun () -> filteri_inplace t ~f)
  ;;

  let get t i =
    debug "get" [ t ] (t, i) [%sexp_of: _ t * int] [%sexp_of: _]
      (fun () -> get t i)
  ;;

  let set t i a =
    debug "set" [ t ] (t, i) [%sexp_of: _ t * int] [%sexp_of: unit]
      (fun () -> set t i a)
  ;;

  let capacity t =
    debug "capacity" [ t ] t [%sexp_of: _ t] [%sexp_of: int]
      (fun () -> capacity t)
  ;;

  let set_capacity t capacity =
    debug "set_capacity" [ t ] (t, capacity) [%sexp_of: _ t * int] [%sexp_of: unit]
      (fun () -> set_capacity t capacity)
  ;;

  let binary_search ?pos ?len t ~compare which v =
    debug "binary_search" [ t ] (t, pos, len)
      [%sexp_of: _ t * int option * int option]
      [%sexp_of: int option]
      (fun () -> binary_search ?pos ?len t ~compare which v)
  ;;

  let binary_search_segmented ?pos ?len t ~segment_of which =
    debug "binary_search_segmented" [ t ] (t, pos, len)
      [%sexp_of: _ t * int option * int option]
      [%sexp_of: int option]
      (fun () -> binary_search_segmented ?pos ?len t ~segment_of which)
  ;;

  module Stable = Stable
end
