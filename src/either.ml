open! Import
include Either_intf
module List = List0
include Either0

let swap = function
  | First x -> Second x
  | Second x -> First x
;;

let is_first = function
  | First _ -> true
  | Second _ -> false
;;

let is_second = function
  | First _ -> false
  | Second _ -> true
;;

let value (First x | Second x) = x

let value_map t ~first ~second =
  match t with
  | First x -> first x
  | Second x -> second x
;;

let iter = value_map

let map t ~first ~second =
  match t with
  | First x -> First (first x)
  | Second x -> Second (second x)
;;

let first x = First x
let second x = Second x

let equal eq1 eq2 t1 t2 =
  match t1, t2 with
  | First x, First y -> eq1 x y
  | Second x, Second y -> eq2 x y
  | First _, Second _ | Second _, First _ -> false
;;

let local_equal eq1 eq2 t1 t2 =
  match t1, t2 with
  | First x, First y -> eq1 x y
  | Second x, Second y -> eq2 x y
  | First _, Second _ | Second _, First _ -> false
;;

let invariant f s = function
  | First x -> f x
  | Second y -> s y
;;

module Focus = struct
  type ('a, 'b) t =
    | Focus of { value : 'a }
    | Other of { value : 'b }
end

module Make_focused (M : sig
  type (+'a, +'b) t

  val return : 'a -> ('a, _) t
  val other : 'b -> (_, 'b) t
  val focus : ('a, 'b) t -> ('a, 'b) Focus.t

  val combine
    :  ('a, 'd) t
    -> ('b, 'd) t
    -> f:('a -> 'b -> 'c)
    -> other:('d -> 'd -> 'd)
    -> ('c, 'd) t

  val bind : ('a, 'b) t -> f:('a -> ('c, 'b) t) -> ('c, 'b) t
end) =
struct
  include M
  open With_return

  let map t ~f =
    let res = bind t ~f:(fun x -> return (f x)) in
    res
  ;;

  include Monad.Make2_local (struct
    type nonrec ('a, 'b) t = ('a, 'b) t

    let return = return
    let bind = bind
    let map = `Custom map
  end)

  module App = Applicative.Make2_using_map2_local (struct
    type nonrec ('a, 'b) t = ('a, 'b) t

    let return = return
    let map = `Custom map

    let map2 : ('a, 'x) t -> ('b, 'x) t -> f:('a -> 'b -> 'c) -> ('c, 'x) t =
      fun t1 t2 ~f ->
      bind t1 ~f:(fun x -> bind t2 ~f:(fun y -> return (f x y)) [@nontail]) [@nontail]
    ;;
  end)

  include App

  let combine_all =
    let rec other_loop f acc = function
      | [] -> other acc
      | t :: ts ->
        (match focus t with
         | Focus _ -> other_loop f acc ts
         | Other o -> other_loop f (f acc o.value) ts)
    in
    let rec return_loop f acc = function
      | [] -> return (List.rev acc)
      | t :: ts ->
        (match focus t with
         | Focus x -> return_loop f (x.value :: acc) ts
         | Other o -> other_loop f o.value ts)
    in
    fun ts ~f -> return_loop f [] ts
  ;;

  let combine_all_unit =
    let rec other_loop f acc = function
      | [] -> other acc
      | t :: ts ->
        (match focus t with
         | Focus _ -> other_loop f acc ts
         | Other o -> other_loop f (f acc o.value) ts)
    in
    let rec return_loop f = function
      | [] -> return ()
      | t :: ts ->
        (match focus t with
         | Focus { value = () } -> return_loop f ts
         | Other { value = o } -> other_loop f o ts)
    in
    fun ts ~f -> return_loop f ts
  ;;

  let to_option t =
    match focus t with
    | Focus x -> Some x.value
    | Other _ -> None
  ;;

  let value t ~default =
    match focus t with
    | Focus x -> x.value
    | Other _ -> default
  ;;

  let with_return f =
    with_return (fun ret -> other (f (With_return.prepend ret ~f:return))) [@nontail]
  ;;
end

module First = Make_focused (struct
  type nonrec ('a, 'b) t = ('a, 'b) t

  let return = first
  let other = second

  let focus t : _ Focus.t =
    match t with
    | First x -> Focus { value = x }
    | Second y -> Other { value = y }
  ;;

  let combine t1 t2 ~f ~other =
    match t1, t2 with
    | First x, First y -> First (f x y)
    | Second x, Second y -> Second (other x y)
    | Second x, _ | _, Second x -> Second x
  ;;

  let bind t ~f =
    match t with
    | First x -> f x
    (* Reuse the value in order to avoid allocation. *)
    | Second _ as y -> y
  ;;
end)

module Second = Make_focused (struct
  type nonrec ('a, 'b) t = ('b, 'a) t

  let return = second
  let other = first

  let focus t : _ Focus.t =
    match t with
    | Second x -> Focus { value = x }
    | First y -> Other { value = y }
  ;;

  let combine t1 t2 ~f ~other =
    match t1, t2 with
    | Second x, Second y -> Second (f x y)
    | First x, First y -> First (other x y)
    | First x, _ | _, First x -> First x
  ;;

  let bind t ~f =
    match t with
    | Second x -> f x
    (* Reuse the value in order to avoid allocation, like [First.bind] above. *)
    | First _ as y -> y
  ;;
end)

module Export = struct
  type ('f, 's) _either = ('f, 's) t =
    | First of 'f
    | Second of 's
end
