open! Import
module Array = Array0
module Either = Either0
module List = List0
include Container_intf

let with_return = With_return.with_return

type ('t, 'a, 'accum) fold = 't -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum
type ('t, 'a) iter = 't -> f:('a -> unit) -> unit
type 't length = 't -> int

let iter ~(fold : (_, _, _) fold) t ~f = fold t ~init:() ~f:(fun () a -> f a) [@nontail]
let count ~fold t ~f = fold t ~init:0 ~f:(fun n a -> if f a then n + 1 else n) [@nontail]

let sum (type a) ~fold (module M : Summable with type t = a) t ~f =
  fold t ~init:M.zero ~f:(fun n a -> M.( + ) n (f a)) [@nontail]
;;

let fold_result ~fold ~init ~f t =
  with_return (fun { return } ->
    Result.Ok
      (fold t ~init ~f:(fun acc item ->
         match f acc item with
         | Result.Ok x -> x
         | Error _ as e -> return e))) [@nontail]
;;

let fold_until ~fold ~init ~f ~finish t =
  with_return (fun { return } ->
    finish
      (fold t ~init ~f:(fun acc item ->
         match f acc item with
         | Continue_or_stop.Continue x -> x
         | Stop x -> return x))) [@nontail]
;;

let min_elt ~fold t ~compare =
  fold t ~init:None ~f:(fun acc elt ->
    match acc with
    | None -> Some elt
    | Some min -> if compare min elt > 0 then Some elt else acc) [@nontail]
;;

let max_elt ~fold t ~compare =
  fold t ~init:None ~f:(fun acc elt ->
    match acc with
    | None -> Some elt
    | Some max -> if compare max elt < 0 then Some elt else acc) [@nontail]
;;

let length ~fold c = fold c ~init:0 ~f:(fun acc _ -> acc + 1)

let is_empty ~iter c =
  with_return (fun r ->
    iter c ~f:(fun _ -> r.return false);
    true)
;;

let mem ~iter c x ~equal =
  with_return (fun r ->
    iter c ~f:(fun y -> if equal x y then r.return true);
    false) [@nontail]
;;

let exists ~iter c ~f =
  with_return (fun r ->
    iter c ~f:(fun x -> if f x then r.return true);
    false) [@nontail]
;;

let for_all ~iter c ~f =
  with_return (fun r ->
    iter c ~f:(fun x -> if not (f x) then r.return false);
    true) [@nontail]
;;

let find_map ~iter t ~f =
  with_return (fun r ->
    iter t ~f:(fun x ->
      match f x with
      | None -> ()
      | Some _ as res -> r.return res);
    None) [@nontail]
;;

let find ~iter c ~f =
  with_return (fun r ->
    iter c ~f:(fun x -> if f x then r.return (Some x));
    None) [@nontail]
;;

let to_list ~fold c = List.rev (fold c ~init:[] ~f:(fun acc x -> x :: acc))

let to_array ~length ~iter c =
  let array = ref [||] in
  let i = ref 0 in
  iter c ~f:(fun x ->
    if !i = 0 then array := Array.create ~len:(length c) x;
    !array.(!i) <- x;
    incr i);
  !array
;;

module Make_gen (T : Make_gen_arg) :
  Generic
    with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) T.t
     and type 'a elt := 'a T.elt = struct
  let fold = T.fold

  let iter =
    match T.iter with
    | `Custom iter -> iter
    | `Define_using_fold -> fun t ~f -> iter ~fold t ~f
  ;;

  let length =
    match T.length with
    | `Custom length -> length
    | `Define_using_fold -> fun t -> length ~fold t
  ;;

  let is_empty t = is_empty ~iter t
  let mem t x ~equal = mem ~iter t x ~equal
  let sum m t = sum ~fold m t
  let count t ~f = count ~fold t ~f
  let exists t ~f = exists ~iter t ~f
  let for_all t ~f = for_all ~iter t ~f
  let find_map t ~f = find_map ~iter t ~f
  let find t ~f = find ~iter t ~f
  let to_list t = to_list ~fold t
  let to_array t = to_array ~length ~iter t
  let min_elt t ~compare = min_elt ~fold t ~compare
  let max_elt t ~compare = max_elt ~fold t ~compare
  let fold_result t ~init ~f = fold_result t ~fold ~init ~f
  let fold_until t ~init ~f ~finish = fold_until t ~fold ~init ~f ~finish
end

module Make (T : Make_arg) = struct
  include Make_gen (struct
    include T

    type ('a, _, _) t = 'a T.t
    type 'a elt = 'a
  end)
end

module Make0 (T : Make0_arg) = struct
  include Make_gen (struct
    include T

    type ('a, _, _) t = T.t
    type 'a elt = T.Elt.t
  end)

  let mem t x = mem t x ~equal:T.Elt.equal
end

module Make_gen_with_creators (T : Make_gen_with_creators_arg) :
  Generic_with_creators
    with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) T.t
     and type 'a elt := 'a T.elt
     and type ('a, 'phantom1, 'phantom2) concat := ('a, 'phantom1, 'phantom2) T.concat =
struct
  include Make_gen (T)

  let of_list = T.of_list
  let of_array = T.of_array
  let concat = T.concat
  let concat_of_array = T.concat_of_array
  let append a b = concat (concat_of_array [| a; b |])
  let concat_map t ~f = concat (concat_of_array (Array.map (to_array t) ~f))

  let filter_map t ~f =
    concat_map t ~f:(fun x ->
      match f x with
      | None -> of_array [||]
      | Some y -> of_array [| y |]) [@nontail]
  ;;

  let map t ~f = filter_map t ~f:(fun x -> Some (f x)) [@nontail]
  let filter t ~f = filter_map t ~f:(fun x -> if f x then Some x else None) [@nontail]

  let partition_map t ~f =
    let array = Array.map (to_array t) ~f in
    let xs =
      Array.fold_right array ~init:[] ~f:(fun either acc ->
        match (either : _ Either.t) with
        | First x -> x :: acc
        | Second _ -> acc)
    in
    let ys =
      Array.fold_right array ~init:[] ~f:(fun either acc ->
        match (either : _ Either.t) with
        | First _ -> acc
        | Second x -> x :: acc)
    in
    of_list xs, of_list ys
  ;;

  let partition_tf t ~f =
    partition_map t ~f:(fun x -> if f x then First x else Second x) [@nontail]
  ;;
end

module Make_with_creators (T : Make_with_creators_arg) = struct
  include Make_gen_with_creators (struct
    include T

    type ('a, _, _) t = 'a T.t
    type 'a elt = 'a
    type ('a, _, _) concat = 'a T.t

    let concat_of_array = of_array
  end)
end

module Make0_with_creators (T : Make0_with_creators_arg) = struct
  include Make_gen_with_creators (struct
    include T

    type ('a, _, _) t = T.t
    type 'a elt = T.Elt.t
    type ('a, _, _) concat = 'a list

    let concat_of_array = Array.to_list
  end)

  let mem t x = mem t x ~equal:T.Elt.equal
end
