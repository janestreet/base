open! Import0
module Array = Array0
module Either = Either0
module Result = Result0
module List = List0
include Container_intf.Definitions

(* We belive the functions in this file will be expressible using mode-polymorphism but it
   requires more complicated lifetime information to express. For example, the [f]
   argument to [fold] needs to promise that it returns an unboxed thing that has the same
   locality as the [elt] argument and that information needs to propagate through [fold].
*)

module Derived : Derived = struct
  include struct
    let[@inline] fold_until ~fold t ~init ~f ~finish =
      (With_return.with_return [@inlined]) (fun [@inline] { return } ->
        (finish [@inlined hint])
          ((fold [@inlined hint]) t ~init ~f:(fun [@inline] acc item ->
             match (f [@inlined hint]) acc item with
             | Continue_or_stop.Continue x -> x
             | Stop x -> return x)))
      [@nontail]
    ;;

    let[@inline] fold_alloc ~fold_until t ~init ~f =
      (fold_until [@inlined hint])
        t
        ~init
        ~f:(fun [@inline] acc x -> Continue_or_stop.Continue (f acc x))
        ~finish:Fn.id [@nontail]
    ;;

    let fold = fold_alloc

    let[@inline] iter_until_alloc ~fold_until t ~f ~finish =
      (fold_until [@inlined hint])
        t
        ~init:()
        ~f:(fun [@inline] () x -> f x)
        ~finish:(fun [@inline] () -> finish ()) [@nontail]
    ;;

    let iter_until = iter_until_alloc

    let[@inline] iter_via_fold ~fold t ~f =
      (fold [@inlined hint]) t ~init:() ~f:(fun [@inline] () x -> f x) [@nontail]
    ;;

    let[@inline] iter_via_iter_until ~iter_until t ~f =
      (iter_until [@inlined hint])
        t
        ~f:(fun [@inline] a ->
          f a;
          Continue_or_stop.Continue ())
        ~finish:Fn.id [@nontail]
    ;;

    let[@inline] count ~fold t ~f =
      (fold [@inlined hint]) t ~init:0 ~f:(fun [@inline] n a -> if f a then n + 1 else n)
      [@nontail]
    ;;

    let[@inline] sum_alloc (type a) ~fold (module M : Summable with type t = a) t ~f =
      (fold [@inlined hint]) t ~init:M.zero ~f:(fun [@inline] n a -> M.( + ) n (f a))
      [@nontail]
    ;;

    let sum = sum_alloc

    let[@inline] fold_result_alloc ~fold_until t ~init ~f =
      (fold_until [@inlined hint])
        t
        ~init
        ~f:(fun [@inline] acc item ->
          match f acc item with
          | Result.Ok x -> Continue_or_stop.Continue x
          | Error _ as e -> Continue_or_stop.Stop e)
        ~finish:(fun [@inline] x -> Result.Ok x) [@nontail]
    ;;

    let fold_result = fold_result_alloc
  end

  let[@inline] min_elt_alloc ~fold t ~compare =
    (fold [@inlined hint]) t ~init:None ~f:(fun [@inline] acc elt ->
      match acc with
      | None -> Some elt
      | Some min -> if compare min elt > 0 then Some elt else acc)
    [@nontail]
  ;;

  let min_elt = min_elt_alloc

  let[@inline] max_elt_alloc ~fold t ~compare =
    (fold [@inlined hint]) t ~init:None ~f:(fun [@inline] acc elt ->
      match acc with
      | None -> Some elt
      | Some max -> if compare max elt < 0 then Some elt else acc)
    [@nontail]
  ;;

  let max_elt = max_elt_alloc

  let[@inline] length ~fold c =
    (fold [@inlined hint]) c ~init:0 ~f:(fun [@inline] acc _ -> acc + 1)
  ;;

  let[@inline] is_empty ~iter_until c =
    (iter_until [@inlined hint])
      c
      ~f:(fun [@inline] _ -> Continue_or_stop.Stop false)
      ~finish:(fun [@inline] () -> true)
  ;;

  let[@inline] mem ~iter_until c x ~equal =
    (iter_until [@inlined hint])
      c
      ~f:(fun [@inline] y ->
        if equal x y then Continue_or_stop.Stop true else Continue_or_stop.Continue ())
      ~finish:(fun [@inline] () -> false) [@nontail]
  ;;

  let[@inline] exists ~iter_until c ~f =
    (iter_until [@inlined hint])
      c
      ~f:(fun [@inline] x ->
        if f x then Continue_or_stop.Stop true else Continue_or_stop.Continue ())
      ~finish:(fun [@inline] () -> false) [@nontail]
  ;;

  let[@inline] for_all ~iter_until c ~f =
    (iter_until [@inlined hint])
      c
      ~f:(fun [@inline] x ->
        if f x then Continue_or_stop.Continue () else Continue_or_stop.Stop false)
      ~finish:(fun [@inline] () -> true) [@nontail]
  ;;

  let[@inline] find_map_alloc ~iter_until c ~f =
    (iter_until [@inlined hint])
      c
      ~f:(fun [@inline] x ->
        match f x with
        | None -> Continue_or_stop.Continue ()
        | Some _ as res -> Continue_or_stop.Stop res)
      ~finish:(fun [@inline] () -> None) [@nontail]
  ;;

  let find_map = find_map_alloc

  let[@inline] find_alloc ~iter_until c ~f =
    (iter_until [@inlined hint])
      c
      ~f:(fun [@inline] x ->
        if f x then Continue_or_stop.Stop (Some x) else Continue_or_stop.Continue ())
      ~finish:(fun [@inline] () -> None) [@nontail]
  ;;

  let find = find_alloc

  let[@inline] to_list ~fold c =
    List.rev ((fold [@inlined hint]) c ~init:[] ~f:(fun [@inline] acc x -> x :: acc))
  ;;

  let[@inline] to_array ~length ~iter c =
    let array = ref [||] in
    let i = ref 0 in
    (iter [@inlined hint]) c ~f:(fun [@inline] x ->
      if !i = 0 then array := Array.create ~len:((length [@inlined hint]) c) x;
      !array.(!i) <- x;
      incr i);
    !array
  ;;
end

include Derived

module Make_gen (T : Make_gen_arg) :
  Generic
  with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) T.t
   and type 'a elt := 'a T.elt = struct
  let fold_until : (_, _, _, _) fold_until = T.fold_until

  let fold =
    match T.fold with
    | `Custom fold -> fold
    | `Define_using_fold_until -> fun t ~init ~f -> fold ~fold_until t ~init ~f
  ;;

  let iter_until : (_, _, _) iter_until =
    match T.iter_until with
    | `Custom iter_until -> iter_until
    | `Define_using_fold_until -> fun t ~f ~finish -> iter_until ~fold_until t ~f ~finish
  ;;

  let iter =
    match T.iter with
    | `Custom iter -> iter
    | `Define_using_fold -> fun t ~f -> iter_via_fold ~fold t ~f
    | `Define_using_iter_until -> fun t ~f -> iter_via_iter_until ~iter_until t ~f
  ;;

  let length =
    match T.length with
    | `Custom length -> length
    | `Define_using_fold -> fun t -> length ~fold t
  ;;

  let is_empty t = is_empty ~iter_until t
  let mem t x ~equal = mem ~iter_until t x ~equal
  let sum m t ~f = sum ~fold m t ~f
  let count t ~f = count ~fold t ~f
  let exists t ~f = exists ~iter_until t ~f
  let for_all t ~f = for_all ~iter_until t ~f
  let find_map t ~f = find_map ~iter_until t ~f
  let find t ~f = find ~iter_until t ~f
  let to_list t = to_list ~fold t
  let to_array t = to_array ~length ~iter t
  let min_elt t ~compare = min_elt ~fold t ~compare
  let max_elt t ~compare = max_elt ~fold t ~compare
  let fold_result t ~init ~f = fold_result t ~fold_until ~init ~f
end

module Make (T : Make_arg) : S1 with type 'a t := 'a T.t = struct
  include Make_gen (struct
      include T

      type ('a, _, _) t = 'a T.t
      type 'a elt = 'a
    end)
end

module Make0 (T : Make0_arg) : S0 with type t := T.t and type elt := T.Elt.t = struct
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
      | Some y -> of_array [| y |])
    [@nontail]
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
