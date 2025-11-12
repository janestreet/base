open! Import
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

module%template [@kind.explicit k = (value, value_or_null)] Derived : Derived
[@kind.explicit k] = struct
  include struct
    [@@@mode.default m = (global, local)]

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
        ~f:(fun [@inline] acc x ->
          Continue_or_stop.Continue (f acc x) [@exclave_if_stack a])
        ~finish:Fn.id [@nontail] [@exclave_if_stack a]
    [@@mode mi = m] [@@alloc a @ mo = (heap_global, stack_local)]
    ;;

    let fold = (fold_alloc [@mode mi] [@alloc heap]) [@@mode mi = m, mo = global]
    let fold = (fold_alloc [@mode mi] [@alloc stack]) [@@mode mi = m, mo = local]

    let[@inline] iter_until_alloc ~fold_until t ~f ~finish =
      (fold_until [@inlined hint])
        t
        ~init:()
        ~f:(fun [@inline] () x -> f x [@exclave_if_local mo])
        ~finish:(fun [@inline] () -> finish () [@exclave_if_local mo])
      [@nontail] [@exclave_if_stack a]
    [@@mode mi = m] [@@alloc a @ mo = (heap_global, stack_local)]
    ;;

    let iter_until = (iter_until_alloc [@mode mi] [@alloc heap])
    [@@mode mi = m, mo = global]
    ;;

    let iter_until = (iter_until_alloc [@mode mi] [@alloc stack])
    [@@mode mi = m, mo = local]
    ;;

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

    let[@inline] sum_alloc
      (type a : k)
      ~fold
      (module M : Summable with type t = a[@kind.explicit k] [@mode mo])
      t
      ~f
      =
      (fold [@inlined hint]) t ~init:M.zero ~f:(fun [@inline] n a ->
        M.( + ) n (f a) [@exclave_if_stack a])
      [@nontail] [@exclave_if_stack a]
    [@@mode mi = m] [@@alloc a @ mo = (heap_global, stack_local)]
    ;;

    let sum = (sum_alloc [@mode mi] [@alloc heap]) [@@mode mi = m, mo = global]
    let sum = (sum_alloc [@mode mi] [@alloc stack]) [@@mode mi = m, mo = local]

    let[@inline] fold_result_alloc ~fold_until t ~init ~f =
      (fold_until [@inlined hint])
        t
        ~init
        ~f:(fun [@inline] acc item ->
          match[@exclave_if_stack a] f acc item with
          | Result.Ok x -> Continue_or_stop.Continue x
          | Error _ as e -> Continue_or_stop.Stop e)
        ~finish:(fun [@inline] x -> Result.Ok x [@exclave_if_local mo])
      [@nontail] [@exclave_if_stack a]
    [@@mode mi = m] [@@alloc a @ mo = (heap_global, stack_local)]
    ;;

    let fold_result = (fold_result_alloc [@mode mi] [@alloc heap])
    [@@mode mi = m, mo = global]
    ;;

    let fold_result = (fold_result_alloc [@mode mi] [@alloc stack])
    [@@mode mi = m, mo = local]
    ;;
  end

  let%template[@inline] min_elt_alloc ~fold t ~compare =
    (fold [@inlined hint]) t ~init:None ~f:(fun [@inline] acc elt ->
      match acc with
      | None -> Some elt [@exclave_if_local m]
      | Some min -> if compare min elt > 0 then Some elt [@exclave_if_local m] else acc)
    [@nontail] [@exclave_if_stack a]
  [@@alloc a @ m = (heap_global, stack_local)]
  ;;

  let%template min_elt = (min_elt_alloc [@alloc heap]) [@@mode global]
  let%template min_elt = (min_elt_alloc [@alloc stack]) [@@mode local]

  let%template[@inline] max_elt_alloc ~fold t ~compare =
    (fold [@inlined hint]) t ~init:None ~f:(fun [@inline] acc elt ->
      match acc with
      | None -> Some elt [@exclave_if_local m]
      | Some max -> if compare max elt < 0 then Some elt [@exclave_if_local m] else acc)
    [@nontail] [@exclave_if_stack a]
  [@@alloc a @ m = (heap_global, stack_local)]
  ;;

  let%template max_elt = (max_elt_alloc [@alloc heap]) [@@mode global]
  let%template max_elt = (max_elt_alloc [@alloc stack]) [@@mode local]

  [%%template
  [@@@mode.default m = (global, local)]

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
  ;;]

  let%template[@inline] find_map_alloc ~iter_until c ~f =
    (iter_until [@inlined hint])
      c
      ~f:(fun [@inline] x ->
        match[@exclave_if_stack a] f x with
        | None -> Continue_or_stop.Continue ()
        | Some _ as res -> Continue_or_stop.Stop res)
      ~finish:(fun [@inline] () -> None) [@nontail] [@exclave_if_stack a]
  [@@mode mi = (global, local)] [@@alloc a @ mo = (heap_global, stack_local)]
  ;;

  let%template find_map = (find_map_alloc [@mode mi] [@alloc heap])
  [@@mode mi = (global, local), mo = global]
  ;;

  let%template find_map = (find_map_alloc [@mode mi] [@alloc stack])
  [@@mode mi = (global, local), mo = local]
  ;;

  let%template[@inline] find_alloc ~iter_until c ~f =
    (iter_until [@inlined hint])
      c
      ~f:(fun [@inline] x ->
        if f x
        then Continue_or_stop.Stop (Some x) [@exclave_if_local m]
        else Continue_or_stop.Continue ())
      ~finish:(fun [@inline] () -> None [@exclave_if_local m])
    [@nontail] [@exclave_if_stack a]
  [@@alloc a @ m = (heap_global, stack_local)]
  ;;

  let%template find = (find_alloc [@alloc heap]) [@@mode global]
  let%template find = (find_alloc [@alloc stack]) [@@mode local]

  let%template[@inline] to_list ~fold c =
    (List.rev [@alloc a])
      ((fold [@inlined hint]) c ~init:[] ~f:(fun [@inline] acc x ->
         x :: acc [@exclave_if_local m])) [@exclave_if_stack a]
  [@@alloc a @ m = (heap_global, stack_local)]
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

include%template Derived [@kind.explicit value]

module%template.portable Make_gen (T : Make_gen_arg [@mode m]) :
  Generic
  [@alloc a]
  with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) T.t
   and type 'a elt := 'a T.elt = struct
  let fold_until : ((_, _, _, _) fold_until[@mode mi mo]) = (T.fold_until [@mode mi mo])
  [@@mode mi = (global, m), mo = (global, m)]
  ;;

  let fold =
    match T.fold [@mode mi mo] with
    | `Custom fold -> fold
    | `Define_using_fold_until ->
      fun t ~init ~f ->
        (fold [@mode mi mo])
          ~fold_until:(fold_until [@mode mi mo])
          t
          ~init
          ~f [@exclave_if_local mo]
  [@@mode mi = (global, m), mo = (global, m)]
  ;;

  let iter_until : ((_, _, _) iter_until[@mode mi mo]) =
    match T.iter_until [@mode mi mo] with
    | `Custom iter_until -> iter_until
    | `Define_using_fold_until ->
      fun t ~f ~finish ->
        (iter_until [@mode mi mo])
          ~fold_until:(fold_until [@mode mi mo])
          t
          ~f
          ~finish [@exclave_if_local mo]
  [@@mode mi = (global, m), mo = (global, m)]
  ;;

  let iter =
    match T.iter [@mode m] with
    | `Custom iter -> iter
    | `Define_using_fold ->
      fun t ~f -> (iter_via_fold [@mode m]) ~fold:(fold [@mode m global]) t ~f
    | `Define_using_iter_until ->
      fun t ~f ->
        (iter_via_iter_until [@mode m]) ~iter_until:(iter_until [@mode m global]) t ~f
  [@@mode m = (global, m)]
  ;;

  let length =
    match T.length with
    | `Custom length -> length
    | `Define_using_fold -> fun t -> (length [@mode m]) ~fold:(fold [@mode m global]) t
  ;;

  let is_empty t = (is_empty [@mode m]) ~iter_until:(iter_until [@mode m global]) t

  let mem t x ~equal =
    (mem [@mode m]) ~iter_until:(iter_until [@mode m global]) t x ~equal
  [@@mode m = (global, m)]
  ;;

  let sum m t ~f =
    (sum [@mode mi mo]) ~fold:(fold [@mode mi mo]) m t ~f [@exclave_if_local mo]
  [@@mode mi = (global, m), mo = (global, m)]
  ;;

  let count t ~f = (count [@mode m]) ~fold:(fold [@mode m global]) t ~f
  [@@mode m = (global, m)]
  ;;

  let exists t ~f = (exists [@mode m]) ~iter_until:(iter_until [@mode m global]) t ~f
  [@@mode m = (global, m)]
  ;;

  let for_all t ~f = (for_all [@mode m]) ~iter_until:(iter_until [@mode m global]) t ~f
  [@@mode m = (global, m)]
  ;;

  let find_map t ~f =
    (find_map [@mode mi mo])
      ~iter_until:(iter_until [@mode mi mo])
      t
      ~f [@exclave_if_local mo]
  [@@mode mi = (global, m), mo = (global, m)]
  ;;

  let find t ~f =
    (find [@mode m]) ~iter_until:(iter_until [@mode m m]) t ~f [@exclave_if_local m]
  [@@mode m = (global, m)]
  ;;

  let to_list t = (to_list [@alloc a]) ~fold:(fold [@mode m m]) t [@exclave_if_stack a]
  [@@alloc a @ m = (heap_global, a @ m)]
  ;;

  let to_array t = to_array ~length ~iter t

  let min_elt t ~compare =
    (min_elt [@mode m]) ~fold:(fold [@mode m m]) t ~compare [@exclave_if_local m]
  [@@mode m = (global, m)]
  ;;

  let max_elt t ~compare =
    (max_elt [@mode m]) ~fold:(fold [@mode m m]) t ~compare [@exclave_if_local m]
  [@@mode m = (global, m)]
  ;;

  let fold_result t ~init ~f =
    (fold_result [@mode mi mo])
      t
      ~fold_until:(fold_until [@mode mi mo])
      ~init
      ~f [@exclave_if_local mo]
  [@@mode mi = (global, m), mo = (global, m)]
  ;;
end
[@@alloc a @ m = (heap_global, stack_local)]

module%template.portable [@modality p] Make (T : Make_arg [@mode m]) :
  S1 [@alloc a] with type 'a t := 'a T.t = struct
  include Make_gen [@modality p] [@alloc a] (struct
      include T

      type ('a, _, _) t = 'a T.t
      type 'a elt = 'a
    end)
end
[@@alloc a @ m = (heap_global, stack_local)]

module%template.portable [@modality p] Make0 (T : Make0_arg [@mode m]) :
  S0 [@alloc a] with type t := T.t and type elt := T.Elt.t = struct
  include Make_gen [@modality p] [@alloc a] (struct
      include T

      type ('a, _, _) t = T.t
      type 'a elt = T.Elt.t
    end)

  let mem t x = (mem [@mode m]) t x ~equal:(T.Elt.equal [@mode m])
  [@@mode m = (global, m)]
  ;;
end
[@@alloc a @ m = (heap_global, stack_local)]

module%template.portable
  [@modality p] Make_gen_with_creators
    (T : Make_gen_with_creators_arg) :
  Generic_with_creators
  with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) T.t
   and type 'a elt := 'a T.elt
   and type ('a, 'phantom1, 'phantom2) concat := ('a, 'phantom1, 'phantom2) T.concat =
struct
  include Make_gen [@modality p] (T)

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

module%template.portable [@modality p] Make_with_creators (T : Make_with_creators_arg) =
struct
  include Make_gen_with_creators [@modality p] (struct
      include T

      type ('a, _, _) t = 'a T.t
      type 'a elt = 'a
      type ('a, _, _) concat = 'a T.t

      let concat_of_array = of_array
    end)
end

module%template.portable [@modality p] Make0_with_creators (T : Make0_with_creators_arg) =
struct
  include Make_gen_with_creators [@modality p] (struct
      include T

      type ('a, _, _) t = T.t
      type 'a elt = T.Elt.t
      type ('a, _, _) concat = 'a list

      let concat_of_array = Array.to_list
    end)

  let mem t x = mem t x ~equal:T.Elt.equal
end
