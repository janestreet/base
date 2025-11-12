open! Import
module Array = Array0
open Container.Export
include Indexed_container_intf.Definitions

module%template [@kind.explicit k = (value, value_or_null)] Derived : Derived
[@kind.explicit k] = struct
  let[@inline] foldi_alloc ~fold t ~init ~f =
    let i = ref 0 in
    (fold [@inlined hint]) t ~init ~f:(fun [@inline] acc v ->
      let idx = !i in
      i := idx + 1;
      f idx acc v [@exclave_if_local mo])
    [@nontail] [@exclave_if_stack a]
  [@@mode mi = (global, local)] [@@alloc a @ mo = (heap_global, stack_local)]
  ;;

  let foldi = (foldi_alloc [@mode mi] [@alloc heap])
  [@@mode mi = (global, local), mo = global]
  ;;

  let foldi = (foldi_alloc [@mode mi] [@alloc stack])
  [@@mode mi = (global, local), mo = local]
  ;;

  let foldi_until ~fold_until t ~init ~f ~finish =
    let i = ref 0 in
    (fold_until [@inlined hint])
      t
      ~init
      ~f:(fun [@inline] acc v ->
        let idx = !i in
        i := idx + 1;
        f idx acc v [@exclave_if_local mo])
      ~finish:(fun [@inline] acc ->
        let i = !i in
        finish i acc [@exclave_if_local mo]) [@nontail] [@exclave_if_stack a]
  [@@mode mi = (global, local)] [@@alloc a @ mo = (heap_global, stack_local)]
  ;;

  let foldi_until = (foldi_until [@mode mi] [@alloc stack])
  [@@mode mi = (global, local), mo = local]
  ;;

  let foldi_until = (foldi_until [@mode mi] [@alloc heap])
  [@@mode mi = (global, local), mo = global]
  ;;

  let[@inline] iteri_until_alloc ~foldi_until t ~f ~finish =
    (foldi_until [@inlined hint])
      t
      ~init:()
      ~f:(fun [@inline] i () x -> f i x [@exclave_if_stack a])
      ~finish:(fun [@inline] i () -> finish i [@exclave_if_stack a])
    [@nontail] [@exclave_if_stack a]
  [@@mode mi = (global, local)] [@@alloc a @ mo = (heap_global, stack_local)]
  ;;

  let iteri_until = (iteri_until_alloc [@mode mi] [@alloc heap])
  [@@mode mi = (global, local), mo = global]
  ;;

  let iteri_until = (iteri_until_alloc [@mode mi] [@alloc stack])
  [@@mode mi = (global, local), mo = local]
  ;;

  include struct
    [@@@mode.default m = (global, local)]

    let[@inline] iteri ~fold t ~f =
      ignore
        ((fold [@inlined hint]) t ~init:0 ~f:(fun [@inline] i x ->
           f i x;
           i + 1)
         : int)
    ;;

    let[@inline] counti ~foldi t ~f =
      (foldi [@inlined hint]) t ~init:0 ~f:(fun [@inline] i n a ->
        if f i a then n + 1 else n)
      [@nontail]
    ;;

    let[@inline] existsi ~iteri_until c ~f =
      (iteri_until [@inlined hint])
        c
        ~f:(fun [@inline] i x ->
          if f i x then Continue_or_stop.Stop true else Continue_or_stop.Continue ())
        ~finish:(fun [@inline] _ -> false) [@nontail]
    ;;

    let[@inline] for_alli ~iteri_until c ~f =
      (iteri_until [@inlined hint])
        c
        ~f:(fun [@inline] i x ->
          if f i x then Continue_or_stop.Continue () else Continue_or_stop.Stop false)
        ~finish:(fun [@inline] _ -> true) [@nontail]
    ;;
  end

  let[@inline] findi_alloc ~iteri_until c ~f =
    (iteri_until [@inlined hint])
      c
      ~f:(fun [@inline] i x ->
        if f i x
        then Continue_or_stop.Stop (Some (i, x)) [@exclave_if_local m]
        else Continue_or_stop.Continue () [@exclave_if_local m])
      ~finish:(fun [@inline] _ -> None [@exclave_if_local m])
    [@nontail] [@exclave_if_stack a]
  [@@alloc a @ m = (heap_global, stack_local)]
  ;;

  let findi = (findi_alloc [@alloc heap]) [@@mode m = global]
  let findi = (findi_alloc [@alloc stack]) [@@mode m = local]

  let[@inline] find_mapi_alloc ~iteri_until t ~f =
    (iteri_until [@inlined hint])
      t
      ~f:(fun [@inline] i x ->
        match[@exclave_if_stack a] f i x with
        | None -> Continue_or_stop.Continue ()
        | Some _ as res -> Continue_or_stop.Stop res)
      ~finish:(fun [@inline] _ -> None [@exclave_if_local mo])
    [@nontail] [@exclave_if_stack a]
  [@@mode mi = (global, local)] [@@alloc a @ mo = (heap_global, stack_local)]
  ;;

  let find_mapi = (find_mapi_alloc [@mode mi] [@alloc heap])
  [@@mode mi = (global, local), mo = global]
  ;;

  let find_mapi = (find_mapi_alloc [@mode mi] [@alloc stack])
  [@@mode mi = (global, local), mo = local]
  ;;
end

include%template Derived [@kind.explicit value]

(* Allows [Make_gen] to share a [Container.Generic] implementation with, e.g.,
   [Container.Make_gen_with_creators]. *)
module%template.portable Make_gen_with_container
    (T : Make_gen_arg
  [@mode m])
    (C : Container.Generic
         [@alloc a]
         with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) T.t
          and type 'a elt := 'a T.elt) :
  Generic
  [@alloc a]
  with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) T.t
   and type 'a elt := 'a T.elt = struct
  include C

  let iteri =
    match T.iteri [@mode m] with
    | `Custom iteri -> iteri
    | `Define_using_fold ->
      fun t ~f -> (iteri [@mode m]) ~fold:(fold [@mode m global]) t ~f [@nontail]
  [@@mode m = (global, m)]
  ;;

  let foldi =
    match T.foldi [@mode mi mo] with
    | `Custom foldi -> foldi
    | `Define_using_fold ->
      fun t ~init ~f ->
        (foldi [@mode mi mo]) ~fold:(fold [@mode mi mo]) t ~init ~f [@exclave_if_local mo]
  [@@mode mi = (global, m), mo = (global, m)]
  ;;

  let foldi_until t ~init ~f ~finish =
    (foldi_until [@mode mi mo])
      ~fold_until:(fold_until [@mode mi mo])
      t
      ~init
      ~f
      ~finish [@exclave_if_local mo]
  [@@mode mi = (global, m), mo = (global, m)]
  ;;

  let iteri_until t ~f ~finish =
    (iteri_until [@mode mi mo])
      ~foldi_until:(foldi_until [@mode mi mo])
      t
      ~f
      ~finish [@exclave_if_local mo]
  [@@mode mi = (global, m), mo = (global, m)]
  ;;

  let counti t ~f = (counti [@mode m]) ~foldi:(foldi [@mode m global]) t ~f
  [@@mode m = (global, m)]
  ;;

  let existsi t ~f = (existsi [@mode m]) ~iteri_until:(iteri_until [@mode m global]) t ~f
  [@@mode m = (global, m)]
  ;;

  let for_alli t ~f =
    (for_alli [@mode m]) ~iteri_until:(iteri_until [@mode m global]) t ~f
  [@@mode m = (global, m)]
  ;;

  let find_mapi t ~f =
    (find_mapi [@mode mi mo])
      ~iteri_until:(iteri_until [@mode mi mo])
      t
      ~f [@exclave_if_local mo]
  [@@mode mi = (global, m), mo = (global, m)]
  ;;

  let findi t ~f =
    (findi [@mode m]) ~iteri_until:(iteri_until [@mode m m]) t ~f [@exclave_if_local m]
  [@@mode m = (global, m)]
  ;;
end
[@@alloc a @ m = (heap_global, stack_local)] [@@inline always]

module%template.portable [@modality p] Make_gen (T : Make_gen_arg [@mode m]) :
  Generic
  [@alloc a]
  with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) T.t
   and type 'a elt := 'a T.elt = struct
  module C = Container.Make_gen [@modality p] [@alloc a] (T)
  include C
  include Make_gen_with_container [@modality p] [@alloc a] (T) (C)
end
[@@alloc a @ m = (heap_global, stack_local)] [@@inline always]

module%template.portable [@modality p] Make (T : Make_arg [@mode m]) = struct
  include Make_gen [@modality p] [@alloc a] (struct
      include T

      type ('a, _, _) t = 'a T.t
      type 'a elt = 'a
    end)

  let to_array t = Container.to_array ~length ~iter t
end
[@@alloc a @ m = (heap_global, stack_local)] [@@inline always]

module%template.portable [@modality p] Make0 (T : Make0_arg [@mode m]) = struct
  include Make_gen [@modality p] [@alloc a] (struct
      include T

      type (_, _, _) t = T.t
      type 'a elt = T.Elt.t
    end)

  let mem t x = (mem [@mode m]) t x ~equal:(T.Elt.equal [@mode m])
  [@@mode m = (global, m)]
  ;;

  let to_array t = Container.to_array ~length ~iter t
end
[@@alloc a @ m = (heap_global, stack_local)]

module%template.portable
  [@modality m] Make_gen_with_creators
    (T : Make_gen_with_creators_arg) :
  Generic_with_creators
  with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) T.t
   and type 'a elt := 'a T.elt
   and type ('a, 'phantom1, 'phantom2) concat := ('a, 'phantom1, 'phantom2) T.concat =
struct
  module C = Container.Make_gen_with_creators [@modality m] (T)
  include C
  include Make_gen_with_container [@modality m] (T) (C)

  let of_array = T.of_array
  let to_array t = Container.to_array ~length ~iter t
  let derived_init n ~f = of_array (Array.init n ~f)

  let init =
    match T.init with
    | `Custom init -> init
    | `Define_using_of_array -> derived_init
  ;;

  let derived_concat_mapi t ~f = concat (T.concat_of_array (Array.mapi (to_array t) ~f))

  let concat_mapi =
    match T.concat_mapi with
    | `Custom concat_mapi -> concat_mapi
    | `Define_using_concat -> derived_concat_mapi
  ;;

  let filter_mapi t ~f =
    concat_mapi t ~f:(fun i x ->
      match f i x with
      | None -> of_array [||]
      | Some y -> of_array [| y |])
    [@nontail]
  ;;

  let mapi t ~f = filter_mapi t ~f:(fun i x -> Some (f i x)) [@nontail]

  let filteri t ~f =
    filter_mapi t ~f:(fun i x -> if f i x then Some x else None) [@nontail]
  ;;

  let partition_mapi t ~f =
    let array = Array.mapi (to_array t) ~f in
    let xs =
      Array.fold_right array ~init:[] ~f:(fun either acc ->
        match (either : _ Either0.t) with
        | First x -> x :: acc
        | Second _ -> acc)
    in
    let ys =
      Array.fold_right array ~init:[] ~f:(fun either acc ->
        match (either : _ Either0.t) with
        | First _ -> acc
        | Second x -> x :: acc)
    in
    of_list xs, of_list ys
  ;;

  let partitioni_tf t ~f =
    partition_mapi t ~f:(fun i x -> if f i x then First x else Second x) [@nontail]
  ;;
end

module%template.portable [@modality m] Make_with_creators (T : Make_with_creators_arg) =
struct
  include Make_gen_with_creators [@modality m] (struct
      include T

      type ('a, _, _) t = 'a T.t
      type 'a elt = 'a
      type ('a, _, _) concat = 'a T.t

      let concat_of_array = of_array
    end)

  let to_array t = Container.to_array ~length ~iter t
  let of_array = T.of_array
end

module%template.portable [@modality m] Make0_with_creators (T : Make0_with_creators_arg) =
struct
  include Make_gen_with_creators [@modality m] (struct
      include T

      type (_, _, _) t = T.t
      type 'a elt = T.Elt.t
      type ('a, _, _) concat = 'a list

      let concat_of_array = Array.to_list
    end)

  let mem t x = mem t x ~equal:T.Elt.equal
  let to_array t = Container.to_array ~length ~iter t
  let of_array = T.of_array
end
