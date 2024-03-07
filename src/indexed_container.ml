open! Import
module Array = Array0
include Indexed_container_intf

let with_return = With_return.with_return

let[@inline always] iteri ~fold t ~f =
  ignore
    (fold t ~init:0 ~f:(fun i x ->
       f i x;
       i + 1)
      : int)
;;

let foldi ~fold t ~init ~f =
  let i = ref 0 in
  fold t ~init ~f:(fun acc v ->
    let acc = f !i acc v in
    i := !i + 1;
    acc) [@nontail]
;;

let counti ~foldi t ~f =
  foldi t ~init:0 ~f:(fun i n a -> if f i a then n + 1 else n) [@nontail]
;;

let existsi ~iteri c ~f =
  with_return (fun r ->
    iteri c ~f:(fun i x -> if f i x then r.return true);
    false) [@nontail]
;;

let for_alli ~iteri c ~f =
  with_return (fun r ->
    iteri c ~f:(fun i x -> if not (f i x) then r.return false);
    true) [@nontail]
;;

let find_mapi ~iteri t ~f =
  with_return (fun r ->
    iteri t ~f:(fun i x ->
      match f i x with
      | None -> ()
      | Some _ as res -> r.return res);
    None) [@nontail]
;;

let findi ~iteri c ~f =
  with_return (fun r ->
    iteri c ~f:(fun i x -> if f i x then r.return (Some (i, x)));
    None) [@nontail]
;;

(* Allows [Make_gen] to share a [Container.Generic] implementation with, e.g.,
   [Container.Make_gen_with_creators]. *)
module Make_gen_with_container
  (T : Make_gen_arg)
  (C : Container.Generic
         with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) T.t
          and type 'a elt := 'a T.elt) :
  Generic
    with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) T.t
     and type 'a elt := 'a T.elt = struct
  include C

  let iteri =
    match T.iteri with
    | `Custom iteri -> iteri
    | `Define_using_fold -> fun t ~f -> iteri ~fold t ~f
  ;;

  let foldi =
    match T.foldi with
    | `Custom foldi -> foldi
    | `Define_using_fold -> fun t ~init ~f -> foldi ~fold t ~init ~f
  ;;

  let counti t ~f = counti ~foldi t ~f
  let existsi t ~f = existsi ~iteri t ~f
  let for_alli t ~f = for_alli ~iteri t ~f
  let find_mapi t ~f = find_mapi ~iteri t ~f
  let findi t ~f = findi ~iteri t ~f
end
[@@inline always]

module Make_gen (T : Make_gen_arg) :
  Generic
    with type ('a, 'phantom1, 'phantom2) t := ('a, 'phantom1, 'phantom2) T.t
     and type 'a elt := 'a T.elt = struct
  module C = Container.Make_gen (T)
  include C
  include Make_gen_with_container (T) (C)
end
[@@inline always]

module Make (T : Make_arg) = struct
  include Make_gen (struct
    include T

    type ('a, _, _) t = 'a T.t
    type 'a elt = 'a
  end)
end
[@@inline always]

module Make0 (T : Make0_arg) = struct
  include Make_gen (struct
    include T

    type (_, _, _) t = T.t
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
  module C = Container.Make_gen_with_creators (T)
  include C
  include Make_gen_with_container (T) (C)

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
      | Some y -> of_array [| y |]) [@nontail]
  ;;

  let mapi t ~f = filter_mapi t ~f:(fun i x -> Some (f i x)) [@nontail]

  let filteri t ~f =
    filter_mapi t ~f:(fun i x -> if f i x then Some x else None) [@nontail]
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

    type (_, _, _) t = T.t
    type 'a elt = T.Elt.t
    type ('a, _, _) concat = 'a list

    let concat_of_array = Array.to_list
  end)

  let mem t x = mem t x ~equal:T.Elt.equal
end
