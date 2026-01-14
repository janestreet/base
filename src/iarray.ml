open! Import
include Iarray_intf.Definitions
include Iarray0
module I = Basement.Stdlib_iarray_labels
include O

let is_empty t = length t = 0
let last_exn t = t.:(length t - 1)
let empty = Obj.magic_portable (unsafe_of_array__promise_no_mutation [||])

let[@inline] get_empty () =
  Portability_hacks.magic_uncontended__promise_deeply_immutable empty
;;

let%template get_opt t n =
  if 0 <= n && n < length t
  then
    Some ((unsafe_get [@mode c]) t n)
    [@exclave_if_stack a] (* SAFETY: bounds checked above *)
  else None
[@@mode c = (uncontended, shared, contended)] [@@alloc a = (heap, stack)]
;;

(** Local allocation *)
module%template Local0 = struct
  let last_exn t = exclave_ t.:(length t - 1)
  let init = I.init_local
  let append = I.append_local
  let iter2_exn t1 t2 ~f = I.iter2_local t1 t2 ~f
  let map t ~f = exclave_ I.map_local t ~f
  let map2_exn t1 t2 ~f = exclave_ I.map2_local t1 t2 ~f
  let sub t ~pos ~len = exclave_ I.sub_local t ~pos ~len

  let subo ?pos ?len t = exclave_
    let pos, len =
      Ordered_collection_common.get_pos_len_exn ?pos ?len () ~total_length:(length t)
    in
    sub t ~pos ~len
  ;;

  let prefix t ~len = exclave_ sub t ~pos:0 ~len
  let suffix t ~len = exclave_ sub t ~pos:(length t - len) ~len
  let drop_prefix t ~len = exclave_ sub t ~pos:len ~len:(length t - len)
  let drop_suffix t ~len = exclave_ sub t ~pos:0 ~len:(length t - len)

  let rev t = exclave_
    let len = length t in
    init len ~f:(fun pos -> exclave_ unsafe_get t (len - pos - 1))
  ;;

  let singleton x = exclave_ init 1 ~f:(fun (_ : int) -> x)

  let[@inline] create ~len x ~mutate = exclave_
    let arr = Array.create_local ~len x in
    (mutate [@inlined hint]) arr;
    unsafe_of_array__promise_no_mutation arr
  ;;

  open struct
    (* VERY UNSAFE: Any of these functions can be used to violate the "no forward
       pointers" restriction for the local stack if not used carefully. Each of these can
       either make a local mutable array or mutate its contents, and if not careful, this
       can lead to an array's contents pointing forwards. The latter three functions could
       be overloaded via [[@local_opt]], but we don't do that in order to isolate the
       unsafety. *)
    external make_mutable_local
      :  int
      -> local_ 'a
      -> local_ 'a array
      @@ portable
      = "caml_make_local_vect"

    external unsafe_set_local
      :  local_ 'a array
      -> int
      -> local_ 'a
      -> unit
      @@ portable
      = "%array_unsafe_set"

    external unsafe_blit_local
      :  src:local_ 'a t
      -> src_pos:int
      -> dst:local_ 'a array
      -> dst_pos:int
      -> len:int
      -> unit
      @@ portable
      = "caml_array_blit"
  end

  let init_with_globals len ~f = exclave_
    if len = 0
    then get_empty ()
    else if len < 0
    then Error.raise_s (Atom "Iarray.Local.init_with_globals: negative length")
    else (
      let res = Array.create_local ~len (f 0) in
      for i = 1 to len - 1 do
        Array.unsafe_set res i (f i)
      done;
      unsafe_of_array__promise_no_mutation res)
  ;;

  let of_list_rev list = exclave_
    match list with
    | [] -> get_empty ()
    | head :: tail ->
      let len = 1 + List.length tail in
      let array = make_mutable_local len head in
      (List.iteri [@mode local]) tail ~f:(fun i x ->
        unsafe_set_local array (len - i - 2) x);
      unsafe_of_array__promise_no_mutation array
  ;;

  let of_list_map list ~f = exclave_
    match list with
    | [] -> get_empty ()
    | first :: list ->
      let rec loop i list ~f ~elt_i = exclave_
        match list with
        | [] -> make_mutable_local (i + 1) elt_i
        | head :: tail ->
          let array = loop (i + 1) tail ~f ~elt_i:(f head) in
          unsafe_set_local array i elt_i;
          array
      in
      unsafe_of_array__promise_no_mutation (loop 0 list ~f ~elt_i:(f first))
  ;;

  let of_list_mapi list ~f = exclave_
    match list with
    | [] -> get_empty ()
    | first :: list ->
      let rec loop i list ~f ~elt_i = exclave_
        match list with
        | [] -> make_mutable_local (i + 1) elt_i
        | head :: tail ->
          let array = loop (i + 1) tail ~f ~elt_i:(f (i + 1) head) in
          unsafe_set_local array i elt_i;
          array
      in
      unsafe_of_array__promise_no_mutation (loop 0 list ~f ~elt_i:(f 0 first))
  ;;

  let of_list_rev_map list ~f = exclave_
    match list with
    | [] -> get_empty ()
    | first :: list ->
      let rec loop i list ~f ~first = exclave_
        match list with
        | [] -> make_mutable_local i (f first)
        | head :: tail ->
          let elt = f head in
          let array = loop (i + 1) tail ~f ~first in
          unsafe_set_local array (Array.length array - i - 1) elt;
          array
      in
      unsafe_of_array__promise_no_mutation (loop 1 list ~f ~first)
  ;;

  let rec concat_loop ~outer ~outer_pos ~outer_len ~dst ~dst_pos ~dst_len = exclave_
    if outer_pos = outer_len
    then unsafe_of_array__promise_no_mutation dst
    else (
      let inner = unsafe_get outer outer_pos in
      let inner_len = length inner in
      if inner_len = 0
      then concat_loop ~outer ~outer_pos:(outer_pos + 1) ~outer_len ~dst ~dst_pos ~dst_len
      else (
        let dst =
          if Array.length dst = 0
          then make_mutable_local dst_len (unsafe_get inner 0)
          else dst
        in
        unsafe_blit_local ~src:inner ~src_pos:0 ~dst ~dst_pos ~len:inner_len;
        concat_loop
          ~outer
          ~outer_pos:(outer_pos + 1)
          ~outer_len
          ~dst
          ~dst_pos:(dst_pos + inner_len)
          ~dst_len))
  ;;

  let sum_length ts =
    let n = length ts in
    let rec loop ts i acc =
      if i = n then acc else loop ts (i + 1) (acc + length (unsafe_get ts i))
    in
    loop ts 0 0
  ;;

  let concat outer = exclave_
    let dst_len = sum_length outer in
    concat_loop
      ~outer
      ~outer_pos:0
      ~outer_len:(length outer)
      ~dst:[||]
      ~dst_pos:0
      ~dst_len
  ;;

  let concat_map t ~f = exclave_ concat (map t ~f)

  let rec filteri_loop t ~f ~len ~pos ~to_ = exclave_
    if len = pos
    then make_mutable_local to_ (unsafe_get t (len - 1))
    else (
      let elt = unsafe_get t pos in
      if f pos elt
      then (
        let array = filteri_loop t ~f ~len ~pos:(pos + 1) ~to_:(to_ + 1) in
        unsafe_set_local array to_ elt;
        array)
      else filteri_loop t ~f ~len ~pos:(pos + 1) ~to_)
  ;;

  let filteri t ~f = exclave_
    let len = length t in
    if len = 0
    then get_empty ()
    else unsafe_of_array__promise_no_mutation (filteri_loop t ~f ~len ~pos:0 ~to_:0)
  ;;

  let make_mutable_local_optional len opt = exclave_
    match opt with
    | Some elt -> make_mutable_local len elt
    | None ->
      assert (len = 0);
      [||]
  ;;

  [%%template
  [@@@mode.default l = (global, local)]

  let rec filter_mapi_loop t ~f ~len ~pos ~to_ ~maybe_res = exclave_
    if len = pos
    then make_mutable_local_optional to_ maybe_res
    else (
      match local_ f pos (unsafe_get t pos) with
      | Some res as maybe_res ->
        let array =
          (filter_mapi_loop [@mode l]) t ~f ~len ~pos:(pos + 1) ~to_:(to_ + 1) ~maybe_res
        in
        unsafe_set_local array to_ res;
        array
      | None -> (filter_mapi_loop [@mode l]) t ~f ~len ~pos:(pos + 1) ~to_ ~maybe_res)
  ;;

  let filter_mapi t ~f = exclave_
    unsafe_of_array__promise_no_mutation
      ((filter_mapi_loop [@mode l]) t ~f ~len:(length t) ~pos:0 ~to_:0 ~maybe_res:None)
  ;;]

  let rec partitioni_tf_loop t ~f ~len ~pos ~l ~r = exclave_
    if len = pos
    then (
      let elt = unsafe_get t (len - 1) in
      make_mutable_local l elt, make_mutable_local r elt)
    else (
      let elt = unsafe_get t pos in
      let bool = f pos elt in
      let ((l_arr, r_arr) as pair) =
        partitioni_tf_loop
          t
          ~f
          ~len
          ~pos:(pos + 1)
          ~l:(l + Bool.to_int bool)
          ~r:(r + Bool.to_int (not bool))
      in
      if bool then unsafe_set_local l_arr l elt else unsafe_set_local r_arr r elt;
      pair)
  ;;

  let partitioni_tf t ~f = exclave_
    let len = length t in
    if len = 0
    then get_empty (), get_empty ()
    else (
      let l_arr, r_arr = partitioni_tf_loop t ~f ~len ~pos:0 ~l:0 ~r:0 in
      ( unsafe_of_array__promise_no_mutation l_arr
      , unsafe_of_array__promise_no_mutation r_arr ))
  ;;

  let is_some = function
    | Some _ -> true
    | None -> false
  ;;

  [%%template
  [@@@mode.default li = (global, local)]

  let rec partition_mapi_loop t ~f ~len ~pos ~l ~r ~maybe_l ~maybe_r = exclave_
    if len = pos
    then make_mutable_local_optional l maybe_l, make_mutable_local_optional r maybe_r
    else (
      match local_ (f pos (unsafe_get t pos) : _ Either.t) with
      | First res ->
        let ((arr, _) as pair) =
          (partition_mapi_loop [@mode li])
            t
            ~f
            ~len
            ~pos:(pos + 1)
            ~l:(l + 1)
            ~r
            ~maybe_l:(if is_some maybe_l then maybe_l else Some res)
            ~maybe_r
        in
        unsafe_set_local arr l res;
        pair
      | Second res ->
        let ((_, arr) as pair) =
          (partition_mapi_loop [@mode li])
            t
            ~f
            ~len
            ~pos:(pos + 1)
            ~l
            ~r:(r + 1)
            ~maybe_l
            ~maybe_r:(if is_some maybe_r then maybe_r else Some res)
        in
        unsafe_set_local arr r res;
        pair)
  ;;

  let partition_mapi t ~f = exclave_
    let len = length t in
    if len = 0
    then get_empty (), get_empty ()
    else (
      let l_arr, r_arr =
        (partition_mapi_loop [@mode li])
          t
          ~f
          ~len
          ~pos:0
          ~l:0
          ~r:0
          ~maybe_l:None
          ~maybe_r:None
      in
      ( unsafe_of_array__promise_no_mutation l_arr
      , unsafe_of_array__promise_no_mutation r_arr ))
  ;;]

  let cartesian_product t1 t2 = exclave_
    concat_map t1 ~f:(fun x1 -> exclave_ map t2 ~f:(fun x2 -> exclave_ x1, x2))
  ;;

  let rec fold_mapi_local_loop t ~acc ~f ~pos ~len ~fst_elt = exclave_
    if pos = len
    then acc, make_mutable_local len fst_elt
    else (
      let acc, elt = f pos acc (unsafe_get t pos) in
      let ((_, array) as res) =
        fold_mapi_local_loop t ~acc ~f ~pos:(pos + 1) ~len ~fst_elt
      in
      unsafe_set_local array pos elt;
      res)
  ;;

  let fold_mapi t ~init ~f = exclave_
    let len = length t in
    if len = 0
    then init, get_empty ()
    else (
      let acc, fst_elt = f 0 init (unsafe_get t 0) in
      let acc, array = fold_mapi_local_loop t ~acc ~f ~pos:1 ~len ~fst_elt in
      acc, unsafe_of_array__promise_no_mutation array)
  ;;

  let fold_map t ~init ~f = exclave_
    fold_mapi t ~init ~f:(fun _ acc x -> exclave_ f acc x) [@nontail]
  ;;

  module Let_syntax = struct
    let return = singleton

    module Let_syntax = struct
      let return = return
      let map = map
      let bind = concat_map
      let both = cartesian_product

      module Open_on_rhs = struct end
    end
  end
end

(** Constructors *)

let%template[@alloc heap] init len ~f =
  unsafe_of_array__promise_no_mutation (Array.init len ~f)
;;

let%template[@alloc stack] init = Local0.init
let singleton x = unsafe_of_array__promise_no_mutation [| x |]

let[@inline] create ~len x ~mutate =
  let arr = Array.create ~len x in
  (mutate [@inlined hint]) arr;
  unsafe_of_array__promise_no_mutation arr
;;

(** Blit operations *)

let sub t ~pos ~len =
  unsafe_of_array__promise_no_mutation
    (Array.sub (unsafe_to_array__promise_no_mutation t) ~pos ~len)
;;

let subo ?pos ?len t =
  unsafe_of_array__promise_no_mutation
    (Array.subo (unsafe_to_array__promise_no_mutation t) ?pos ?len)
;;

module Of_array = struct
  let unsafe_sub src ~pos ~len =
    if len = 0
    then get_empty ()
    else (
      let dst = Array.create ~len (Array.unsafe_get src 0) in
      Array.unsafe_blit ~src ~src_pos:pos ~dst ~dst_pos:0 ~len;
      unsafe_of_array__promise_no_mutation dst)
  ;;

  let sub array ~pos ~len =
    Ordered_collection_common.check_pos_len_exn
      ~pos
      ~len
      ~total_length:(Array.length array);
    unsafe_sub array ~pos ~len
  ;;

  let subo ?pos ?len array =
    let pos, len =
      Ordered_collection_common.get_pos_len_exn
        ?pos
        ?len
        ()
        ~total_length:(Array.length array)
    in
    unsafe_sub array ~pos ~len
  ;;
end

module To_array = struct
  let sub t ~pos ~len = Array.sub (unsafe_to_array__promise_no_mutation t) ~pos ~len
  let subo ?pos ?len t = Array.subo (unsafe_to_array__promise_no_mutation t) ?pos ?len

  let blito ~src ?src_pos ?src_len ~dst ?dst_pos () =
    let src = unsafe_to_array__promise_no_mutation src in
    Array.blito ~src ?src_pos ?src_len ~dst ?dst_pos ()
  ;;

  let blit ~src ~src_pos ~dst ~dst_pos ~len =
    let src = unsafe_to_array__promise_no_mutation src in
    Array.blit ~src ~src_pos ~dst ~dst_pos ~len
  ;;

  let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
    let src = unsafe_to_array__promise_no_mutation src in
    Array.unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len
  ;;
end

(** Conversions *)

let%template[@alloc heap] of_list = I.of_list
let%template[@alloc stack] of_list = I.of_list_local
let%template[@alloc heap] of_array = I.of_array

let%template[@alloc stack] of_array (a @ local) = exclave_
  (init [@alloc stack]) (Array.length a) ~f:(fun i -> exclave_ Array.unsafe_get a i)
;;

let of_list_rev list = unsafe_of_array__promise_no_mutation (Array.of_list_rev list)
let of_list_map list ~f = unsafe_of_array__promise_no_mutation (Array.of_list_map list ~f)

let of_list_mapi list ~f =
  unsafe_of_array__promise_no_mutation (Array.of_list_mapi list ~f)
;;

let of_list_rev_map list ~f =
  unsafe_of_array__promise_no_mutation (Array.of_list_rev_map list ~f)
;;

let of_sequence seq = Sequence.to_list_rev seq |> of_list_rev

let to_sequence t =
  let len = length t in
  Sequence.unfold_step ~init:0 ~f:(fun i ->
    if i = len then Done else Yield { value = unsafe_get t i; state = i + 1 })
;;

(** Exports for deriving *)

let globalize globalize_elt (local_ t) =
  init (length t) ~f:(fun i -> globalize_elt (get t i) [@nontail]) [@nontail]
;;

let%template compare compare_elt ta tb =
  if phys_equal ta tb
  then 0
  else (
    let na = length ta in
    let nb = length tb in
    match Int.compare na nb with
    | 0 ->
      let rec local_ loop pos =
        if pos = na
        then 0
        else (
          match compare_elt (unsafe_get ta pos) (unsafe_get tb pos) with
          | 0 -> loop (pos + 1)
          | c -> c)
      in
      loop 0 [@nontail]
    | c -> c)
[@@mode __ = (local, global)]
;;

let hash_fold_t hash_fold_elt state t =
  I.fold_left t ~init:(hash_fold_int state (length t)) ~f:hash_fold_elt
;;

(* sexp serialization is copied from that of [array] in [Sexplib0] *)

let t_of_sexp elt_of_sexp sexp =
  match (sexp : Sexp.t) with
  | Atom _ -> Sexplib0.Sexp_conv.of_sexp_error "iarray_of_sexp: list expected" sexp
  | List sexps -> of_list_map ~f:elt_of_sexp sexps
;;

let t_sexp_grammar (a_sexp_grammar : 'a Sexplib0.Sexp_grammar.t)
  : 'a t Sexplib0.Sexp_grammar.t
  =
  { untyped = List (Many a_sexp_grammar.untyped) }
;;

(** Indexed container *)

[%%template
[@@@mode.default m = (global, local)]

let iteri t ~(local_ f : _ -> _ @ m -> _) =
  for pos = 0 to length t - 1 do
    f pos (unsafe_get t pos)
  done
;;

let iter t ~f = (iteri [@mode m]) t ~f:(fun _ x -> f x) [@nontail]]

[%%template
[@@@kind.default ka = value, kacc = base_or_null]
[@@@mode.default mi = (global, local), mo = (global, local)]

let foldi (type (a : ka) (acc : kacc)) (t : a t) ~(init : acc) ~(local_ f) : acc =
  let n = length t in
  (let rec local_ loop pos acc =
     if [@exclave_if_local mo ~reasons:[ May_return_local ]] pos = n
     then acc
     else loop (pos + 1) (f pos acc (unsafe_get t pos))
   in
   loop 0 init [@nontail])
  [@exclave_if_local mo ~reasons:[ May_return_local ]]
;;

let fold t ~init ~f =
  (foldi [@inlined hint] [@kind ka kacc] [@mode mi mo])
    t
    ~init
    ~f:(fun [@inline always] _ acc x -> f acc x [@exclave_if_local mo])
  [@nontail] [@exclave_if_local mo ~reasons:[ May_return_local ]]
;;

let fold_right (type (a : ka) (acc : kacc)) (t : a t) ~(init : acc) ~f =
  (let rec local_ loop pos acc =
     let pos = pos - 1 in
     if [@exclave_if_local mo ~reasons:[ May_return_local ]] pos < 0
     then acc
     else loop pos (f (unsafe_get t pos) acc)
   in
   loop (length t) init [@nontail])
  [@exclave_if_local mo ~reasons:[ May_return_local ]]
;;]

[%%template
[@@@mode.default m = (global, local)]

let existsi t ~(local_ f : _ -> _ @ m -> _) =
  let n = length t in
  let rec local_ loop pos =
    if pos = n then false else f pos (unsafe_get t pos) || loop (pos + 1)
  in
  loop 0 [@nontail]
;;

let for_alli t ~(local_ f : _ -> _ @ m -> _) =
  let n = length t in
  let rec local_ loop pos =
    if pos = n then true else f pos (unsafe_get t pos) && loop (pos + 1)
  in
  loop 0 [@nontail]
;;

let counti t ~f =
  (foldi [@mode m global]) t ~init:0 ~f:(fun pos acc x -> acc + Bool.to_int (f pos x))
  [@nontail]
;;]

let%template find_mapi t ~f =
  let n = length t in
  (let rec local_ loop pos =
     if [@exclave_if_local mo ~reasons:[ May_return_local ]] pos = n
     then None
     else (
       match f pos (unsafe_get t pos) with
       | Some _ as some -> some
       | None -> loop (pos + 1))
   in
   loop 0 [@nontail])
  [@exclave_if_local mo ~reasons:[ May_return_local ]]
[@@mode mi = (global, local), mo = (global, local)]
;;

let%template findi t ~f =
  (find_mapi [@mode m m]) t ~f:(fun i x ->
    if [@exclave_if_local m ~reasons:[ May_return_regional; Will_return_unboxed ]] f i x
    then Some (i, x)
    else None)
  [@nontail] [@exclave_if_local m ~reasons:[ May_return_local ]]
[@@mode m = (global, local)]
;;

[%%template
[@@@mode.default mi = (global, local), mo = (global, local)]

let fold_result t ~init ~f =
  let n = length t in
  (let rec local_ loop pos acc =
     if [@exclave_if_local mo ~reasons:[ May_return_local ]] pos = n
     then Ok acc
     else (
       match f acc (unsafe_get t pos) with
       | Error _ as err -> err
       | Ok acc -> loop (pos + 1) acc)
   in
   loop 0 init [@nontail])
  [@exclave_if_local mo ~reasons:[ May_return_local ]]
;;

let foldi_until t ~init ~f ~finish =
  let n = length t in
  (let rec local_ loop pos acc =
     if [@exclave_if_local mo ~reasons:[ May_return_local ]] pos = n
     then finish pos acc
     else (
       match (f pos acc (unsafe_get t pos) : (_, _) Container.Continue_or_stop.t) with
       | Stop x -> x
       | Continue acc -> loop (pos + 1) acc)
   in
   loop 0 init [@nontail])
  [@exclave_if_local mo ~reasons:[ May_return_local ]]
;;

let fold_until t ~init ~f ~finish =
  (foldi_until [@mode mi mo])
    t
    ~init
    ~f:(fun _ acc x -> f acc x [@exclave_if_local mo])
    ~finish:(fun _ acc -> finish acc [@exclave_if_local mo])
  [@nontail] [@exclave_if_local mo ~reasons:[ May_return_local ]]
;;

let iteri_until t ~f ~finish =
  let n = length t in
  (let rec loop pos =
     if [@exclave_if_local mo ~reasons:[ May_return_local ]] pos = n
     then finish pos
     else (
       match (f pos (unsafe_get t pos) : (_, _) Container.Continue_or_stop.t) with
       | Stop x -> x
       | Continue () -> loop (pos + 1))
   in
   loop 0 [@nontail])
  [@exclave_if_local mo ~reasons:[ May_return_local ]]
;;

let iter_until t ~f ~finish =
  (iteri_until [@mode mi mo])
    t
    ~f:(fun _ x -> f x [@exclave_if_local mo])
    ~finish:(fun _ -> finish () [@exclave_if_local mo])
  [@nontail] [@exclave_if_local mo ~reasons:[ May_return_local ]]
;;]

[%%template
[@@@mode.default m = (global, local)]

let exists t ~f = (existsi [@mode m]) t ~f:(fun _ x -> f x) [@nontail]
let for_all t ~f = (for_alli [@mode m]) t ~f:(fun _ x -> f x) [@nontail]
let count t ~f = (counti [@mode m]) t ~f:(fun _ x -> f x) [@nontail]]

[%%template
  let sum (type a) (module M : Container.Summable with type t = a[@mode mo]) t ~f =
    (fold [@mode mi mo]) t ~init:M.zero ~f:(fun acc x ->
      M.( + ) acc (f x) [@exclave_if_local mo ~reasons:[ May_return_local ]])
    [@nontail] [@exclave_if_local mo ~reasons:[ May_return_local ]]
  [@@mode mi = (global, local), mo = (global, local)]
  ;;]

let%template mem t elt ~equal = (exists [@mode m]) t ~f:(fun x -> equal elt x) [@nontail]
[@@mode m = (global, local)]
;;

let%template find_map t ~f =
  (find_mapi [@mode mi mo]) t ~f:(fun _ x -> f x [@exclave_if_local mo])
  [@nontail] [@exclave_if_local mo ~reasons:[ May_return_local ]]
[@@mode mi = (global, local), mo = (global, local)]
;;

[%%template
[@@@mode.default m = (global, local)]

let find t ~f =
  (find_map [@mode m m]) t ~f:(fun x ->
    match[@exclave_if_local m ~reasons:[ May_return_regional; Will_return_unboxed ]]
      f x
    with
    | true -> Some x
    | false -> None)
  [@nontail] [@exclave_if_local m ~reasons:[ May_return_local ]]
;;

let[@inline] best_elt t ~first_is_better_than_second =
  match[@exclave_if_local m ~reasons:[ May_return_regional; Will_return_unboxed ]]
    length t
  with
  | 0 -> None
  | n ->
    let rec local_ loop pos acc =
      if [@exclave_if_local m ~reasons:[ May_return_regional; Will_return_unboxed ]] pos
                                                                                     = n
      then Some acc
      else (
        let x = unsafe_get t pos in
        let acc =
          Bool.select ((first_is_better_than_second [@inlined always]) x acc) x acc
        in
        loop (pos + 1) acc)
    in
    loop 1 (unsafe_get t 0) [@nontail]
;;

let min_elt t ~compare =
  (let local_ first_is_better_than_second x y = compare x y < 0 in
   (best_elt [@mode m]) t ~first_is_better_than_second [@nontail])
  [@exclave_if_local m ~reasons:[ May_return_regional; Will_return_unboxed ]]
;;

let max_elt t ~compare =
  (let local_ first_is_better_than_second x y = compare x y > 0 in
   (best_elt [@mode m]) t ~first_is_better_than_second [@nontail])
  [@exclave_if_local m ~reasons:[ May_return_regional; Will_return_unboxed ]]
;;]

let%template[@alloc heap] to_list = I.to_list
let%template[@alloc stack] to_list = I.to_list_local
let to_array = I.to_array

(** Invariants *)

let invariant elt_invariant t = iter t ~f:elt_invariant

(** Random element *)

let random_element_unchecked t ~random_state =
  get t (Random.State.int random_state (length t))
;;

let random_element_exn ?(random_state = Random.State.get_default ()) t =
  if is_empty t
  then Error.raise_s (Atom "Iarray.random_element_exn: empty array")
  else random_element_unchecked t ~random_state
;;

let random_element ?(random_state = Random.State.get_default ()) t =
  if is_empty t then None else Some (random_element_unchecked t ~random_state)
;;

(** Concatenation *)

let%template[@alloc heap] append = I.append
let%template[@alloc stack] append = Local0.append
let[@zero_alloc] int_m () = (module Int : Container.Summable with type t = int)
let int_m = Portability_hacks.magic_portable__needs_base_and_core int_m

let%template[@alloc heap] concat outer =
  let total_len = sum (int_m ()) outer ~f:length in
  if total_len = 0
  then get_empty ()
  else (
    let local_ pos = ref 0 in
    let local_ blit_append array inner =
      let inner_len = length inner in
      if inner_len = 0
      then array
      else (
        let dst_pos = !pos in
        let array =
          if dst_pos = 0 then Array.create ~len:total_len (unsafe_get inner 0) else array
        in
        To_array.blit ~src:inner ~src_pos:0 ~dst:array ~dst_pos ~len:inner_len;
        pos := dst_pos + inner_len;
        array)
    in
    let array = fold outer ~init:[||] ~f:blit_append in
    assert (!pos = total_len);
    unsafe_of_array__promise_no_mutation array)
;;

let%template[@alloc stack] concat = Local0.concat

(** Subsequences *)

let prefix t ~len = sub t ~pos:0 ~len
let suffix t ~len = sub t ~pos:(length t - len) ~len
let drop_prefix t ~len = sub t ~pos:len ~len:(length t - len)
let drop_suffix t ~len = sub t ~pos:0 ~len:(length t - len)

let group t ~break =
  let len = length t in
  if len = 0
  then get_empty ()
  else (
    let local_ out = Array.create_local ~len (get_empty ()) in
    let rec local_ loop ~pos ~start_pos ~out_len =
      if start_pos <> pos
         && (pos = len || break (unsafe_get t (pos - 1)) (unsafe_get t pos))
      then (
        Array.unsafe_set out out_len (sub t ~pos:start_pos ~len:(pos - start_pos));
        loop ~pos ~start_pos:pos ~out_len:(out_len + 1))
      else if pos = len
      then Of_array.sub out ~pos:0 ~len:out_len
      else loop ~pos:(pos + 1) ~start_pos ~out_len
    in
    loop ~pos:0 ~start_pos:0 ~out_len:0 [@nontail])
;;

(** Transformations *)

let%template mapi t ~f = init (length t) ~f:(fun i -> f i (unsafe_get t i)) [@nontail]
[@@mode global] [@@alloc heap]
;;

let%template mapi t ~f = I.mapi_local_input t ~f [@@mode local] [@@alloc heap]
let%template mapi t ~f = exclave_ I.mapi_local t ~f [@@mode local] [@@alloc stack]
let%template mapi t ~f = exclave_ I.mapi_local_output t ~f [@@mode global] [@@alloc stack]

let%template map t ~f = init (length t) ~f:(fun i -> f (unsafe_get t i)) [@nontail]
[@@mode global] [@@alloc heap]
;;

let%template map t ~f = I.map_local_input t ~f [@@mode local] [@@alloc heap]
let%template map t ~f = exclave_ I.map_local t ~f [@@mode local] [@@alloc stack]
let%template map t ~f = exclave_ I.map_local_output t ~f [@@mode global] [@@alloc stack]

let%template[@alloc heap] filteri t ~f =
  let len = length t in
  if len = 0
  then get_empty ()
  else (
    let local_ out = Array.create_local ~len (unsafe_get t 0) in
    let rec local_ loop pos out_len =
      if pos = len
      then if out_len = len then t else Of_array.sub out ~pos:0 ~len:out_len [@nontail]
      else (
        let x = unsafe_get t pos in
        match f pos x with
        | false -> loop (pos + 1) out_len
        | true ->
          Array.unsafe_set out out_len x;
          loop (pos + 1) (out_len + 1))
    in
    loop 0 0 [@nontail])
;;

let%template[@alloc stack] filteri = Local0.filteri

let%template filter t ~f =
  (filteri [@alloc a]) t ~f:(fun _ x -> f x) [@nontail] [@exclave_if_stack a]
[@@alloc a @ m = (heap_global, stack_local)]
;;

let%template filter_mapi (t @ mi) ~(local_ f : _ -> _ @ mi -> _) =
  let len = length t in
  let rec local_ loop pos out out_len =
    if pos = len
    then Of_array.sub out ~pos:0 ~len:out_len [@nontail]
    else (
      match f pos (unsafe_get t pos) with
      | None -> loop (pos + 1) out out_len
      | Some y ->
        if out_len = 0
        then (
          let out = Array.create_local ~len y in
          loop (pos + 1) out 1 [@nontail])
        else (
          Array.unsafe_set out out_len y;
          loop (pos + 1) out (out_len + 1)))
  in
  loop 0 [||] 0 [@nontail]
[@@mode mi = (global, local)] [@@alloc heap]
;;

let%template filter_mapi = (Local0.filter_mapi [@mode mi])
[@@mode mi = (global, local)] [@@alloc stack]
;;

[%%template
[@@@mode.default mi = (global, local)]
[@@@alloc.default a @ mo = (heap_global, stack_local)]

let filter_map t ~(local_ f : _ @ mi -> _ @ mo) =
  (filter_mapi [@mode mi] [@alloc a]) t ~f:(fun _ x -> f x [@exclave_if_stack a])
  [@nontail] [@exclave_if_stack a]
;;

let concat_map t ~f =
  (concat [@alloc a]) ((map [@mode mi] [@alloc a]) t ~f) [@exclave_if_stack a]
;;

let concat_mapi t ~f =
  (concat [@alloc a]) ((mapi [@mode mi] [@alloc a]) t ~f) [@exclave_if_stack a]
;;]

(** Partitions *)

let%template[@alloc heap] partitioni_tf t ~f =
  let len = length t in
  if len = 0
  then get_empty (), get_empty ()
  else (
    let x0 = unsafe_get t 0 in
    let fsts = Array.create_local ~len x0 in
    let snds = Array.create_local ~len x0 in
    let rec local_ loop pos fsts_len snds_len =
      if pos = len
      then Of_array.sub fsts ~pos:0 ~len:fsts_len, Of_array.sub snds ~pos:0 ~len:snds_len
      else (
        let x = unsafe_get t pos in
        match f pos x with
        | true ->
          Array.unsafe_set fsts fsts_len x;
          loop (pos + 1) (fsts_len + 1) snds_len
        | false ->
          Array.unsafe_set snds snds_len x;
          loop (pos + 1) fsts_len (snds_len + 1))
    in
    loop 0 0 0 [@nontail])
;;

let%template[@alloc stack] partitioni_tf t ~f = exclave_
  Local0.partitioni_tf t ~f [@nontail]
;;

let%template partition_tf (t @ m) ~f @ m =
  (partitioni_tf [@alloc a]) t ~f:(fun _ x -> f x) [@nontail] [@exclave_if_stack a]
[@@alloc a @ m = (heap_global, stack_local)]
;;

let%template partition_mapi t ~f =
  let len = length t in
  let rec local_ loop pos fsts fsts_len snds snds_len =
    if pos = len
    then Of_array.sub fsts ~pos:0 ~len:fsts_len, Of_array.sub snds ~pos:0 ~len:snds_len
    else (
      match (f pos (unsafe_get t pos) : _ Either.t) with
      | First x ->
        if fsts_len = 0
        then (
          let fsts = Array.create_local ~len x in
          loop (pos + 1) fsts 1 snds snds_len [@nontail])
        else (
          Array.unsafe_set fsts fsts_len x;
          loop (pos + 1) fsts (fsts_len + 1) snds snds_len)
      | Second x ->
        if snds_len = 0
        then (
          let snds = Array.create_local ~len x in
          loop (pos + 1) fsts fsts_len snds 1 [@nontail])
        else (
          Array.unsafe_set snds snds_len x;
          loop (pos + 1) fsts fsts_len snds (snds_len + 1)))
  in
  loop 0 [||] 0 [||] 0 [@nontail]
[@@mode mi = (global, local)] [@@alloc a @ mo = heap_global]
;;

let%template partition_mapi t ~f = exclave_
  (Local0.partition_mapi [@mode mi]) t ~f [@nontail]
[@@mode mi = (global, local)] [@@alloc a @ mo = stack_local]
;;

let%template partition_map t ~f =
  (partition_mapi [@mode mi] [@alloc a]) t ~f:(fun _ x -> f x [@exclave_if_stack a])
  [@nontail] [@exclave_if_stack a]
[@@mode mi = (global, local)] [@@alloc a @ mo = (heap_global, stack_local)]
;;

(** Functional update *)

let set t i v =
  if i < 0 || i >= length t then Error.raise_s (Atom "Iarray.set: index out of bounds");
  init (length t) ~f:(fun index -> if i = index then v else unsafe_get t index)
;;

let update t i ~f =
  if i < 0 || i >= length t then Error.raise_s (Atom "Iarray.update: index out of bounds");
  mapi t ~f:(fun index value -> if i = index then f value else value)
;;

(** Combining elements *)

let unsafe_reduce t ~f =
  let len = length t in
  let rec local_ loop pos acc =
    if pos = len then acc else loop (pos + 1) (f acc (unsafe_get t pos))
  in
  loop 1 (unsafe_get t 0) [@nontail]
;;

let reduce t ~f = if is_empty t then None else Some (unsafe_reduce t ~f)

let reduce_exn t ~f =
  if is_empty t
  then Error.raise_s (Atom "Iarray.reduce_exn: empty array")
  else unsafe_reduce t ~f
;;

let error_of_t t = Error.of_lazy_t (lazy (Error.of_list (to_list t)))

let combine_errors t =
  match partition_map t ~f:Result.to_either with
  | oks, errors when is_empty errors -> Ok oks
  | _, errors -> Error (error_of_t errors)
;;

let combine_errors_unit t =
  let errors =
    filter_map t ~f:(function
      | Ok () -> None
      | Error error -> Some error)
  in
  if is_empty errors then Ok () else Error (error_of_t errors)
;;

let fold_map t ~init ~f =
  let acc, array = Array.fold_map (unsafe_to_array__promise_no_mutation t) ~init ~f in
  acc, unsafe_of_array__promise_no_mutation array
;;

let fold_mapi t ~init ~f =
  let acc, array = Array.fold_mapi (unsafe_to_array__promise_no_mutation t) ~init ~f in
  acc, unsafe_of_array__promise_no_mutation array
;;

(** Multiple arrays *)

let zip t1 t2 =
  Array.zip
    (unsafe_to_array__promise_no_mutation t1)
    (unsafe_to_array__promise_no_mutation t2)
  |> Option.map ~f:unsafe_of_array__promise_no_mutation
;;

let zip_exn t1 t2 =
  Array.zip_exn
    (unsafe_to_array__promise_no_mutation t1)
    (unsafe_to_array__promise_no_mutation t2)
  |> unsafe_of_array__promise_no_mutation
;;

let unzip t =
  let fsts, snds = Array.unzip (unsafe_to_array__promise_no_mutation t) in
  unsafe_of_array__promise_no_mutation fsts, unsafe_of_array__promise_no_mutation snds
;;

let map2_exn t1 t2 ~f =
  Array.map2_exn
    (unsafe_to_array__promise_no_mutation t1)
    (unsafe_to_array__promise_no_mutation t2)
    ~f
  |> unsafe_of_array__promise_no_mutation
;;

let iter2_exn t1 t2 ~f = I.iter2 t1 t2 ~f

let cartesian_product t1 t2 =
  Array.cartesian_product
    (unsafe_to_array__promise_no_mutation t1)
    (unsafe_to_array__promise_no_mutation t2)
  |> unsafe_of_array__promise_no_mutation
;;

(** Reordering *)

let rev t =
  let n = length t in
  init n ~f:(fun pos -> unsafe_get t (n - pos - 1)) [@nontail]
;;

(* In [split_n] and [chunks_of], we can't use [empty] here because it is [contended], so
   we use
   {[
     unsafe_of_array__promise_no_mutation [||]
   ]}
   . *)

let split_n t_orig n =
  if n <= 0
  then unsafe_of_array__promise_no_mutation [||], t_orig
  else (
    let length = length t_orig in
    if n >= length
    then t_orig, unsafe_of_array__promise_no_mutation [||]
    else (
      let first = prefix t_orig ~len:n in
      let second = drop_prefix t_orig ~len:n in
      first, second))
;;

let chunks_of t ~length:chunk_length =
  if chunk_length <= 0
  then Printf.invalid_argf "Iarray.chunks_of: Expected length > 0, got %d" chunk_length ();
  let length = length t in
  if length = 0
  then unsafe_of_array__promise_no_mutation [||]
  else (
    let num_chunks = (length + chunk_length - 1) / chunk_length in
    init num_chunks ~f:(fun i ->
      let start = i * chunk_length in
      let current_chunk_length = min chunk_length (length - start) in
      sub t ~pos:start ~len:current_chunk_length))
;;

let sort t ~compare =
  Array.sorted_copy ~compare (unsafe_to_array__promise_no_mutation t)
  |> unsafe_of_array__promise_no_mutation
;;

let stable_sort t ~compare =
  let array = to_array t in
  Array.stable_sort array ~compare;
  unsafe_of_array__promise_no_mutation array
;;

let dedup_and_sort t ~compare =
  let sorted = sort t ~compare in
  filteri sorted ~f:(fun i x -> i = 0 || compare (unsafe_get sorted (i - 1)) x <> 0)
  [@nontail]
;;

let sort_and_group t ~compare =
  group (sort t ~compare) ~break:(fun a b -> compare a b <> 0) [@nontail]
;;

let is_sorted t ~compare =
  Array.is_sorted ~compare (unsafe_to_array__promise_no_mutation t)
;;

let is_sorted_strictly t ~compare =
  Array.is_sorted_strictly ~compare (unsafe_to_array__promise_no_mutation t)
;;

module%template Local = struct
  include Local0

  [%%template
  [@@@kind.default ka = value, kacc = base_or_null]

  let fold = (fold [@kind ka kacc] [@mode local local])
  let foldi = (foldi [@kind ka kacc] [@mode local local])
  let fold_right = (fold_right [@kind ka kacc] [@mode local local])]

  (* The following are exported (and thus tested) through [Iarray]'s [Container] interface *)

  let length = length
  let is_empty = is_empty
  let mem = (mem [@mode local])
  let iter = (iter [@mode local])
  let fold_result = (fold_result [@mode local local])
  let fold_until = (fold_until [@mode local local])
  let exists = (exists [@mode local])
  let for_all = (for_all [@mode local])
  let count = (count [@mode local])
  let sum = (sum [@mode local local])
  let find = (find [@mode local])
  let find_map = (find_map [@mode local local])
  let to_list = (to_list [@alloc stack])
  let min_elt = (min_elt [@mode local])
  let max_elt = (max_elt [@mode local])
  let of_list = (of_list [@alloc stack])
  let append = (append [@alloc stack])
  let concat = (concat [@alloc stack])
  let map = (map [@mode local] [@alloc stack])
  let map_to_global = (map [@mode local] [@alloc heap])
  let map_of_global = (map [@mode global] [@alloc stack])
  let filter = (filter [@alloc stack])
  let filter_map = (filter_map [@mode local] [@alloc stack])
  let concat_map = (concat_map [@mode local] [@alloc stack])
  let partition_tf = (partition_tf [@alloc stack])
  let partition_map = (partition_map [@mode local] [@alloc stack])
  let iteri = (iteri [@mode local])
  let existsi = (existsi [@mode local])
  let for_alli = (for_alli [@mode local])
  let counti = (counti [@mode local])
  let findi = (findi [@mode local])
  let find_mapi = (find_mapi [@mode local local])
  let partitioni_tf = (partitioni_tf [@alloc stack])
  let partition_mapi = (partition_mapi [@mode local] [@alloc stack])
  let init = (init [@alloc stack])
  let mapi = (mapi [@mode local] [@alloc stack])
  let mapi_to_global = (mapi [@mode local] [@alloc heap])
  let mapi_of_global = (mapi [@mode global] [@alloc stack])
  let filteri = (filteri [@alloc stack])
  let filter_mapi = (filter_mapi [@mode local] [@alloc stack])
  let concat_mapi = (concat_mapi [@mode local] [@alloc stack])
end

module Unique = struct
  let[@inline] init len ~(f : _ -> _ @ unique) : _ @ unique =
    (* SAFETY:

       - We know the elements are unique because the function [f] is annotated to return
         them
       - We know the array itself is unique because the implementation of [init] returns a
         newly allocated array.
    *)
    Obj.magic_unique (init len ~f)
  ;;

  let mapi (t @ unique) ~f =
    init (length t) ~f:(fun i ->
      f
        i
        (* SAFETY:

           - We only call this once on each index of the original array
           - We discard the original array once we're done, so no references remain
        *)
        (Obj.magic_unique (unsafe_get t i)))
    [@nontail]
  ;;

  let map t ~f = (mapi [@inlined]) t ~f:(fun [@inline] _ x -> f x) [@nontail]

  let iteri (t @ unique) ~f =
    for i = 0 to length t - 1 do
      (* SAFETY:

         - We only call this once on each index of the original array
         - We discard the original array once we're done, so no references remain
      *)
      f i (Obj.magic_unique (unsafe_get t i))
    done
  ;;

  let iter (t @ unique) ~f = (iteri [@inlined]) t ~f:(fun [@inline] _ x -> f x) [@nontail]

  let[@inline] unzip (t @ unique) : (_ t * _ t) @ unique =
    (* SAFETY: We know the returned arrays are unique because of the contract of [unzip] -
       each element of the argument array, which is unique, is split into one of the two
       returned arrays, both of which are newly allocated
    *)
    Obj.magic_unique (unzip t)
  ;;

  let[@inline] zip_exn (t1 @ unique) (t2 @ unique) : (_ * _) t @ unique =
    (* SAFETY:

       - We know the elements of the returned array are unique because of the contract of
         [zip_exn] - each element is a newly allocated tuple containing distinct elements
         from the two argument arrays, both of which are known to be unique.
       - We know the array itself is unique because it's newly allocated by [zip_exn]
    *)
    Obj.magic_unique (zip_exn t1 t2)
  ;;
end

(** Binary search *)

include%template Binary_searchable.Make1 [@mode local] [@modality portable] (struct
    type nonrec 'a t = 'a t

    let[@mode m = (global, local)] get = get
    let[@mode m = (global, local)] length = length
  end)

(** Private exports *)

module Private = struct
  module Test_unsafe_local_implementations = Local
end
