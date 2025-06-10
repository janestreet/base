open! Import
include Iarray_intf.Definitions
include Iarray0
module I = Basement.Stdlib_iarray_labels
include O

let is_empty t = length t = 0
let last_exn t = t.:(length t - 1)
let empty = Stdlib.Obj.magic_portable (unsafe_of_array__promise_no_mutation [||])

let[@inline] get_empty () =
  Portability_hacks.magic_uncontended__promise_deeply_immutable empty
;;

(** Local allocation *)
module Local = struct
  let length = length
  let is_empty = is_empty
  let last_exn t = exclave_ t.:(length t - 1)
  let of_list = I.of_list_local
  let to_list = I.to_list_local
  let init = I.init_local
  let append = I.append_local
  let for_all t ~f = I.for_all_local t ~f
  let exists t ~f = I.exists_local t ~f
  let iteri t ~f = I.iteri_local t ~f
  let iter t ~f = I.iter_local t ~f
  let iter2_exn t1 t2 ~f = I.iter2_local t1 t2 ~f
  let find t ~f = exclave_ I.find_opt_local t ~f
  let find_map t ~f = exclave_ I.find_map_local t ~f
  let mapi t ~f = exclave_ I.mapi_local t ~f
  let mapi_to_global t ~f = I.mapi_local_input t ~f
  let mapi_of_global t ~f = exclave_ I.mapi_local_output t ~f
  let map t ~f = exclave_ I.map_local t ~f
  let map_to_global t ~f = I.map_local_input t ~f
  let map_of_global t ~f = exclave_ I.map_local_output t ~f
  let map2_exn t1 t2 ~f = exclave_ I.map2_local t1 t2 ~f
  let sub t ~pos ~len = exclave_ I.sub_local t ~pos ~len

  let subo ?pos ?len t = exclave_
    let pos, len =
      Ordered_collection_common.get_pos_len_exn ?pos ?len () ~total_length:(length t)
    in
    sub t ~pos ~len
  ;;

  [%%template
  [@@@kind ka = value, kacc = (value, bits64, bits32, word, float64)]

  let rec foldi_loop t ~f ~len ~pos ~acc = exclave_
    if len = pos
    then acc
    else foldi_loop t ~f ~len ~pos:(pos + 1) ~acc:(f pos acc (unsafe_get t pos))
  ;;

  [@@@kind.default ka = ka, kacc = kacc]

  let foldi (type (a : ka) (acc : kacc)) (t : a t) ~(init : acc) ~f = exclave_
    foldi_loop t ~f ~len:(length t) ~pos:0 ~acc:init
  ;;

  let fold t ~init ~f = exclave_
    (foldi [@inlined] [@kind ka kacc])
      ~f:(fun [@inline always] _ acc a -> exclave_ f acc a)
      ~init
      t
  ;;

  let fold_right (type (a : ka) (acc : kacc)) (t : a t) ~(init : acc) ~f = exclave_
    let rec local_ loop pos acc = exclave_
      let pos = pos - 1 in
      if pos < 0 then acc else loop pos (f (unsafe_get t pos) acc)
    in
    loop (length t) init [@nontail]
  ;;]

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

  let rec existsi_loop t ~f ~len ~pos =
    if len = pos
    then false
    else if f pos (unsafe_get t pos)
    then true
    else existsi_loop t ~f ~len ~pos:(pos + 1)
  ;;

  let rec for_alli_loop t ~f ~len ~pos =
    if len = pos
    then true
    else if f pos (unsafe_get t pos)
    then for_alli_loop t ~f ~len ~pos:(pos + 1)
    else false
  ;;

  let for_alli t ~f = for_alli_loop t ~f ~len:(length t) ~pos:0
  let existsi t ~f = existsi_loop t ~f ~len:(length t) ~pos:0
  let mem t x ~equal = exists t ~f:(fun y -> equal x y) [@nontail]

  let rec fold_result_loop t ~f ~len ~pos ~acc = exclave_
    if len = pos
    then Ok acc
    else (
      match local_ f acc (unsafe_get t pos) with
      | Error _ as error -> error
      | Ok acc -> fold_result_loop t ~f ~len ~pos:(pos + 1) ~acc)
  ;;

  let fold_result t ~init ~f = exclave_
    fold_result_loop t ~f ~len:(length t) ~pos:0 ~acc:init
  ;;

  let rec fold_until_loop t ~f ~finish ~len ~pos ~acc = exclave_
    if len = pos
    then finish acc
    else (
      match local_ (f acc (unsafe_get t pos) : _ Container.Continue_or_stop.t) with
      | Stop res -> res
      | Continue acc -> fold_until_loop t ~f ~finish ~len ~pos:(pos + 1) ~acc)
  ;;

  let fold_until t ~init ~f ~finish = exclave_
    fold_until_loop t ~f ~finish ~len:(length t) ~pos:0 ~acc:init
  ;;

  let counti t ~f = foldi t ~init:0 ~f:(fun i n x -> n + Bool.to_int (f i x)) [@nontail]
  let count t ~f = counti t ~f:(fun _ x -> f x) [@nontail]

  let sum (type a) (module Summable : Container_with_local.Summable with type t = a) t ~f
    = exclave_
    fold t ~init:Summable.zero ~f:(fun sum elt -> exclave_ Summable.( + ) sum (f elt))
  ;;

  let rec findi_loop t ~f ~len ~pos = exclave_
    if len = pos
    then None
    else (
      let local_ x = unsafe_get t pos in
      if f pos x then Some (pos, x) else findi_loop t ~f ~len ~pos:(pos + 1))
  ;;

  let findi t ~f = exclave_ findi_loop t ~f ~len:(length t) ~pos:0

  let rec find_mapi_loop t ~f ~len ~pos = exclave_
    if len = pos
    then None
    else (
      match local_ f pos (unsafe_get t pos) with
      | Some _ as some -> some
      | None -> find_mapi_loop t ~f ~len ~pos:(pos + 1))
  ;;

  let find_mapi t ~f = exclave_ find_mapi_loop t ~f ~len:(length t) ~pos:0

  let rec min_elt_loop t ~compare ~len ~pos ~acc = exclave_
    if len = pos
    then acc
    else (
      let elt = unsafe_get t pos in
      let acc = Bool.select (compare elt acc < 0) elt acc in
      min_elt_loop t ~compare ~len ~pos:(pos + 1) ~acc)
  ;;

  let min_elt t ~compare = exclave_
    let len = length t in
    if len = 0
    then None
    else Some (min_elt_loop t ~compare ~len ~pos:1 ~acc:(unsafe_get t 0))
  ;;

  let rec max_elt_loop t ~compare ~len ~pos ~acc = exclave_
    if len = pos
    then acc
    else (
      let elt = unsafe_get t pos in
      let acc = Bool.select (compare elt acc > 0) elt acc in
      max_elt_loop t ~compare ~len ~pos:(pos + 1) ~acc)
  ;;

  let max_elt t ~compare = exclave_
    let len = length t in
    if len = 0
    then None
    else Some (max_elt_loop t ~compare ~len ~pos:1 ~acc:(unsafe_get t 0))
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

  let iteri_list_local
    : type a. local_ a list -> f:local_ (int -> local_ a -> unit) -> unit
    =
    fun list ~f ->
    let rec loop i list ~f =
      match list with
      | [] -> ()
      | head :: tail ->
        f i head;
        loop (i + 1) tail ~f
    in
    loop 0 list ~f
  ;;

  let of_list_rev list = exclave_
    match list with
    | [] -> get_empty ()
    | head :: tail ->
      let len = 1 + List.length tail in
      let array = make_mutable_local len head in
      iteri_list_local tail ~f:(fun i x -> unsafe_set_local array (len - i - 2) x);
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

  let[@zero_alloc] int_m () =
    (module Int : Container_with_local.Summable with type t = int)
  ;;

  let int_m = Portability_hacks.magic_portable__needs_base_and_core int_m

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

  let concat outer = exclave_
    let dst_len = sum (int_m ()) outer ~f:length in
    concat_loop
      ~outer
      ~outer_pos:0
      ~outer_len:(length outer)
      ~dst:[||]
      ~dst_pos:0
      ~dst_len
  ;;

  let concat_map t ~f = exclave_ concat (map t ~f)
  let concat_mapi t ~f = exclave_ concat (mapi t ~f)

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

  let filter t ~f = exclave_ filteri t ~f:(fun _ x -> f x)

  let make_mutable_local_optional len opt = exclave_
    match opt with
    | Some elt -> make_mutable_local len elt
    | None ->
      assert (len = 0);
      [||]
  ;;

  let rec filter_mapi_loop t ~f ~len ~pos ~to_ ~maybe_res = exclave_
    if len = pos
    then make_mutable_local_optional to_ maybe_res
    else (
      match local_ f pos (unsafe_get t pos) with
      | Some res as maybe_res ->
        let array = filter_mapi_loop t ~f ~len ~pos:(pos + 1) ~to_:(to_ + 1) ~maybe_res in
        unsafe_set_local array to_ res;
        array
      | None -> filter_mapi_loop t ~f ~len ~pos:(pos + 1) ~to_ ~maybe_res)
  ;;

  let filter_mapi t ~f = exclave_
    unsafe_of_array__promise_no_mutation
      (filter_mapi_loop t ~f ~len:(length t) ~pos:0 ~to_:0 ~maybe_res:None)
  ;;

  let filter_map t ~f = exclave_ filter_mapi t ~f:(fun _ x -> exclave_ f x)

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

  let partition_tf t ~f = exclave_ partitioni_tf t ~f:(fun _ x -> exclave_ f x)

  let is_some = function
    | Some _ -> true
    | None -> false
  ;;

  let rec partition_mapi_loop t ~f ~len ~pos ~l ~r ~maybe_l ~maybe_r = exclave_
    if len = pos
    then make_mutable_local_optional l maybe_l, make_mutable_local_optional r maybe_r
    else (
      match local_ (f pos (unsafe_get t pos) : _ Either.t) with
      | First res ->
        let ((arr, _) as pair) =
          partition_mapi_loop
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
          partition_mapi_loop
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
        partition_mapi_loop t ~f ~len ~pos:0 ~l:0 ~r:0 ~maybe_l:None ~maybe_r:None
      in
      ( unsafe_of_array__promise_no_mutation l_arr
      , unsafe_of_array__promise_no_mutation r_arr ))
  ;;

  let partition_map t ~f = exclave_ partition_mapi t ~f:(fun _ x -> exclave_ f x)

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

let init len ~f = unsafe_of_array__promise_no_mutation (Array.init len ~f)
let%template[@alloc stack] init = Local.init
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

let of_array = I.of_array
let of_list = I.of_list
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

let compare__local compare_elt ta tb =
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
;;

let equal__local equal_elt ta tb =
  if phys_equal ta tb
  then true
  else (
    let na = length ta in
    let nb = length tb in
    match Int.equal na nb with
    | false -> false
    | true ->
      let rec local_ loop pos =
        if pos = na
        then true
        else equal_elt (unsafe_get ta pos) (unsafe_get tb pos) && loop (pos + 1)
      in
      loop 0 [@nontail])
;;

let compare compare_elt ta tb =
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
;;

let equal equal_elt ta tb =
  if phys_equal ta tb
  then true
  else (
    let na = length ta in
    let nb = length tb in
    match Int.equal na nb with
    | false -> false
    | true ->
      let rec local_ loop pos =
        if pos = na
        then true
        else equal_elt (unsafe_get ta pos) (unsafe_get tb pos) && loop (pos + 1)
      in
      loop 0 [@nontail])
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

let sexp_of_t sexp_of__a ar =
  let lst_ref = ref [] in
  for i = length ar - 1 downto 0 do
    lst_ref := sexp_of__a ar.:(i) :: !lst_ref
  done;
  Sexp.List !lst_ref
;;

let sexp_of_t__local sexp_of__a ar = exclave_
  let rec loop i acc = exclave_
    if i < 0 then Sexp.List acc else loop (i - 1) (sexp_of__a (get ar i) :: acc)
  in
  loop (length ar - 1) []
;;

let t_sexp_grammar (a_sexp_grammar : 'a Sexplib0.Sexp_grammar.t)
  : 'a t Sexplib0.Sexp_grammar.t
  =
  { untyped = List (Many a_sexp_grammar.untyped) }
;;

(** Indexed container *)

let iteri t ~(local_ f : _ -> _ -> _) =
  for pos = 0 to length t - 1 do
    f pos (unsafe_get t pos)
  done
;;

[%%template
[@@@kind.default ka = value, kacc = (value, bits64, bits32, word, float64)]

let foldi (type (a : ka) (acc : kacc)) (t : a t) ~(init : acc) ~(local_ f) : acc =
  let n = length t in
  let rec local_ loop pos acc =
    if pos = n then acc else loop (pos + 1) (f pos acc (unsafe_get t pos))
  in
  loop 0 init [@nontail]
;;

let fold t ~init ~f =
  (foldi [@inlined] [@kind ka kacc]) t ~init ~f:(fun [@inline always] _ acc x -> f acc x)
  [@nontail]
;;

let fold_right (type (a : ka) (acc : kacc)) (t : a t) ~(init : acc) ~f =
  let rec local_ loop pos acc =
    let pos = pos - 1 in
    if pos < 0 then acc else loop pos (f (unsafe_get t pos) acc)
  in
  loop (length t) init [@nontail]
;;]

let existsi t ~(local_ f : _ -> _ -> _) =
  let n = length t in
  let rec local_ loop pos =
    if pos = n then false else f pos (unsafe_get t pos) || loop (pos + 1)
  in
  loop 0 [@nontail]
;;

let for_alli t ~(local_ f : _ -> _ -> _) =
  let n = length t in
  let rec local_ loop pos =
    if pos = n then true else f pos (unsafe_get t pos) && loop (pos + 1)
  in
  loop 0 [@nontail]
;;

let counti t ~f =
  foldi t ~init:0 ~f:(fun pos acc x -> acc + Bool.to_int (f pos x)) [@nontail]
;;

let find_mapi t ~f =
  let n = length t in
  let rec local_ loop pos =
    if pos = n
    then None
    else (
      match f pos (unsafe_get t pos) with
      | Some _ as some -> some
      | None -> loop (pos + 1))
  in
  loop 0 [@nontail]
;;

let findi t ~f =
  find_mapi t ~f:(fun i x -> if f i x then Some (i, x) else None) [@nontail]
;;

let fold_result t ~init ~f =
  let n = length t in
  let rec local_ loop pos acc =
    if pos = n
    then Ok acc
    else (
      match f acc (unsafe_get t pos) with
      | Error _ as err -> err
      | Ok acc -> loop (pos + 1) acc)
  in
  loop 0 init [@nontail]
;;

let fold_until t ~init ~f ~finish =
  let n = length t in
  let rec local_ loop pos acc =
    if pos = n
    then finish acc
    else (
      match (f acc (unsafe_get t pos) : (_, _) Container.Continue_or_stop.t) with
      | Stop x -> x
      | Continue acc -> loop (pos + 1) acc)
  in
  loop 0 init [@nontail]
;;

let iter t ~f = iteri t ~f:(fun _ x -> f x) [@nontail]
let exists t ~f = existsi t ~f:(fun _ x -> f x) [@nontail]
let for_all t ~f = for_alli t ~f:(fun _ x -> f x) [@nontail]
let count t ~f = counti t ~f:(fun _ x -> f x) [@nontail]

let sum (type a) (module M : Container.Summable with type t = a) t ~f =
  fold t ~init:M.zero ~f:(fun acc x -> M.( + ) acc (f x)) [@nontail]
;;

let mem t elt ~equal = exists t ~f:(fun x -> equal elt x) [@nontail]
let find_map t ~f = find_mapi t ~f:(fun _ x -> f x) [@nontail]
let find t ~f = find_map t ~f:(fun x -> Option.some_if (f x) x) [@nontail]

let[@inline] best_elt t ~first_is_better_than_second =
  match length t with
  | 0 -> None
  | n ->
    let rec local_ loop pos acc =
      if pos = n
      then Some acc
      else (
        let x = unsafe_get t pos in
        let acc =
          Bool.select ((first_is_better_than_second [@inlined hint]) x acc) x acc
        in
        loop (pos + 1) acc)
    in
    loop 1 (unsafe_get t 0) [@nontail]
;;

let min_elt t ~compare =
  best_elt t ~first_is_better_than_second:(fun x y -> compare x y < 0) [@nontail]
;;

let max_elt t ~compare =
  best_elt t ~first_is_better_than_second:(fun x y -> compare x y > 0) [@nontail]
;;

let to_list = I.to_list
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

let append = I.append
let[@zero_alloc] int_m () = (module Int : Container.Summable with type t = int)
let int_m = Portability_hacks.magic_portable__needs_base_and_core int_m

let concat outer =
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

let mapi t ~f = init (length t) ~f:(fun i -> f i (unsafe_get t i)) [@nontail]
let map t ~f = init (length t) ~f:(fun i -> f (unsafe_get t i)) [@nontail]

let filteri t ~f =
  let len = length t in
  if len = 0
  then get_empty ()
  else (
    let local_ out = Array.create_local ~len (unsafe_get t 0) in
    let rec local_ loop pos out_len =
      if pos = len
      then Of_array.sub out ~pos:0 ~len:out_len [@nontail]
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

let filter t ~f = filteri t ~f:(fun _ x -> f x) [@nontail]

let filter_mapi t ~f =
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
;;

let filter_map t ~f = filter_mapi t ~f:(fun _ x -> f x) [@nontail]
let concat_map t ~f = concat (map t ~f)
let concat_mapi t ~f = concat (mapi t ~f)

(** Partitions *)

let partitioni_tf t ~f =
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

let partition_tf t ~f = partitioni_tf t ~f:(fun _ x -> f x) [@nontail]

let partition_mapi t ~f =
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
;;

let partition_map t ~f = partition_mapi t ~f:(fun _ x -> f x) [@nontail]

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
