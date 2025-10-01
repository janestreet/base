open! Import
open Container.Export
module Array = Array0
module List = List1

module Step = struct
  (* 'a is an item in the sequence, 's is the state that will produce the remainder of
     the sequence *)
  type (+'a : any, 's) t =
    | Done
    | Skip of { state : 's }
    | Yield :
        ('a : value) 's.
        { value : 'a
        ; state : 's
        }
        -> ('a, 's) t
  [@@deriving sexp_of ~stackify]
end

open Step

module T = struct
  (* 'a is an item in the sequence, 's is the state that will produce the remainder of the
     sequence *)
  type (+_ : any) t =
    | Sequence :
        ('a : any) 's.
        { global_ state : 's
        ; global_ next : 's -> ('a, 's) Step.t
        }
        -> 'a t
end

include T

let globalize _ (Sequence { state; next }) = Sequence { state; next }

module Expert = struct
  module View = T

  let view t = t

  let next_step (Sequence { state = s; next = f }) =
    match f s with
    | Done -> Done
    | Skip { state = s } -> Skip { state = Sequence { state = s; next = f } }
    | Yield { value = a; state = s } ->
      Yield { value = a; state = Sequence { state = s; next = f } }
  ;;

  let delayed_fold_step s ~init ~f ~finish =
    let rec loop s next finish f acc =
      match next s with
      | Done -> finish acc
      | Skip { state = s } -> f acc None ~k:(loop s next finish f)
      | Yield { value = a; state = s } -> f acc (Some a) ~k:(loop s next finish f)
    in
    match s with
    | Sequence { state = s; next } -> loop s next finish f init
  ;;
end

let unfold_step ~init ~f = Sequence { state = init; next = f }

let unfold ~init ~f =
  unfold_step ~init ~f:(fun s ->
    match f s with
    | None -> Step.Done
    | Some (a, s) -> Step.Yield { value = a; state = s })
;;

let unfold_with s ~init ~f =
  match s with
  | Sequence { state = s; next } ->
    Sequence
      { state = init, s
      ; next =
          (fun (seed, s) ->
            match next s with
            | Done -> Done
            | Skip { state = s } -> Skip { state = seed, s }
            | Yield { value = a; state = s } ->
              (match f seed a with
               | Done -> Done
               | Skip { state = seed } -> Skip { state = seed, s }
               | Yield { value = a; state = seed } -> Yield { value = a; state = seed, s }))
      }
;;

let unfold_with_and_finish s ~init ~running_step ~inner_finished ~finishing_step =
  match s with
  | Sequence { state = s; next } ->
    Sequence
      { state = `Inner_running (init, s)
      ; next =
          (fun state ->
            match state with
            | `Inner_running (state, inner_state) ->
              (match next inner_state with
               | Done -> Skip { state = `Inner_finished (inner_finished state) }
               | Skip { state = inner_state } ->
                 Skip { state = `Inner_running (state, inner_state) }
               | Yield { value = x; state = inner_state } ->
                 (match running_step state x with
                  | Done -> Done
                  | Skip { state } -> Skip { state = `Inner_running (state, inner_state) }
                  | Yield { value = y; state } ->
                    Yield { value = y; state = `Inner_running (state, inner_state) }))
            | `Inner_finished state ->
              (match finishing_step state with
               | Done -> Done
               | Skip { state } -> Skip { state = `Inner_finished state }
               | Yield { value = y; state } ->
                 Yield { value = y; state = `Inner_finished state }))
      }
;;

let of_list l =
  unfold_step ~init:l ~f:(function
    | [] -> Done
    | x :: l -> Yield { value = x; state = l })
;;

let fold t ~init ~f =
  let rec loop seed v next f =
    match next seed with
    | Done -> v
    | Skip { state = s } -> loop s v next f
    | Yield { value = a; state = s } -> loop s (f v a) next f
  in
  match t with
  | Sequence { state = seed; next } -> loop seed init next f
;;

let to_list_rev t = fold t ~init:[] ~f:(fun l x -> x :: l)

let to_list (Sequence { state = s; next }) =
  let[@tail_mod_cons] rec to_list s next =
    match next s with
    | Done -> []
    | Skip { state = s } -> (to_list [@tailcall]) s next
    | Yield { value = a; state = s } -> a :: (to_list [@tailcall]) s next
  in
  to_list s next
;;

let sexp_of_t sexp_of_a t = sexp_of_list sexp_of_a (to_list t)

let range ?(stride = 1) ?(start = `inclusive) ?(stop = `exclusive) start_v stop_v =
  let step =
    match stop with
    | `inclusive when stride >= 0 ->
      fun i -> if i > stop_v then Done else Yield { value = i; state = i + stride }
    | `inclusive ->
      fun i -> if i < stop_v then Done else Yield { value = i; state = i + stride }
    | `exclusive when stride >= 0 ->
      fun i -> if i >= stop_v then Done else Yield { value = i; state = i + stride }
    | `exclusive ->
      fun i -> if i <= stop_v then Done else Yield { value = i; state = i + stride }
  in
  let init =
    match start with
    | `inclusive -> start_v
    | `exclusive -> start_v + stride
  in
  unfold_step ~init ~f:step
;;

let of_lazy t_lazy =
  unfold_step ~init:t_lazy ~f:(fun t_lazy ->
    let (Sequence { state = s; next }) = Lazy.force t_lazy in
    match next s with
    | Done -> Done
    | Skip { state = s } ->
      Skip
        { state =
            (let v = Sequence { state = s; next } in
             lazy v)
        }
    | Yield { value = x; state = s } ->
      Yield
        { value = x
        ; state =
            (let v = Sequence { state = s; next } in
             lazy v)
        })
;;

let map t ~f =
  match t with
  | Sequence { state = seed; next } ->
    Sequence
      { state = seed
      ; next =
          (fun seed ->
            match next seed with
            | Done -> Done
            | Skip { state = s } -> Skip { state = s }
            | Yield { value = a; state = s } -> Yield { value = f a; state = s })
      }
;;

let mapi t ~f =
  match t with
  | Sequence { state = s; next } ->
    Sequence
      { state = 0, s
      ; next =
          (fun (i, s) ->
            match next s with
            | Done -> Done
            | Skip { state = s } -> Skip { state = i, s }
            | Yield { value = a; state = s } -> Yield { value = f i a; state = i + 1, s })
      }
;;

let folding_map t ~init ~f =
  unfold_with t ~init ~f:(fun acc x ->
    let acc, x = f acc x in
    Yield { value = x; state = acc })
;;

let folding_mapi t ~init ~f =
  unfold_with t ~init:(0, init) ~f:(fun (i, acc) x ->
    let acc, x = f i acc x in
    Yield { value = x; state = i + 1, acc })
;;

let filter t ~f =
  match t with
  | Sequence { state = seed; next } ->
    Sequence
      { state = seed
      ; next =
          (fun seed ->
            match next seed with
            | Done -> Done
            | Skip { state = s } -> Skip { state = s }
            | Yield { value = a; state = s } when f a -> Yield { value = a; state = s }
            | Yield { value = _; state = s } -> Skip { state = s })
      }
;;

let filteri t ~f =
  map ~f:snd (filter (mapi t ~f:(fun i s -> i, s)) ~f:(fun (i, s) -> f i s))
;;

let length t =
  let rec loop i s next =
    match next s with
    | Done -> i
    | Skip { state = s } -> loop i s next
    | Yield { value = _; state = s } -> loop (i + 1) s next
  in
  match t with
  | Sequence { state = seed; next } -> loop 0 seed next
;;

let to_list_rev_with_length t = fold t ~init:([], 0) ~f:(fun (l, i) x -> x :: l, i + 1)

let to_array t =
  let l, len = to_list_rev_with_length t in
  match l with
  | [] -> [||]
  | x :: l ->
    let a = Array.create ~len x in
    let rec loop i l =
      match l with
      | [] -> assert (i = -1)
      | x :: l ->
        a.(i) <- x;
        loop (i - 1) l
    in
    loop (len - 2) l;
    a
;;

let find t ~f =
  let rec loop s next f =
    match next s with
    | Done -> None
    | Yield { value = a; state = _ } when f a -> Some a
    | Yield { value = _; state = s } | Skip { state = s } -> loop s next f
  in
  match t with
  | Sequence { state = seed; next } -> loop seed next f
;;

let find_map t ~f =
  let rec loop s next f =
    match next s with
    | Done -> None
    | Yield { value = a; state = s } ->
      (match f a with
       | None -> loop s next f
       | some_b -> some_b)
    | Skip { state = s } -> loop s next f
  in
  match t with
  | Sequence { state = seed; next } -> loop seed next f
;;

let find_mapi t ~f =
  let rec loop s next f i =
    match next s with
    | Done -> None
    | Yield { value = a; state = s } ->
      (match f i a with
       | None -> loop s next f (i + 1)
       | some_b -> some_b)
    | Skip { state = s } -> loop s next f i
  in
  match t with
  | Sequence { state = seed; next } -> loop seed next f 0
;;

let for_all t ~f =
  let rec loop s next f =
    match next s with
    | Done -> true
    | Yield { value = a; state = _ } when not (f a) -> false
    | Yield { value = _; state = s } | Skip { state = s } -> loop s next f
  in
  match t with
  | Sequence { state = seed; next } -> loop seed next f
;;

let for_alli t ~f =
  let rec loop s next f i =
    match next s with
    | Done -> true
    | Yield { value = a; state = _ } when not (f i a) -> false
    | Yield { value = _; state = s } -> loop s next f (i + 1)
    | Skip { state = s } -> loop s next f i
  in
  match t with
  | Sequence { state = seed; next } -> loop seed next f 0
;;

let exists t ~f =
  let rec loop s next f =
    match next s with
    | Done -> false
    | Yield { value = a; state = _ } when f a -> true
    | Yield { value = _; state = s } | Skip { state = s } -> loop s next f
  in
  match t with
  | Sequence { state = seed; next } -> loop seed next f
;;

let existsi t ~f =
  let rec loop s next f i =
    match next s with
    | Done -> false
    | Yield { value = a; state = _ } when f i a -> true
    | Yield { value = _; state = s } -> loop s next f (i + 1)
    | Skip { state = s } -> loop s next f i
  in
  match t with
  | Sequence { state = seed; next } -> loop seed next f 0
;;

let iter t ~f =
  let rec loop seed next f =
    match next seed with
    | Done -> ()
    | Skip { state = s } -> loop s next f
    | Yield { value = a; state = s } ->
      f a;
      loop s next f
  in
  match t with
  | Sequence { state = seed; next } -> loop seed next f
;;

let is_empty t =
  let rec loop s next =
    match next s with
    | Done -> true
    | Skip { state = s } -> loop s next
    | Yield _ -> false
  in
  match t with
  | Sequence { state = seed; next } -> loop seed next
;;

let mem t a ~equal =
  let rec loop s next a =
    match next s with
    | Done -> false
    | Yield { value = b; state = _ } when equal a b -> true
    | Yield { value = _; state = s } | Skip { state = s } -> loop s next a
  in
  match t with
  | Sequence { state = seed; next } -> loop seed next a [@nontail]
;;

let empty : ('a : any). 'a t = Sequence { state = (); next = (fun () -> Done) }
let[@inline] get_empty () = Sequence { state = (); next = (fun () -> Done) }

let bind t ~f =
  unfold_step
    ~f:(function
      | Sequence { state = seed; next }, rest ->
        (match next seed with
         | Done ->
           (match rest with
            | Sequence { state = seed; next } ->
              (match next seed with
               | Done -> Done
               | Skip { state = s } ->
                 Skip { state = get_empty (), Sequence { state = s; next } }
               | Yield { value = a; state = s } ->
                 Skip { state = f a, Sequence { state = s; next } }))
         | Skip { state = s } -> Skip { state = Sequence { state = s; next }, rest }
         | Yield { value = a; state = s } ->
           Yield { value = a; state = Sequence { state = s; next }, rest }))
    ~init:(get_empty (), t)
;;

let return x =
  unfold_step ~init:(Some x) ~f:(function
    | None -> Done
    | Some x -> Yield { value = x; state = None })
;;

include%template Monad.Make [@modality portable] (struct
    type nonrec 'a t = 'a t

    let map = `Custom map
    let bind = bind
    let return = return
  end)

let nth s n =
  if n < 0
  then None
  else (
    let rec loop i s next =
      match next s with
      | Done -> None
      | Skip { state = s } -> loop i s next
      | Yield { value = a; state = s } ->
        if phys_equal i 0 then Some a else loop (i - 1) s next
    in
    match s with
    | Sequence { state = s; next } -> loop n s next)
;;

let nth_exn s n =
  if n < 0
  then invalid_arg "Sequence.nth"
  else (
    match nth s n with
    | None -> failwith "Sequence.nth"
    | Some x -> x)
;;

module Merge_with_duplicates_element = struct
  type ('a, 'b) t =
    | Left of 'a
    | Right of 'b
    | Both of 'a * 'b
  [@@deriving compare ~localize, equal ~localize, hash, sexp ~stackify, sexp_grammar]
end

let merge_with_duplicates
  (Sequence { state = s1; next = next1 })
  (Sequence { state = s2; next = next2 })
  ~compare
  =
  let unshadowed_compare = compare in
  let open Merge_with_duplicates_element in
  let next = function
    | Skip { state = s1 }, s2 -> Skip { state = next1 s1, s2 }
    | s1, Skip { state = s2 } -> Skip { state = s1, next2 s2 }
    | (Yield { value = a; state = s1' } as s1), (Yield { value = b; state = s2' } as s2)
      ->
      let comparison = unshadowed_compare a b in
      if comparison < 0
      then Yield { value = Left a; state = Skip { state = s1' }, s2 }
      else if comparison = 0
      then
        Yield { value = Both (a, b); state = Skip { state = s1' }, Skip { state = s2' } }
      else Yield { value = Right b; state = s1, Skip { state = s2' } }
    | Done, Done -> Done
    | Yield { value = a; state = s1 }, Done ->
      Yield { value = Left a; state = Skip { state = s1 }, Done }
    | Done, Yield { value = b; state = s2 } ->
      Yield { value = Right b; state = Done, Skip { state = s2 } }
  in
  Sequence { state = Skip { state = s1 }, Skip { state = s2 }; next }
;;

let merge_deduped_and_sorted s1 s2 ~compare =
  map (merge_with_duplicates s1 s2 ~compare) ~f:(function
    | Left x | Right x | Both (x, _) -> x)
;;

let merge_sorted
  (Sequence { state = s1; next = next1 })
  (Sequence { state = s2; next = next2 })
  ~compare
  =
  let next = function
    | Skip { state = s1 }, s2 -> Skip { state = next1 s1, s2 }
    | s1, Skip { state = s2 } -> Skip { state = s1, next2 s2 }
    | (Yield { value = a; state = s1' } as s1), (Yield { value = b; state = s2' } as s2)
      ->
      let comparison = compare a b in
      if comparison <= 0
      then Yield { value = a; state = Skip { state = s1' }, s2 }
      else Yield { value = b; state = s1, Skip { state = s2' } }
    | Done, Done -> Done
    | Yield { value = a; state = s1 }, Done ->
      Yield { value = a; state = Skip { state = s1 }, Done }
    | Done, Yield { value = b; state = s2 } ->
      Yield { value = b; state = Done, Skip { state = s2 } }
  in
  Sequence { state = Skip { state = s1 }, Skip { state = s2 }; next }
;;

let hd s =
  let rec loop s next =
    match next s with
    | Done -> None
    | Skip { state = s } -> loop s next
    | Yield { value = a; state = _ } -> Some a
  in
  match s with
  | Sequence { state = s; next } -> loop s next
;;

let hd_exn s =
  match hd s with
  | None -> failwith "hd_exn"
  | Some a -> a
;;

let tl s =
  let rec loop s next =
    match next s with
    | Done -> None
    | Skip { state = s } -> loop s next
    | Yield { value = _; state = a } -> Some a
  in
  match s with
  | Sequence { state = s; next } ->
    (match loop s next with
     | None -> None
     | Some s -> Some (Sequence { state = s; next }))
;;

let tl_eagerly_exn s =
  match tl s with
  | None -> failwith "Sequence.tl_exn"
  | Some s -> s
;;

let lift_identity next s =
  match next s with
  | Done -> Done
  | Skip { state = s } -> Skip { state = `Identity s }
  | Yield { value = a; state = s } -> Yield { value = a; state = `Identity s }
;;

let next s =
  let rec loop s next =
    match next s with
    | Done -> None
    | Skip { state = s } -> loop s next
    | Yield { value = a; state = s } -> Some (a, Sequence { state = s; next })
  in
  match s with
  | Sequence { state = s; next } -> loop s next
;;

let filter_opt s =
  match s with
  | Sequence { state = s; next } ->
    Sequence
      { state = s
      ; next =
          (fun s ->
            match next s with
            | Done -> Done
            | Skip { state = s } -> Skip { state = s }
            | Yield { value = None; state = s } -> Skip { state = s }
            | Yield { value = Some a; state = s } -> Yield { value = a; state = s })
      }
;;

let filter_map s ~f = filter_opt (map s ~f)
let filter_mapi s ~f = filter_map (mapi s ~f:(fun i s -> i, s)) ~f:(fun (i, s) -> f i s)

let split_n s n =
  let rec loop s i accum next =
    if i <= 0
    then List.rev accum, Sequence { state = s; next }
    else (
      match next s with
      | Done -> List.rev accum, get_empty ()
      | Skip { state = s } -> loop s i accum next
      | Yield { value = a; state = s } -> loop s (i - 1) (a :: accum) next)
  in
  match s with
  | Sequence { state = s; next } -> loop s n [] next
;;

let chunks_exn t n =
  if n <= 0
  then invalid_arg "Sequence.chunks_exn"
  else
    unfold_step ~init:t ~f:(fun t ->
      match split_n t n with
      | [], _empty -> Done
      | (_ :: _ as xs), t -> Yield { value = xs; state = t })
;;

let findi t ~f =
  let rec loop s next i f =
    match next s with
    | Done -> None
    | Yield { value = a; state = _ } when f i a -> Some (i, a)
    | Yield { value = _; state = s } -> loop s next (i + 1) f
    | Skip { state = s } -> loop s next i f
  in
  match t with
  | Sequence { state = seed; next } -> loop seed next 0 f
;;

let find_exn s ~f =
  match find s ~f with
  | None -> failwith "Sequence.find_exn"
  | Some x -> x
;;

let append s1 s2 =
  match s1, s2 with
  | Sequence { state = s1; next = next1 }, Sequence { state = s2; next = next2 } ->
    Sequence
      { state = `First_list s1
      ; next =
          (function
            | `First_list s1 ->
              (match next1 s1 with
               | Done -> Skip { state = `Second_list s2 }
               | Skip { state = s1 } -> Skip { state = `First_list s1 }
               | Yield { value = a; state = s1 } ->
                 Yield { value = a; state = `First_list s1 })
            | `Second_list s2 ->
              (match next2 s2 with
               | Done -> Done
               | Skip { state = s2 } -> Skip { state = `Second_list s2 }
               | Yield { value = a; state = s2 } ->
                 Yield { value = a; state = `Second_list s2 }))
      }
;;

let concat_map s ~f = bind s ~f
let concat s = concat_map s ~f:Fn.id
let concat_mapi s ~f = concat_map (mapi s ~f:(fun i s -> i, s)) ~f:(fun (i, s) -> f i s)

let zip (Sequence { state = s1; next = next1 }) (Sequence { state = s2; next = next2 }) =
  let next = function
    | Yield { value = a; state = s1 }, Yield { value = b; state = s2 } ->
      Yield { value = a, b; state = Skip { state = s1 }, Skip { state = s2 } }
    | Done, _ | _, Done -> Done
    | Skip { state = s1 }, s2 -> Skip { state = next1 s1, s2 }
    | s1, Skip { state = s2 } -> Skip { state = s1, next2 s2 }
  in
  Sequence { state = Skip { state = s1 }, Skip { state = s2 }; next }
;;

let zip_full
  (Sequence { state = s1; next = next1 })
  (Sequence { state = s2; next = next2 })
  =
  let next = function
    | Yield { value = a; state = s1 }, Yield { value = b; state = s2 } ->
      Yield { value = `Both (a, b); state = Skip { state = s1 }, Skip { state = s2 } }
    | Done, Done -> Done
    | Skip { state = s1 }, s2 -> Skip { state = next1 s1, s2 }
    | s1, Skip { state = s2 } -> Skip { state = s1, next2 s2 }
    | Done, Yield { value = b; state = s2 } ->
      Yield { value = `Right b; state = Done, next2 s2 }
    | Yield { value = a; state = s1 }, Done ->
      Yield { value = `Left a; state = next1 s1, Done }
  in
  Sequence { state = Skip { state = s1 }, Skip { state = s2 }; next }
;;

let bounded_length (Sequence { state = seed; next }) ~at_most =
  let rec loop i seed next =
    if i > at_most
    then `Greater
    else (
      match next seed with
      | Done -> `Is i
      | Skip { state = seed } -> loop i seed next
      | Yield { value = _; state = seed } -> loop (i + 1) seed next)
  in
  loop 0 seed next
;;

let length_is_bounded_by ?(min = -1) ?max t =
  let length_is_at_least (Sequence { state = s; next }) =
    let rec loop s acc =
      if acc >= min
      then true
      else (
        match next s with
        | Done -> false
        | Skip { state = s } -> loop s acc
        | Yield { value = _; state = s } -> loop s (acc + 1))
    in
    loop s 0
  in
  match max with
  | None -> length_is_at_least t
  | Some max ->
    (match bounded_length t ~at_most:max with
     | `Is len when len >= min -> true
     | _ -> false)
;;

let iteri s ~f = iter (mapi s ~f:(fun i s -> i, s)) ~f:(fun (i, s) -> f i s) [@nontail]

let foldi s ~init ~f =
  fold ~init (mapi s ~f:(fun i s -> i, s)) ~f:(fun acc (i, s) -> f i acc s) [@nontail]
;;

let reduce s ~f =
  match next s with
  | None -> None
  | Some (a, s) -> Some (fold s ~init:a ~f)
;;

let reduce_exn s ~f =
  match reduce s ~f with
  | None -> failwith "Sequence.reduce_exn"
  | Some res -> res
;;

let group (Sequence { state = s; next }) ~break =
  unfold_step
    ~init:(Some ([], s))
    ~f:(function
      | None -> Done
      | Some (acc, s) ->
        (match acc, next s with
         | _, Skip { state = s } -> Skip { state = Some (acc, s) }
         | [], Done -> Done
         | acc, Done -> Yield { value = List.rev acc; state = None }
         | [], Yield { value = cur; state = s } -> Skip { state = Some ([ cur ], s) }
         | (prev :: _ as acc), Yield { value = cur; state = s } ->
           if break prev cur
           then Yield { value = List.rev acc; state = Some ([ cur ], s) }
           else Skip { state = Some (cur :: acc, s) }))
;;

let find_consecutive_duplicate (Sequence { state = s; next }) ~equal =
  let rec loop last_elt s =
    match next s with
    | Done -> None
    | Skip { state = s } -> loop last_elt s
    | Yield { value = a; state = s } ->
      (match last_elt with
       | Some b when equal a b -> Some (b, a)
       | None | Some _ -> loop (Some a) s)
  in
  loop None s [@nontail]
;;

let remove_consecutive_duplicates s ~equal =
  unfold_with s ~init:None ~f:(fun prev a ->
    match prev with
    | Some b when equal a b -> Skip { state = Some a }
    | None | Some _ -> Yield { value = a; state = Some a })
;;

let count s ~f = fold s ~init:0 ~f:(fun acc elt -> acc + Bool.to_int (f elt)) [@nontail]

let counti t ~f =
  foldi t ~init:0 ~f:(fun i acc elt -> acc + Bool.to_int (f i elt)) [@nontail]
;;

let sum m t ~f = Container.sum ~fold m t ~f
let min_elt t ~compare = Container.min_elt ~fold t ~compare
let max_elt t ~compare = Container.max_elt ~fold t ~compare

let init n ~f =
  unfold_step ~init:0 ~f:(fun i ->
    if i >= n then Done else Yield { value = f i; state = i + 1 })
;;

let sub s ~pos ~len =
  if pos < 0 || len < 0 then failwith "Sequence.sub";
  match s with
  | Sequence { state = s; next } ->
    Sequence
      { state = 0, s
      ; next =
          (fun (i, s) ->
            if i - pos >= len
            then Done
            else (
              match next s with
              | Done -> Done
              | Skip { state = s } -> Skip { state = i, s }
              | Yield { value = a; state = s } when i >= pos ->
                Yield { value = a; state = i + 1, s }
              | Yield { value = _; state = s } -> Skip { state = i + 1, s }))
      }
;;

let take s len =
  if len < 0 then failwith "Sequence.take";
  match s with
  | Sequence { state = s; next } ->
    Sequence
      { state = 0, s
      ; next =
          (fun (i, s) ->
            if i >= len
            then Done
            else (
              match next s with
              | Done -> Done
              | Skip { state = s } -> Skip { state = i, s }
              | Yield { value = a; state = s } -> Yield { value = a; state = i + 1, s }))
      }
;;

let drop s len =
  if len < 0 then failwith "Sequence.drop";
  match s with
  | Sequence { state = s; next } ->
    Sequence
      { state = 0, s
      ; next =
          (fun (i, s) ->
            match next s with
            | Done -> Done
            | Skip { state = s } -> Skip { state = i, s }
            | Yield { value = a; state = s } when i >= len ->
              Yield { value = a; state = i + 1, s }
            | Yield { value = _; state = s } -> Skip { state = i + 1, s })
      }
;;

let take_while s ~f =
  match s with
  | Sequence { state = s; next } ->
    Sequence
      { state = s
      ; next =
          (fun s ->
            match next s with
            | Done -> Done
            | Skip { state = s } -> Skip { state = s }
            | Yield { value = a; state = s } when f a -> Yield { value = a; state = s }
            | Yield { value = _; state = _ } -> Done)
      }
;;

let drop_while s ~f =
  match s with
  | Sequence { state = s; next } ->
    Sequence
      { state = `Dropping s
      ; next =
          (function
            | `Dropping s ->
              (match next s with
               | Done -> Done
               | Skip { state = s } -> Skip { state = `Dropping s }
               | Yield { value = a; state = s } when f a -> Skip { state = `Dropping s }
               | Yield { value = a; state = s } ->
                 Yield { value = a; state = `Identity s })
            | `Identity s -> lift_identity next s)
      }
;;

let shift_right s x =
  match s with
  | Sequence { state = seed; next } ->
    Sequence
      { state = `Consing (seed, x)
      ; next =
          (function
            | `Consing (seed, x) -> Yield { value = x; state = `Identity seed }
            | `Identity s -> lift_identity next s)
      }
;;

let shift_right_with_list s l = append (of_list l) s
let shift_left = drop

module Infix = struct
  let ( @ ) = append
end

let intersperse s ~sep =
  match s with
  | Sequence { state = s; next } ->
    Sequence
      { state = `Init s
      ; next =
          (function
            | `Init s ->
              (match next s with
               | Done -> Done
               | Skip { state = s } -> Skip { state = `Init s }
               | Yield { value = a; state = s } -> Yield { value = a; state = `Running s })
            | `Running s ->
              (match next s with
               | Done -> Done
               | Skip { state = s } -> Skip { state = `Running s }
               | Yield { value = a; state = s } ->
                 Yield { value = sep; state = `Putting (a, s) })
            | `Putting (a, s) -> Yield { value = a; state = `Running s })
      }
;;

let repeat x = unfold_step ~init:x ~f:(fun x -> Yield { value = x; state = x })

let cycle_list_exn xs =
  if List.is_empty xs then invalid_arg "Sequence.cycle_list_exn";
  let s = of_list xs in
  concat_map ~f:(fun () -> s) (repeat ())
;;

let cartesian_product sa sb = concat_map sa ~f:(fun a -> zip (repeat a) sb)
let singleton x = return x

let delayed_fold s ~init ~f ~finish =
  Expert.delayed_fold_step s ~init ~finish ~f:(fun acc option ~k ->
    match option with
    | None -> k acc
    | Some a -> f acc a ~k)
;;

let fold_m ~bind ~return t ~init ~f =
  Expert.delayed_fold_step
    t
    ~init
    ~f:(fun acc option ~k ->
      match option with
      | None -> bind (return acc) ~f:k
      | Some a -> bind (f acc a) ~f:k)
    ~finish:return
;;

let iter_m ~bind ~return t ~f =
  Expert.delayed_fold_step
    t
    ~init:()
    ~f:(fun () option ~k ->
      match option with
      | None -> bind (return ()) ~f:k
      | Some a -> bind (f a) ~f:k)
    ~finish:return
;;

let fold_until s ~init ~f ~finish =
  let rec loop s next f acc =
    match next s with
    | Done -> finish acc
    | Skip { state = s } -> loop s next f acc
    | Yield { value = a; state = s } ->
      (match (f acc a : ('a, 'b) Continue_or_stop.t) with
       | Stop x -> x
       | Continue acc -> loop s next f acc)
  in
  match s with
  | Sequence { state = s; next } -> loop s next f init [@nontail]
;;

let fold_result s ~init ~f =
  let rec loop s next f acc =
    match next s with
    | Done -> Result.return acc
    | Skip { state = s } -> loop s next f acc
    | Yield { value = a; state = s } ->
      (match (f acc a : (_, _) Result.t) with
       | Error _ as e -> e
       | Ok acc -> loop s next f acc)
  in
  match s with
  | Sequence { state = s; next } -> loop s next f init
;;

let foldi_until s ~init ~f ~finish =
  let rec loop s next f i acc =
    match next s with
    | Done -> finish i acc
    | Skip { state = s } -> loop s next f i acc
    | Yield { value = a; state = s } ->
      (match (f i acc a : ('a, 'b) Continue_or_stop.t) with
       | Stop x -> x
       | Continue acc -> loop s next f (i + 1) acc)
  in
  match s with
  | Sequence { state = s; next } -> loop s next f 0 init [@nontail]
;;

let iter_until s ~f ~finish =
  let rec loop s next f =
    match next s with
    | Done -> finish ()
    | Skip { state = s } -> loop s next f
    | Yield { value = a; state = s } ->
      (match (f a : ('a, 'b) Continue_or_stop.t) with
       | Stop x -> x
       | Continue () -> loop s next f)
  in
  match s with
  | Sequence { state = s; next } -> loop s next f [@nontail]
;;

let iteri_until s ~f ~finish =
  let rec loop s next f i =
    match next s with
    | Done -> finish i
    | Skip { state = s } -> loop s next f i
    | Yield { value = a; state = s } ->
      (match (f i a : ('a, 'b) Continue_or_stop.t) with
       | Stop x -> x
       | Continue () -> loop s next f (i + 1))
  in
  match s with
  | Sequence { state = s; next } -> loop s next f 0 [@nontail]
;;

let force_eagerly t = of_list (to_list t)

let memoize (type a) (Sequence { state = s; next }) =
  let module M = struct
    type t = T of (a, t) Step.t Lazy.t
  end
  in
  let rec memoize s = M.T (lazy (find_step s))
  and find_step s =
    match next s with
    | Done -> Done
    | Skip { state = s } -> find_step s
    | Yield { value = a; state = s } -> Yield { value = a; state = memoize s }
  in
  Sequence { state = memoize s; next = (fun (M.T l) -> Lazy.force l) }
;;

let drop_eagerly s len =
  let rec loop i ~len s next =
    if i >= len
    then Sequence { state = s; next }
    else (
      match next s with
      | Done -> get_empty ()
      | Skip { state = s } -> loop i ~len s next
      | Yield { value = _; state = s } -> loop (i + 1) ~len s next)
  in
  match s with
  | Sequence { state = s; next } -> loop 0 ~len s next
;;

let drop_while_option (Sequence { state = s; next }) ~f =
  let rec loop s =
    match next s with
    | Done -> None
    | Skip { state = s } -> loop s
    | Yield { value = x; state = s } ->
      if f x then loop s else Some (x, Sequence { state = s; next })
  in
  loop s [@nontail]
;;

let rec skip_loop s next =
  match next s with
  | Skip { state } -> skip_loop state next
  | (Done | Yield _) as next -> next
;;

let compare compare_a (Sequence l) (Sequence r) =
  let rec loop compare_a s_l next_l s_r next_r =
    match skip_loop s_l next_l, skip_loop s_r next_r with
    | Done, Done -> 0
    | Done, Yield _ -> -1
    | Yield _, Done -> 1
    | Yield l, Yield r ->
      let c = compare_a l.value r.value in
      if c <> 0 then c else loop compare_a l.state next_l r.state next_r
    | Skip _, _ | _, Skip _ -> failwith "Bug: This branch should be unreachable"
  in
  loop compare_a l.state l.next r.state r.next
;;

let compare__local compare_a__local t1 t2 =
  compare (fun x y -> compare_a__local x y) (globalize () t1) (globalize () t2)
;;

let equal equal_a t1 t2 =
  for_all (zip_full t1 t2) ~f:(function
    | `Both (a1, a2) -> equal_a a1 a2
    | `Left _ | `Right _ -> false)
;;

let equal__local equal_a__local t1 t2 =
  equal (fun x y -> equal_a__local x y) (globalize () t1) (globalize () t2)
;;

let round_robin list =
  let next (todo_stack, done_stack) =
    match todo_stack with
    | Sequence { state = s; next = f } :: todo_stack ->
      (match f s with
       | Yield { value = x; state = s } ->
         Yield
           { value = x
           ; state = todo_stack, Sequence { state = s; next = f } :: done_stack
           }
       | Skip { state = s } ->
         Skip { state = Sequence { state = s; next = f } :: todo_stack, done_stack }
       | Done -> Skip { state = todo_stack, done_stack })
    | [] ->
      if List.is_empty done_stack then Done else Skip { state = List.rev done_stack, [] }
  in
  let state = list, [] in
  Sequence { state; next }
;;

let interleave (Sequence { state = s1; next = f1 }) =
  let next (todo_stack, done_stack, s1) =
    match todo_stack with
    | Sequence { state = s2; next = f2 } :: todo_stack ->
      (match f2 s2 with
       | Yield { value = x; state = s2 } ->
         Yield
           { value = x
           ; state = todo_stack, Sequence { state = s2; next = f2 } :: done_stack, s1
           }
       | Skip { state = s2 } ->
         Skip { state = todo_stack, Sequence { state = s2; next = f2 } :: done_stack, s1 }
       | Done -> Skip { state = todo_stack, done_stack, s1 })
    | [] ->
      (match f1 s1, done_stack with
       | Yield { value = t; state = s1 }, _ ->
         Skip { state = List.rev (t :: done_stack), [], s1 }
       | Skip { state = s1 }, _ -> Skip { state = List.rev done_stack, [], s1 }
       | Done, _ :: _ -> Skip { state = List.rev done_stack, [], s1 }
       | Done, [] -> Done)
  in
  let state = [], [], s1 in
  Sequence { state; next }
;;

let interleaved_cartesian_product s1 s2 =
  map s1 ~f:(fun x1 -> map s2 ~f:(fun x2 -> x1, x2)) |> interleave
;;

let of_seq (seq : _ Stdlib.Seq.t) =
  unfold_step ~init:seq ~f:(fun seq ->
    match seq () with
    | Nil -> Done
    | Cons (hd, tl) -> Yield { value = hd; state = tl })
;;

let to_seq (Sequence { state; next }) =
  let rec loop state =
    match next state with
    | Done -> Stdlib.Seq.Nil
    | Skip { state } -> loop state
    | Yield { value = hd; state } -> Stdlib.Seq.Cons (hd, fun () -> loop state)
  in
  fun () -> loop state
;;

module Generator = struct
  type 'elt steps = Wrap of ('elt, unit -> 'elt steps) Step.t

  let unwrap (Wrap step) = step

  module T = struct
    type ('a, 'elt) t = ('a -> 'elt steps) -> 'elt steps

    let return x k = k x

    let bind m ~f k =
      m (fun a ->
        let m' = f a in
        m' k)
    ;;

    let map m ~f k = m (fun a -> k (f a))
    let map = `Custom map
  end

  include T

  include%template Monad.Make2 [@modality portable] (T)

  let yield e k = Wrap (Yield { value = e; state = k })
  let to_steps t = t (fun () -> Wrap Done)

  let of_sequence sequence =
    delayed_fold
      sequence
      ~init:()
      ~f:(fun () x ~k f -> Wrap (Yield { value = x; state = (fun () -> k () f) }))
      ~finish:return
  ;;

  let run t =
    let init () = to_steps t in
    let f thunk = unwrap (thunk ()) in
    unfold_step ~init ~f
  ;;
end
