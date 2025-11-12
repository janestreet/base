open! Import
include Linked_queue0

let enqueue t x = Linked_queue0.push x t
let dequeue_or_null t = if is_empty t then Null else This (Linked_queue0.pop t)
let[@inline] dequeue t = dequeue_or_null t |> Or_null.to_option
let dequeue_exn = [%eta1 Linked_queue0.pop]
let dequeue_and_ignore_exn (type elt) (t : elt t) = ignore (dequeue_exn t : elt)
let peek_or_null t = if is_empty t then Null else This (Linked_queue0.peek t)
let[@inline] peek t = peek_or_null t |> Or_null.to_option
let peek_exn = [%eta1 Linked_queue0.peek]

let drain t ~f ~while_ =
  while (not (is_empty t)) && while_ (peek_exn t) do
    f (dequeue_exn t)
  done
;;

module%template C = Indexed_container.Make [@modality portable] (struct
    type nonrec 'a t = 'a t

    let fold_until t ~init ~f ~finish = Container.fold_until ~fold ~init ~f t ~finish
    let fold = `Custom fold
    let iter_until = `Define_using_fold_until
    let iter = `Custom iter
    let length = `Custom length
    let foldi = `Define_using_fold
    let foldi_until = `Define_using_fold_until
    let iteri = `Define_using_fold
  end)

let count = C.count
let exists = C.exists
let find = C.find
let find_map = C.find_map
let fold_result = C.fold_result
let fold_until = C.fold_until
let foldi_until = C.foldi_until
let for_all = C.for_all
let iter_until = C.iter_until
let iteri_until = C.iteri_until
let max_elt = C.max_elt
let mem = C.mem
let min_elt = C.min_elt
let sum = C.sum
let to_list = C.to_list
let counti = C.counti
let existsi = C.existsi
let find_mapi = C.find_mapi
let findi = C.findi
let foldi = C.foldi
let for_alli = C.for_alli
let iteri = C.iteri
let transfer ~src ~dst = Linked_queue0.transfer src dst

let concat_map t ~f =
  let res = create () in
  iter t ~f:(fun a -> List.iter (f a) ~f:(fun b -> enqueue res b));
  res
;;

let concat_mapi t ~f =
  let res = create () in
  iteri t ~f:(fun i a -> List.iter (f i a) ~f:(fun b -> enqueue res b));
  res
;;

let filter_map t ~f =
  let res = create () in
  iter t ~f:(fun a ->
    match f a with
    | None -> ()
    | Some b -> enqueue res b);
  res
;;

let filter_mapi t ~f =
  let res = create () in
  iteri t ~f:(fun i a ->
    match f i a with
    | None -> ()
    | Some b -> enqueue res b);
  res
;;

let filter t ~f =
  let res = create () in
  iter t ~f:(fun a -> if f a then enqueue res a);
  res
;;

let filteri t ~f =
  let res = create () in
  iteri t ~f:(fun i a -> if f i a then enqueue res a);
  res
;;

let map t ~f =
  let res = create () in
  iter t ~f:(fun a -> enqueue res (f a));
  res
;;

let mapi t ~f =
  let res = create () in
  iteri t ~f:(fun i a -> enqueue res (f i a));
  res
;;

let filter_inplace q ~f =
  let q' = filter q ~f in
  clear q;
  transfer ~src:q' ~dst:q
;;

let filteri_inplace q ~f =
  let q' = filteri q ~f in
  clear q;
  transfer ~src:q' ~dst:q
;;

let enqueue_all t list = List.iter list ~f:(fun x -> enqueue t x)

let of_list list =
  let t = create () in
  List.iter list ~f:(fun x -> enqueue t x);
  t
;;

let of_array array =
  let t = create () in
  Array.iter array ~f:(fun x -> enqueue t x);
  t
;;

let init len ~f =
  let t = create () in
  for i = 0 to len - 1 do
    enqueue t (f i)
  done;
  t
;;

let to_array t =
  match length t with
  | 0 -> [||]
  | len ->
    let arr = Array.create ~len (peek_exn t) in
    let i = ref 0 in
    iter t ~f:(fun v ->
      arr.(!i) <- v;
      incr i);
    arr
;;

let t_of_sexp a_of_sexp sexp = of_list (list_of_sexp a_of_sexp sexp)
let sexp_of_t sexp_of_a t = sexp_of_list sexp_of_a (to_list t)

let t_sexp_grammar (type a) (grammar : a Sexplib0.Sexp_grammar.t)
  : a t Sexplib0.Sexp_grammar.t
  =
  Sexplib0.Sexp_grammar.coerce (List.t_sexp_grammar grammar)
;;

let singleton a =
  let t = create () in
  enqueue t a;
  t
;;
