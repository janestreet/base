open! Import0

type 'a t = 'a Caml.Queue.t

let create = Caml.Queue.create
let clear = Caml.Queue.clear
let copy = Caml.Queue.copy
let is_empty = Caml.Queue.is_empty
let length = Caml.Queue.length
let peek = Caml.Queue.peek
let pop = Caml.Queue.pop
let push = Caml.Queue.push
let transfer = Caml.Queue.transfer

let iter t ~f:((f : _ -> _) [@local]) =
  let caml_iter : (('a -> unit)[@local]) -> 'a t -> unit =
    Caml.Obj.magic (Caml.Queue.iter : ('a -> unit) -> 'a t -> unit)
  in
  caml_iter f t
;;

let fold t ~init ~f:((f : _ -> _ -> _) [@local]) =
  let caml_fold : (('b -> 'a -> 'b)[@local]) -> 'b -> 'a t -> 'b =
    Caml.Obj.magic (Caml.Queue.fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b)
  in
  caml_fold f init t
;;
