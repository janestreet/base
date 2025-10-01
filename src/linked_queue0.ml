open! Import0

type 'a t = 'a Stdlib.Queue.t

let create = Stdlib.Queue.create
let clear = [%eta1 Stdlib.Queue.clear]
let copy = Stdlib.Queue.copy
let is_empty = Stdlib.Queue.is_empty
let length = Stdlib.Queue.length
let peek = [%eta1 Stdlib.Queue.peek]
let pop = [%eta1 Stdlib.Queue.pop]
let push = Stdlib.Queue.push
let transfer = [%eta2 Stdlib.Queue.transfer]
let iter t ~f = Stdlib.Queue.iter f t
let fold t ~init ~f = Stdlib.Queue.fold f init t
