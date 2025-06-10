@@ portable

type 'a t = 'a Stdlib.Queue.t

val create : unit -> 'a t
val clear : 'a t -> unit [@@zero_alloc]
val copy : 'a t -> 'a t
val is_empty : 'a t -> bool
val length : 'a t -> int
val peek : 'a t -> 'a [@@zero_alloc]
val pop : 'a t -> 'a [@@zero_alloc]
val push : 'a -> 'a t -> unit
val transfer : 'a t -> 'a t -> unit
val iter : 'a t -> f:('a -> unit) @ local -> unit
val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) @ local -> 'b
