module Either = Either0

module type List0 = sig
  module Constructors : sig
    (*_ Expose the main [t] with explicit mangling. Having this last also makes it the
        default constructors. *)
    type 'a t = 'a list =
      | []
      | ( :: ) of 'a * 'a t
  end

  open Constructors

  val max_non_tailcall : int
  val hd_exn : 'a. 'a t -> 'a
  val tl_exn : 'a. 'a t -> 'a t
  val unzip : 'a 'b. ('a * 'b) t -> 'a t * 'b t
  val is_empty : 'a. 'a list -> bool

  val partition_map
    : 'a 'b 'c.
    'a list -> f:('a -> ('b, 'c) Either.t) -> 'b list * 'c list

  val length : 'a. 'a t -> int
  val exists : 'a. 'a t -> f:('a -> bool) -> bool
  val iter : 'a. 'a t -> f:('a -> unit) -> unit
  val rev_append : 'a. 'a t -> 'a t -> 'a t
  val rev : 'a. 'a t -> 'a t
  val for_all : 'a. 'a t -> f:('a -> bool) -> bool
  val fold : 'a 'b. 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
  val rev_map : 'a 'b. 'a t -> f:('a -> 'b) -> 'b t
  val fold2_ok : 'a 'b 'c. 'a t -> 'b t -> init:'c -> f:('c -> 'a -> 'b -> 'c) -> 'c
  val exists2_ok : 'a 'b. 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool
  val iter2_ok : 'a 'b. 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit
  val for_all2_ok : 'a 'b. 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool
  val map : 'a 'b. 'a t -> f:('a -> 'b) -> 'b t
  val rev_map2_ok : 'a 'b 'c. 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val mapi : 'a 'b. 'a t -> f:(int -> 'a -> 'b) -> 'b t
  val partition : 'a. 'a t -> f:('a -> bool) -> 'a t * 'a t
  val fold_right : 'a 'acc. 'a t -> f:('a -> 'acc -> 'acc) -> init:'acc -> 'acc
  val fold_right2_ok : 'a 'b 'c. 'a t -> 'b t -> f:('a -> 'b -> 'c -> 'c) -> init:'c -> 'c
end
