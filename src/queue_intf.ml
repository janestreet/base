(** An interface for queues that follows Base's conventions, as opposed to OCaml's
    standard [Queue] module. *)

open! Import

module type S = sig
  type 'a t [@@deriving_inline sexp]
  include
  sig
    [@@@ocaml.warning "-32"]
    val t_of_sexp : (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a t
    val sexp_of_t : ('a -> Sexplib.Sexp.t) -> 'a t -> Sexplib.Sexp.t
  end
  [@@@end]

  include Indexed_container.S1 with type 'a t := 'a t

  (** [create ()] returns an empty queue. *)
  val create : unit -> _ t

  (** [singleton a] returns a queue with one element. *)
  val singleton : 'a -> 'a t

  (** [of_list list] returns a queue [t] with the elements of [list] in the same order as
      the elements of [list] (i.e. the first element of [t] is the first element of the
      list). *)
  val of_list  : 'a list  -> 'a t
  val of_array : 'a array -> 'a t

  (** [init n ~f] is equivalent to [of_list (List.init n ~f)] *)
  val init : int -> f:(int -> 'a) -> 'a t

  (** [enqueue t a] adds [a] to the end of [t].*)
  val enqueue : 'a t -> 'a -> unit

  (** [enqueue_all t list] adds all elements in [list] to [t] in order of [list]. *)
  val enqueue_all : 'a t -> 'a list -> unit

  (** [dequeue t] removes and returns the front element of [t], if any. *)
  val dequeue     : 'a t -> 'a option
  val dequeue_exn : 'a t -> 'a

  (** [peek t] returns but does not remove the front element of [t], if any. *)
  val peek     : 'a t -> 'a option
  val peek_exn : 'a t -> 'a

  (** [clear t] discards all elements from [t]. *)
  val clear : _ t -> unit

  (** [copy t] returns a copy of [t]. *)
  val copy : 'a t -> 'a t

  val map  : 'a t -> f:(       'a -> 'b) -> 'b t
  val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t

  (** creates a new queue with elements equal to [List.concat_map ~f (to_list t)]. *)
  val concat_map  : 'a t -> f:(       'a -> 'b list) -> 'b t
  val concat_mapi : 'a t -> f:(int -> 'a -> 'b list) -> 'b t

  (** [filter_map] creates a new queue with elements equal to [List.filter_map ~f (to_list
      t)]. *)
  val filter_map  : 'a t -> f:(       'a -> 'b option) -> 'b t
  val filter_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b t

  (** [filter] is like [filter_map], except with [List.filter]. *)
  val filter  : 'a t -> f:(       'a -> bool) -> 'a t
  val filteri : 'a t -> f:(int -> 'a -> bool) -> 'a t

  (** [filter_inplace t ~f] removes all elements of [t] that don't satisfy [f].  If [f]
      raises, [t] is unchanged.  This is inplace in that it modifies [t]; however, it uses
      space linear in the final length of [t]. *)
  val filter_inplace  : 'a t -> f:(       'a -> bool) -> unit
  val filteri_inplace : 'a t -> f:(int -> 'a -> bool) -> unit

end

