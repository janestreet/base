open! Import

(** An interface for queues that follows Base's conventions, as opposed to OCaml's
    standard [Queue] module. *)
module type S = sig
  type 'a t [@@deriving_inline sexp, sexp_grammar]

  include Sexplib0.Sexpable.S1 with type 'a t := 'a t

  val t_sexp_grammar : 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t

  [@@@end]

  include Indexed_container.S1 with type 'a t := 'a t

  (** [singleton a] returns a queue with one element. *)
  val singleton : 'a -> 'a t

  (** [of_list list] returns a queue [t] with the elements of [list] in the same order as
      the elements of [list] (i.e. the first element of [t] is the first element of the
      list). *)
  val of_list : 'a list -> 'a t

  val of_array : 'a array -> 'a t

  (** [init n ~f] is equivalent to [of_list (List.init n ~f)]. *)
  val init : int -> f:(int -> 'a) -> 'a t

  (** [enqueue t a] adds [a] to the end of [t].*)
  val enqueue : 'a t -> 'a -> unit

  (** [enqueue_all t list] adds all elements in [list] to [t] in order of [list]. *)
  val enqueue_all : 'a t -> 'a list -> unit

  (** [dequeue t] removes and returns the front element of [t], if any. *)
  val dequeue : 'a t -> 'a option

  val dequeue_exn : 'a t -> 'a

  (** [dequeue_and_ignore_exn t] removes the front element of [t], or raises if the queue
      is empty. *)
  val dequeue_and_ignore_exn : 'a t -> unit

  (** [drain t ~f ~while_] repeatedly calls [while_] on the head of [t], and if it returns
      true then dequeues it and calls [f] on it. It stops when [t] is empty or [while_]
      returns false. A common use case is tracking the sum of data in a recent time
      interval: [t] contains timestamped data, [while_] checks for elements with an old
      timestamp, and [f] subtracts data from the sum. *)
  val drain : 'a t -> f:('a -> unit) -> while_:('a -> bool) -> unit

  (** [peek t] returns but does not remove the front element of [t], if any. *)
  val peek : 'a t -> 'a option

  val peek_exn : 'a t -> 'a

  (** [clear t] discards all elements from [t]. *)
  val clear : _ t -> unit

  (** [copy t] returns a copy of [t]. *)
  val copy : 'a t -> 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t

  (** Creates a new queue with elements equal to [List.concat_map ~f (to_list t)]. *)
  val concat_map : 'a t -> f:('a -> 'b list) -> 'b t

  val concat_mapi : 'a t -> f:(int -> 'a -> 'b list) -> 'b t

  (** [filter_map] creates a new queue with elements equal to [List.filter_map ~f (to_list
      t)]. *)
  val filter_map : 'a t -> f:('a -> 'b option) -> 'b t

  val filter_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b t

  (** [filter] is like [filter_map], except with [List.filter]. *)
  val filter : 'a t -> f:('a -> bool) -> 'a t

  val filteri : 'a t -> f:(int -> 'a -> bool) -> 'a t

  (** [filter_inplace t ~f] removes all elements of [t] that don't satisfy [f].  If [f]
      raises, [t] is unchanged.  This is inplace in that it modifies [t]; however, it uses
      space linear in the final length of [t]. *)
  val filter_inplace : 'a t -> f:('a -> bool) -> unit

  val filteri_inplace : 'a t -> f:(int -> 'a -> bool) -> unit
end

module type Queue = sig
  (** A queue implemented with an array.

      The implementation will grow the array as necessary.  The array will
      never automatically be shrunk, but the size can be interrogated and set
      with [capacity] and [set_capacity].

      Iteration functions ([iter], [fold], [map], [concat_map], [filter],
      [filter_map], [filter_inplace], and some functions from [Container.S1])
      will raise if the queue is modified during iteration.

      Also see {!Linked_queue}, which has different performance characteristics. *)

  module type S = S

  type 'a t [@@deriving_inline compare ~localize, globalize]

  include Ppx_compare_lib.Comparable.S1 with type 'a t := 'a t
  include Ppx_compare_lib.Comparable.S_local1 with type 'a t := 'a t

  val globalize : ('a -> 'a) -> 'a t -> 'a t

  [@@@end]

  include S with type 'a t := 'a t
  include Equal.S1 with type 'a t := 'a t
  include Ppx_compare_lib.Equal.S_local1 with type 'a t := 'a t
  include Invariant.S1 with type 'a t := 'a t

  (** Create an empty queue. *)
  val create : ?capacity:int (** default is [1]. *) -> unit -> _ t

  (** [last t] returns the most recently enqueued element in [t], if any. *)
  val last : 'a t -> 'a option

  val last_exn : 'a t -> 'a

  (** Add an element to the front of the queue, as opposed to [enqueue] which adds to the
      back of the queue. *)
  val enqueue_front : 'a t -> 'a -> unit

  (** [dequeue_back t] removes and returns the back element of [t], if any. *)
  val dequeue_back : 'a t -> 'a option

  val dequeue_back_exn : 'a t -> 'a

  (** [peek_back t] returns but does not remove the back element of [t], if any. *)
  val peek_back : 'a t -> 'a option

  val peek_back_exn : 'a t -> 'a

  (** Transfers up to [len] elements from the front of [src] to the end of [dst], removing
      them from [src].  It is an error if [len < 0].

      Aside from a call to [set_capacity dst] if needed, runs in O([len]) time *)
  val blit_transfer
    :  src:'a t
    -> dst:'a t
    -> ?len:int (** default is [length src] *)
    -> unit
    -> unit

  (** [get t i] returns the [i]'th element in [t], where the 0'th element is at the front of
      [t] and the [length t - 1] element is at the back. *)
  val get : 'a t -> int -> 'a

  val set : 'a t -> int -> 'a -> unit

  (** Returns the current length of the backing array. *)
  val capacity : _ t -> int

  (** [set_capacity t c] sets the capacity of [t]'s backing array to at least [max c (length
      t)].  If [t]'s capacity changes, then this involves allocating a new backing array and
      copying the queue elements over.  [set_capacity] may decrease the capacity of [t], if
      [c < capacity t]. *)
  val set_capacity : _ t -> int -> unit

  (** Use [Iteration] to implement iteration functions that guard against mutation. *)
  module Iteration : sig
    type 'a queue := 'a t

    (** A token representing state from the beginning of an iteration. *)
    type t [@@immediate]

    (** Capture state at the start of iteration. *)
    val start : _ queue -> t

    (** [assert_no_mutation_since_start t queue] raises if any mutation has happened to
        [queue] since [t] was created by [start queue]. Results are unspecified if you
        call [assert_no_mutation_since_start] with a different queue from the one passed
        to [start].

        Call [assert_no_mutation_since_start] after each step of an iteration loop, before
        checking if the queue is empty or advancing to the next element. This ensures
        these read operations are consistent with the queue's state at the start of
        iteration. *)
    val assert_no_mutation_since_start : t -> _ queue -> unit
  end
end
