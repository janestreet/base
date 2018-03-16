open! Core_kernel

module Debug (Queue : module type of Queue) : sig

  (** The following [include] exposes the type equivalence [Debug(Queue).t = Queue.t]. *)
  include module type of struct include Queue end

  val check_invariant : bool ref
  val show_messages   : bool ref

end
