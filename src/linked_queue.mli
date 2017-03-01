(** This module is a wrapper around OCaml's standard [Queue] module that follows Base
    idioms and adds some functions.  See [Queue_intf] for documentation of standard queue
    functions. *)

open! Import

include Queue_intf.S with type 'a t = 'a Linked_queue0.t

(** [transfer ~src ~dst] adds all of the elements of [src] to the end of [dst], then
    clears [src].  It is equivalent to the sequence:

    {[
      iter ~src ~f:(enqueue dst);
      clear src
    ]}

    but runs in constant time. *)
val transfer : src:'a t -> dst:'a t -> unit
