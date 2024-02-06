(** Versions of [Obj] functions that work with locals. *)

open! Import

type t = Stdlib.Obj.t
type raw_data = Stdlib.Obj.raw_data

external magic : (_[@local_opt]) -> (_[@local_opt]) = "%identity"
external repr : (_[@local_opt]) -> (t[@local_opt]) = "%identity"
external obj : (t[@local_opt]) -> (_[@local_opt]) = "%identity"
external raw_field : (t[@local_opt]) -> int -> raw_data = "caml_obj_raw_field"
val size : t -> int
external is_int : (t[@local_opt]) -> bool = "%obj_is_int"

external set_raw_field
  :  (t[@local_opt])
  -> int
  -> raw_data
  -> unit
  = "caml_obj_set_raw_field"

external tag : (t[@local_opt]) -> int = "caml_obj_tag" [@@noalloc]

type stack_or_heap =
  | Immediate
  | Stack
  | Heap
[@@deriving_inline sexp, compare]

val sexp_of_stack_or_heap : stack_or_heap -> Sexplib0.Sexp.t
val stack_or_heap_of_sexp : Sexplib0.Sexp.t -> stack_or_heap
val compare_stack_or_heap : stack_or_heap -> stack_or_heap -> int

[@@@end]

(** Checks if a value is immediate, stack-allocated, or heap-allocated. *)
val stack_or_heap : t -> stack_or_heap
