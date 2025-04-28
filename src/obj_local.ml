open! Import

type t = Stdlib.Obj.t
type raw_data = Stdlib.Obj.raw_data

external magic : (_[@local_opt]) -> (_[@local_opt]) = "%identity"
external repr : (_[@local_opt]) -> (t[@local_opt]) = "%identity"
external obj : (t[@local_opt]) -> (_[@local_opt]) = "%identity"
external size : (t[@local_opt]) -> int = "%obj_size"

let[@inline always] size t = size (Sys.opaque_identity t)

external is_int : (t[@local_opt]) -> bool = "%obj_is_int"

(* The result doesn't need to be marked local because the data is copied into a fresh
   nativeint block regardless. *)
external raw_field : (t[@local_opt]) -> int -> raw_data = "caml_obj_raw_field"

external set_raw_field
  :  (t[@local_opt])
  -> int
  -> raw_data
  -> unit
  = "caml_obj_set_raw_field"

external tag : (t[@local_opt]) -> int = "caml_obj_tag" [@@noalloc]

(* Checks if the given value is on the local stack. Returns [false] for immediates. *)
external is_stack : (t[@local_opt]) -> bool = "caml_dummy_obj_is_stack"

type stack_or_heap =
  | Immediate
  | Stack
  | Heap
[@@deriving sexp ~localize, compare]

let stack_or_heap repr =
  if is_int repr
  then Immediate
  else (
    match Sys.backend_type with
    | Sys.Native -> if is_stack repr then Stack else Heap
    | Sys.Bytecode -> Heap
    | Sys.Other _ -> Heap)
;;
