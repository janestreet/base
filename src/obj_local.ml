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
[@@deriving_inline sexp, compare]

let stack_or_heap_of_sexp =
  (let error_source__003_ = "obj_local.ml.stack_or_heap" in
   function
   | Sexplib0.Sexp.Atom ("immediate" | "Immediate") -> Immediate
   | Sexplib0.Sexp.Atom ("stack" | "Stack") -> Stack
   | Sexplib0.Sexp.Atom ("heap" | "Heap") -> Heap
   | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("immediate" | "Immediate") :: _) as
     sexp__004_ -> Sexplib0.Sexp_conv_error.stag_no_args error_source__003_ sexp__004_
   | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("stack" | "Stack") :: _) as sexp__004_ ->
     Sexplib0.Sexp_conv_error.stag_no_args error_source__003_ sexp__004_
   | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("heap" | "Heap") :: _) as sexp__004_ ->
     Sexplib0.Sexp_conv_error.stag_no_args error_source__003_ sexp__004_
   | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__002_ ->
     Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__003_ sexp__002_
   | Sexplib0.Sexp.List [] as sexp__002_ ->
     Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__003_ sexp__002_
   | sexp__002_ -> Sexplib0.Sexp_conv_error.unexpected_stag error_source__003_ sexp__002_
    : Sexplib0.Sexp.t -> stack_or_heap)
;;

let sexp_of_stack_or_heap =
  (function
   | Immediate -> Sexplib0.Sexp.Atom "Immediate"
   | Stack -> Sexplib0.Sexp.Atom "Stack"
   | Heap -> Sexplib0.Sexp.Atom "Heap"
    : stack_or_heap -> Sexplib0.Sexp.t)
;;

let compare_stack_or_heap = (Stdlib.compare : stack_or_heap -> stack_or_heap -> int)

[@@@end]

let stack_or_heap repr =
  if is_int repr
  then Immediate
  else (
    match Sys.backend_type with
    | Sys.Native -> if is_stack repr then Stack else Heap
    | Sys.Bytecode -> Heap
    | Sys.Other _ -> Heap)
;;
