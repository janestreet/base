(** Versions of [Obj] functions that work with locals. *)

open! Import

type t = Stdlib.Obj.t
type raw_data = Stdlib.Obj.raw_data

external magic : (_[@local_opt]) -> (_[@local_opt]) = "%identity"
external repr : (_[@local_opt]) -> (t[@local_opt]) = "%identity"
external obj : (t[@local_opt]) -> (_[@local_opt]) = "%identity"
external raw_field : (t[@local_opt]) -> int -> raw_data = "caml_obj_raw_field"

external set_raw_field
  :  (t[@local_opt])
  -> int
  -> raw_data
  -> unit
  = "caml_obj_set_raw_field"

external tag : (t[@local_opt]) -> int = "caml_obj_tag" [@@noalloc]
