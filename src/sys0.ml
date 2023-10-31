(* [Sys0] defines functions that are primitives or can be simply defined in
   terms of [Stdlib.Sys].  [Sys0] is intended to completely express the part of
   [Stdlib.Sys] that [Base] uses -- no other file in Base other than sys.ml
   should use [Stdlib.Sys]. [Sys0] has few dependencies, and so is available
   early in Base's build order.  All Base files that need to use these
   functions and come before [Base.Sys] in build order should do
   [module Sys = Sys0].  Defining [module Sys = Sys0] is also necessary because
   it prevents ocamldep from mistakenly causing a file to depend on [Base.Sys]. *)

open! Import0

type backend_type = Stdlib.Sys.backend_type =
  | Native
  | Bytecode
  | Other of string

let backend_type = Stdlib.Sys.backend_type
let interactive = Stdlib.Sys.interactive
let os_type = Stdlib.Sys.os_type
let unix = Stdlib.Sys.unix
let win32 = Stdlib.Sys.win32
let cygwin = Stdlib.Sys.cygwin
let word_size_in_bits = Stdlib.Sys.word_size
let int_size_in_bits = Stdlib.Sys.int_size
let big_endian = Stdlib.Sys.big_endian
let max_string_length = Stdlib.Sys.max_string_length
let max_array_length = Stdlib.Sys.max_array_length
let runtime_variant = Stdlib.Sys.runtime_variant
let runtime_parameters = Stdlib.Sys.runtime_parameters
let argv = Stdlib.Sys.argv
let get_argv () = Stdlib.Sys.argv
let ocaml_version = Stdlib.Sys.ocaml_version
let enable_runtime_warnings = Stdlib.Sys.enable_runtime_warnings
let runtime_warnings_enabled = Stdlib.Sys.runtime_warnings_enabled

module Make_immediate64
  (Imm : Stdlib.Sys.Immediate64.Immediate)
  (Non_imm : Stdlib.Sys.Immediate64.Non_immediate) =
  Stdlib.Sys.Immediate64.Make (Imm) (Non_imm)

let getenv_exn var =
  try Stdlib.Sys.getenv var with
  | Stdlib.Not_found ->
    Printf.failwithf "Sys.getenv_exn: environment variable %s is not set" var ()
;;

let getenv var =
  match Stdlib.Sys.getenv var with
  | x -> Some x
  | exception Stdlib.Not_found -> None
;;

external opaque_identity : ('a[@local_opt]) -> ('a[@local_opt]) = "%opaque"
external opaque_identity_global : 'a -> 'a = "%opaque"

exception Break = Stdlib.Sys.Break
