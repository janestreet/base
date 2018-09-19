(* [String0] defines string functions that are primitives or can be simply defined in
   terms of [Caml.String]. [String0] is intended to completely express the part of
   [Caml.String] that [Base] uses -- no other file in Base other than string0.ml should
   use [Caml.String].  [String0] has few dependencies, and so is available early in Base's
   build order.

   All Base files that need to use strings, including the subscript syntax
   [x.(i)] or [x.(i) <- e] which the OCaml parser desugars into calls to
   [String], and come before [Base.String] in build order should do

   {[
     module String = String0]
   ]}

   Defining [module String = String0] is also necessary because it prevents
   ocamldep from mistakenly causing a file to depend on [Base.String]. *)

let capitalize      = Caml.String.capitalize_ascii
let lowercase       = Caml.String.lowercase_ascii
let uncapitalize    = Caml.String.uncapitalize_ascii
let uppercase       = Caml.String.uppercase_ascii

open! Import0

module Sys = Sys0

module String = struct
  external get        : string -> int -> char = "%string_safe_get"
  external length     : string        -> int  = "%string_length"
  external unsafe_get : string -> int -> char = "%string_unsafe_get"

  include Bytes_set_primitives
end

include String

let max_length = Sys.max_string_length

let (^) = (^)

let blit            = Caml.String.blit
let compare         = Caml.String.compare
let copy            = Caml.String.copy [@@warning "-3"]
let escaped         = Caml.String.escaped
let index_exn       = Caml.String.index
let index_from_exn  = Caml.String.index_from
let make            = Caml.String.make
let rindex_exn      = Caml.String.rindex
let rindex_from_exn = Caml.String.rindex_from
let trim            = Caml.String.trim
let sub             = Caml.String.sub
let unsafe_blit     = Caml.String.unsafe_blit

let concat ?(sep = "") l =
  match l with
  | [] -> ""
  (* The stdlib does not specialize this case because it could break existing projects. *)
  | [x] -> x
  | l -> Caml.String.concat ~sep l

(* These are eta expanded in order to permute parameter order to follow Base
   conventions. *)
let iter t ~f = Caml.String.iter t ~f
