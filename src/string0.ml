(* [String0] defines string functions that are primitives or can be simply defined in
   terms of [Stdlib.String]. [String0] is intended to completely express the part of
   [Stdlib.String] that [Base] uses -- no other file in Base other than string0.ml should
   use [Stdlib.String].  [String0] has few dependencies, and so is available early in Base's
   build order.

   All Base files that need to use strings, including the subscript syntax [x.[i]] which
   the OCaml parser desugars into calls to [String], and come before [Base.String] in
   build order should do

   {[
     module String = String0
   ]}

   Defining [module String = String0] is also necessary because it prevents
   ocamldep from mistakenly causing a file to depend on [Base.String]. *)

open! Import0
module Sys = Sys0

module String = struct
  external get : (string[@local_opt]) -> (int[@local_opt]) -> char = "%string_safe_get"
  external length : (string[@local_opt]) -> int = "%string_length"

  external unsafe_get
    :  (string[@local_opt])
    -> (int[@local_opt])
    -> char
    = "%string_unsafe_get"
end

include String

let max_length = Sys.max_string_length
let ( ^ ) = ( ^ )
let capitalize = Stdlib.String.capitalize_ascii
let compare = Stdlib.String.compare
let escaped = Stdlib.String.escaped
let lowercase = Stdlib.String.lowercase_ascii
let make = Stdlib.String.make
let sub = Stdlib.String.sub
let uncapitalize = Stdlib.String.uncapitalize_ascii
let unsafe_blit = Stdlib.String.unsafe_blit
let uppercase = Stdlib.String.uppercase_ascii
let split_on_char = Stdlib.String.split_on_char

let concat ?(sep = "") l =
  match l with
  | [] -> ""
  (* The stdlib does not specialize this case because it could break existing projects. *)
  | [ x ] -> x
  | l -> Stdlib.String.concat ~sep l
;;

let iter t ~f:(f [@local]) =
  for i = 0 to length t - 1 do
    f (unsafe_get t i)
  done
;;
