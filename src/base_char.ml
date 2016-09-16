open! Import

module Char  = Caml.Char
module List  = Caml.ListLabels
module Array = Caml.ArrayLabels

let failwithf = Base_printf.failwithf

module T = struct
  type t = char [@@deriving hash, sexp]

  let compare = Char.compare

  let to_string t = String.make 1 t

  let of_string s =
    match String.length s with
    | 1 -> String.get s 0
    | _ -> failwithf "Char.of_string: %S" s ()
end
include T
include Identifiable.Make (struct
  include T
  let module_name = "Core.Std.Char"
end)

let to_int = Char.code

let unsafe_of_int = Char.unsafe_chr

(* We use our own range test when converting integers to chars rather than
   calling [Caml.Char.chr] because it's simple and it saves us a function call
   and the try-with (exceptions cost, especially in the world with backtraces). *)
let int_is_ok i =
  let open Int_replace_polymorphic_compare in
  0 <= i && i <= 255

let min_value = unsafe_of_int 0
let max_value = unsafe_of_int 255

let of_int i =
  if int_is_ok i
  then Some (unsafe_of_int i)
  else None
;;

let of_int_exn i =
  if int_is_ok i
  then unsafe_of_int i
  else failwithf "Char.of_int_exn got integer out of range: %d" i ()
;;

let escaped = Char.escaped

let lowercase = Char.lowercase_ascii

let uppercase = Char.uppercase_ascii

let is_lowercase = function
  | 'a' .. 'z' -> true
  | _ -> false

let is_uppercase = function
  | 'A' .. 'Z' -> true
  | _ -> false

let is_print = function
  | ' ' .. '~' -> true
  | _ -> false

let is_whitespace = function
  | '\t'
  | '\n'
  | '\011' (* vertical tab *)
  | '\012' (* form feed *)
  | '\r'
  | ' '
    -> true
  | _
    -> false
;;

let%test _ = not (is_whitespace '\008') (* backspace *)
let%test _ =      is_whitespace '\009'  (* '\t': horizontal tab *)
let%test _ =      is_whitespace '\010'  (* '\n': line feed *)
let%test _ =      is_whitespace '\011'  (* '\v': vertical tab *)
let%test _ =      is_whitespace '\012'  (* '\f': form feed *)
let%test _ =      is_whitespace '\013'  (* '\r': carriage return *)
let%test _ = not (is_whitespace '\014') (* shift out *)
let%test _ =      is_whitespace '\032'  (* space *)

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

(* Writing these out, instead of calling [is_alpha] and [is_digit], reduces
   runtime by approx. 30% *)
let is_alphanum = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false

let get_digit_unsafe t = to_int t - to_int '0'

let get_digit_exn t =
  if is_digit t
  then get_digit_unsafe t
  else failwithf "Char.get_digit_exn %C: not a digit" t ()
;;

let get_digit t = if is_digit t then Some (get_digit_unsafe t) else None
